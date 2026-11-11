{
  Copyright 2020-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Log handler sending logs to server using asynchronous HTTP POST (TLogHandler). }
unit GameLogHandler;

{$mode delphi}

interface

uses 
// System
  SysUtils, Math, Classes,
// Castle
  CastleWindow, CastleLog, CastleApplicationProperties, CastleKeysMouse,
  CastleNotifications;

type
  { Log handler sending logs to server using asynchronous HTTP POST. }
  TLogHandler = class(TComponent)
  strict private
    class var FInstance: TLogHandler;
    InsideLogCallback: Boolean;
    Guid: string;
    class function GetInstance: TLogHandler; static;
    function ShouldSkipLog(const AMessage: string): Boolean;
    procedure HttpPostFinish(const AContent: string; ASuccess: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LogCallback(const Message: String);
    class property Instance: TLogHandler read GetInstance;
  end;

var
  { Set by UI state. }
  LogNotifications: TCastleNotifications;

procedure WriteAnalyticsLog(const AMessage: string);

implementation

uses 
// System
  fpjson,
// Castle
  CastleUtils, CastleStringUtils,
// Own
  castlerest;

const
  Analytics = 'Analytics:';
  IncludedLogPrefixes: array[0..0] of string = (
    Analytics
  );

procedure WriteAnalyticsLog(const AMessage: string);
begin
  WritelnLog(Analytics + ' ' + AMessage);
end;

constructor TLogHandler.Create(AOwner: TComponent);
begin
  inherited;
  Guid := GUIDToString(TGuid.NewGuid).Trim(['{', '}']);
  WritelnLog('Network: Log handler created, guid: ' + Guid);
end;

destructor TLogHandler.Destroy;
begin
  if FInstance = Self then
    FInstance := nil;
  inherited;
end;

class function TLogHandler.GetInstance: TLogHandler;
begin
  if FInstance = nil then
    FInstance := TLogHandler.Create(Application);
  Result := FInstance;
end;

function TLogHandler.ShouldSkipLog(const AMessage: string): Boolean;
var
  LSubstring: string;
begin
  for LSubstring in IncludedLogPrefixes do
    if AMessage.StartsWith(LSubstring) then
      Exit(False);
  Result := True;
end;

procedure TLogHandler.LogCallback(const Message: String);
var
  LJson: TJSONObject;
begin
  { Use InsideLogCallback to prevent from infinite recursion,
    in case anything inside would also cause WritelnLog. }
  if InsideLogCallback then Exit;

  { Skip network and deprecated API messages to avoid noise and recursion. }
  if ShouldSkipLog(Message) then
    Exit;

  InsideLogCallback := true;
  try
    try
      // We use TrimRight to strip traling newline
      LJson := TJSONObject.Create;
      try
        LJson.Add('guid', Guid);
        LJson.Add('entry', TrimRight(Message));
        TCastleRest.ServerRequest(ServerLoggingApiUrl, HttpPostFinish,
          LJson.AsJSON, '', 'POST', False);
      finally
        LJson.Free;
      end;
    except
      { Catch exception, like
          EInOutError: Could not initialize OpenSSL library
        (easily possible on Linux with FPC < 3.3.1 due to libssl incompatibility).
        Display it as nice warning, instead of crashing application. }
      on E: Exception do
        WritelnWarning('Sending log remotely failed: ' + ExceptMessage(E));
    end;
  finally InsideLogCallback := false end;
end;

procedure TLogHandler.HttpPostFinish(const AContent: string; ASuccess: Boolean);
begin
  WritelnLog('Network: Log POST finished, success: ' + BoolToStr(ASuccess, True) + ', content: ' + AContent);
  if LogNotifications <> nil then
  begin
    if ASuccess then
      LogNotifications.Show(Format('SUCCESS: Posted log to "%s".' + NL +
        'Server response: %s', [
        ServerLoggingApiUrl,
        AContent
      ]))
    else
      LogNotifications.Show(Format('ERROR: Cannot post log to "%s".' + NL +
        'Server response: %s', [
        ServerLoggingApiUrl,
        AContent
      ]));
  end;
end;

end.
