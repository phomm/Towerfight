unit CastleRest;

{$mode delphi}

interface

uses
// System
  Classes,
  // Castle
  castledownload
  ;

type
  TRetStringEvent = procedure(const AStr: string) of object;

  TCastleRest = class
    private
      class var FEvent: TRetStringEvent;
      class procedure RequestFinish(const ASender: TCastleDownload; var AFreeSender: Boolean);
      class procedure OnServerRequestCompleted(const AContent: string; AEventKey: Integer);  
    public
      class function ServerRequest(const AUrl: string; ACompletedEvent: TRetStringEvent; 
        const ABody: string = ''; const AHeaders: string = ''; const AMethod: string = ''): Boolean;
  end;

implementation

uses
// System
  SysUtils,
// Castle
  CastleLog, CastleUriUtils, CastleStringUtils, CastleClassUtils, CastleHttps, 
// Own
  Common
;

class function TCastleRest.ServerRequest(const AUrl: string; ACompletedEvent: TRetStringEvent; 
  const ABody: string = ''; const AHeaders: string = ''; const AMethod: string = ''): Boolean;
var
  LDownload: TCastleDownload;
  LStringStream: TStringStream;
  LHeaderParts: TArray<string>;
  LMethod: string;
  procedure SetMethod();
  begin
    if AMethod = 'POST' then
      LDownload.HttpMethod := hmPost
    else if AMethod = 'DELETE' then
      LDownload.HttpMethod := hmDelete
    else if ABody <> '' then
      LDownload.HttpMethod := hmPost
    else
      LDownload.HttpMethod := hmGet;
  end;
begin
  Result := False;
  Assert(Assigned(ACompletedEvent), 'NRE:ACompletedEvent');

// https://github.com/castle-engine/castle-engine/blob/master/examples/network/asynchronous_download/code/gameviewmain.pas
  LDownload := TCastleDownload.Create(nil);
  FEvent := ACompletedEvent;
  SetMethod();
  LMethod := EnumName(TypeInfo(THttpMethod), Ord(LDownload.HttpMethod));
  WriteLnLog(LMethod + ' request to ' + AUrl);
  LDownload.Url := AUrl;
  LDownload.OnFinish := RequestFinish;
  if (AHeaders <> '') then
  begin
    LHeaderParts := AHeaders.Split(':');
    if Length(LHeaderParts) = 2 then
      LDownload.HttpHeader(LHeaderParts[0], LHeaderParts[1]);
  end;
  if (ABody <> '') then 
  begin
    LDownload.HttpHeader('Content-Type', 'application/json');
    LStringStream := TStringStream.Create(ABody);
    try
      LDownload.HttpRequestBody.CopyFrom(LStringStream, 0);
    finally
      FreeAndNil(LStringStream);
    end;
  end;

  try  
    LDownload.Start(); // must be last, async call
    Result := True;
  except
  end;
end;

class procedure TCastleRest.RequestFinish(const ASender: TCastleDownload; var AFreeSender: Boolean);
const
  NL = #13#10;
var
  LHttpResponseHeaders, LContent: string;
  function Status(): string;
  begin
    case ASender.HttpResponseCode of
      200: Result := 'OK';
      400: Result := 'Bad Request';
      401: Result := 'Unauthorized';
      403: Result := 'Forbidden';
      404: Result := 'Not Found';
      500: Result := 'Internal Server Error';
    else
      Result := 'Unknown';
    end;
  end;
begin
  AFreeSender := True;
  LContent := '';
  { Gracefully handle the case when ASender.HttpResponseHeaders = nil,
    which will happen if you try to download non-HTTP/HTTPS URL,
    like 'castle-data:/gears.gltf' . }
  if Assigned(ASender.HttpResponseHeaders) then
    LHttpResponseHeaders := ASender.HttpResponseHeaders.Text
  else
    LHttpResponseHeaders := '';

  if ASender.Status = dsError then
    WriteLnLog(Format('Downloading "%s" failed: %s.' + NL +
      'HTTP response code: %d' + NL +
      'HTTP response headers: %s' + NL +
      'Final URL: %s', [
      URIDisplay(ASender.Url),
      ASender.ErrorMessage,
      ASender.HttpResponseCode,
      LHttpResponseHeaders,
      URIDisplay(ASender.FinalUrl)
    ]))
  else
  begin
    {$IFDEF Debug1} // don't spam success
    WriteLnLog(Format('Downloading "%s" successful.' + NL +
      'HTTP response code: %d' + NL +
      'HTTP response headers: %s' + NL +
      'Final URL: %s', [
      URIDisplay(ASender.Url),
      ASender.HttpResponseCode,
      LHttpResponseHeaders,
      URIDisplay(ASender.FinalUrl)
    ]));
    {$ENDIF}
  end;
  if (Assigned(ASender.Contents) and (ASender.Contents.Size > 0)) then
    LContent := StreamToString(ASender.Contents);
  if (LContent = '') and (ASender.Status = dsError) then
    LContent := Format('Server response code: %d %s', [ASender.HttpResponseCode, Status()]);

  OnServerRequestCompleted(LContent, ASender.Tag);
end;

class procedure TCastleRest.OnServerRequestCompleted(const AContent: string; AEventKey: Integer);
var
  LCompletedEvent: TRetStringEvent;
begin
  try
    if Assigned(FEvent) then
      FEvent(AContent);
  except
    WriteLnLog('Failed processing client event after ServerRequestCompleted: ' + AContent);
  end;
end;

end.