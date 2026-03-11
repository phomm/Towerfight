unit CastleRest;

{$mode delphi}

interface

uses
// System
  Classes, fpjson,
  // Castle
  castledownload
  ;

type
  TRetStringEvent = procedure(const AStr: string; ASuccess: Boolean) of object;

  TCastleRest = class
    private
      class var Download: TCastleDownload;
      class var FEvent: TRetStringEvent;
      class procedure RequestFinish(const ASender: TCastleDownload; var AFreeSender: Boolean);
      class procedure OnServerRequestCompleted(const AContent: string; ASuccess: Boolean);  
    public
      class function ServerRequest(const AUrl: string; ACompletedEvent: TRetStringEvent; 
        const ABody: string = ''; const AHeaders: string = ''; const AMethod: string = ''): Boolean;
      class function IsRunning(): Boolean;
  end;

  function ParseJsonArray(AJson: TJSONStringType; out AJsonArray: TJSONArray): Boolean;
  function ParseJsonObject(AJson: TJSONStringType; out AJsonObject: TJSONObject): Boolean;
  procedure ReadJsonToObject<T>(AJson: TJSONObject; AObject: T);
  function Serialize(AObject: TObject): TJSONObject;

  const 
    ServerApiUrl = {'https://localhost:7150/api/leaders';//}'https://towerfightserver.onrender.com/api/leaders';  

implementation

uses
// System
  SysUtils, fpjsonrtti, jsonparser, jsonscanner,
// Castle
  CastleLog, CastleUriUtils, CastleStringUtils, CastleClassUtils, CastleHttps, 
// Own
  Common
;

function ParseJsonArray(AJson: TJSONStringType; out AJsonArray: TJSONArray): Boolean;
begin
  Result := False;
  if AJson[1] <> '[' then
    Exit;
  with TJSONParser.Create(AJson, DefaultOptions) do
  try
    AJsonArray := Parse() as TJSONArray;
    Result := Assigned(AJsonArray);
  finally
    Free();
  end;
end;

function ParseJsonObject(AJson: TJSONStringType; out AJsonObject: TJSONObject): Boolean;
begin
  Result := False;
  if AJson[1] <> '{' then
    Exit;
  with TJSONParser.Create(AJson, DefaultOptions) do
  try
    AJsonObject := Parse() as TJSONObject;
    Result := Assigned(AJsonObject);
  finally
    Free();
  end;
end;

procedure ReadJsonToObject<T>(AJson: TJSONObject; AObject: T);
begin
  with TJSONDeStreamer.Create(nil) do
  try
    Options := [jdoCaseInsensitive, jdoIgnoreNulls, jdoNullClearsProperty];
    JSONToObject(AJson, AObject);
  finally
    Free();
  end;
end;

function Serialize(AObject: TObject): TJSONObject;
begin
  with TJSONStreamer.Create(nil) do
  try
    Options := [jsoLowerPropertyNames];
    Result := ObjectToJSON(AObject);
  finally
    Free();
  end;
end;

class function TCastleRest.ServerRequest(const AUrl: string; ACompletedEvent: TRetStringEvent; 
  const ABody: string = ''; const AHeaders: string = ''; const AMethod: string = ''): Boolean;
var
  LStringStream: TStringStream;
  LHeaderParts: TArray<string>;
  LMethod: string;
  procedure SetMethod();
  begin
    if AMethod = 'POST' then
      Download.HttpMethod := hmPost
    else if AMethod = 'DELETE' then
      Download.HttpMethod := hmDelete
    else if ABody <> '' then
      Download.HttpMethod := hmPost
    else
      Download.HttpMethod := hmGet;
  end;
begin
  Result := False;
  Assert(Assigned(ACompletedEvent), 'NRE:ACompletedEvent');

// https://github.com/castle-engine/castle-engine/blob/master/examples/network/asynchronous_download/code/gameviewmain.pas
  Download := TCastleDownload.Create(nil);
  FEvent := ACompletedEvent;
  SetMethod();
  LMethod := EnumName(TypeInfo(THttpMethod), Ord(Download.HttpMethod));
  WriteLnLog(LMethod + ' request to ' + AUrl);
  Download.Url := AUrl;
  Download.OnFinish := RequestFinish;
  if (AHeaders <> '') then
  begin
    LHeaderParts := AHeaders.Split(':');
    if Length(LHeaderParts) = 2 then
      Download.HttpHeader(LHeaderParts[0], LHeaderParts[1]);
  end;
  if (ABody <> '') then 
  begin
    Download.HttpHeader('Content-Type', 'application/json');
    LStringStream := TStringStream.Create(ABody);
    try
      WriteLnLog(ABody);
      Download.HttpRequestBody.CopyFrom(LStringStream, 0);
    finally
      FreeAndNil(LStringStream);
    end;
  end;

  try
    Download.Start(); // must be last, async call
    Result := True;
  except
  end;
end;

class function TCastleRest.IsRunning(): Boolean;
begin
  Result := Assigned(Download) and (Download.Status = dsDownloading);
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

  Download := nil;
  OnServerRequestCompleted(LContent, ASender.Status = dsSuccess);
end;

class procedure TCastleRest.OnServerRequestCompleted(const AContent: string; ASuccess: Boolean);
begin
  try
    if Assigned(FEvent) then
      FEvent(AContent, ASuccess);
  except
    WriteLnLog('Failed processing client event after ServerRequestCompleted: ' + AContent);
  end;
end;

end.