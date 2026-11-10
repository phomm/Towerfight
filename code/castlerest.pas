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
      class var FActiveRequests: Integer;
    public
      class function ServerRequest(const AUrl: string; ACompletedEvent: TRetStringEvent; 
        const ABody: string = ''; const AHeaders: string = ''; const AMethod: string = '';
        const ATrackRunning: Boolean = True): Boolean;
      class function IsRunning(): Boolean;
  end;

  function ParseJsonArray(AJson: TJSONStringType; out AJsonArray: TJSONArray): Boolean;
  function ParseJsonObject(AJson: TJSONStringType; out AJsonObject: TJSONObject): Boolean;
  procedure ReadJsonToObject<T>(AJson: TJSONObject; AObject: T);
  function Serialize(AObject: TObject): TJSONObject;

  const 
    ServerApiUrl = 'https://towerfightserver.onrender.com/api/leaders'; //'https://towerfight.ephyre.ru/api/leaders'; //}'https://localhost:7150/api/leaders';//'https://towerfightserver.onrender.com/api/leaders';  
    ServerLoggingApiUrl = 'https://towerfightserver.onrender.com/api/banzai-log';

implementation

uses
// System
  SysUtils, TypInfo, fpjsonrtti, jsonparser, jsonscanner,
// Castle
  CastleLog, CastleUriUtils, CastleStringUtils, CastleClassUtils, CastleHttps, 
// Own
  Common
;

type
  TCastleRestRequest = class
  private
    FDownload: TCastleDownload;
    FCompletedEvent: TRetStringEvent;
    FTrackRunning: Boolean;
    procedure RequestFinish(const ASender: TCastleDownload; var AFreeSender: Boolean);
  public
    constructor Create(const ACompletedEvent: TRetStringEvent; ATrackRunning: Boolean);
    destructor Destroy; override;
    function Start(const AUrl, ABody, AHeaders, AMethod: string): Boolean;
  end;

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

constructor TCastleRestRequest.Create(const ACompletedEvent: TRetStringEvent; ATrackRunning: Boolean);
begin
  inherited Create;
  FCompletedEvent := ACompletedEvent;
  FTrackRunning := ATrackRunning;
  FDownload := TCastleDownload.Create(nil);
  FDownload.OnFinish := RequestFinish;
end;

destructor TCastleRestRequest.Destroy;
begin
  FreeAndNil(FDownload);
  inherited;
end;

function TCastleRestRequest.Start(const AUrl, ABody, AHeaders, AMethod: string): Boolean;
var
  LStringStream: TStringStream;
  LHeaderLines: TStringList;
  LHeader, LHeaderName, LHeaderValue, LMethodName: string;
  LSeparator, LMethodOrdinal: Integer;
  LHasContentType: Boolean;
begin
  Result := False;
  FDownload.Url := AUrl;
  LMethodName := Trim(AMethod);
  if LMethodName = '' then
    if ABody <> '' then
      LMethodName := 'POST'
    else
      LMethodName := 'GET';
  LMethodOrdinal := GetEnumValue(TypeInfo(THttpMethod), 'hm' + LMethodName);
  if LMethodOrdinal < 0 then
    raise EArgumentException.CreateFmt('Unsupported HTTP method: %s', [LMethodName]);
  FDownload.HttpMethod := THttpMethod(LMethodOrdinal);

  LHasContentType := False;
  LHeaderLines := TStringList.Create;
  try
    LHeaderLines.Text := AHeaders;
    for LHeader in LHeaderLines do
    begin
      if Trim(LHeader) = '' then
        Continue;
      LSeparator := Pos(':', LHeader);
      if LSeparator < 2 then
        raise EArgumentException.CreateFmt('Invalid HTTP header: %s', [LHeader]);
      LHeaderName := Trim(Copy(LHeader, 1, LSeparator - 1));
      if LHeaderName = '' then
        raise EArgumentException.CreateFmt('Invalid HTTP header: %s', [LHeader]);
      LHeaderValue := Trim(Copy(LHeader, LSeparator + 1, MaxInt));
      if SameText(LHeaderName, 'Content-Type') then
        LHasContentType := True;
      FDownload.HttpHeader(LHeaderName, LHeaderValue);
    end;
  finally
    FreeAndNil(LHeaderLines);
  end;

  if ABody <> '' then
  begin
    if not LHasContentType then
      FDownload.HttpHeader('Content-Type', 'application/json');
    LStringStream := TStringStream.Create(ABody);
    try
      FDownload.HttpRequestBody.CopyFrom(LStringStream, 0);
    finally
      FreeAndNil(LStringStream);
    end;
  end;

  WriteLnLog('Network: ' + LMethodName + ' request to ' + AUrl);
  if FTrackRunning then
    Inc(TCastleRest.FActiveRequests);
  try
    FDownload.Start();
    Result := True;
  except
    if FTrackRunning then
      Dec(TCastleRest.FActiveRequests);
    raise;
  end;
end;

procedure TCastleRestRequest.RequestFinish(const ASender: TCastleDownload; var AFreeSender: Boolean);
const
  NL = #13#10;
var
  LHttpResponseHeaders, LContent: string;
  LIsError: Boolean;
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
  FDownload := nil;
  if FTrackRunning then
    Dec(TCastleRest.FActiveRequests);
  try
    LContent := '';
    if Assigned(ASender.HttpResponseHeaders) then
      LHttpResponseHeaders := ASender.HttpResponseHeaders.Text
    else
      LHttpResponseHeaders := '';
    LIsError := (ASender.Status = dsError) or (ASender.HttpResponseCode >= 400);

    if LIsError then
      WriteLnLog(Format('Network: Downloading "%s" failed: %s.' + NL +
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
      {$IFDEF Debug1}
      WriteLnLog(Format('Network: Downloading "%s" successful.' + NL +
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
    if Assigned(ASender.Contents) and (ASender.Contents.Size > 0) then
      LContent := StreamToString(ASender.Contents);
    if (LContent = '') and LIsError then
      LContent := Format('Server response code: %d %s', [ASender.HttpResponseCode, Status()]);

    try
      FCompletedEvent(LContent, not LIsError);
    except
      WriteLnLog('Network: Failed processing client event after request completion: ' + LContent);
    end;
  finally
    Free;
  end;
end;

class function TCastleRest.ServerRequest(const AUrl: string; ACompletedEvent: TRetStringEvent;
  const ABody: string = ''; const AHeaders: string = ''; const AMethod: string = '';
  const ATrackRunning: Boolean = True): Boolean;
var
  LRequest: TCastleRestRequest;
begin
  Result := False;
  if not Assigned(ACompletedEvent) then
    raise EArgumentNilException.Create('ACompletedEvent');
  LRequest := TCastleRestRequest.Create(ACompletedEvent, ATrackRunning);
  try
    WriteLnLog('Network: POST request to ' + AUrl + ' with body: ' + ABody);
    Result := LRequest.Start(AUrl, ABody, AHeaders, AMethod);
  except
    on E: Exception do
    begin
      WriteLnLog('Network: Failed to start request to "' + AUrl + '": ' + E.Message);
      LRequest.Free;
    end;
  end;
end;

class function TCastleRest.IsRunning(): Boolean;
begin
  Result := FActiveRequests > 0;
end;

end.