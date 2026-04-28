unit hasher;

{$mode delphi}

interface

uses
  SysUtils, Classes,
  {$IF FPC_FULLVERSION >= 30301}
    {$IF unit_exists(fpsha256)}
      {$DEFINE USE_FPC_FPSHA256}
      fpsha256, fphashutils,
    {$ELSE}
      HlpSHA2_256, HlpIHash, HlpIHashResult,
    {$ENDIF}
  {$ELSE}
    HlpSHA2_256, HlpIHash, HlpIHashResult,
  {$ENDIF}
  TypInfo;

function GetSHA256(const AInput: string): string;

implementation

function GetSHA256(const AInput: string): string;
begin
  {$IFDEF USE_FPC_FPSHA256}
    { Logic based on demosha256.pp }
    TSHA256.DigestHexa(TEncoding.UTF8.GetBytes(AInput), Result);
  {$ELSE}
    Result := TSHA2_256.Create().ComputeString(AInput, TEncoding.UTF8).ToString();
  {$ENDIF}
end;

end.