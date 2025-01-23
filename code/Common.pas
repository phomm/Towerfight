unit Common;

{$mode delphi}

interface

uses
// System
  Classes;

function EnumName(PEnum: Pointer; Ordinal: Integer; EnumPrefixLen: Byte = 2): string;
function EnumValue(PEnum: Pointer; AStr: string; APrefixChars: Byte = 0): Integer;
function EnumPrefix(PEnum: Pointer; APrefixChars: Byte = 0): string;
function TernOP(ACond: Boolean; const IfTrue, IfFalse: Variant): Variant;       {$IFDEF PASCAL_INLINE}inline;{$ENDIF}
function IIF(ACond: Boolean; IfTrue, IfFalse: Integer): Integer;                overload; {$IFDEF PASCAL_INLINE}inline;{$ENDIF}
function IIF(ACond: Boolean; IfTrue, IfFalse: Boolean): Boolean;                overload; {$IFDEF PASCAL_INLINE}inline;{$ENDIF}
function IIF(ACond: Boolean; IfTrue, IfFalse: string): string;                  overload; {$IFDEF PASCAL_INLINE}inline;{$ENDIF}
function IIF(ACond: Boolean; IfTrue, IfFalse: TObject): TObject;                overload; {$IFDEF PASCAL_INLINE}inline;{$ENDIF}
function IFF(ACond: Boolean; IfTrue, IfFalse: Single): Single;                  {$IFDEF PASCAL_INLINE}inline;{$ENDIF}
function PostInc(var AInt: Integer; const Modifier: Integer = 1): Integer;
function PreInc(var AInt: Integer; const Modifier: Integer = 1): Integer;
function SplitString(const AStr: string; ADelim: Char): TArray<string>;
function SortStringArray(const AArray: TArray<string>): TArray<string>;

{$IFNDEF FPC}
function ConcatPaths(const Paths: array of string): string;
{$ENDIF}

implementation

uses
// System
  SysUtils, Math, TypInfo
{$IFNDEF FPC}
  , IOUtils
{$ENDIF}
  ;

function EnumName(PEnum: Pointer; Ordinal: Integer; EnumPrefixLen: Byte = 2): string;
begin
  Result := GetEnumName(PEnum, Ordinal);
  Delete(Result, 1, EnumPrefixLen);
end;

function EnumValue(PEnum: Pointer; AStr: string; APrefixChars: Byte = 0): Integer;
begin
  Result := GetEnumValue(PEnum, AStr);
  if (Result = 255) or (Result = -1) then
    Result := GetEnumValue(PEnum, EnumPrefix(PEnum, APrefixChars) + AStr);
end;

function EnumPrefix(PEnum: Pointer; APrefixChars: Byte = 0): string;
var
  LValue: string;
  LChar: Char;
begin
  Result := '';
  LValue := EnumName(PEnum, 0, 0);
  if APrefixChars = 0 then
    for LChar in LValue do
      if LChar = LowerCase(LChar) then
        Inc(APrefixChars)
      else
        Break; 
  Result := LValue.Substring(0, APrefixChars);
end;

function PostInc(var AInt: Integer; const Modifier: Integer = 1): Integer;
begin
  Result := AInt;
  AInt := AInt + Modifier;
end;

function PreInc(var AInt: Integer; const Modifier: Integer = 1): Integer;
begin
  AInt := AInt + Modifier;
  Result := AInt;
end;

function TernOP(ACond: Boolean; const IfTrue, IfFalse: Variant): Variant;
begin
  if ACond then
    Result := IfTrue
  else
    Result := IfFalse;
end;

function IIF(ACond: Boolean; IfTrue, IfFalse: Integer): Integer;
begin
  if ACond then
    Result := IfTrue
  else
    Result := IfFalse;
end;

function IIF(ACond: Boolean; IfTrue, IfFalse: Boolean): Boolean;
begin
  if ACond then
    Result := IfTrue
  else
    Result := IfFalse;
end;

function IIF(ACond: Boolean; IfTrue, IfFalse: string): string;
begin
  if ACond then
    Result := IfTrue
  else
    Result := IfFalse;
end;

function IIF(ACond: Boolean; IfTrue, IfFalse: TObject): TObject;
begin
  if ACond then
    Result := IfTrue
  else
    Result := IfFalse;
end;

function IFF(ACond: Boolean; IfTrue, IfFalse: Single): Single;
begin
  if ACond then
    Result := IfTrue
  else
    Result := IfFalse;
end;

function SortStringArray(const AArray: TArray<string>): TArray<string>;
var 
  LStrings: TStringList;
  LKey: string;
begin
  LStrings := TStringList.Create();
  for LKey in AArray do
    LStrings.Append(LKey);
  LStrings.Sort();
  Result := LStrings.ToStringArray();
  FreeAndNil(LStrings);
end;

function SplitString(const AStr: string; ADelim: Char): TArray<string>;
var
  sl: TStringList;
  trail: string;
begin
  Result := nil;
  sl := TStringList.Create();
  try
    sl.Delimiter := ADelim;
    sl.DelimitedText := AStr;
    SetLength(Result, sl.Count);
    Result := sl.ToStringArray;
    trail := Copy(AStr, Length(sl.Text), Length(AStr));
    if trail <> '' then
    begin
      SetLength(Result, sl.Count + 1);
      Result[sl.Count] := trail;
    end;
  finally
    sl.Free();
  end;
end;

{$IFNDEF FPC}
function ConcatPaths(const Paths: array of string): string;
var
  LIndex: Integer;
begin
  Assert(Length(Paths) > 1);
  Result := Paths[0];
  LIndex := 1;
  while LIndex < Length(Paths) do
    Result := TPath.Combine(Result, Paths[PostInc(LIndex)]);
end;
{$ENDIF}


end.

