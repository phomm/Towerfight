unit GameOptions;

{$mode delphi}

interface

uses Classes, generics.collections;  

type
  NDifficulty = (gdEasy, gdNormal, gdHard, gdInsane);

/// <summary>
/// Current Difficulty, default is Easy
/// </summary>
function Difficulty(): NDifficulty;
/// <summary>
/// Current Music Level 0..10, default is 5
/// </summary>
function MusicLevel(): Byte;
procedure SetMusicLevel(AValue: Byte);
procedure SetDifficulty(AValue: NDifficulty);
function DifficultyName(AValue: NDifficulty): string;

implementation

uses 
// System
  SysUtils, typinfo,
// Castle
  CastleConfig, castlelog,
// Own  
  Common;

var 
  ETPDifficulty: Pointer;

const
  DifficultyDefault = gdEasy;
  DifficultyKey = 'Difficulty';
  MusicKey = 'Music';
  MusicLevelDefault = 5;

function Difficulty(): NDifficulty;
begin
  Result := NDifficulty(EnumValue(ETPDifficulty, UserConfig.GetValue(DifficultyKey, '')));
  if Ord(Result) in [0..254] then
    Exit;
  Result := DifficultyDefault;
  UserConfig.SetValue(DifficultyKey, EnumName(ETPDifficulty, Ord(Result)));
end;

function MusicLevel(): Byte;
begin
  Result := UserConfig.GetValue(MusicKey, MusicLevelDefault);
end;

procedure SetMusicLevel(AValue: Byte);
begin
  UserConfig.SetValue(MusicKey, AValue);
end;

procedure SetDifficulty(AValue: NDifficulty);
begin
  UserConfig.SetValue(DifficultyKey, DifficultyName(AValue));
end;

function DifficultyName(AValue: NDifficulty): string;
begin
  Result := EnumName(ETPDifficulty, Ord(AValue));
end;

initialization
  ETPDifficulty := TypeInfo(NDifficulty);

end.