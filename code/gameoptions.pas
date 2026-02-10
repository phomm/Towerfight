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
/// <summary>
/// Current Fullscreen state, default is False
/// </summary>
function Fullscreen(): Boolean;
procedure SetFullscreen(AValue: Boolean);

implementation

uses 
// System
  SysUtils, typinfo,
// Castle
  CastleConfig, castlelog, CastleWindow,
// Own  
  Common;

var 
  ETPDifficulty: Pointer;

const
  DifficultyDefault = gdEasy;
  DifficultyKey = 'Difficulty';
  MusicKey = 'Music';
  MusicLevelDefault = 20;
  FullscreenKey = 'Fullscreen';
  FullscreenDefault = False;

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

function Fullscreen(): Boolean;
begin
  Result := UserConfig.GetValue(FullscreenKey, FullscreenDefault);
end;

procedure SetFullscreen(AValue: Boolean);
begin
  UserConfig.SetValue(FullscreenKey, AValue);
  Application.MainWindow.FullScreen := AValue;
end;

initialization
  ETPDifficulty := TypeInfo(NDifficulty);

end.