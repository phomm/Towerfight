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

function UseTimer(): Boolean;
procedure SetUseTimer(AValue: Boolean);

function UserName(): string;
procedure SetUserName(const AValue: string);

function UserGuid(): string;
procedure SetUserGuid(const AValue: string);

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
  UseTimerKey = 'UseTimer';
  UseTimerDefault = False;

procedure SetValue<T>(const AKey: string; AValue: T);
begin
  UserConfig.SetValue(AKey, AValue);
  if UserConfig.Modified then
    UserConfig.Save();
end;

function Difficulty(): NDifficulty;
begin
  Result := NDifficulty(EnumValue(ETPDifficulty, UserConfig.GetValue(DifficultyKey, '')));
  if Ord(Result) in [0..254] then
    Exit;
  Result := DifficultyDefault;
  SetValue<string>(DifficultyKey, EnumName(ETPDifficulty, Ord(Result)));
end;

function MusicLevel(): Byte;
begin
  Result := UserConfig.GetValue(MusicKey, MusicLevelDefault);
end;

procedure SetMusicLevel(AValue: Byte);
begin
  SetValue<byte>(MusicKey, AValue);
end;

procedure SetDifficulty(AValue: NDifficulty);
begin
  SetValue<string>(DifficultyKey, DifficultyName(AValue));
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
  SetValue<Boolean>(FullscreenKey, AValue);
  Application.MainWindow.FullScreen := AValue;
end;

function UseTimer(): Boolean;
begin
  Result := UserConfig.GetValue(UseTimerKey, UseTimerDefault);
end;

procedure SetUseTimer(AValue: Boolean);
begin
  SetValue<Boolean>(UseTimerKey, AValue);
end;

function UserName(): string;
begin
  Result := UserConfig.GetValue('UserName', '');
end;

procedure SetUserName(const AValue: string);
begin
  SetValue<string>('UserName', AValue);
end;

function UserGuid(): string;
begin
  Result := UserConfig.GetValue('UserGuid', '');
end;

procedure SetUserGuid(const AValue: string);
begin
  SetValue<string>('UserGuid', AValue);
end;

initialization
  ETPDifficulty := TypeInfo(NDifficulty);

end.