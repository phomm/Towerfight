unit AudioComponent;

{$MODE DELPHI}

interface

uses 
// System
  Classes, Generics.Collections,
// Castle
  CastleSoundEngine;

type

  { TAudioComponent }

  TAudioComponent = class(TComponent)
  private class var
    FInstance: TAudioComponent;
  private
    MenuThemes: TArray<TCastleSound>;
    BattleThemes: TArray<TCastleSound>;
  public
    constructor Create(AOwner: TComponent); override;
    function RandomMenuTheme(): TCastleSound;
    function RandomBattleTheme(): TCastleSound;
    procedure PlaySoundByName(const AName: string);
  end;

function Audio(): TAudioComponent;

implementation

uses 
// System
  SysUtils,
// Castle
  CastleComponentSerialize, castlelog,
// Own
  Common, gameoptions
  ;

function Audio(): TAudioComponent;
begin
  if not Assigned(TAudioComponent.FInstance) then
    TAudioComponent.FInstance := TAudioComponent.Create(nil);
  Result := TAudioComponent.FInstance;
end;

constructor TAudioComponent.Create(AOwner: TComponent);
var
  LIndex: Integer;
  LSound: TCastleSound;
  procedure FetchSounds(var ASounds: TArray<TCastleSound>; const AName: string);
  begin
    LIndex := 0;
    while True do
    begin
      LSound := FindComponent(AName + LIndex.ToString) as TCastleSound;
      if not Assigned(LSound) then
        Break;
      Inc(LIndex);  
      SetLength(ASounds, LIndex);
      ASounds[High(ASounds)] := LSound;
    end;
  end;
begin
  inherited Create(AOwner);
  ComponentLoad('castle-data:/audio.castle-component', Self);
  FetchSounds(MenuThemes, 'MenuTheme');
  FetchSounds(BattleThemes, 'BattleTheme');
end;

function TAudioComponent.RandomMenuTheme(): TCastleSound;
begin
  Result := MenuThemes[Random(Length(MenuThemes))];
end;

function TAudioComponent.RandomBattleTheme(): TCastleSound;
begin
  Result := BattleThemes[Random(Length(BattleThemes))];
end;

procedure TAudioComponent.PlaySoundByName(const AName: string);
var
  LPlayingSound: TCastlePlayingSound;
  LSound: TCastleSound;
begin
  LSound := FindComponent(AName) as TCastleSound;
  if not Assigned(LSound) then
  begin
    WriteLog('Audio', Format('Sound "%s" not found', [AName]));
    Exit;
  end;
  LPlayingSound := TCastlePlayingSound.Create(nil);
  LPlayingSound.FreeOnStop := True;
  LPlayingSound.Sound := LSound;
  LPlayingSound.Volume := MusicLevel() / 10;
  SoundEngine.Play(LPlayingSound);
end;

initialization
finalization
  FreeAndNil(TAudioComponent.FInstance);
end.
