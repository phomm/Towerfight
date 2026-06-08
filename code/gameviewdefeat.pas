unit GameViewDefeat;

{$mode delphi}

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewDefeat = class(TCastleView)
  published
    ButtonMenu: TCastleButton;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    procedure ButtonMenuClick(Sender: TObject);
    procedure BackToMenu();
  end;

var
  ViewDefeat: TViewDefeat;

implementation

uses
// Castle
  CastleSoundEngine,
// Own
  gameviewmain, audiocomponent, gameoptions, gameviewcredits;

constructor TViewDefeat.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewdefeat.castle-user-interface';
  DesignPreload := True;
end;

procedure TViewDefeat.Start;
begin
  inherited;
  ButtonMenu.OnClick := ButtonMenuClick;
  SoundEngine.LoopingChannel[0].Sound := Audio.RandomLoseTheme;
  SoundEngine.LoopingChannel[0].Sound.Volume := 5 * MusicLevel() / 100;
end;

procedure TViewDefeat.ButtonMenuClick(Sender: TObject);
begin
  BackToMenu();
end;

procedure TViewDefeat.BackToMenu();
begin
  if IsSchool() then
    Container.View := ViewCredits
  else
    Container.View := ViewMain;
end;

function TViewDefeat.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEscape) or Event.IsKey(keyEnter) or Event.IsKey(keyBackSpace) then
  begin
    BackToMenu();
    Exit(true); // key was handled
  end; 
end;

end.
