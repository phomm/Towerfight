unit GameViewDefeat;

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
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    procedure ButtonMenuClick(Sender: TObject);
  end;

var
  ViewDefeat: TViewDefeat;

implementation

uses
// Castle
  CastleSoundEngine,
// Own
  gameviewmain, audiocomponent, gameoptions;

constructor TViewDefeat.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewdefeat.castle-user-interface';
end;

procedure TViewDefeat.Start;
begin
  inherited;
  ButtonMenu.OnClick := @ButtonMenuClick;
  SoundEngine.LoopingChannel[0].Sound := Audio.RandomLoseTheme;
  SoundEngine.LoopingChannel[0].Sound.Volume := 5 * MusicLevel() / 100;
end;

procedure TViewDefeat.ButtonMenuClick(Sender: TObject);
begin
  Container.View := ViewMain;
end;

procedure TViewDefeat.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }
end;

function TViewDefeat.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEscape) or Event.IsKey(keyEnter) then
  begin
    Container.View := ViewMain;
    Exit(true); // key was handled
  end; 
end;

end.
