unit GameViewWin;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewWin = class(TCastleView)
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
  ViewWin: TViewWin;

implementation

uses
// Castle
  CastleSoundEngine,
// Own
  gameviewmain, audiocomponent, gameoptions;

constructor TViewWin.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewwin.castle-user-interface';
end;

procedure TViewWin.Start;
begin
  inherited;
  ButtonMenu.OnClick := @ButtonMenuClick;
  SoundEngine.LoopingChannel[0].Sound := Audio.RandomWinTheme;
  SoundEngine.LoopingChannel[0].Sound.Volume := 5 * MusicLevel() / 100;
end;

procedure TViewWin.ButtonMenuClick(Sender: TObject);
begin
  Container.View := ViewMain;
end;

procedure TViewWin.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }
end;

function TViewWin.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEscape) or Event.IsKey(keyEnter) or Event.IsKey(keyBackSpace) then
  begin
    Container.View := ViewMain;
    Exit(true); // key was handled
  end; 
end;

end.
