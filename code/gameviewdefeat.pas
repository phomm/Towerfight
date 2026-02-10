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
// Own
  gameviewmain;

constructor TViewDefeat.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewdefeat.castle-user-interface';
end;

procedure TViewDefeat.Start;
begin
  inherited;
  ButtonMenu.OnClick := @ButtonMenuClick;
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

  if Event.IsKey(keyEscape) then
  begin
    Container.View := ViewMain;
    Exit(true); // key was handled
  end; 
end;

end.
