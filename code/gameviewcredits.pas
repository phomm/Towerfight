unit GameViewCredits;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewCredits = class(TCastleView)
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
  ViewCredits: TViewCredits;

implementation

uses 
// System
  SysUtils,
// Castle  
  castlewindow;

constructor TViewCredits.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewcredits.castle-user-interface';
end;

procedure TViewCredits.Start;
begin
  inherited;
  ButtonMenu.OnClick := @ButtonMenuClick;
end;

procedure TViewCredits.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }

end;

function TViewCredits.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEscape) or Event.IsKey(keyBackSpace) then
  begin
    Container.PopView();
    Exit(true); // key was handled
  end; 
end;

procedure TViewCredits.ButtonMenuClick(Sender: TObject);
begin
  Container.PopView();
end;

end.
