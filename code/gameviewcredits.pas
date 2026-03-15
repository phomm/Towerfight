unit GameViewCredits;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewCredits = class(TCastleView)
  published
    ButtonMenu, ButtonCastle, ButtonAuthor: TCastleButton;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    procedure ButtonMenuClick(Sender: TObject);
    procedure ButtonCastleClick(Sender: TObject);
    procedure ButtonAuthorClick(Sender: TObject);
  end;

var
  ViewCredits: TViewCredits;

implementation

uses 
// System
  SysUtils,
// Castle  
  castlewindow, CastleOpenDocument,
// Own  
  gameviewmain;

constructor TViewCredits.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewcredits.castle-user-interface';
end;

procedure TViewCredits.Start;
begin
  inherited;
  ButtonMenu.OnClick := @ButtonMenuClick;
  ButtonCastle.OnClick := @ButtonCastleClick;
  ButtonAuthor.OnClick := @ButtonAuthorClick;
end;

function TViewCredits.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEscape) or Event.IsKey(keyBackSpace) then
  begin
    Container.View := ViewMain;
    Exit(true); // key was handled
  end; 
end;

procedure TViewCredits.ButtonMenuClick(Sender: TObject);
begin
  Container.View := ViewMain;
end;

procedure TViewCredits.ButtonCastleClick(Sender: TObject);
begin
  OpenURL('https://castle-engine.io');
end;

procedure TViewCredits.ButtonAuthorClick(Sender: TObject);
begin
  OpenURL('https://github.com/phomm/Towerfight');
end;  

end.
