unit GameViewCredits;

{$mode delphi}

interface

uses 
// System
  Classes,
// Castle
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewCredits = class(TCastleView)
  published
    ButtonMenu, ButtonCredits, ButtonRules, ButtonKeys, ButtonWeapon,
      ButtonCastle, ButtonAuthor, ButtonItch, ButtonDiscord, ButtonWeb: TCastleButton;
    GroupCredits, GroupRules, GroupKeys, GroupWeapon: TCastleUserInterface;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    procedure ButtonMenuClick(Sender: TObject);
    procedure ButtonPanelClick(Sender: TObject);
    procedure ButtonCastleClick(Sender: TObject);
    procedure ButtonAuthorClick(Sender: TObject);
    procedure ButtonItchClick(Sender: TObject);
    procedure ButtonDiscordClick(Sender: TObject);
    procedure ButtonWebClick(Sender: TObject);
  end;

var
  ViewCredits: TViewCredits;

implementation

uses 
// System
  SysUtils,
// Castle  
  castlewindow, CastleOpenDocument, CastleApplicationProperties,
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
  ButtonMenu.OnClick := ButtonMenuClick;
  ButtonCredits.OnClick := ButtonPanelClick;
  ButtonRules.OnClick := ButtonPanelClick;
  ButtonKeys.OnClick := ButtonPanelClick;
  ButtonWeapon.OnClick := ButtonPanelClick;
  ButtonCastle.OnClick := ButtonCastleClick;
  ButtonAuthor.OnClick := ButtonAuthorClick;
  ButtonItch.OnClick := ButtonItchClick;
  ButtonDiscord.OnClick := ButtonDiscordClick;
  ButtonWeb.OnClick := ButtonWebClick;

  ButtonKeys.Exists := ApplicationProperties.ShowUserInterfaceToQuit {$IFDEF WASI} or True {$ENDIF};
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

procedure TViewCredits.ButtonItchClick(Sender: TObject);
begin
  OpenURL('https://phomm.itch.io/tower-fight');
end;

procedure TViewCredits.ButtonDiscordClick(Sender: TObject);
begin
  OpenURL('https://discord.gg/fz7bbM3dr3');
end;

procedure TViewCredits.ButtonWebClick(Sender: TObject);
begin
  OpenURL('https://ephyre.ru');
end;

procedure TViewCredits.ButtonPanelClick(Sender: TObject);
begin
  GroupCredits.Exists := Sender = ButtonCredits;
  GroupRules.Exists := Sender = ButtonRules;
  GroupKeys.Exists := Sender = ButtonKeys;
  GroupWeapon.Exists := Sender = ButtonWeapon;
  ButtonCredits.Pressed := Sender = ButtonCredits;
  ButtonRules.Pressed := Sender = ButtonRules;
  ButtonKeys.Pressed := Sender = ButtonKeys;
  ButtonWeapon.Pressed := Sender = ButtonWeapon;
end;

end.
