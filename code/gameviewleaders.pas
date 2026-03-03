unit GameViewLeaders;

{$mode delphi}

interface

uses 
// System
  Classes,
// Castle  
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse, CastleComponentSerialize;

type
  TViewLeaders = class(TCastleView)
  published
    ButtonMenu, ButtonSync: TCastleButton;
    FactoryButton: TCastleComponentFactory;
    GroupDifficulty: TCastleUserInterface;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    procedure ButtonMenuClick(Sender: TObject);
    procedure ButtonSyncClick(Sender: TObject);
    procedure ButtonDifficultyClick(Sender: TObject);
  const 
    ServerApiUrl = 'https://localhost:7150/api/';//'https://towerfightserver.onrender.com/api/';
  end;

var
  ViewLeaders: TViewLeaders;

implementation

uses 
// System
  SysUtils,
// Castle  
  castlewindow,
// Own
  gameoptions
;

constructor TViewLeaders.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewleaders.castle-user-interface';
end;

procedure TViewLeaders.Start;
var
  LDifficulty: NDifficulty;
  LButton: TCastleButton;
  LCurrentDifficultyName: string;
begin
  inherited;
  ButtonMenu.OnClick := ButtonMenuClick;
  ButtonSync.OnClick := ButtonSyncClick;
  LCurrentDifficultyName := DifficultyName(Difficulty());
  for LDifficulty in NDifficulty do
  begin
    LButton := FactoryButton.ComponentLoad(GroupDifficulty) as TCastleButton;
    LButton.Caption := DifficultyName(LDifficulty);
    LButton.Pressed := LButton.Caption = LCurrentDifficultyName;
    LButton.Tag := Ord(LDifficulty);
    LButton.OnClick := ButtonDifficultyClick;
    GroupDifficulty.InsertFront(LButton);
  end;
end;

function TViewLeaders.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEscape) or Event.IsKey(keyBackSpace) then
  begin
    Container.PopView();
    Exit(true); // key was handled
  end; 
end;

procedure TViewLeaders.ButtonMenuClick(Sender: TObject);
begin
  Container.PopView();
end;

procedure TViewLeaders.ButtonSyncClick(Sender: TObject);
begin
// Sync with server, and update the list of leaders for the current difficulty.
end;

procedure TViewLeaders.ButtonDifficultyClick(Sender: TObject);
var
  LButton: TCastleUserInterface;
begin
  for LButton in GroupDifficulty do
    if LButton is TCastleButton then
    begin
      TCastleButton(LButton).Pressed := LButton = Sender;
    end;
// Load from server if not yet done, and populate the list of leaders for the selected difficulty. 
  
end;

end.
