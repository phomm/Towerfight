unit GameViewFormula;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
// Own  
  gameentities;

type
  TViewFormula = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonGo: TCastleButton;
    GroupElements: TCastleHorizontalGroup;
  public
    Formula: String;
    Weapon: NHeroWeapon;
    RoomButton: TCastleButton;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    procedure ButtonGoClick(Sender: TObject);
  end;

var
  ViewFormula: TViewFormula;

implementation

uses 
// System
  SysUtils,
// Castle  
  castlewindow,
  // Own
  gameviewgame;

constructor TViewFormula.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewformula.castle-user-interface';
end;

procedure TViewFormula.Start;
begin
  inherited;
  buttonGo.OnClick := @ButtonGoClick;
end;

procedure TViewFormula.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }

end;

procedure TViewFormula.ButtonGoClick(Sender: TObject);
var
  LActor: TActor;
begin
  LActor := TMap.Map.GetRoomByIndex(RoomButton.Tag).Actors[0];
  // update Actor formula and level
  //LActor.Visual := NewFormula;
  //LActor.Level := NewLevel;
  Container.PopView();
  ViewGame.RoomFight(RoomButton);
end;

function TViewFormula.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEnter) then
  begin
    ButtonGoClick(ButtonGo);
    Exit(true); // key was handled
  end;

  if Event.IsKey(keyArrowLeft) or Event.IsKey(keyA) then
  begin
    
    Exit(true); // key was handled
  end;
  if Event.IsKey(keyArrowRight) or Event.IsKey(keyD) then
  begin
    
    Exit(true); // key was handled
  end;
end;

end.
