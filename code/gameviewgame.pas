unit GameViewGame;

{$mode delphi}

interface

uses 
// System
Classes,
// Castle
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse, CastleComponentSerialize, 
// Own
  gameentities;

type
  TViewGame = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonDefeat: TCastleButton;
    GroupTowers: TCastleHorizontalGroup;
    FactoryTower, FactoryRoom: TCastleComponentFactory;
    function GetMap(): TMap;
    property Map: TMap read GetMap;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    FPreviouslyActiveButton: TCastleButton;
    procedure ButtonDefeatClick(Sender: TObject);
    procedure ButtonRoomClick(Sender: TObject);
  end;

var
  ViewGame: TViewGame;

implementation

uses 
// System
  SysUtils, 
// Castle  
  castlewindow, castlemessages, CastleLog,
// Own
  Common, GameViewDefeat, gameviewmain, gameoptions;

constructor TViewGame.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewgame.castle-user-interface';
end;

procedure TViewGame.Start();
var 
  LRoom: TRoom;
  LGroupTower, LVisualTower, LVisualRoom, LRoof: TCastleUserInterface;
  LRoomButton: TCastleButton;
  LStockIndex, LTowerIndex: Integer;
begin
  inherited;
  ButtonDefeat.OnClick := ButtonDefeatClick;
  GroupTowers.ClearControls();
  // test
  for LTowerIndex := 0 to Pred(Map.Towers.Count) do
  begin
    LVisualTower := FactoryTower.ComponentLoad(GroupTowers) as TCastleUserInterface;
    LGroupTower := GroupTowers.FindRequiredComponent('GroupTower' + LTowerIndex.ToString) as TCastleUserInterface;
    GroupTowers.InsertFront(LVisualTower);
    LStockIndex := 0;
    for LRoom in Map.Towers[LTowerIndex].Rooms do 
    begin
      LVisualRoom := FactoryRoom.ComponentLoad(LGroupTower) as TCastleUserInterface;
      LGroupTower.InsertFront(LVisualRoom);
      LRoomButton := LGroupTower.FindRequiredComponent('ControlRoom' + LStockIndex.ToString) as TCastleButton;
      LRoomButton.OnClick := ButtonRoomClick;
      LRoomButton.Tag := Map.GetRoomIndex(LTowerIndex, LStockIndex);
      if (LRoom.Actors.Count > 0) and Assigned(LRoom.Actors[0]) then
      begin
        (LRoomButton.Controls[0].Controls[1] as TCastleLabel).Caption := LRoom.Actors[0].Visual;
        (LRoomButton.Controls[0].Controls[0] as TCastleImageControl).Url := LRoom.Actors[0].AssetId;
      end;
      (LRoomButton.Controls[1].Controls[1] as TCastleLabel).Caption := '';
      if Map.IsHeroRoom(LRoomButton.Tag) then
      begin
        (LRoomButton.Controls[1].Controls[1] as TCastleLabel).Caption := Map.Hero.Visual;
        (LRoomButton.Controls[1].Controls[0] as TCastleImageControl).Url := Map.Hero.AssetId;
        (LRoomButton.Controls[0].Controls[1] as TCastleLabel).Caption := '';
        FPreviouslyActiveButton := LRoomButton;
      end;
      Inc(LStockIndex);
    end;
    LRoof := LGroupTower.Controls[0];
    LGroupTower.RemoveControl(LRoof);
    LGroupTower.InsertFront(LRoof);
  end;
end;

function TViewGame.GetMap(): TMap;
begin
  Result := TMap.Map;
end;

procedure TViewGame.Stop();
begin
  inherited;
  TMap.Die();
end;

procedure TViewGame.ButtonDefeatClick(Sender: TObject);
begin
  if MessageYesNo(Application.MainWindow, 'Game will be lost. Give up?') then
    Container.View := (ViewDefeat);
end;

procedure TViewGame.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
end;

function TViewGame.Press(const Event: TInputPressRelease): Boolean;
const 
  DirKeys: array[0..7] of TKey = (keyW, keyArrowUp, keyA, keyArrowLeft, keyS, keyArrowDown, keyD, keyArrowRight);
  KeyToDelta: array[0..7] of TPoint = (
    (X: 0; Y: 1), // W, Up
    (X: 0; Y: 1),
    (X: -1; Y: 0), // A, Left
    (X: -1; Y: 0),
    (X: 0; Y: -1), // S, Down
    (X: 0; Y: -1),
    (X: 1; Y: 0), // D, Right
    (X: 1; Y: 0)
  );
var
  key: Integer;
  LGroupTower: TCastleUserInterface;
  LRoomButton: TCastleButton; 
  LTowerIndex, LStockIndex: Integer;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEscape) then
  begin
    ButtonDefeat.DoClick();
    Exit(True); // key was handled
  end;
  for key := 0 to High(DirKeys) do
  begin
    if Event.IsKey(DirKeys[key]) then
    begin
      LTowerIndex := Map.HeroTowerIndex + KeyToDelta[key].X;
      LStockIndex := Map.HeroStockIndex + KeyToDelta[key].Y;
      if Map.GetRoomIndex(LTowerIndex, LStockIndex) = -1 then
        Exit(True); // key was handled, even if hero didn't move
      LGroupTower := GroupTowers.FindRequiredComponent('GroupTower' + LTowerIndex.ToString) as TCastleUserInterface;
      LRoomButton := LGroupTower.FindRequiredComponent('ControlRoom' + LStockIndex.ToString) as TCastleButton;      
      LRoomButton.DoClick();
      Exit(True); // key was handled
    end;
  end;
end;

procedure TViewGame.ButtonRoomClick(Sender: TObject);
var
  LButton: TCastleButton;
begin
  LButton := Sender as TCastleButton;
  if not Map.SetHeroRoom(LButton.Tag) then
    Exit;
  
  // Hide image on previously active button
  if Assigned(FPreviouslyActiveButton) and (FPreviouslyActiveButton <> Sender) then
  begin
    FPreviouslyActiveButton.Caption := '';
    (FPreviouslyActiveButton.Controls[1].Controls[0] as TCastleImageControl).Url := '';
    (FPreviouslyActiveButton.Controls[1].Controls[1] as TCastleLabel).Caption := '';
  end;
  
  // Show image on currently clicked button
  (LButton.Controls[1].Controls[0] as TCastleImageControl).Url := Map.Hero.AssetId;
  (LButton.Controls[1].Controls[1] as TCastleLabel).Caption := Map.Hero.Visual;

  FPreviouslyActiveButton := LButton;
end;

end.
