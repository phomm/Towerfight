unit GameViewGame;

{$mode delphi}

interface

uses 
// System
  Classes,
// Castle
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse, CastleComponentSerialize, 
  CastleScene, CastleScenecore, castleviewport, x3dnodes,
// Own
  gameentities, roomcomponent;

type
  TViewGame = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonDefeat, WeaponPlus, WeaponMinus, WeaponNo, WeaponMultiply: TCastleButton;
    GroupTowers: TCastleHorizontalGroup;
    FactoryTower, FactoryRoom: TCastleComponentFactory;
    BloodSplash0, BloodSplash1, BloodSplash2: TCastleScene;
    Viewport1: TCastleViewport;
    ImageWeapon: TCastleImageControl;
    function GetMap(): TMap;
    property Map: TMap read GetMap;
  protected
    function GetWeapon(AIndex: NHeroWeapon): TCastleButton;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    procedure RoomFight(ARoom: TRoomComponent);
    property WeaponButton[AIndex: NHeroWeapon]: TCastleButton read GetWeapon;
  private
    FPreviousRoom: TRoomComponent;
    FWeapons: array[0..3] of TCastleButton;
    FSkip: Boolean;
    FPosFrom, FPosTo: TVector2;
    FTicks: Integer;
    procedure ButtonDefeatClick(Sender: TObject);
    procedure ButtonRoomClick(Sender: TObject);
    procedure ButtonWeaponClick(Sender: TObject);
    procedure RunAnimation(AScene: TCastleScene; ARoom: TCastleUserInterface);
    procedure AnimationStopped(const AScene: TCastleSceneCore; const ATimeSensorNode: TTimeSensorNode);
    function RandomBloodSplash(): TCastleScene;
    procedure DefeatQuestionYes(Sender: TObject);
  private const
    TicksToFlyWeapon = 20; // animation will last 0.3 seconds (20 ticks * 16 ms)
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
  Common, GameViewDefeat, gameviewmain, gameviewwin, gameoptions, gameviewformula, gameviewdialog;

procedure CastleSleep(AMilliseconds: Integer);
var
  I: Integer;
begin
  Exit; // disabled, as fails on web, Sleep must be reworked with timers
  for I := 0 to AMilliseconds div 50 do
  begin
    Sleep(50); // milliseconds);
    Application.ProcessAllMessages();
  end;
end;  

constructor TViewGame.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewgame.castle-user-interface';
end;

procedure TViewGame.Start();
var 
  LRoom: TRoom;
  LGroupTower, LVisualTower, LRoof, LRoomUI: TCastleUserInterface;
  LRoomComponent: TRoomComponent;
  LStockIndex, LTowerIndex: Integer;
begin
  inherited;
  ButtonDefeat.OnClick := ButtonDefeatClick;
  WeaponPlus.OnClick := ButtonWeaponClick;
  WeaponMinus.OnClick := ButtonWeaponClick;
  WeaponNo.OnClick := ButtonWeaponClick;
  WeaponMultiply.OnClick := ButtonWeaponClick;
  FWeapons[0] := WeaponNo;
  FWeapons[1] := WeaponPlus;
  FWeapons[2] := WeaponMinus;
  FWeapons[3] := WeaponMultiply;
  WeaponNo.Doclick(); 
  GroupTowers.ClearControls();
  
  for LTowerIndex := 0 to Pred(Map.Towers.Count) do
  begin
    LVisualTower := FactoryTower.ComponentLoad(GroupTowers) as TCastleUserInterface;
    LGroupTower := GroupTowers.FindRequiredComponent('GroupTower' + LTowerIndex.ToString) as TCastleUserInterface;
    GroupTowers.InsertFront(LVisualTower);
    LStockIndex := 0;
    for LRoom in Map.Towers[LTowerIndex].Rooms do 
    begin
      LRoomComponent := TRoomComponent.Create(LGroupTower);
      LRoomUI := FactoryRoom.ComponentLoad(LGroupTower, LRoomComponent) as TCastleUserInterface; 
      LRoomComponent.InsertFront(LRoomUI);
      LRoomComponent.Name := 'Room' + LTowerIndex.ToString + '_' + LStockIndex.ToString;
      LGroupTower.InsertFront(LRoomComponent);
      LRoomComponent.ControlRoom.OnClick := ButtonRoomClick;
      LRoomComponent.Tag := Map.GetRoomIndex(LTowerIndex, LStockIndex);
      if (LRoom.Actors.Count > 0) and Assigned(LRoom.Actors[0]) then
      begin
        LRoomComponent.LabelRight.Caption := LRoom.Actors[0].Visual;
        LRoomComponent.ImageRight.Url := LRoom.Actors[0].AssetId;
      end;
      LRoomComponent.LabelLeft.Caption := '';
      if Map.IsHeroRoom(LRoomComponent.Tag) then
      begin
        LRoomComponent.LabelLeft.Caption := Map.Hero.Visual;
        LRoomComponent.ImageLeft.Url := Map.Hero.AssetId;
        LRoomComponent.LabelRight.Caption := '';
        FPreviousRoom := LRoomComponent;
      end;
      Inc(LStockIndex);
    end;
    LRoof := LGroupTower.Controls[0];
    LGroupTower.RemoveControl(LRoof);
    LGroupTower.InsertFront(LRoof);
  end;
end;

procedure TViewGame.ButtonWeaponClick(Sender: TObject);
var
  i: Integer; 
  LIsWeapon: Boolean;
  LWeaponButton: TCastleButton;
begin
  LWeaponButton := Sender as TCastleButton;
  for i := 0 to High(FWeapons) do
  begin
    LIsWeapon := FWeapons[i] = LWeaponButton;    
    if LIsWeapon then
      Map.Hero.Weapon := NHeroWeapon(i);
    FWeapons[i].Pressed := LIsWeapon or (i = Ord(hwNo));
    FWeapons[i].Border.AllSides := IIF(LIsWeapon or (i = Ord(hwNo)), 4, 0);
    if i <> Ord(hwNo) then
    begin
      FWeapons[i].Enabled := Map.Hero.Weapons[NHeroWeapon(i)] > 0;
      FWeapons[i].Caption := Map.Hero.Weapons[NHeroWeapon(i)].ToString;
    end;
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
  DialogYesNo(Container, 'Game will be lost| Give up?', DefeatQuestionYes, nil);
end;

procedure TViewGame.DefeatQuestionYes(Sender: TObject);
begin
  Container.View := ViewDefeat;
end;

function TViewGame.GetWeapon(AIndex: NHeroWeapon): TCastleButton;
begin
  Result := FWeapons[Ord(AIndex)];
end;

procedure TViewGame.Update(const SecondsPassed: Single; var HandleInput: boolean);
var
  I: Integer;
begin
  inherited;
  if FTicks > 0 then
  begin
    Dec(FTicks);
    ImageWeapon.Translation := TVector2.Lerp(1 - FTicks / TicksToFlyWeapon, FPosFrom, FPosTo);
    if FTicks = 0 then
    begin
      ImageWeapon.Url := '';
      WeaponNo.DoClick();
    end;  
  end;
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
  LKey, W: Integer;
  LGroupTower: TCastleUserInterface;
  LTowerIndex, LStockIndex: Integer;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEscape) or Event.IsKey(keyBackSpace) then
  begin
    ButtonDefeat.DoClick();
    Exit(True); // key was handled
  end;

  if Event.IsKey(keyTab) then
  begin
    W := Ord(Map.Hero.Weapon);
    repeat
      W := (W + 1) mod Length(FWeapons);
    until FWeapons[W].Enabled;        
    FWeapons[W].DoClick();    
    Exit(True); // key was handled
  end;

  for LKey := 0 to High(DirKeys) do
  begin
    if Event.IsKey(DirKeys[LKey]) then
    begin
      LTowerIndex := Map.HeroTowerIndex + KeyToDelta[LKey].X;
      LStockIndex := Map.HeroStockIndex + KeyToDelta[LKey].Y;
      if Map.GetRoomIndex(LTowerIndex, LStockIndex) = -1 then
        Exit(True); // key was handled, even if hero didn't move
      LGroupTower := GroupTowers.FindRequiredComponent('GroupTower' + LTowerIndex.ToString) as TCastleUserInterface;
      (LGroupTower.Controls[LStockIndex] as TRoomComponent).ControlRoom.DoClick();
      Exit(True); // key was handled
    end;
  end;
end;

procedure TViewGame.ButtonRoomClick(Sender: TObject);
var
  LRoom: TRoomComponent;
begin
  if FSkip then Exit;
  
  LRoom := (Sender as TCastleButton).Parent as TRoomComponent;
  if not Map.SetHeroRoom(LRoom.Tag) then
    Exit;
  
  // Hide image on previously active button
  if Assigned(FPreviousRoom) and (FPreviousRoom <> LRoom) then
  begin
    FPreviousRoom.LabelLeft.Caption := '';
    FPreviousRoom.ImageLeft.Url := '';
  end;
  
  // Show image on currently clicked button
  FPreviousRoom := LRoom;
  LRoom.ImageLeft.Url := Map.Hero.AssetId;
  LRoom.LabelLeft.Caption := Map.Hero.Visual;
  if not Map.HeroRoom.HasEnemy() then
    Exit;

  if Map.Hero.Weapon = hwNo then
    RoomFight(LRoom)
  else 
  begin
    ViewFormula.RoomComponent := LRoom;
    Container.PushView(ViewFormula);
  end;
end;

procedure TViewGame.RoomFight(ARoom: TRoomComponent);
var
  LActor: TActor;
  LScene: TCastleScene;
  LRoom: TRoom;
  LWeapon: NHeroWeapon;
begin
  LRoom := Map.GetRoomByIndex(ARoom.Tag);
  LActor := LRoom.Actors[0];
  LActor.Reveal();
  ARoom.LabelRight.Caption := LActor.Visual;
  if Map.HeroRoom.Fight() then
  begin
    LScene := RandomBloodSplash();
    RunAnimation(LScene, ARoom);
    // loot
    if LRoom.Actors.Count > 0 then
      ARoom.ImageRight.Url := LRoom.Actors[0].AssetId
    else
      ARoom.ImageRight.Url := TMap.BloodAsset;
  end
  else
    ARoom.ImageLeft.Url := Map.Hero.AssetId; 

  FSkip := True;
  CastleSleep(500);
  if Map.Hero.Dead then
  begin
    CastleSleep(500);
    Container.View := ViewDefeat
  end
  else
  begin
    LWeapon := LRoom.PickWeapon();
    if LWeapon <> hwNo then
    begin  
      ImageWeapon.Translation := ARoom.LocalToContainerPosition(Vector2(ARoom.Width / 2, ARoom.Height), False);
      ImageWeapon.Url := ARoom.ImageRight.Url;
      FTicks := TicksToFlyWeapon;
      FPosFrom := ImageWeapon.Translation;
      with FWeapons[Ord(LWeapon)] do
        FPosTo := LocalToContainerPosition(Vector2(Width / 2, Height / 2), False);
    end;
    ARoom.ImageRight.Url := '';
    ARoom.LabelRight.Caption := '';
    ARoom.LabelLeft.Caption := Map.Hero.Visual;

    if LWeapon = hwNo then
      WeaponNo.DoClick();
    //WriteLnLog(Format('T%d S%d L%d L%d', [Map.HeroTowerIndex, Map.HeroStockIndex, Map.LastTower, Map.LastStock]));
    if Map.IsFinalRoom(Map.HeroTowerIndex + 1, Map.HeroStockIndex + 1) then
    begin
      CastleSleep(500);
      Container.View := ViewWin;
    end;
  end;
  FSkip := False;  
end;

function TViewGame.RandomBloodSplash(): TCastleScene;
begin
  case Random(3) of
    0: Result := BloodSplash0;
    1: Result := BloodSplash1;
    2: Result := BloodSplash2;
  end;
end;  

procedure TViewGame.RunAnimation(AScene: TCastleScene; ARoom: TCastleUserInterface);
var
  LAnimationParams: TPlayAnimationParameters;
begin
  Viewport1.Translation := ARoom.LocalToContainerPosition(Vector2(-ARoom.Width / 2, -ARoom.Height), False);
  AScene.Exists := True;
  LAnimationParams := TPlayAnimationParameters.Create();
  try
    LAnimationParams.Name := 'default';
    LAnimationParams.StopNotification := AnimationStopped;
    AScene.StopAnimation(True);
    AScene.PlayAnimation(LAnimationParams);
  finally 
    FreeAndNil(LAnimationParams);
  end;
end;

procedure TViewGame.AnimationStopped(const AScene: TCastleSceneCore; const ATimeSensorNode: TTimeSensorNode);
begin
  AScene.Exists := False;
end;

end.
