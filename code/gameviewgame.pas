unit GameViewGame;

{$mode delphi}

interface

uses 
// System
Classes,
// Castle
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
// Own
  gameentities;

type
  TViewGame = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonDefeat: TCastleButton;
    GroupTowers: TCastleHorizontalGroup;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    FMap: TMap;
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
  castlewindow, castlemessages, CastleComponentSerialize, 
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
  LGroupTower, LVisualTower,
  LVisualRoom: TCastleUserInterface;
  LRoomButton: TCastleButton;
  LRoomIndex, LTowerIndex: Integer;
  LTowerFactory, LRoomFactory: TCastleComponentFactory;
begin
  inherited;
  ButtonDefeat.OnClick := ButtonDefeatClick;
  FMap := TMap.Create(Self);
  GroupTowers.ClearControls();
  // test
  ButtonDefeat.Caption := DifficultyName(Difficulty());
  LTowerFactory := TCastleComponentFactory.Create(nil); 
  LRoomFactory := TCastleComponentFactory.Create(nil);
  try
    LTowerFactory.Url := 'castle-data:/tower.castle-user-interface';
    LRoomFactory.Url := 'castle-data:/room.castle-user-interface';
    
    for LTowerIndex := 0 to Pred(FMap.Towers.Count) do
    begin
      LVisualTower := LTowerFactory.ComponentLoad(GroupTowers) as TCastleUserInterface;
      LGroupTower := GroupTowers.FindRequiredComponent('GroupTower' + LTowerIndex.ToString) as TCastleUserInterface;
      GroupTowers.InsertFront(LVisualTower);
      LRoomIndex := 0;
      for LRoom in FMap.Towers[LTowerIndex].Rooms do 
      begin
        LVisualRoom := LRoomFactory.ComponentLoad(LGroupTower) as TCastleUserInterface;
        LGroupTower.InsertFront(LVisualRoom);
        LRoomButton := LGroupTower.FindRequiredComponent('ControlRoom' + PostInc(LRoomIndex).ToString) as TCastleButton;
        LRoomButton.OnClick := ButtonRoomClick;
        LRoomButton.Tag := LTowerIndex * 10 + LRoomIndex;
      end;
    end;
  finally
    FreeAndNil(LRoomFactory);
    FreeAndNil(LTowerFactory);
  end;
end;

procedure TViewGame.Stop();
begin
  FreeAndNil(FMap);
end;

procedure TViewGame.ButtonDefeatClick(Sender: TObject);
begin
  Container.View := (ViewDefeat);
end;

procedure TViewGame.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }

end;

function TViewGame.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEscape) and MessageYesNo(Application.MainWindow, 'Game would be lost. Exit to menu?') then
  begin
    Container.View := (ViewMain);
    Exit(True); // key was handled
  end; 
end;

procedure TViewGame.ButtonRoomClick(Sender: TObject);
var
  LButton: TCastleButton;
  LImage: TCastleImageControl;
begin
  // Hide image on previously active button
  if Assigned(FPreviouslyActiveButton) and (FPreviouslyActiveButton <> Sender) then
  begin
    FPreviouslyActiveButton.Caption := '';
    LImage := FPreviouslyActiveButton.Controls[0].Controls[0] as TCastleImageControl;
    LImage.Url := '';
  end;

  // Show image on currently clicked button
  LButton := Sender as TCastleButton;
  LButton.Caption := IntToStr(LButton.Tag);
  LImage := LButton.Controls[0].Controls[0] as TCastleImageControl;
  LImage.Url := 'castle-data:/resources/good.bmp';
  
  // Update the reference
  FPreviouslyActiveButton := LButton;
end;

end.
