unit GameEntities;

{$mode delphi}

interface

uses 
// System
  Classes, generics.collections,
// Own
  gameoptions;

type
  TActor = class(TComponent)
  private
    FLevel: Integer;
    FAssetId: string;
  protected
    FRevealed: Boolean;
    procedure SetLevel(AValue: Integer); virtual;
    function GetVisual(): string; virtual;
  public
    property Level: Integer read FLevel write SetLevel;
    property Visual: string read GetVisual;
    property AssetId: string read FAssetId write FAssetId;
    procedure Reveal();
  end;

  NHeroWeapon = (hwNo, hwPlus, hwMinus);
  THero = class(TActor)
  private
    FDead: Boolean;
    FWeapons: array[NHeroWeapon] of Byte;
    FWeapon: NHeroWeapon;
    procedure Die();
    function GetWeapon(AIndex: NHeroWeapon): Byte;
  public
    constructor Create(AOwner: TComponent); override;
    property Dead: Boolean read FDead;
    property Weapon: NHeroWeapon read FWeapon write FWeapon;
    property Weapons[AIndex: NHeroWeapon]: Byte read GetWeapon;
  end;

  TEnemy = class(TActor)
  private
    FFormula: string;
  protected
    function CalcLevel(ATower, AStock: Integer): Integer; virtual;
    function GetVisual(): string; override; 
    procedure CreateFormula(ATower, AStock: Integer);
  public
    constructor Create(AOwner: TComponent; ATower, AStock: Integer); overload;
  end;

  TDragon = class(TEnemy)
  protected
    function CalcLevel(ATower, AStock: Integer): Integer; override;
    function GetVisual(): string; override;  
  public
    constructor Create(AOwner: TComponent; ATower, AStock: Integer); overload;
  end;

  TRoom = class(TComponent)
  private
    FActors: TObjectList<TActor>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    function Fight(): Boolean;
    property Actors: TObjectList<TActor> read FActors;
    function HasEnemy(): Boolean;
  end;

  TTower = class(TComponent)
  private
    FRooms: TObjectList<TRoom>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    property Rooms: TObjectList<TRoom> read FRooms;
  end;
  
  TMap = class(TComponent)
  private
    FTowers: TObjectList<TTower>;
    FDifficulty: NDifficulty;
    FLastTower, FLastStock: Integer;
    FHero: THero;
    FHeroRoom: TRoom;
    FHeroTowerIndex, FHeroStockIndex: Integer; 
  class var FMap: TMap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    property Towers: TObjectList<TTower> read FTowers;
    property Hero: THero read FHero;
    property LastTower: Integer read FLastTower;
    property LastStock: Integer read FLastStock;
    property HeroStockIndex: Integer read FHeroStockIndex;
    property HeroTowerIndex: Integer read FHeroTowerIndex;
    property HeroRoom: TRoom read FHeroRoom;
    function IsLastTower(ATowerIndex: Integer): Boolean;
    function IsLastStock(AStockIndex: Integer): Boolean;
    function IsFinalRoom(ATowerIndex, AStockIndex: Integer): Boolean;
    function SetHeroRoom(ARoomIndex: Integer): Boolean;
    function IsHeroRoom(ARoomIndex: Integer): Boolean;
    function GetRoomIndex(ATowerIndex, AStockIndex: Integer): Integer;
    function GetRoomByIndex(ARoomIndex: Integer): TRoom;
  class function Map(): TMap;
  class procedure Die();
  const BloodAsset = 'castle-data:/resources/blood_splat.png';
  end;

implementation

uses 
// System
  SysUtils, typinfo, Math,
// Own  
  Common, gameviewgame;

procedure TActor.SetLevel(AValue: Integer);
begin
  FLevel := AValue;
end;

function TActor.GetVisual(): string;
begin
  Result := IntToStr(FLevel);
end;

procedure TActor.Reveal();
begin
  FRevealed := True;
end;

constructor TRoom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActors := TObjectList<TActor>.Create(True);
end;

destructor TRoom.Destroy();
begin
  FreeAndNil(FActors);
  inherited Destroy();
end;

function TRoom.Fight(): Boolean;
begin
  if not HasEnemy() then
    Exit(True);
  if TMap.Map.Hero.Level >= FActors[0].Level then
  begin
    TMap.Map.Hero.Level := TMap.Map.Hero.Level + FActors[0].Level div Max(1, TMap.Map.HeroTowerIndex * 2);
    FActors.Delete(0);
    Exit(True);
  end;
  TMap.Map.Hero.Die();
  Result := False;
end;

function TRoom.HasEnemy(): Boolean;
begin
  Result := (FActors.Count > 0) and Assigned(FActors[0]);
end;

constructor TTower.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRooms := TObjectList<TRoom>.Create(True);
end;

destructor TTower.Destroy();
begin
  FreeAndNil(FRooms);
  inherited Destroy();
end;

constructor TMap.Create(AOwner: TComponent);
var
  T, R: Integer;
  LTower: TTower;
  LRoom: TRoom;
begin
  inherited Create(AOwner);
  FMap := Self;
  FHero := THero.Create(nil);
  
  FTowers := TObjectList<TTower>.Create(True);
  FDifficulty := Difficulty();
  FLastTower := 3 + Ord(FDifficulty);
  for T := 1 to FLastTower do
  begin
    LTower := TTower.Create(nil);
    FLastStock := Min(T + 3, 8);
    for R := 1 to FLastStock do
    begin
      LRoom := TRoom.Create(nil);
      if IsFinalRoom(T, R) then
        LRoom.Actors.Add(TDragon.Create(nil, T, R))
      else if (T <> 1) or (R <> 1) then
        LRoom.Actors.Add(TEnemy.Create(nil, T, R))
      else
      begin
        FHeroRoom := LRoom;
        FHeroTowerIndex := 0;
        FHeroStockIndex := 0;
      end;  
      LTower.FRooms.Add(LRoom);
    end;
    FTowers.Add(LTower);
  end;
end;

destructor TMap.Destroy();
begin
  FreeAndNil(FHero);
  FreeAndNil(FTowers);
  inherited Destroy();
end;

class function TMap.Map(): TMap;
begin
  if FMap = nil then
    FMap := TMap.Create(ViewGame);
  Result := FMap;
end;

class procedure TMap.Die();
begin
  FreeAndNil(FMap);
end;

function TMap.IsLastTower(ATowerIndex: Integer): Boolean;
begin
  Result := ATowerIndex = FLastTower;
end;

function TMap.IsLastStock(AStockIndex: Integer): Boolean;
begin
  Result := AStockIndex = FLastStock;
end;

function TMap.IsFinalRoom(ATowerIndex, AStockIndex: Integer): Boolean;
begin
  Result := IsLastTower(ATowerIndex) and IsLastStock(AStockIndex);
end;

function TMap.SetHeroRoom(ARoomIndex: Integer): Boolean;
var
  LTowerIndex, LStockIndex, LTowerDelta, LStockDelta: Integer;
begin
  LTowerIndex := ARoomIndex div 10;
  LStockIndex := ARoomIndex mod 10;
  LTowerDelta := Abs(LTowerIndex - FHeroTowerIndex);
  LStockDelta := Abs(LStockIndex - FHeroStockIndex);
  if FTowers[LTowerIndex].Rooms[LStockIndex].HasEnemy() and 
    ((LTowerDelta > 1) or (LStockDelta > 1) or (LTowerDelta = LStockDelta)) then
    Exit(False);
  FHeroRoom := FTowers[LTowerIndex].Rooms[LStockIndex];
  FHeroTowerIndex := LTowerIndex;
  FHeroStockIndex := LStockIndex;
  Result := True;
end;

function TMap.IsHeroRoom(ARoomIndex: Integer): Boolean;
begin
  Result := FHeroRoom = GetRoomByIndex(ARoomIndex);
end;

function TMap.GetRoomIndex(ATowerIndex, AStockIndex: Integer): Integer;
begin
  Result := ATowerIndex * 10 + AStockIndex;
  if (ATowerIndex < 0) or (ATowerIndex >= FTowers.Count) or 
    (AStockIndex < 0) or (AStockIndex >= FTowers[ATowerIndex].Rooms.Count) then
    Result := -1;
end;

function TMap.GetRoomByIndex(ARoomIndex: Integer): TRoom;
begin
  Result := FTowers[ARoomIndex div 10].Rooms[ARoomIndex mod 10];
end;

constructor TEnemy.Create(AOwner: TComponent; ATower, AStock: Integer);
begin
  inherited Create(AOwner);
  FLevel := CalcLevel(ATower, AStock);
  FAssetId := 'castle-data:/resources/' + IIF(Random(2) = 0, 'neutral', 'bad') + '.bmp';
  CreateFormula(ATower, AStock);
end;

function TEnemy.CalcLevel(ATower, AStock: Integer): Integer;
begin
  Result := (ATower + Random(AStock)) * (Max(ATower + 3, AStock + Random(8) - ATower))
   * ATower * ATower + Ord(ATower > 1) * (Random(ATower + AStock) - 5);
end;

procedure TEnemy.CreateFormula(ATower, AStock: Integer);
var
  LOp: Integer;
  a1, a2: Integer;
begin
  LOp := 0;
  
  if FLevel < 40 then
    LOp := Random(2)
  else if FLevel mod 70 = 0 then
    LOp := 7  
  else if FLevel mod 50 = 0 then
    LOp := 5
  else if FLevel mod 30 = 0 then
    LOp := 3
  else if FLevel mod 20 = 0 then
    LOp := 2;
  
  if FLevel > 20 then
  begin
    if FLevel mod 7 = 0 then
      LOp := 7
    else if FLevel mod 5 = 0 then
      LOp := 5
    else if FLevel mod 3 = 0 then
      LOp := 3
    else if FLevel mod 2 = 0 then
      LOp := 2
    else
      LOp := Random(2);
  end;
  
  if LOp = 0 then
  begin
    a1 := Random(FLevel + 1);
    a2 := FLevel - a1;
    FFormula := Format('%d+%d', [a1, a2]);
  end
  else if LOp = 1 then
  begin
    a1 := Random(Round(Sqrt(FLevel))) + ATower * Round(Log2(FLevel));
    a2 := FLevel + a1;
    FFormula := Format('%d-%d', [a2, a1]);
  end
  else
  begin
    a2 := FLevel div LOp;
    FFormula := Format('%d*%d', [a2, LOp]);
  end;
end;

function TEnemy.GetVisual(): string;
begin
  Result := IIF(FRevealed, FLevel.ToString, FFormula);
end;

function TDragon.CalcLevel(ATower, AStock: Integer): Integer;
begin
  case Difficulty of
    gdEasy: Result := 444;
    gdNormal: Result := 1000 + Random(5) * 100 + 40 + ValueOrZero(4); 
    gdHard: Result := 2400 + Random(5) * 10 + ValueOrZero(4); 
    gdInsane: Result := 4000 + Random(5) * 100 + ValueOrZero(40) + ValueOrZero(4);  
  end;
end;

function TDragon.GetVisual(): string;
begin
  if FRevealed then
    Exit(FLevel.ToString);
  Result := IIF(Level div 1000 = 0, '', IIF(Level div 1000 = 4, '4', '?'));
  Result := Result + IIF(Level mod 1000 div 100 = 4, '4', '?');
  Result := Result + IIF(Level mod 1000 mod 100 div 10 = 4, '4', '?');
  Result := Result + IIF(Level mod 10 = 4, '4', '?');
end;

constructor TDragon.Create(AOwner: TComponent; ATower, AStock: Integer);
begin
  inherited Create(AOwner, ATower, AStock);
  FAssetId := 'castle-data:/resources/dragon.png';
end;

constructor THero.Create(AOwner: TComponent);
var
  LAntiDifficulty: Integer;
begin
  inherited Create(AOwner);
  LAntiDifficulty := Ord(High(NDifficulty)) - Ord(Difficulty());
  FLevel := 4 + Random(4) + Random(4) + Random(4 + LAntiDifficulty);
  FAssetId := 'castle-data:/resources/good.bmp';
  Fweapons[hwNo] := 0;
  Fweapons[hwPlus] := 1 + Random(2 + LAntiDifficulty div 2);
  Fweapons[hwMinus] := 1 + Random(2 + LAntiDifficulty div 2);
end;

procedure THero.Die();
begin
  FDead := True;
  AssetId := TMap.BloodAsset;
  Level := 0;
end;

function THero.GetWeapon(AIndex: NHeroWeapon): Byte;
begin
  Result := FWeapons[AIndex];
end;

end.