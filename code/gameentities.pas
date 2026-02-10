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
    procedure SetLevel(AValue: Integer); virtual;
    function GetVisual(): string; virtual;
  public
    property Level: Integer read FLevel write SetLevel;
    property Visual: string read GetVisual;
    property AssetId: string read FAssetId write FAssetId;
  end;

  THero = class(TActor)
  private

  public
    constructor Create(AOwner: TComponent); override;
  end;

  TEnemy = class(TActor)
  private

  public
    constructor Create(AOwner: TComponent; ATower, AStock: Integer); overload;
  end;

  TRoom = class(TComponent)
  private
    FActors: TObjectList<TActor>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    property Actors: TObjectList<TActor> read FActors;
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
class var FMap: TMap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    property Towers: TObjectList<TTower> read FTowers;
    property Hero: THero read FHero;
property LastTower: Integer read FLastTower;
    property LastStock: Integer read FLastStock;
    function IsLastTower(ATowerIndex: Integer): Boolean;
    function IsLastStock(AStockIndex: Integer): Boolean;
    function IsFinalRoom(ATowerIndex, AStockIndex: Integer): Boolean;
  class function Map(): TMap;
  class procedure Die();
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
  LEnemy: TActor;
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
      LEnemy := TEnemy.Create(nil, T, R);
      LRoom.Actors.Add(LEnemy);
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

constructor TEnemy.Create(AOwner: TComponent; ATower, AStock: Integer);
begin
  inherited Create(AOwner);
  FLevel := ATower;
  FAssetId := 'castle-data:/resources/bad.bmp';
end;

constructor THero.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLevel := Random(4) + 3;
  FAssetId := 'castle-data:/resources/good.bmp';
end;

end.