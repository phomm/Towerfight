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
  
  end;

  TEnemy = class(TActor)
  private

  public
  
  end;

  TRoom = class(TComponent)
  private
    FActors: TObjectList<TActor>;
    FActorsReadOnly: TCustomList<TActor>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    property Actors: TCustomList<TActor> read FActorsReadOnly;
  end;

  TTower = class(TComponent)
  private
    FRooms: TObjectList<TRoom>;
    FRoomsReadOnly: TCustomList<TRoom>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    property Rooms: TCustomList<TRoom> read FRoomsReadOnly;
  end;
  
  TMap = class(TComponent)
  private
    FTowers: TObjectList<TTower>;
    FTowersReadOnly: TCustomList<TTower>;
    FDifficulty: NDifficulty;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    property Towers: TCustomList<TTower> read FTowersReadOnly;
  end;

implementation

uses 
// System
  SysUtils, typinfo, Math,
// Own  
  Common;

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
  FActorsReadOnly := FActors;
end;

destructor TRoom.Destroy();
begin
  FreeAndNil(FActors);
  FActorsReadOnly := nil;
  inherited Destroy();
end;

constructor TTower.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRooms := TObjectList<TRoom>.Create(True);
  FRoomsReadOnly := FRooms;
end;

destructor TTower.Destroy();
begin
  FreeAndNil(FRooms);
  FRoomsReadOnly := nil;
  inherited Destroy();
end;

constructor TMap.Create(AOwner: TComponent);
var
  T, R: Integer;
  LTower: TTower;
begin
  inherited Create(AOwner);
  FTowers := TObjectList<TTower>.Create(True);
  FTowersReadOnly := FTowers;
  FDifficulty := Difficulty();
  for T := 1 to 3 + Ord(FDifficulty) do
  begin
    LTower := TTower.Create(nil);
    for R := 1 to Min(T + 3, 8) do
      LTower.FRooms.Add(TRoom.Create(nil));
    FTowers.Add(LTower);
  end;
end;

destructor TMap.Destroy();
begin
  FreeAndNil(FTowers);
  FTowersReadOnly := nil;
  inherited Destroy();
end;

end.