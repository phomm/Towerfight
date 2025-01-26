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
    FActors: TList<TActor>;
    FActorsReadOnly: TCustomList<TActor>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    property Actors: TCustomList<TActor> read FActorsReadOnly;    
  end;

  TTower = class(TComponent)
  private
    FRooms: TList<TRoom>;
    FRoomsReadOnly: TCustomList<TRoom>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    property Rooms: TCustomList<TRoom> read FRoomsReadOnly;
  end;
  
  TMap = class(TComponent)
  private
    FTowers: TList<TTower>;
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
  SysUtils, typinfo,
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
  FActors := TList<TActor>.Create();
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
  FRooms := TList<TRoom>.Create();
  FRoomsReadOnly := FRooms;
end;

destructor TTower.Destroy();
begin
  FreeAndNil(FRooms);
  FRoomsReadOnly := nil;
  inherited Destroy();
end;

constructor TMap.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTowers := TList<TTower>.Create();
  FTowersReadOnly := FTowers;
  FDifficulty := Difficulty();
end;

destructor TMap.Destroy();
begin
  FreeAndNil(FTowers);
  FTowersReadOnly := nil;
  inherited Destroy();
end;

end.