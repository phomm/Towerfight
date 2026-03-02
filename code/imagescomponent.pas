unit ImagesComponent;

{$MODE DELPHI}

interface

uses 
// System
  Classes, Generics.Collections,
// Castle
  CastleTerrain;

type

  NActorPicture = (apHero, apBoss, apMiniBoss1, apMiniBoss2, apEnemy1, apEnemy2, apEnemy3, apBlood);

  { TImagesComponent }

  TImagesComponent = class(TComponent)
  private class var
    FInstance: TImagesComponent;
  private
    FImages: TArray<TCastleTerrainImage>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    function ImageUrl(APicture: NActorPicture): string;
  end;

function Images(): TImagesComponent;

implementation

uses 
// System
  SysUtils, TypInfo,
// Castle
  CastleComponentSerialize, castlelog,
// Own
  Common, gameoptions
  ;

function Images(): TImagesComponent;
begin
  if not Assigned(TImagesComponent.FInstance) then
    TImagesComponent.FInstance := TImagesComponent.Create(nil);
  Result := TImagesComponent.FInstance;
end;

constructor TImagesComponent.Create(AOwner: TComponent);
  procedure FetchImages();
  var
    LIndex: NActorPicture;
    LImage: TCastleTerrainImage;
  begin
    Setlength(FImages, Ord(High(NActorPicture)) + 1);
    for LIndex := Low(NActorPicture) to High(NActorPicture) do
    begin
      LImage := FindComponent(EnumName(TypeInfo(NActorPicture), Ord(LIndex))) as TCastleTerrainImage;
      if not Assigned(LImage) then
        Continue;
      FImages[Ord(LIndex)] := LImage;
    end;
  end;
begin
  inherited Create(AOwner);
  ComponentLoad('castle-data:/images.castle-component', Self);
  FetchImages();
end;

destructor TImagesComponent.Destroy();
var
  I: Integer;
begin
  for I := Low(FImages) to High(FImages) do
    FreeAndNil(FImages[I]);
  inherited Destroy();
end;

function TImagesComponent.ImageUrl(APicture: NActorPicture): string;
begin
  Result := FImages[Ord(APicture)].Url;
end;

initialization
finalization
  FreeAndNil(TImagesComponent.FInstance);
end.
