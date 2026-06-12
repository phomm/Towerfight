unit ImagesComponent;

{$MODE DELPHI}

interface

uses 
// System
  Classes, Generics.Collections,
// Castle
  Castlefonts;

type

  NActorPicture = (apHero, apBoss, apMiniBoss1, apMiniBoss2, apEnemy1, apEnemy2, apEnemy3, apBlood);

  { TImagesComponent }

  TImagesComponent = class(TComponent)
  private class var
    FInstance: TImagesComponent;
  private
    FActorImages: TArray<TCastleBitmapFont>;
    FWeaponImages: TArray<TCastleBitmapFont>;
  public
    constructor Create(AOwner: TComponent); override;
    function ImageUrl(APicture: NActorPicture): string;
    function WeaponUrl(AWeaponIndex: Integer): string;
  end;

function Images(): TImagesComponent;

implementation

uses 
// System
  SysUtils, TypInfo,
// Castle
  CastleComponentSerialize, castlelog,
// Own
  Common, gameoptions, gameentities
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
    LIndex: Integer;
    LImage: TCastleBitmapFont;
  begin
    Setlength(FActorImages, Ord(High(NActorPicture)) + 1);
    Setlength(FWeaponImages, Ord(High(NHeroWeapon)) + 1);
    for LIndex := Ord(Low(NActorPicture)) to Ord(High(NActorPicture)) do
    begin
      LImage := FindComponent(EnumName(TypeInfo(NActorPicture), Ord(LIndex))) as TCastleBitmapFont;
      if not Assigned(LImage) then
        Continue;
      FActorImages[LIndex] := LImage;
    end;
    for LIndex := Ord(Low(NHeroWeapon)) to Ord(High(NHeroWeapon)) do
    begin
      LImage := FindComponent('Weapon' + EnumName(TypeInfo(NHeroWeapon), Ord(LIndex))) as TCastleBitmapFont;
      if not Assigned(LImage) then
        Continue;
      FWeaponImages[LIndex] := LImage;
    end;    
  end;
begin
  inherited Create(AOwner);
  ComponentLoad('castle-data:/images.castle-component', Self);
  FetchImages();
end;

function TImagesComponent.ImageUrl(APicture: NActorPicture): string;
begin
  Result := FActorImages[Ord(APicture)].ImageUrl;
end;

function TImagesComponent.WeaponUrl(AWeaponIndex: Integer): string;
begin
  Result := FWeaponImages[AWeaponIndex].ImageUrl;
end;

initialization
finalization
  FreeAndNil(TImagesComponent.FInstance);
end.
