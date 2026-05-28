unit roomcomponent;

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
  TRoomComponent = class(TCastleUserInterface)
  published
    ControlRoom: TCastleButton;
    ImageRight, ImageLeft, ImageHeroWeapon: TCastleImageControl;
    {LabelRight,} LabelLeft, LabelRight1, LabelRight2, LabelRight3: TCastleLabel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetEnemy(AEnemy: TActor);
  end;

implementation

uses
// System
  SysUtils, 
// Own
  gameviewmain, Common;

const ArmorBorderWidth = 2;  

function HighlightCaption(const ACaption: string): string;
var
  I: Integer;
begin
  Result := '';
  if Pos('?', ACaption) <= 0 then  
    Result := ACaption
  else
    for I := 1 to Length(ACaption) do
      if ACaption[I] = '?' then
        Result := Result + '<font color="#FF0000">?</font>'
      else 
        Result := Result + ACaption[I];
end;  

constructor TRoomComponent.Create(AOwner: TComponent);
begin
  inherited;
  AutoSizeToChildren := True;
end;

procedure TRoomComponent.SetEnemy(AEnemy: TActor);
var
  LSplit: TArray<string>;
begin
  if not Assigned(AEnemy) then
  begin
    LabelRight1.Exists := False;
    LabelRight2.Exists := False;
    LabelRight3.Exists := False;
    Exit;
  end;
  ImageRight.Url := AEnemy.AssetId;
  LabelRight2.Exists := not (AEnemy is TBoss);
  LabelRight3.Exists := not (AEnemy is TBoss);
  if AEnemy is TBoss then
  begin
    LabelRight1.Caption := HighlightCaption(AEnemy.Visual);
    LabelRight1.Border.AllSides := ArmorBorderWidth;
    Exit;
  end;
  LSplit := AEnemy.Visual.Split(['+', '-', '*']);
  if Length(LSplit) = 1 then
  begin
    LabelRight1.Caption := AEnemy.Visual;
    LabelRight1.Border.AllSides := 0;
    LabelRight2.Exists := False;
    LabelRight3.Exists := False;    
    Exit;
  end;
  LabelRight1.Caption := HighlightCaption(LSplit[0]);  
  LabelRight2.Caption := Copy(AEnemy.Visual, Length(LSplit[0]) + 1, 1);
  LabelRight3.Caption := HighlightCaption(LSplit[1]);
  LabelRight1.Border.AllSides := IIF(AEnemy is TMiniBoss, ArmorBorderWidth, 0);
  LabelRight3.Border.AllSides := IIF(AEnemy is TMiniBoss, ArmorBorderWidth, 0);
end;

end.
