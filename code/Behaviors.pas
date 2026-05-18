unit Behaviors;

{$mode delphi}

interface

uses 
// System
  Classes, 
// Castle
  CastleClassUtils, CastleVectors, castletransform, castlecolors;

type

  TMoveCycleBehavior = class(TCastleBehavior)
  strict private
    FSpeedX, FSavedTranslationX, FAmplitudeX, FSpeedY, FSavedTranslationY, FAmplitudeY, FSeconds: Single;
    FActiveInEditor: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const APropertyName: String): TPropertySections; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure ParentAfterAttach(); override;
  published
    property SpeedX: Single read FSpeedX write FSpeedX default 0;
    property SpeedY: Single read FSpeedY write FSpeedY default 0;
    property ActiveInEditor: Boolean read FActiveInEditor write FActiveInEditor default False;
    property AmplitudeX: Single read FAmplitudeX write FAmplitudeX default 0;
    property AmplitudeY: Single read FAmplitudeY write FAmplitudeY default 0;
  end;

implementation

uses 
// System
  SysUtils,
// Castle
  CastleComponentSerialize, CastleUtils
// Own
  
  ;

constructor TMoveCycleBehavior.Create(AOwner: TComponent);
begin
  inherited;
  FSpeedX := 0;
  FSpeedY := 0;
  FAmplitudeX := 0;
  FAmplitudeY := 0;
end;

function TMoveCycleBehavior.PropertySections(const APropertyName: String): TPropertySections;
begin
  if ArrayContainsString(APropertyName, ['SpeedX', 'SpeedY', 'AmplitudeX', 'AmplitudeY', 'ActiveInEditor']) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(APropertyName);
end;

procedure TMoveCycleBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  if Assigned(Parent) and Parent.Exists then
{$IFDEF CASTLE_DESIGN_MODE}
    if FActiveInEditor then
{$ENDIF}
    begin
      Parent.Translation := Vector3(FSavedTranslationX + Sin(FSpeedX * FSeconds) * FAmplitudeX,
        FSavedTranslationY + Sin(FSpeedY * FSeconds) * FAmplitudeY, Parent.Translation.Z);
      FSeconds := FSeconds + SecondsPassed;
    end;
  if Assigned(Parent) and not Parent.Exists then
    FSeconds := 0;
end;

procedure TMoveCycleBehavior.ParentAfterAttach();
begin
  inherited;
  FSavedTranslationX := Parent.Translation.X;
  FSavedTranslationY := Parent.Translation.Y;
end;

end.
