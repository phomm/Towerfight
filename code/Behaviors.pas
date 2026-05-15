unit Behaviors;

{$mode delphi}

interface

uses 
// System
  Classes, 
// Castle
  CastleClassUtils, CastleVectors, castletransform, castlecolors;

type

  TMoveCycleVerticalBehavior = class(TCastleBehavior)
  strict private
    FSpeed, FSavedTranslationY, FAmplitude, FSeconds: Single;
    FActiveInEditor: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const APropertyName: String): TPropertySections; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure ParentAfterAttach(); override;
  published
    property Speed: Single read FSpeed write FSpeed default 1;
    property ActiveInEditor: Boolean read FActiveInEditor write FActiveInEditor default False;
    property Amplitude: Single read FAmplitude write FAmplitude default 1;
  end;

implementation

uses 
// System
  SysUtils,
// Castle
  CastleComponentSerialize, CastleUtils
// Own
  
  ;

constructor TMoveCycleVerticalBehavior.Create(AOwner: TComponent);
begin
  inherited;
  FSpeed := 1;
  FAmplitude := 1;
end;

function TMoveCycleVerticalBehavior.PropertySections(const APropertyName: String): TPropertySections;
begin
  if ArrayContainsString(APropertyName, ['Speed', 'Amplitude', 'ActiveInEditor']) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(APropertyName);
end;

procedure TMoveCycleVerticalBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  if Assigned(Parent) and Parent.Exists then
{$IFDEF CASTLE_DESIGN_MODE}
    if FActiveInEditor then
{$ENDIF}
    begin
      Parent.Translation := Vector3(Parent.Translation.X, FSavedTranslationY + Sin(FSpeed * FSeconds) * FAmplitude, Parent.Translation.Z);
      FSeconds := FSeconds + SecondsPassed;
    end;
  if Assigned(Parent) and not Parent.Exists then
    FSeconds := 0;
end;

procedure TMoveCycleVerticalBehavior.ParentAfterAttach();
begin
  inherited;
  FSavedTranslationY := Parent.Translation.Y;
end;

end.
