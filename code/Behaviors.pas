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

  TLifeTimeBehavior = class(TCastleBehavior)
  strict private
    FLifeTimeSeconds, FLife: Single;
    FSavedExists: Boolean;
    FActiveInEditor: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const APropertyName: String): TPropertySections; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure Reset();
  published
    property LifeTimeSeconds: Single read FLifeTimeSeconds write FLifeTimeSeconds default 1;
    property ActiveInEditor: Boolean read FActiveInEditor write FActiveInEditor default False;
  end;

  TMoveUpBehavior = class(TCastleBehavior)
  strict private
    FSpeedX, FSpeedY: Single;
    FActiveInEditor: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySections(const APropertyName: String): TPropertySections; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  published
    property SpeedX: Single read FSpeedX write FSpeedX default 0;
    property SpeedY: Single read FSpeedY write FSpeedY default 1;
    property ActiveInEditor: Boolean read FActiveInEditor write FActiveInEditor default False;
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

constructor TLifeTimeBehavior.Create(AOwner: TComponent);
begin
  inherited;
  FLifeTimeSeconds := 1;
  FSavedExists := False;
end;

function TLifeTimeBehavior.PropertySections(const APropertyName: String): TPropertySections;
begin
  if ArrayContainsString(APropertyName, ['LifeTimeSeconds', 'ActiveInEditor']) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(APropertyName);
end;

procedure TLifeTimeBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
{$IFDEF CASTLE_DESIGN_MODE}
  if FActiveInEditor then
{$ENDIF}
  begin
    if Parent.Exists and not FSavedExists then 
    begin
      FLife := FLifeTimeSeconds;
      FSavedExists := True;
    end;
    if Parent.Exists then 
    begin
      FLife := FLife - SecondsPassed;
      if FLife <= 0 then
        Reset();
    end;
  end;
end;

procedure TLifeTimeBehavior.Reset();
begin
  FLife := FLifeTimeSeconds;
  Parent.Exists := False;
  FSavedExists := False;
end;

constructor TMoveUpBehavior.Create(AOwner: TComponent);
begin
  inherited;
  FSpeedX := 0;
  FSpeedY := 1;
end;

function TMoveUpBehavior.PropertySections(const APropertyName: String): TPropertySections;
begin
  if ArrayContainsString(APropertyName, ['SpeedX', 'SpeedY', 'ActiveInEditor']) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(APropertyName);
end;

procedure TMoveUpBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
{$IFDEF CASTLE_DESIGN_MODE}
  if FActiveInEditor then
{$ENDIF}
    if Assigned(Parent) and Parent.Exists then
      Parent.Translation := Parent.Translation + Vector3(FSpeedX, FSpeedY, 0) * SecondsPassed;
end;

end.
