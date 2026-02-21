unit GameViewFormula;

interface

uses 
// System
  Classes, generics.collections,
// Castle
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
// Own  
  gameentities, roomcomponent;

type
  NElementReplacement = (erNone, erRight, erInPlace);

  TViewFormula = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonGo: TCastleButton;
    GroupElements: TCastleHorizontalGroup;
  public
    RoomComponent: TRoomComponent;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Resume; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    procedure ButtonGoClick(Sender: TObject);
  private
    Formula: string;
    Weapon: NHeroWeapon;
    FPositions: specialize TArray<NElementReplacement>;
    FPosition: Integer;
    function GetFormula(): string;
    function CalcFormula(AFormula: string): Integer;
    procedure HandleClick(ASender: TObject);
    procedure SetPosition(AValue: Integer);
  end;

var
  ViewFormula: TViewFormula;

implementation

uses 
// System
  SysUtils, TypInfo,
// Castle  
  castlewindow, castlelog, castleglimages, castlecolors,
  // Own
  gameviewgame, Common;

constructor TViewFormula.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewformula.castle-user-interface';
end;

procedure TViewFormula.Start;
begin
  inherited;
  ButtonGo.OnClick := @ButtonGoClick;
  InterceptInput := True;
end;

procedure TViewFormula.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
end;

procedure TViewFormula.HandleClick(ASender: TObject);
begin
  SetPosition((ASender as TCastleButton).Tag);
end;

procedure TViewFormula.Resume;
var
  I: Integer;
  LButton: TCastleButton;
  procedure Insert(const AValue: String; AMarked: Boolean = True);
  begin
    LButton := TCastleButton.Create(Self);
    LButton.FontSize := 70;
    LButton.CustomBackground := True;
    if AMarked then
    begin
      LButton.ImageScale := 0.2;
      LButton.Image.Url := 'castle-data:/resources/' + WeaponFileNames[Weapon];      
    end
    else
    begin
      LButton.Caption := AValue;
      if FPositions[I] <> erNone then
      begin
        LButton.OnClick := @HandleClick;
        LButton.Tag := I;
        LButton.Border.Bottom := 4;
        LButton.BorderColor := Black;
      end;
    end;
    GroupElements.InsertFront(LButton);
  end;
begin
  inherited;
  Weapon := TMap.Map.Hero.Weapon;
  Formula := TMap.Map.GetRoomByIndex(RoomComponent.Tag).Actors[0].Visual;
  GroupElements.ClearControls();
  SetLength(FPositions, Length(Formula) + 1);
  for I := 1 to Length(Formula) do
  begin
    if (Formula[I] in ['+', '-', '*']) and (Formula[I] = WeaponToOperation[Weapon]) then
      FPositions[I] := erNone
    else if (Formula[I] in ['+', '-', '*']) and (Formula[I] <> WeaponToOperation[Weapon]) then
      FPositions[I] := erInPlace
    else if ((Formula[I] in ['0'..'9']) and (I < Length(Formula)) and (Formula[I + 1] in ['0'..'9'])) then
      FPositions[I] := erRight
    else
      FPositions[I] := erNone;
  end;
  FPosition := 0;
  for I := 1 to Length(Formula) do
  begin
    Insert(Formula[I], False);
    if (FPosition = 0) and (FPositions[I] = erRight) then
    begin
      Insert(WeaponToOperation[Weapon]);
      FPosition := I;
    end;
    if (FPosition = 0) and (FPositions[I] = erInPlace) then
    begin
      GroupElements.Controls[GroupElements.ControlsCount-1].Exists := False;
      Insert(WeaponToOperation[Weapon]);
      FPosition := I;
    end;
  end;
end;

procedure TViewFormula.ButtonGoClick(Sender: TObject);
var
  LActor: TActor;
  LNewLevel: Integer;
  LNewFormula: String;
begin
  LActor := TMap.Map.GetRoomByIndex(RoomComponent.Tag).Actors[0];
  // update Actor formula and level
  LNewFormula := GetFormula();
  LNewLevel := CalcFormula(LNewFormula);
  (LActor as TEnemy).Formula := LNewFormula;
  LActor.Level := IIF(LNewLevel < 0, 0, LNewLevel);
  TMap.Map.Hero.UseWeapon();
  Container.PopView();
  ViewGame.RoomFight(RoomComponent);
end;

function TViewFormula.GetFormula(): String;
var
  I: Integer;
  LButton: TCastleButton;
begin
  Result := '';
  for I := 0 to GroupElements.ControlsCount - 1 do
  begin
    LButton := GroupElements.Controls[I] as TCastleButton;
    if LButton.Exists then
      Result := Result + IIF(LButton.Tag > 0, WeaponToOperation[TMap.Map.Hero.Weapon], LButton.Caption);
  end;
end;

function TViewFormula.CalcFormula(AFormula: string): Integer;
var
  I, Arg1: Integer;
  LValue: Integer;
  LOp: NHeroWeapon;
  procedure Calc();
  begin
    case LOp of
      hwPlus: Result := Result + LValue;
      hwMinus: Result := Result - LValue;
      hwMultiply: Result := Result * LValue;
    end;
  end;
  function MultLen(): Integer;
  begin
    Result := Length(LValue.ToString) + 1 + Length(Arg1.ToString);
  end;
begin
  LOp := hwNo;
  LValue := 0;
  WriteLnLog('Original formula: ' + AFormula);
  I := 1;
  while I <= Length(AFormula) do
  begin
    if AFormula[I] in ['0'..'9'] then
      LValue := LValue * 10 + Ord(AFormula[I]) - Ord('0')
    else if (AFormula[I] in ['+', '-']) and (LOp = hwNo) then
      LValue := 0
    else if (AFormula[I] in ['*']) and (LOp = hwNo) then
    begin
      LOp := hwMultiply;
      Arg1 := LValue;
      LValue := 0;
    end
    else if (AFormula[I] in ['+', '-', '*']) and (LOp = hwMultiply) then
    begin
      AFormula := (Arg1 * LValue).ToString + Copy(AFormula, MultLen + 1);
      LOp := hwNo; 
    end;
    Inc(I);
  end;
  if LOp = hwMultiply then
    AFormula := Copy(AFormula, 1, Length(AFormula) - MultLen) + (Arg1 * LValue).ToString;
  WriteLnLog('Modified formula: ' + AFormula);

  Result := 0;
  LValue := 0;  
  LOp := hwNo; // reset operator to avoid applying it to the last value
  for I := 1 to Length(AFormula) do
  begin
    if AFormula[I] in ['0'..'9'] then
      LValue := LValue * 10 + Ord(AFormula[I]) - Ord('0')
    else if AFormula[I] in ['+', '-', '*'] then
    begin
      if Result > 0 then 
        Calc()
      else
        Result := LValue; 
      case AFormula[I] of
        '+': LOp := hwPlus;
        '-': LOp := hwMinus;
        '*': LOp := hwMultiply;
      end;
      LValue := 0;
    end;
  end;
  Calc(); // add the last value
end;

function TViewFormula.Press(const Event: TInputPressRelease): Boolean;
var
  I, LNewPosition: Integer;
  LFound: Boolean;
begin
  Result := inherited;
  //if Result then Exit; // allow the ancestor to handle keys
  //WriteLnLog('Pressed: ' + EnumName(TypeInfo(TKey), Ord(Event.Key)));
  if Event.IsKey(keyEnter) then
  begin
    ButtonGo.DoClick();
    Exit(true); // key was handled
  end;

  if Event.IsKey(keyTab) then
  begin
    LFound := False;
    for I := FPosition + 1 to Length(FPositions) - 1 do
    begin
      if FPositions[I] <> erNone then
      begin
        LNewPosition := I;
        LFound := True;
        Break;
      end;
    end;
    if not LFound then
      for I := 1 to FPosition - 1 do
      begin 
        if FPositions[I] <> erNone then
        begin 
          LNewPosition := I;
          LFound := True;
          Break;
        end;
      end;
    if LFound then
      SetPosition(LNewPosition);
    Exit(true); // key was handled
  end;
end;

procedure TViewFormula.SetPosition(AValue: Integer);
var
  LControl: TCastleButton;
begin
  LControl := GroupElements.Controls[FPosition] as TCastleButton;
  GroupElements.RemoveControl(LControl);
  if FPositions[FPosition] = erInPlace then
    GroupElements.Controls[FPosition - 1].Exists := True;
  GroupElements.InsertControl(AValue, LControl);
  if FPositions[AValue] = erInPlace then
    GroupElements.Controls[AValue - 1].Exists := False;
  FPosition := AValue;
end;

end.
