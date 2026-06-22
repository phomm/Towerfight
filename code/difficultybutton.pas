unit difficultybutton;

{$mode delphi}

interface

uses 
// System
  Classes,
// Castle
  CastleUIControls, CastleControls, CastleKeysMouse, castlescene,
// Own
  gameoptions;

type
  TDifficultyButton = class(TCastleButton)
  published
    ImageMarkerRight, ImageMarkerLeft: TCastleImageControl;
  public
    procedure Init(ADifficulty: NDifficulty);
    procedure ButtonDifficultyMotion(const Sender: TCastleUserInterface; const Event: TInputMotion; var Handled: Boolean);  
  end;

implementation

uses
// System
  SysUtils, 
// Castle  
  CastleVectors, castleColors, 
// Own
  gameviewmain, Common;

procedure TDifficultyButton.Init(ADifficulty: NDifficulty);
begin
  Caption := DifficultyName(ADifficulty);
  Tag := Ord(ADifficulty);
  Pressed := Caption = DifficultyName(Difficulty());
  ImageMarkerLeft.Color := Vector4((NonVisualComponents[Tag] as TCastleFog).Color, 1);
  ImageMarkerRight.Color := ImageMarkerLeft.Color;
  ImageMarkerRight.Exists := False;
  ImageMarkerLeft.Exists := False;
  OnMotion := ButtonDifficultyMotion;
end;

procedure TDifficultyButton.ButtonDifficultyMotion(const Sender: TCastleUserInterface; const Event: TInputMotion; var Handled: Boolean);
var
  LButton: TCastleUserInterface;
begin
  for LButton in Sender.Parent do
    if LButton is TDifficultyButton then
    with (LButton as TDifficultyButton) do
    begin
      ImageMarkerLeft.Exists := Sender = LButton;
      ImageMarkerRight.Exists := Sender = LButton;
    end;
end;

end.
