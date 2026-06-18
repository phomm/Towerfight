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
  TDifficultyButton = class(TCastleUserInterface)
  published
    ImageMarkerRight, ImageMarkerLeft: TCastleImageControl;
    Tutorial, Easy, Normal, Hard, Insane: TCastleFog;
  public
    procedure Init(ADifficulty: NDifficulty; AButton: TCastleButton; AOnClick: TNotifyEvent);
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

procedure TDifficultyButton.Init(ADifficulty: NDifficulty; AButton: TCastleButton; AOnClick: TNotifyEvent);
begin
  AButton.Caption := DifficultyName(ADifficulty);
  AButton.Tag := Ord(ADifficulty);
  AButton.Pressed := AButton.Caption = DifficultyName(Difficulty());
  ImageMarkerLeft.Color := Vector4((AButton.Parent.FindComponent(AButton.Caption) as TCastleFog).Color, 1);
  ImageMarkerRight.Color := ImageMarkerLeft.Color;
  ImageMarkerRight.Exists := False;
  ImageMarkerLeft.Exists := False;
  AButton.OnMotion := ButtonDifficultyMotion;
  AButton.OnClick := AOnClick;
end;

procedure TDifficultyButton.ButtonDifficultyMotion(const Sender: TCastleUserInterface; const Event: TInputMotion; var Handled: Boolean);
var
  LButton: TCastleUserInterface;
begin
  for LButton in Sender.Parent do
    if LButton is TCastleButton and (LButton.ControlsCount > 0) then
    begin
      LButton.Controls[0].Exists := Sender = LButton;
      LButton.Controls[1].Exists := Sender = LButton;
    end;
end;

end.
