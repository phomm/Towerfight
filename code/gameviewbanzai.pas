unit GameViewBanzai;

{$mode delphi}

interface

uses 
// system
  Classes,
// castle
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse, CastleScene, castlesoundengine,
// Own
  RoomComponent;

type
  TViewBanzai = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    TimerClose: TCastleTimer;
    SoundBanzai: TCastleSound;
    ImageBattleCry: TCastleImageTransform;
  public
    CallbackRoomComponent: TRoomComponent;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  private
    procedure TimerCloseTick(Sender: TObject);
  end;

var
  ViewBanzai: TViewBanzai;

implementation

uses
// Own
  Behaviors, gameviewgame;

constructor TViewBanzai.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewbanzai.castle-user-interface';
end;

procedure TViewBanzai.Start;
var
  LScalingBehavior: TScalingBehavior;
begin
  inherited;
  InterceptInput := True;
  TimerClose.OnTimer := TimerCloseTick;
  LScalingBehavior := TScalingBehavior.Create(Self);
  LScalingBehavior.ScaleAdd := Vector3(2, 2, 2);
  ImageBattleCry.AddBehavior(LScalingBehavior);
  SoundEngine.Play(SoundBanzai);
end;

procedure TViewBanzai.TimerCloseTick(Sender: TObject);
begin
  (ImageBattleCry.FindBehavior(TScalingBehavior) as TScalingBehavior).Reset();
  Container.PopView();
  if Assigned(CallbackRoomComponent) then
    ViewGame.RunFight(CallbackRoomComponent);
end;

end.
