unit GameViewBanzai;

{$mode delphi}

interface

uses 
// system
  Classes,
// castle
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse, CastleScene, castlesoundengine,
// Own
  RoomComponent, Behaviors;

type
  TViewBanzai = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    TimerClose: TCastleTimer;
    SoundBanzai: TCastleSound;
    ImageBanzai: TCastleImageTransform;
  public
    CallbackRoomComponent: TRoomComponent;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  private
    FScalingBehavior: TScalingBehavior;
    procedure TimerCloseTick(Sender: TObject);
  end;

var
  ViewBanzai: TViewBanzai;

implementation

uses
// Own
  gameviewgame;

constructor TViewBanzai.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewbanzai.castle-user-interface';
  FScalingBehavior := TScalingBehavior.Create(Self);
  DesignPreload := True;
end;

procedure TViewBanzai.Start;
begin
  inherited;
  InterceptInput := True;
  TimerClose.OnTimer := TimerCloseTick;  
  FScalingBehavior.ScaleAdd := Vector3(2, 2, 2);
  ImageBanzai.AddBehavior(FScalingBehavior);
  SoundEngine.Play(SoundBanzai);
end;

procedure TViewBanzai.TimerCloseTick(Sender: TObject);
begin
  FScalingBehavior.Reset();
  Container.PopView();
  if Assigned(CallbackRoomComponent) then
    ViewGame.RunFight(CallbackRoomComponent);
end;

end.
