unit GameViewScore;

{$mode delphi}

interface

uses 
// system
  Classes, sysutils,
// castle
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse, CastleScene, CastleViewport, CastleSoundEngine,
// own
  Behaviors;

type
  TViewScore = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    TimerClose: TCastleTimer;
    SoundScore: TCastleSound;
    TextScore: TCastleText;
    ViewportScore: TCastleViewport;
  private
    FCounter: Integer;
    FTimeout: Single;
    FPosition: TVector2;
    FPlayingSound: TCastlePlayingSound;
    FScalingBehavior: TScalingBehavior;
    procedure TimerCloseTick(Sender: TObject);
    function GetIsShown(): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Show(AAnimTime: Single; const APosition: TVector2);
    procedure UpdateScore();
    procedure StopScaling();
    property IsShown: Boolean read GetIsShown;
  end;

var
  ViewScore: TViewScore;

implementation

uses
// castle
  CastleColors, CastleFonts,
// Own
  GameViewWin, gameentities;

constructor TViewScore.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewscore.castle-user-interface';
  FScalingBehavior := TScalingBehavior.Create(Self);
  DesignPreload := True;
end;

procedure TViewScore.Start;
var
  LTextColor: TCastleColor;
begin
  inherited;
  InterceptInput := True;
  TimerClose.OnTimer := TimerCloseTick;
  TimerClose.IntervalSeconds := FTimeout;
  ViewportScore.Translation := FPosition;

  FPlayingSound := TCastlePlayingSound.Create(Self);
  FPlayingSound.Sound := SoundScore;
  FPlayingSound.Loop := True;
  FPlayingSound.Pitch := 10;
  if ViewWin.Score <> TMap.Map.Hero.Level then
    SoundEngine.Play(FPlayingSound);
  
  LTextColor := TextScore.Color;
  TextScore.CustomFont := Container.DefaultFont as TCastleFont;
  TextScore.Color := LTextColor;
  FScalingBehavior.ScaleAdd := Vector3(30, 30, 30);
  TextScore.AddBehavior(FScalingBehavior);
  UpdateScore();
end;

procedure TViewScore.Show(AAnimTime: Single; const APosition: TVector2);
begin
  FTimeout := AAnimTime;
  FPosition := APosition;
  FCounter := 0;
  Container.PushView(Self);
end;

procedure TViewScore.UpdateScore();
begin
  TextScore.Caption := Format('Score: %d', [TMap.Map.Hero.Level]);
  TMap.Map.Hero.Level := TMap.Map.Hero.Level + 1;
  Inc(FCounter);
  if FCounter >= 100 then
    FScalingBehavior.ScaleAdd := Vector3(1, 1, 1);
end;

procedure TViewScore.StopScaling();
begin
  FScalingBehavior.ScaleAdd := Vector3(1, 1, 1);
  FPlayingSound.Stop();
end;

procedure TViewScore.TimerCloseTick(Sender: TObject);
begin
  Container.View := ViewWin;
end;

function TViewScore.GetIsShown(): Boolean;
begin
  Result := Container.CurrentFrontView = Self;
end;

end.
