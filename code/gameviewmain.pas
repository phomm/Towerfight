{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewMain;

{$mode delphi}

interface

uses 
// System
  Classes, generics.collections,
// Castle  
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonStart, ButtonLeaders, ButtonExit, ButtonOptions, ButtonCredits: TCastleButton;
    GroupOptions: TCastleUserInterface;
    SliderMusic: TCastleIntegerSlider;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start(); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    procedure WindowCloseQuery(Container: TCastleContainer);
  private
    Buttons: TArray<TCastleButton>;
    procedure ButtonMotion(const Sender: TCastleUserInterface; const Event: TInputMotion; var Handled: Boolean);
    procedure ButtonExitClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonLeadersClick(Sender: TObject);
    procedure ButtonOptionsClick(Sender: TObject);
    procedure ButtonCreditsClick(Sender: TObject);
    procedure ButtonDifficultyClick(Sender: TObject);
    procedure SliderMusicChange(Sender: TObject);

  end;

var
  ViewMain: TViewMain;

implementation

uses 
// System
  SysUtils, 
// Castle  
  castlewindow, castlemessages, castlesoundengine, CastleComponentSerialize,
// Own
  Common, gameviewgame, gameviewleaders, gameviewcredits, gameentities, gameoptions, audiocomponent
  ;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.WindowCloseQuery(Container: TCastleContainer);
begin
  if MessageYesNo(Application.MainWindow, 'Do you really want to exit?') then
    Application.MainWindow.Close();
end;

procedure TViewMain.Start();
var
  LButtonFactory: TCastleComponentFactory;
  LDifficulty: NDifficulty;
  LButton: TCastleButton;
  LMusicLevel: Integer;
  LCurrentDifficultyName: string;
begin
  inherited;
  SetLength(Buttons, 5);
  Buttons[0] := ButtonStart;
  Buttons[1] := ButtonLeaders;
  Buttons[2] := ButtonOptions;
  Buttons[3] := ButtonCredits;
  Buttons[4] := ButtonExit;
  for LButton in Buttons do
    LButton.OnMotion := ButtonMotion;
  
  ButtonExit.OnClick := ButtonExitClick;
  ButtonStart.OnClick := ButtonStartClick;
  ButtonLeaders.OnClick := ButtonLeadersClick;
  ButtonOptions.OnClick := ButtonOptionsClick;
  ButtonCredits.OnClick := ButtonCreditsClick;

  LMusicLevel := MusicLevel();
  if not Assigned(SoundEngine.LoopingChannel[0].Sound) then 
  begin
    SoundEngine.LoopingChannel[0].Sound := Audio.RandomMenuTheme;
  end;  
  SoundEngine.LoopingChannel[0].Sound.Volume := LMusicLevel / 10;
  SliderMusic.Value := LMusicLevel;
  SliderMusic.OnChange := SliderMusicChange;

  LButtonFactory := TCastleComponentFactory.Create(Self);
  try
    LButtonFactory.Url := 'castle-data:/buttonDifficulty.castle-user-interface';
    LCurrentDifficultyName := DifficultyName(Difficulty());
    for LDifficulty in NDifficulty do
    begin
      LButton := LButtonFactory.ComponentLoad(GroupOptions) as TCastleButton;
      LButton.Caption := DifficultyName(LDifficulty);
      LButton.Pressed := LButton.Caption = LCurrentDifficultyName;
      LButton.Tag := Ord(LDifficulty);
      LButton.OnClick := ButtonDifficultyClick;
      GroupOptions.InsertFront(LButton);
    end;
  finally
    FreeAndNil(LButtonFactory);
  end;
end;


procedure TViewMain.ButtonDifficultyClick(Sender: TObject);
var
  LButton: TCastleUserInterface;
begin
  for LButton in GroupOptions do
    if LButton is TCastleButton then
    begin
      TCastleButton(LButton).Pressed := LButton = Sender;
      if TCastleButton(LButton).Pressed then
        SetDifficulty(NDifficulty(TCastleButton(LButton).Tag));
    end;
end;

procedure TViewMain.SliderMusicChange(Sender: TObject);
var
  LMusicLevel: Integer;
begin
  LMusicLevel := (Sender as TCastleIntegerSlider).Value;
  SoundEngine.LoopingChannel[0].Sound.Volume := LMusicLevel / 10;
  SetMusicLevel(LMusicLevel);
end;

procedure TViewMain.ButtonMotion(const Sender: TCastleUserInterface; const Event: TInputMotion; var Handled: Boolean);
var
  LButton: TCastleButton;
begin
  for LButton in Buttons do 
    LButton.ImageScale := 0;
  (Sender as TCastleButton).ImageScale := 0.08;
  if Sender <> ButtonOptions then
    GroupOptions.Exists := False;
end;

procedure TViewMain.ButtonExitClick(Sender: TObject);
begin
  WindowCloseQuery(Container);
end;

procedure TViewMain.ButtonStartClick(Sender: TObject);
begin
  Container.View := (ViewGame);
end;

procedure TViewMain.ButtonLeadersClick(Sender: TObject);
begin
  Container.PushView(ViewLeaders);
end;

procedure TViewMain.ButtonOptionsClick(Sender: TObject);
begin
  GroupOptions.Exists := not GroupOptions.Exists;
end;

procedure TViewMain.ButtonCreditsClick(Sender: TObject);
begin
  Container.PushView(ViewCredits);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TViewMain.Press method should be used to handle keys
    not handled in children controls.
  }

  // Use this to handle keys:
  {
  if Event.IsKey(keyXxx) then
  begin
    // DoSomething;
    Exit(true); // key was handled
  end;
  }
end;

end.
