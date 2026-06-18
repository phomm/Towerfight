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
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse, CastleComponentSerialize;

type
  TViewMain = class(TCastleView)
  published
    ButtonStart, ButtonLeaders, ButtonExit, ButtonOptions, ButtonCredits: TCastleButton;
    GroupOptions, GroupDifficulty: TCastleUserInterface;
    SliderMusic, SliderFullscreen, SliderUseTimer: TCastleIntegerSlider;
    FactoryButton: TCastleComponentFactory;
    ImageRoomRoof2, ImageRoomRoof3, ImageOptions, ImageDifficulty: TCastleImageControl;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start(); override;
    procedure Resume; override;
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
    procedure SliderFullscreenChange(Sender: TObject);
    procedure SliderUseTimerChange(Sender: TObject);
    procedure WindowCloseConfirmed(Sender: TObject);
  end;

var
  ViewMain: TViewMain;

implementation

uses 
// System
  SysUtils, 
// Castle  
  castlewindow, castlemessages, castlesoundengine, CastleApplicationProperties, castlelog,
// Own
  Common, gameviewgame, gameviewleaders, gameviewcredits, gameentities, gameoptions, 
  audiocomponent, gameviewdialog, difficultybutton
  ;

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
  DesignPreload := True;
end;

procedure TViewMain.WindowCloseQuery(Container: TCastleContainer);
begin
  DialogYesNo(Container, 'Do you really|want to exit?', WindowCloseConfirmed, nil);
end;

procedure TViewMain.WindowCloseConfirmed(Sender: TObject);
begin
  Application.MainWindow.Close();
end;

procedure TViewMain.Start();
var  
  LDifficulty: NDifficulty;
  LButton: TCastleButton;
  LDifficultyButton: TDifficultyButton;
begin
  inherited;
  SetLength(Buttons, 5);
  Buttons[0] := ButtonStart;
  Buttons[1] := ButtonLeaders;
  Buttons[2] := ButtonOptions;
  Buttons[3] := ButtonCredits;
  Buttons[4] := ButtonExit;
  ButtonExit.Exists := ApplicationProperties.ShowUserInterfaceToQuit;
  ImageRoomRoof3.Exists := ApplicationProperties.ShowUserInterfaceToQuit;
  SliderFullScreen.Exists := ApplicationProperties.ShowUserInterfaceToQuit;
  
  for LButton in Buttons do
    LButton.OnMotion := ButtonMotion;
  ImageOptions.OnMotion := ButtonMotion;
  ImageDifficulty.OnMotion := ButtonMotion;
  
  ButtonExit.OnClick := ButtonExitClick;
  ButtonStart.OnClick := ButtonStartClick;
  ButtonLeaders.OnClick := ButtonLeadersClick;
  ButtonOptions.OnClick := ButtonOptionsClick;
  ButtonCredits.OnClick := ButtonCreditsClick;

  SliderMusic.Value := MusicLevel();
  SliderMusic.OnChange := SliderMusicChange;

  SliderFullscreen.Value := Ord(Fullscreen());
  SliderFullscreen.OnChange := SliderFullscreenChange;

  SliderUseTimer.Value := Ord(UseTimer());
  SliderUseTimer.OnChange := SliderUseTimerChange;

  for LDifficulty in NDifficulty do
  begin
    LDifficultyButton := TDifficultyButton.Create(GroupDifficulty);
    LButton := FactoryButton.ComponentLoad(GroupDifficulty, LDifficultyButton) as TCastleButton;
    WriteLnLog(LButton.ComponentCount.toString);
    GroupDifficulty.InsertFront(LButton);
    LDifficultyButton.Init(LDifficulty, LButton, ButtonDifficultyClick);
  end;
end;

procedure TViewMain.Resume;
begin
  inherited;
  if not Assigned(SoundEngine.LoopingChannel[0].Sound) or 
    (Pos('Menu', SoundEngine.LoopingChannel[0].Sound.Name) <= 0) then 
  begin
    SoundEngine.LoopingChannel[0].Sound := Audio.RandomMenuTheme;
    SoundEngine.LoopingChannel[0].Sound.Volume := MusicLevel() / 100;
  end;
  SetIsSchool(False);
end;

procedure TViewMain.ButtonDifficultyClick(Sender: TObject);
var
  LButton: TCastleUserInterface;
begin
  for LButton in GroupDifficulty do
    if LButton is TCastleButton then
    begin
      TCastleButton(LButton).Pressed := LButton = Sender;
      if TCastleButton(LButton).Pressed then
      begin
        if LButton.Tag = Ord(gdTutorial) then
          SetIsSchool(True) 
        else 
          SetDifficulty(NDifficulty(TCastleButton(LButton).Tag));
        Container.View := ViewGame;
      end;
    end;
end;

procedure TViewMain.SliderMusicChange(Sender: TObject);
var
  LMusicLevel: Integer;
begin
  LMusicLevel := (Sender as TCastleIntegerSlider).Value;
  SoundEngine.LoopingChannel[0].Sound.Volume := LMusicLevel / 100;
  SetMusicLevel(LMusicLevel);
end;

procedure TViewMain.SliderFullscreenChange(Sender: TObject);
begin
  SetFullscreen((Sender as TCastleIntegerSlider).Value = 1);
end;

procedure TViewMain.SliderUseTimerChange(Sender: TObject);
begin
  SetUseTimer((Sender as TCastleIntegerSlider).Value = 1);
end;

procedure TViewMain.ButtonMotion(const Sender: TCastleUserInterface; const Event: TInputMotion; var Handled: Boolean);
var
  LButton: TCastleButton;
begin
  for LButton in Buttons do 
    LButton.ImageScale := 0;
  if not (Sender is TCastleButton) then
  begin
    Handled := True;
    Exit;
  end;
  
  (Sender as TCastleButton).ImageScale := 0.08;
  (Sender as TCastleButton).ImageScale := 0.08;
  if Sender <> ButtonOptions then
    GroupOptions.Exists := False;
  if Sender <> ButtonStart then
    GroupDifficulty.Exists := False;
end;

procedure TViewMain.ButtonExitClick(Sender: TObject);
begin
  WindowCloseQuery(Container);
end;

procedure TViewMain.ButtonStartClick(Sender: TObject);
begin
  GroupDifficulty.Exists := not GroupDifficulty.Exists;
  if not GroupDifficulty.Exists then
    Container.View := ViewGame;
end;

procedure TViewMain.ButtonLeadersClick(Sender: TObject);
begin
  Container.View := ViewLeaders;
end;

procedure TViewMain.ButtonOptionsClick(Sender: TObject);
begin
  GroupOptions.Exists := not GroupOptions.Exists;
end;

procedure TViewMain.ButtonCreditsClick(Sender: TObject);
begin
  {
  if not IsSchoolDone() then
  begin
    SetIsSchool(True);
    Container.View := ViewGame;
  end
  else
  }  
    Container.View := ViewCredits;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEscape) then
  begin
    ButtonExit.DoClick();
    Exit(true); // key was handled
  end;
  if Event.IsKey(keyEnter) then
  begin
    ButtonStart.DoClick();
    Exit(true); // key was handled
  end;
end;

end.
