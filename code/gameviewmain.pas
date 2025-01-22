{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewMain;

{$mode delphi}

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonStart, ButtonLeaders, ButtonExit, ButtonOptions, ButtonCredits: TCastleButton;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    SelectedButton: TCastleButton;
    procedure ButtonMotion(const Sender: TCastleUserInterface; const Event: TInputMotion; var Handled: Boolean);
    procedure ButtonExitClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonLeadersClick(Sender: TObject);
    procedure ButtonOptionsClick(Sender: TObject);
    procedure ButtonCreditsClick(Sender: TObject);
  end;

var
  ViewMain: TViewMain;

implementation

uses 
// System
  SysUtils,
// Castle  
  castlewindow, castlemessages,
// Own
  gameviewgame, gameviewleaders, gameviewcredits
  ;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonExit.OnMotion := ButtonMotion;
  ButtonStart.OnMotion := ButtonMotion;
  ButtonLeaders.OnMotion := ButtonMotion;
  ButtonOptions.OnMotion := ButtonMotion;
  ButtonCredits.OnMotion := ButtonMotion;
  ButtonExit.OnClick := ButtonExitClick;
  ButtonStart.OnClick := ButtonStartClick;
  ButtonLeaders.OnClick := ButtonLeadersClick;
  ButtonOptions.OnClick := ButtonOptionsClick;
  ButtonCredits.OnClick := ButtonCreditsClick;
end;

procedure TViewMain.ButtonMotion(const Sender: TCastleUserInterface; const Event: TInputMotion; var Handled: Boolean);
begin
  if SelectedButton = Sender then Exit;
  if SelectedButton <> nil then
    SelectedButton.ImageScale := 0;
  SelectedButton := Sender as TCastleButton;
  SelectedButton.ImageScale := 1;
end;

procedure TViewMain.ButtonExitClick(Sender: TObject);
begin
  if MessageYesNo(Application.MainWindow, 'Do you really want to exit?') then
    Application.MainWindow.Close();
end;

procedure TViewMain.ButtonStartClick(Sender: TObject);
begin
  Container.PushView(ViewGame);
end;

procedure TViewMain.ButtonLeadersClick(Sender: TObject);
begin
  Container.PushView(ViewLeaders);
end;

procedure TViewMain.ButtonOptionsClick(Sender: TObject);
begin
  // 
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
