unit GameViewGame;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewGame = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    // ButtonXxx: TCastleButton;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewGame: TViewGame;

implementation

uses 
// System
  SysUtils,
// Castle  
  castlewindow;

constructor TViewGame.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewgame.castle-user-interface';
end;

procedure TViewGame.Start;
begin
  inherited;
  { Executed once when view starts. }
end;

procedure TViewGame.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }

end;

function TViewGame.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEscape) then
  begin
    Container.PopView();
    Exit(true); // key was handled
  end; 
end;

end.
