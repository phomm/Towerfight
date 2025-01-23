unit GameViewGame;

{$mode delphi}

interface

uses 
// System
Classes,
// Castle
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
// Own
  gameentities;

type
  TViewGame = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonDefeat: TCastleButton;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    procedure ButtonDefeatClick(Sender: TObject);
  private
    FMap: TMap;
  end;

var
  ViewGame: TViewGame;

implementation

uses 
// System
  SysUtils, typinfo,
// Castle  
  castlewindow, castleconfig, castlemessages,
// Own
  Common, GameViewDefeat, gameviewmain;

constructor TViewGame.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewgame.castle-user-interface';
end;

procedure TViewGame.Start();
var
  LDifficultyString: string;
  LDifficulty: NDifficulty;
begin
  inherited;
  ButtonDefeat.OnClick := ButtonDefeatClick;
  LDifficultyString := UserConfig.GetValue(DifficultyKey, '');
  LDifficulty := NDifficulty(EnumValue(TypeInfo(NDifficulty), LDifficultyString));
  if LDifficulty = gdNone then
    LDifficulty := gdEasy;
  FMap := TMap.Create(Self, LDifficulty);
  // test
  ButtonDefeat.Caption := LDifficultyString;
end;

procedure TViewGame.Stop();
begin
  FreeAndNil(FMap);
end;

procedure TViewGame.ButtonDefeatClick(Sender: TObject);
begin
  Container.PushView(ViewDefeat);
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

  if Event.IsKey(keyEscape) and MessageYesNo(Application.MainWindow, 'Game would be lost. Exit to menu?') then
  begin
    Container.PopView();
    Exit(true); // key was handled
  end; 
end;

end.
