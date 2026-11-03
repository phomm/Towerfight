unit GameViewWin;

{$mode delphi}

interface

uses 
// System
  Classes,
// Castle
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse, CastleNotifications;

type
  TViewWin = class(TCastleView)
  published
    ButtonMenu, ButtonSubmit, ButtonRandomName: TCastleButton;
    LabelScore: TCastleLabel;
    EditName: TCastleEdit;
    PanelNotifications: TCastleNotifications;
    ImageControl1: TCastleImageControl;
  public
    Score: Integer;    
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Resume; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    procedure ButtonMenuClick(Sender: TObject);
    procedure ButtonSubmitClick(Sender: TObject);
    procedure ButtonRandomNameClick(Sender: TObject);
    procedure SubmitScores();
    procedure SubmitScoresFinished(const AContent: string; ASuccess: Boolean);
    procedure BackToMenu();
    procedure GuiMode(AEnable: Boolean);
  end;

var
  ViewWin: TViewWin;

implementation

uses
// System
  SysUtils, fpjson,
// Castle
  CastleSoundEngine, castlelog,
// Own
  gameviewmain, audiocomponent, gameoptions, gameviewleaders, gameviewcredits, castleRest, gameentities, models, Common;

constructor TViewWin.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewwin.castle-user-interface';
  DesignPreload := True;
end;

procedure TViewWin.Start;
begin
  inherited;
  ButtonMenu.OnClick := ButtonMenuClick;
  ButtonSubmit.OnClick := ButtonSubmitClick;
  ButtonRandomName.OnClick := ButtonRandomNameClick;
  SoundEngine.LoopingChannel[0].Sound := Audio.RandomWinTheme;
  SoundEngine.LoopingChannel[0].Sound.Volume := 5 * MusicLevel() / 100;
  ImageControl1.Exists := not IsSchool();
end;

procedure TViewWin.Resume;
begin
  inherited;
  LabelScore.Caption := Score.ToString();
  EditName.Text := UserName();
  EditName.Enabled := True;
end;

procedure TViewWin.ButtonMenuClick(Sender: TObject);
begin
  BackToMenu();
end;

procedure TViewWin.BackToMenu();
begin
  Container.View := ViewMain;
end;

procedure TViewWin.ButtonRandomNameClick(Sender: TObject);
  function RandomName(): string;
  const
    FirstParts: array[0..23] of string = (
      'Aki', 'Haru', 'Kazu', 'Masa', 'Nobu', 'Ryu',
      'Tada', 'Taka', 'Yoshi', 'Kiyo', 'Mitsu', 'Naga',
      'Shige', 'Tomo', 'Sada', 'Katsu', 'Haya', 'Ichi',
      'Kane', 'Mune', 'Suke', 'Yuki', 'Hachi', 'Raiden');
    SecondParts: array[0..23] of string = (
      'hiro', 'nobu', 'taka', 'kage', 'mori', 'yoshi',
      'masa', 'hiko', 'zane', 'haru', 'tsune', 'akira',
      'suke', 'maru', 'tora', 'katsu', 'fusa', 'chika',
      'take', 'mitsu', 'sada', 'kuni', 'hide', 'yori');
    Suffixes: array[0..7] of string = (
      '-san', '-sama', '-kun', '-chan', '-hime', '-maru', '-dono', '-no-kami');
  function Part(const A: array of string): string;
  begin
    Result := A[Random(Length(A))];
  end;
  begin
    Result := Part(FirstParts) + Part(SecondParts);
    if Random(4) <> 0 then
      Result := Result + Part(Suffixes);
  end;
begin
  EditName.Text := RandomName();
end;

procedure TViewWin.ButtonSubmitClick(Sender: TObject);
begin
  if Trim(EditName.Text) = '' then
  begin
    PanelNotifications.Show('Empty name passed');
    Exit;
  end;
  
  GuiMode(False);
  SubmitScores();
end;

procedure TViewWin.GuiMode(AEnable: Boolean);
begin
  EditName.Enabled := AEnable;
  ButtonSubmit.Enabled := AEnable;
  ButtonRandomName.Enabled := AEnable;
end;

function TViewWin.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEnter) then
    ButtonSubmit.DoClick();

  if Event.IsKey(keyEscape) then
  begin
    BackToMenu();
    Exit(True); // key was handled
  end;
end;

procedure TViewWin.SubmitScores();
const 
  Forbidden: set of Char = [':','/'];
var
  LLeader: TSubmitLeader;
  LName, LForbidden: string;
  Ch: Char;
begin
  if not TCastleRest.IsRunning() then
  begin
    PanelNotifications.Show('Another request is running');
    Exit;
  end;  
  LForbidden := '';
  LName := '';
  EditName.Text := EditName.Text.Trim().ToLower();
  for Ch in EditName.Text do
    if Ch in Forbidden then
      LForbidden := LForbidden + Ch
    else
      LName := LName + Ch;
  EditName.Text := LName;
  if LForbidden <> '' then
    PanelNotifications.Show('Name Corrected. ' + LForbidden + ' symbols are not allowed');
  PanelNotifications.Show('Sending Score to server...');
  LLeader := TSubmitLeader.Create(LName, Score, Difficulty());
  try
    TCastleRest.ServerRequest(ServerApiUrl, SubmitScoresFinished, LLeader.Serialize());
  finally
    FreeAndNil(LLeader);
  end;
end;

procedure TViewWin.SubmitScoresFinished(const AContent: string; ASuccess: Boolean);
var
  LJsonObject: TJsonObject;
  LProblemDetails: TProblemDetails;
begin
  if ASuccess then
  begin
    if ParseJsonObject(AContent, LJsonObject) then
    begin
      SetUserGuid(LJsonObject.Strings['guid']);
      SetUserName(EditName.Text);
    end;
    ViewLeaders.NeedsSync := True;
    Container.View := ViewLeaders;
    Exit;
  end;
  if ParseJsonObject(AContent, LJsonObject) then
  begin
    LProblemDetails := TProblemDetails.Create();
    ReadJsonToObject<TProblemDetails>(LJSONObject, LProblemDetails);
    PanelNotifications.Show(LProblemDetails.ToString());
    FreeAndnil(LProblemDetails);
  end
  else 
    PanelNotifications.Show('Submit failed: ' + AContent);
  GuiMode(True);
end;

end.
