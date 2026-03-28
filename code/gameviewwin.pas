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
    ButtonMenu, ButtonSubmit: TCastleButton;
    LabelScore: TCastleLabel;
    EditName: TCastleEdit;
    PanelNotifications: TCastleNotifications;
  public
    Score: Integer;    
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Resume; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    procedure ButtonMenuClick(Sender: TObject);
    procedure ButtonSubmitClick(Sender: TObject);
    procedure SubmitScores();
    procedure SubmitScoresFinished(const AContent: string; ASuccess: Boolean);
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
  gameviewmain, audiocomponent, gameoptions, gameviewleaders, castleRest, gameentities, models, Common;

constructor TViewWin.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewwin.castle-user-interface';
end;

procedure TViewWin.Start;
begin
  inherited;
  ButtonMenu.OnClick := ButtonMenuClick;
  ButtonSubmit.OnClick := ButtonSubmitClick;
  SoundEngine.LoopingChannel[0].Sound := Audio.RandomWinTheme;
  SoundEngine.LoopingChannel[0].Sound.Volume := 5 * MusicLevel() / 100;
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
  Container.View := ViewMain;
end;

procedure TViewWin.ButtonSubmitClick(Sender: TObject);
begin
  if Trim(EditName.Text) = '' then
  begin
    PanelNotifications.Show('Empty name passed');
    Exit;
  end;
  
  EditName.Enabled := False;
  SubmitScores();
end;

function TViewWin.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEnter) then
    ButtonSubmit.DoClick();

  if Event.IsKey(keyEscape) then
  begin
    Container.View := ViewMain;
    Exit(true); // key was handled
  end;
end;

procedure TViewWin.SubmitScores();
var
  LLeader: TSubmitLeader;
  LName: string;
  I: Integer;
  LNameCorrected: Boolean;
begin
  if not TCastleRest.IsRunning() then
  begin
    LNameCorrected := False;
    LName := '';
    for I := 1 to Length(EditName.Text) do
      if EditName.Text[I] in [':', '/'] then
        LNameCorrected := True
      else
        LName := LName + EditName.Text[I];
    if LNameCorrected then
    begin
      EditName.Text := LName;
      PanelNotifications.Show('Name Corrected. ":/" symbols are not allowed');
    end;
    PanelNotifications.Show('Sending Score to server...');
    LLeader := TSubmitLeader.Create(LName, Score, Difficulty());
    TCastleRest.ServerRequest(ServerApiUrl, SubmitScoresFinished, LLeader.Serialize());
    FreeAndNil(LLeader);
  end
  else
    PanelNotifications.Show('Another request is running');
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
  EditName.Enabled := True;
end;

end.
