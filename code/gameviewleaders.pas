unit GameViewLeaders;

{$mode delphi}

interface

uses 
// System
  Classes, generics.collections,
// Castle  
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse, CastleComponentSerialize, CastleNotifications,
// Own
  models  
  ;

type
  TViewLeaders = class(TCastleView)
  published
    ButtonMenu, ButtonSync: TCastleButton;
    FactoryButton: TCastleComponentFactory;
    GroupDifficulty, GroupLeaders: TCastleUserInterface;
    PanelNotifications: TCastleNotifications;
  public
    NeedsSync: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Resume; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    FLeaders: TObjectList<TLeader>;
    FCurrentDifficultyButton: TCastleButton;
    procedure ButtonMenuClick(Sender: TObject);
    procedure ButtonSyncClick(Sender: TObject);
    procedure ButtonDifficultyClick(Sender: TObject);
    procedure SwitchDifficulty(AButton: TCastleButton);
    procedure GetLeadersFinished(const AContent: string; ASuccess: Boolean);
  end;

var
  ViewLeaders: TViewLeaders;

implementation

uses 
// System
  SysUtils, fpjson,
// Castle  
  castlewindow, castlelog,
// Own
  gameoptions, castlerest, gameviewmain
;

constructor TViewLeaders.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewleaders.castle-user-interface';
  FLeaders := TObjectList<TLeader>.Create(True);
end;

destructor TViewLeaders.Destroy;
begin
  FreeAndNil(FLeaders);
  inherited;
end;

procedure TViewLeaders.Start;
var
  LDifficulty: NDifficulty;
  LButton: TCastleButton;
  LCurrentDifficultyName: string;
begin
  inherited;
  ButtonMenu.OnClick := ButtonMenuClick;
  ButtonSync.OnClick := ButtonSyncClick;
  LCurrentDifficultyName := DifficultyName(Difficulty());
  for LDifficulty in NDifficulty do
  begin
    LButton := FactoryButton.ComponentLoad(GroupDifficulty) as TCastleButton;
    LButton.Caption := DifficultyName(LDifficulty);
    LButton.Pressed := LButton.Caption = LCurrentDifficultyName;
    if LButton.Pressed then
      FCurrentDifficultyButton := LButton;
    LButton.Tag := Ord(LDifficulty);
    LButton.OnClick := ButtonDifficultyClick;
    GroupDifficulty.InsertFront(LButton);
  end;
  GroupLeaders.ClearControls();
end;

procedure TViewLeaders.Resume;
begin
  inherited;
  if (FLeaders.Count = 0) or NeedsSync then
    ButtonSync.DoClick();
end;  

function TViewLeaders.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEscape) or Event.IsKey(keyBackSpace) then
  begin
    Container.View := ViewMain;
    Exit(true); // key was handled
  end; 
end;

procedure TViewLeaders.ButtonMenuClick(Sender: TObject);
begin
  Container.View := ViewMain;
end;

procedure TViewLeaders.ButtonSyncClick(Sender: TObject);
begin
// Sync with server, and update the list of leaders for the current difficulty.
  if not TCastleRest.IsRunning() then 
    TCastleRest.ServerRequest(ServerApiUrl, GetLeadersFinished)
  else
    PanelNotifications.Show('Another request is running');
end;

procedure TViewLeaders.ButtonDifficultyClick(Sender: TObject);
begin
  if not (Sender is TCastleButton) or TCastleButton(Sender).Pressed then 
    Exit;
  SwitchDifficulty(TCastleButton(Sender));
end;

procedure TViewLeaders.SwitchDifficulty(AButton: TCastleButton);
var
  LButton: TCastleUserInterface;
  LDifficulty: NDifficulty;
  LLeader: TLeader;
  LLabel: TCastleLabel;
begin
  for LButton in GroupDifficulty do
    if LButton is TCastleButton then
      TCastleButton(LButton).Pressed := LButton = AButton;
  FCurrentDifficultyButton := AButton;

//  populate the list of leaders for the selected difficulty. 
  GroupLeaders.ClearControls();
  LDifficulty := NDifficulty(FCurrentDifficultyButton.Tag);
  for LLeader in FLeaders do
    if LLeader.Difficulty = Ord(LDifficulty) then
    begin
      LLabel := TCastleLabel.Create(GroupLeaders);
      LLabel.Caption := Format('%d             %d         %s', [LLeader.Number, LLeader.Score, LLeader.Name]);
      GroupLeaders.InsertFront(LLabel);
    end;
end;

procedure TViewLeaders.GetLeadersFinished(const AContent: string; ASuccess: Boolean);
var
  LJsonArray: TJSONArray;
  LElement: TJSONEnum;
  LLeader: TLeader;
begin
  if ParseJSONArray(AContent, LJsonArray) then
  begin
    FLeaders.Clear();
    for LElement in LJsonArray do
    try
      LLeader := TLeader.Create();
      //WriteLnLog('leader json: ' + LElement.Value.AsJson);
      ReadJsonToObject<TLeader>(LElement.Value as TJSONObject, LLeader);
      FLeaders.Add(LLeader);
      //WriteLnLog(Format('Loaded leader: %d, %d, %s, difficulty: %d', [LLeader.Number, LLeader.Score, LLeader.Name, LLeader.Difficulty]));
    except
      WriteLnLog('Incorrect leader: ' + LElement.Key);
    end;
    //WriteLnLog('Leaders loaded: ' + IntToStr(FLeaders.Count));
    SwitchDifficulty(FCurrentDifficultyButton);
    PanelNotifications.Show('Leaders loaded');
    NeedsSync := False;
  end
  else
    WriteLnLog('Failed to parse leaders: ' + AContent);
end;

end.
