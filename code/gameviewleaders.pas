unit GameViewLeaders;

{$mode delphi}

interface

uses 
// System
  Classes, generics.collections,
// Castle  
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse, CastleComponentSerialize;

type

  TLeader = class
  private
    FNumber: Integer;
    FScore: Integer;
    FDifficulty: Integer;
    FName: string;
  published
    property Number: Integer read FNumber write FNumber;
    property Score: Integer read FScore write FScore;
    property Difficulty: Integer read FDifficulty write FDifficulty;
    property Name: string read FName write FName;
  end;

  TViewLeaders = class(TCastleView)
  published
    ButtonMenu, ButtonSync: TCastleButton;
    FactoryButton: TCastleComponentFactory;
    GroupDifficulty: TCastleUserInterface;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    FLeaders: TObjectList<TLeader>;
    procedure ButtonMenuClick(Sender: TObject);
    procedure ButtonSyncClick(Sender: TObject);
    procedure ButtonDifficultyClick(Sender: TObject);
    procedure GetLeadersFinished(const AContent: string);
  const 
    ServerApiUrl = 'https://localhost:7150/api/';//'https://towerfightserver.onrender.com/api/';
  end;

var
  ViewLeaders: TViewLeaders;

implementation

uses 
// System
  SysUtils, fpjson, fpjsonrtti, jsonparser, jsonscanner,
// Castle  
  castlewindow, castlelog,
// Own
  gameoptions, castlerest
;

function ParseJSONArray(AJson: TJSONStringType; out AJsonArray: TJSONArray): Boolean;
begin
  Result := False;
  with TJSONParser.Create(AJson, DefaultOptions) do
  try
    AJsonArray := Parse() as TJSONArray;
    Result := Assigned(AJsonArray);
  finally
    Free();
  end;
end;

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
    LButton.Tag := Ord(LDifficulty);
    LButton.OnClick := ButtonDifficultyClick;
    GroupDifficulty.InsertFront(LButton);
  end;
end;

function TViewLeaders.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(keyEscape) or Event.IsKey(keyBackSpace) then
  begin
    Container.PopView();
    Exit(true); // key was handled
  end; 
end;

procedure TViewLeaders.ButtonMenuClick(Sender: TObject);
begin
  Container.PopView();
end;

procedure TViewLeaders.ButtonSyncClick(Sender: TObject);
begin
// Sync with server, and update the list of leaders for the current difficulty.
  TCastleRest.ServerRequest(ServerApiUrl + 'leaders', GetLeadersFinished);
end;

procedure TViewLeaders.ButtonDifficultyClick(Sender: TObject);
var
  LButton: TCastleUserInterface;
begin
  for LButton in GroupDifficulty do
    if LButton is TCastleButton then
    begin
      TCastleButton(LButton).Pressed := LButton = Sender;
    end;
// Load from server if not yet done, and populate the list of leaders for the selected difficulty. 
  
end;

procedure TViewLeaders.GetLeadersFinished(const AContent: string);
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
      with TJSONDeStreamer.Create(nil) do
        try
          JSONToObject(LElement.Value as TJSONObject, LLeader);
        finally
          Free();
        end;
      FLeaders.Add(LLeader);
    except
      WriteLnLog('Incorrect leader: ' + LElement.Key);
    end;
    WriteLnLog('Leaders loaded: ' + IntToStr(FLeaders.Count));
  end
  else
    WriteLnLog('Failed to parse leaders: ' + AContent);
end;

end.
