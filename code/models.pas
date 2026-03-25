unit models;

interface

uses 
// Own
  Classes,
// Own
  gameoptions  
  ;

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

  TSubmitLeader = class(TLeader)
  private 
    FHash, FGuid: string;
    const {$I salt.inc}
  protected
    procedure CalcHash();
  public 
    constructor Create(const AName: string; AScore: Integer; ADifficulty: NDifficulty);
    function Serialize(): string;
  published
    property Guid: string read FGuid write FGuid;
    property Hash: string read FHash;   
  end;

  TProblemDetails = class
  private
    FStatus: Integer;
    FType, FDetail, FTitle: string;
  public 
    function ToString(): string; override;
  published
    property &Type: string read FType write FType;
    property Title: string read FTitle write FTitle;
    property Status: Integer read FStatus write FStatus;
    property Detail: string read FDetail write FDetail;
  end;

  TSubmitResult = class
  private 
    FGuid: string;
  published
    property Guid: string read FGuid write FGuid;
  end;

implementation

uses
// System
  SysUtils,
// Thirdparty
  HlpSHA2_256, HlpIHash, HlpIHashResult,
// Castle
  castlelog,
// Own
  Common;

function TProblemDetails.ToString(): string; 
begin
  Result := Title + ': ' + Detail;
end;

constructor TSubmitLeader.Create(const AName: string; AScore: Integer; ADifficulty: NDifficulty);
begin
  inherited Create();
  Name := AName;
  Guid := UserGuid;
  Score := AScore;
  Difficulty := Ord(ADifficulty);
  CalcHash();
end;

procedure TSubmitLeader.CalcHash();
const
// sorted alphabetically
  HashTemplate = 'Difficulty=%d%s:Name=%s:Salt=%s:Score=%d'; 
begin
  FHash := Format(HashTemplate, [Difficulty, IIF(Guid = '', '', ':Guid='+ Guid), Name, Salt, Score]);
  //WriteLnLog('ForHash ' + FHash);
  FHash := TSHA2_256.Create().ComputeString(FHash, TEncoding.UTF8).ToString(); 
end;

function TSubmitLeader.Serialize(): string;
const
  Template = '{"difficulty":%d,"name":"%s","score":%d,"hash":"%s" %s}';    
begin
  Result := Format(Template, [Difficulty, Name, Score, Hash, IIF(Guid = '', '', ',"guid":"'+ Guid + '"')]);
end;

end.
