unit GameViewDialog;

{$mode delphi}

interface

uses 
// System
  Classes,
// Castle  
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
// third party
  castletypinglabel;

type
  TViewDialog = class(TCastleView)
  published
    ButtonYes, ButtonNo: TCastleButton;
    LabelText: TCastleLabel;
    ImageBack: TCastleImageControl;
    Group1: TCastleVerticalGroup;
  public
    OnYes, OnNo: TNotifyEvent;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    Text: string;
    YesNoMode: Boolean;
    TypingLabel: TCastleTypingLabel;  
    procedure ButtonClick(Sender: TObject);
    procedure Yes();
    procedure No();
  end;

procedure DialogYesNo(AContainer: TCastleContainer; const AText: string; AOnYes, AOnNo: TNotifyEvent);
procedure DialogYes(AContainer: TCastleContainer; const AText: string; AOnYes: TNotifyEvent);

var
  ViewDialog: TViewDialog;

implementation

uses
// System
  SysUtils,
// Castle
  castleutils, castlelog, castlerectangles, castlecomponentserialize, 
// Own
  Common  
  ;

procedure DialogYesNo(AContainer: TCastleContainer; const AText: string; AOnYes, AOnNo: TNotifyEvent);
begin
  ViewDialog.Text := string.Join(NL, SplitString(AText, '|'));
  ViewDialog.OnYes := AOnYes;
  ViewDialog.OnNo := AOnNo;
  ViewDialog.YesNoMode := True;
  if AContainer.CurrentFrontView = ViewDialog then
    AContainer.PopView();
  AContainer.PushView(ViewDialog);
end;

procedure DialogYes(AContainer: TCastleContainer; const AText: string; AOnYes: TNotifyEvent);
begin
  DialogYesNo(AContainer, AText, AOnYes, nil);
  ViewDialog.YesNoMode := False;
  if Assigned(ViewDialog.TypingLabel) then
    ViewDialog.TypingLabel.ResetText();
end;

constructor TViewDialog.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewdialog.castle-user-interface';
  DesignPreload := True;
end;

destructor TViewDialog.Destroy;
begin
  if Assigned(TypingLabel) then
    FreeAndNil(TypingLabel);
  inherited;
end;

procedure TViewDialog.Start;
const
  Anchors: array [Boolean] of THorizontalPosition = (hpLeft, hpMiddle);
var
  LComponentData: string;  
begin
  inherited;
  InterceptInput := YesNoMode;
  ButtonYes.OnClick := ButtonClick;
  ButtonNo.OnClick := ButtonClick;
  ButtonNo.Exists := YesNoMode;
  ImageBack.HorizontalAnchorSelf := Anchors[YesNoMode];
  
  if not Assigned(TypingLabel) then
  begin
    LComponentData := StringReplace(ComponentToString(LabelText), 
      LabelText.ClassName, TCastleTypingLabel.ClassName, []);
    LComponentData := StringReplace(LComponentData, '"LabelText"', '"TypingLabel"', []);
    TypingLabel := StringToComponent(LComponentData, Self) as TCastleTypingLabel;
  end;
  Group1.InsertBack(TypingLabel);
  LabelText.Exists := YesNoMode;
  LabelText.Text.Text := Text;
  TypingLabel.Exists := not YesNoMode;
  TypingLabel.Text.Text := Text;  
end;

procedure TViewDialog.ButtonClick(Sender: TObject);
begin
  if Sender = ButtonYes then
    Yes()
  else
    No();
end;

function TViewDialog.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited Press(Event);
  //if Result then Exit;

  if Event.IsKey(keyEscape) or Event.IsKey(keyBackSpace) then
    No();
  if Event.IsKey(keyEnter) then
    Yes();
end;

procedure TViewDialog.Yes();
begin
  Container.PopView();
  if Assigned(OnYes) then
    OnYes(Self);
end;

procedure TViewDialog.No();
begin
  Container.PopView();
  if Assigned(OnNo) then
    OnNo(Self);
end;

end.
