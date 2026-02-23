unit GameViewDialog;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TViewDialog = class(TCastleView)
  published
    ButtonYes, ButtonNo: TCastleButton;
    LabelText: TCastleLabel;
  public
    OnYes, OnNo: TNotifyEvent;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  private
    Text: string;
    procedure ButtonClick(Sender: TObject);
    procedure Yes();
    procedure No();
  end;

procedure DialogYesNo(AContainer: TCastleContainer; const AText: string; AOnYes, AOnNo: TNotifyEvent);

var
  ViewDialog: TViewDialog;

implementation

uses
// System
  SysUtils,
// Castle
  castleutils, castlelog,
// Own
  Common  
  ;

procedure DialogYesNo(AContainer: TCastleContainer; const AText: string; AOnYes, AOnNo: TNotifyEvent);
begin
  ViewDialog.Text := string.Join(NL, SplitString(AText, '|'));
  ViewDialog.OnYes := AOnYes;
  ViewDialog.OnNo := AOnNo;
  if AContainer.FrontView = ViewDialog then
    AContainer.PopView();
  AContainer.PushView(ViewDialog);
end;

constructor TViewDialog.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewdialog.castle-user-interface';
end;

procedure TViewDialog.Start;
begin
  inherited;
  InterceptInput := true;
  ButtonYes.OnClick := @ButtonClick;
  ButtonNo.OnClick := @ButtonClick;
  ViewDialog.LabelText.Text.Text := Text;
end;

procedure TViewDialog.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  { Executed every frame. }
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

  if Event.IsKey(keyEscape) then
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
