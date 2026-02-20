unit roomcomponent;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse;

type
  TRoomComponent = class(TCastleUserInterface)
  published
    ControlRoom: TCastleButton;
    ImageRight, ImageLeft: TCastleImageControl;
    LabelRight, LabelLeft: TCastleLabel;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
// Own
  gameviewmain;

constructor TRoomComponent.Create(AOwner: TComponent);
begin
  inherited;
end;

end.
