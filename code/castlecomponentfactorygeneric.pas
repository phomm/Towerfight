unit CastleComponentFactoryGeneric;

{$MODE DELPHI}

interface

uses 
// System
  Classes,
// Castle
  CastleComponentSerialize;

type

  TCastleComponentFactoryNew<T: TComponent> = class(TCastleComponentFactory)
  public
    function ComponentLoadNew(const InstanceOwner: TComponent): T;
  end;

implementation

uses 
// System
  SysUtils, Generics.Collections,
// Castle
  castleclassutils
// Own
  ;

function TCastleComponentFactoryNew<T>.ComponentLoadNew(const InstanceOwner: TComponent): T;
var
  ComponentMap: TComponentMap;
  //Component: TComponent;
  procedure MakeAssociateReferences(const Instance: T);
  var
    MapPair: TPair<string, TComponent>;
    FieldAddr: ^TComponent;
  begin
    for MapPair in ComponentMap do
    begin
      FieldAddr := Instance.FieldAddress(MapPair.Key);
      if FieldAddr <> nil then
      begin
        Assert(MapPair.Value is TComponent); 
        FieldAddr^ := MapPair.Value as TComponent;
      end;
    end;
  end;
begin
  Result := ComponentLoadWithMap(InstanceOwner, ComponentMap) as T; // invalid cast
  //Result := T.Create(InstanceOwner);
  //Component := ComponentLoadWithMap(InstanceOwner, ComponentMap);
  //Result.Assign(Component); // doesn't work either, "not assignable"
  try
    MakeAssociateReferences(Result);
  finally 
    FreeAndNil(ComponentMap);
    //FreeAndNil(Component);
  end;
end;

end.