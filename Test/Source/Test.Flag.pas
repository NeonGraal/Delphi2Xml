unit Test.Flag;

interface

uses
  D2X.Global;

type
  TD2XBoolFlag = class(TD2XInterfaced)
  private
    fFlag: Boolean;

    function GetFlagRef: TD2XFlagRef;

  public
    procedure SetFlag(pVal: Boolean);

    property FlagRef: TD2XFlagRef read GetFlagRef;

  end;

implementation

{ TD2XBoolFlag }

function TD2XBoolFlag.GetFlagRef: TD2XFlagRef;
begin
  Result := function: Boolean
    begin
      result := fFlag;
    end;
end;

procedure TD2XBoolFlag.SetFlag(pVal: Boolean);
begin
  fFlag := pVal;
end;

end.
