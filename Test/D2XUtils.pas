unit D2XUtils;

interface

function ReduceString(pStr: string): string;

implementation

uses
  System.RegularExpressions,
  System.SysUtils;

function ReduceString(pStr: string): string;
begin
  Result := Trim(TRegEx.Replace(pStr, '\s+', ' ', []));
end;

end.
