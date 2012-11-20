unit TestSubDir;

interface

uses
  System.Classes;

{$DEFINE TEST}

implementation

uses
  System.SysUtils;

{$IFDEF TEST}
{$DEFINE TEST1}
{$ENDIF}

end.
