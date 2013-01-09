unit Testing.Test.AUnit;

interface

uses
  System.Classes;

{$DEFINE TEST}

{$INCLUDE Test.inc}

{$D+}

implementation

uses
  System.SysUtils;

{$IFDEF TEST}
{$DEFINE TEST1}
{$ELSE}
{$DEFINE TEST2}
{$ENDIF}

{$IFNDEF TEST}
{$DEFINE TEST3}
{$ENDIF}

{$IFOPT D+}
{$DEFINE TEST6}
{$ENDIF}

{$IF Defined(TEST)}
{$DEFINE TEST4}
{$ELSEIF Defined(TEST1)}
{$DEFINE TEST5}
{$ENDIF}

end.
