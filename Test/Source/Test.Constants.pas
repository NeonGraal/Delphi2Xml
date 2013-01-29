unit Test.Constants;

interface

const
  INPUT_PROCESSING = 'Processing (Input) ... done';
  UNIT_PROCESSING = 'Processing Testing.Test.AUnit.pas ... done';
  PROGRAM_PROCESSING = 'Processing Testing.Test.AProgram.dpr ... done';
  PACKAGE_PROCESSING = 'Processing Testing.Test.APackage.dpk ... done';
  BOTH_PROCESSING = 'Processing ... ' + UNIT_PROCESSING + ' ' + PROGRAM_PROCESSING + ' ' +
    PACKAGE_PROCESSING + ' Processed';

  DESCRIPTION_SUFFIX =
    ' Definitions: <codes> Flag codes, optionally interspersed with "+" or "-"' +
    ' <labels> Comma list of Flag Labels, each optionally prefixed or suffixed with "+"' +
    ' or "-" <f/e> If value begins with "." is appended to global name to give file name' +
    ' <d/e> [<dir>][,<extn>] <dir> has trailing delimiter stripped and <extn> has leading' +
    ' dot stripped Either of <dir> and/or <extn> can be "!" to specify the default value';

  EXPECTED_SHOW_OPTIONS = ' ? Show valid options ! Reset all options to defaults ' +
    '@<file> Report/Output Current options F<codes> | :<labels> Flags ' +
    'V Verbose - Log all Parser methods called T Timestamp - Timestamp global output files ' +
    'R Recurse - Recurse into subdirectories N LogNotSupp - Log Not Supported messages ' +
    'F FinalToken + Record Final Token E LogErrors + Log Error messages ' +
  //    'G<str> Delphi2XmlTests Sets global name ' +
  //    'I[+-]:<dir> :Config\ Use <dir> as a base for all Config files ' +
  //    'O[+-]:<dir> :Log\ Use <dir> as a base for all Log files ' +
  //    'B[+-]:<dir> -(.\) Use <dir> as a base for all Input files ' +
    'M<mode> Full Parser type (F[ull], U[ses]) ' +
    'P<per> File Result per (F[ile], S[ubdir], D[ir], W[ildcard], P[aram], R[un]) ' +
    'E<mode> Quiet Elapsed time display (N[one], T[otal], D[ir], F[ile], P[rocessing], [Q]uiet) '
    + 'X[+-]:<d/e> :Xml,xml Generate XML files into current or given <d/e> ' +
    'W[+-]:<d/e> -(Defines,def) Generate Final Defines files into current or given <d/e> ' +
    'CC[+-]:<f/e> :.chld Report Min/Max Children into <f/e> ' +
    'CF[+-]:<f/e> :.final Count Final Defines into <f/e> ' +
    'CU[+-]:<f/e> :.used Count Defines Used into <f/e> ' +
    'S[+-]:<f/e> -(.skip) Load Skipped Methods from <f/e> ' +
    'D[+-!:]<def> Add(+), Remove(-), Clear(!) or Load(:) Defines ' +
    'H[+-!:]<def> Add(+), Remove(-), Clear(!) or Load(:) Held Defines' + DESCRIPTION_SUFFIX;

  REPORT_HEADING = 'Current option settings:';
  BASE_REPORT_OPTIONS = REPORT_HEADING +
    ' Flags FinalToken+,LogErrors+,LogNotSupp-,Recurse-,Timestamp-,Verbose- ' +
  //    'Recurse - Timestamp - Global name Delphi2XmlTests Config dir :Config\ ' +
  //    'Log dir :Log\ Base dir -(.\) Parse mode Full Results per File Show elapsed Quiet ' +
    'Parse mode Full Results per File Show elapsed Quiet ' +
    'Generate XML :Xml,xml Write Defines -(Defines,def) Count Children :.chld ' +
    'Count Final Defines :.final Count Defines Used :.used Skipped Methods -(.skip) ';
  DEFAULT_REPORT_OPTIONS = BASE_REPORT_OPTIONS + 'Defines Default Held Defines Default';
{$IFDEF WIN32}
  EXPECTED_DEFINES = 'CONDITIONALEXPRESSIONS,CPU386,MSWINDOWS,UNICODE,VER230,WIN32';
{$ELSE}
  EXPECTED_DEFINES = 'CONDITIONALEXPRESSIONS,MSWINDOWS,UNICODE,VER230';
{$ENDIF}
  TESTING_UNIT = 'unit Testing.Test.AUnit; interface uses System.Classes; ' +
    '{$DEFINE TEST} {$INCLUDE Test.inc} {$D+} implementation uses System.SysUtils; ' +
    '{$IFDEF TEST} {$DEFINE TEST1} {$ELSE} {$DEFINE TEST2} {$ENDIF} ' +
    '{$IFNDEF TEST} {$DEFINE TEST3} {$ENDIF} {$IFOPT D+} {$DEFINE TEST6} {$ENDIF} ' +
    '{$IF Defined(TEST)} {$DEFINE TEST4} {$ELSEIF Defined(TEST1)} {$DEFINE TEST5} {$ENDIF} ' +
    'end.';
  TESTING_PROGRAM = 'program Testing.Test.AProgram; ' +
    'uses Testing.Test.AUnit in ''Testing.Test.AUnit.pas''; begin end.';
  TESTING_PACKAGE = 'package Testing.Test.APackage; ' +
    'contains Testing.Test.AUnit in ''Testing.Test.AUnit.pas''; end.';

function UsageDescription: string;

implementation

uses
  System.SysUtils;

const
  DESCRIPTION_PREFIX = 'Usage: %s [ Option | @Params | mFilename | Wildcard ] ... ' +
    'Options: Default Description';

function UsageDescription: string;
begin
  Result := Format(DESCRIPTION_PREFIX, [ChangeFileExt(ExtractFilename(ParamStr(0)), '')]);
end;

end.
