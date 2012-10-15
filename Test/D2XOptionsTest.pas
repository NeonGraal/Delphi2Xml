unit D2XOptionsTest;

interface

uses
  System.Classes,
  TestFramework,
  System.StrUtils,
  D2XParam,
  D2XOptions,
  System.SysUtils;

type
  // Test methods for class TD2X
  TestTD2XOptionBase = class(TTestCase)
  private
    fLog: TStringStream;

  strict protected
    fOpts: TD2XOptions;

    procedure CheckLog(pMsg: string);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  // Test methods for class TD2X
  TestTD2XOptionEnums = class(TTestCase)
  strict private
    function ConvertParsingMode(pStr: string; pDflt: TD2XParseMode;
      out pVal: TD2XParseMode): boolean;
    function FormatParsingMode(pVal: TD2XParseMode): string;

    function ConvertResultPer(pStr: string; pDflt: TD2XResultPer;
      out pVal: TD2XResultPer): boolean;
    function FormatResultPer(pVal: TD2XResultPer): string;

    function ConvertElapsedMode(pStr: string; pDflt: TD2XElapsedMode;
      out pVal: TD2XElapsedMode): boolean;
    function FormatElapsedMode(pVal: TD2XElapsedMode): string;
  published
    procedure TestElapsedModeInvalidCreate;
    procedure TestParseModeInvalidCreate;
    procedure TestResultPerInvalidCreate;

    procedure TestElapsedModeParam;
    procedure TestParseModeParam;
    procedure TestResultPerParam;
  end;

  // Test methods for class TD2XOptions
  TestTD2XOptionFilenames = class(TestTD2XOptionBase)
  published
    procedure TestInputFile;
    procedure TestInputExtn;
    procedure TestInputDirFile;
    procedure TestInputDirExtn;
    procedure TestInputOffFile;
    procedure TestInputOffExtn;
    procedure TestInputOnFile;
    procedure TestInputOnExtn;

    procedure TestOutputFile;
    procedure TestOutputExtn;
    procedure TestOutputDirFile;
    procedure TestOutputDirExtn;
    procedure TestOutputOffFile;
    procedure TestOutputOffExtn;
    procedure TestOutputOnFile;
    procedure TestOutputOnExtn;
    procedure TestOutputTimestampFile;
    procedure TestOutputTimestampExtn;
    procedure TestOutputNoTimestampFile;
    procedure TestOutputNoTimestampExtn;
  end;

  // Test methods for class TD2XOptions
  TestTD2XOptions = class(TestTD2XOptionBase)
  public
    procedure SetUp; override;

  published
    procedure TestParseOptionBlank;
    procedure TestParseOptionWrongPrefix;
    procedure TestParseOptionA;
    procedure TestParseOptionB;
    procedure TestParseOptionBOff;
    procedure TestParseOptionBOn;
    procedure TestParseOptionBBlank;
    procedure TestParseOptionBValue;
    procedure TestParseOptionC;
    procedure TestParseOptionCBlank;
    procedure TestParseOptionCExtn;
    procedure TestParseOptionCOff;
    procedure TestParseOptionCOn;
    procedure TestParseOptionCFile;
    procedure TestParseOptionD;
    procedure TestParseOptionDAdd;
    procedure TestParseOptionDDelete;
    procedure TestParseOptionDClear;
    procedure TestParseOptionDEmpty;
    procedure TestParseOptionDLoad;
    procedure TestParseOptionDMany;
    procedure TestParseOptionE;
    procedure TestParseOptionEValue;
    procedure TestParseOptionF;
    procedure TestParseOptionFOff;
    procedure TestParseOptionFOn;
    procedure TestParseOptionG;
    procedure TestParseOptionGValue;
    procedure TestParseOptionH;
    procedure TestParseOptionI;
    procedure TestParseOptionIOff;
    procedure TestParseOptionIOn;
    procedure TestParseOptionIBlank;
    procedure TestParseOptionIValue;
    procedure TestParseOptionJ;
    procedure TestParseOptionK;
    procedure TestParseOptionL;
    procedure TestParseOptionLOff;
    procedure TestParseOptionLOn;
    procedure TestParseOptionM;
    procedure TestParseOptionMValue;
    procedure TestParseOptionN;
    procedure TestParseOptionNOff;
    procedure TestParseOptionNOn;
    procedure TestParseOptionO;
    procedure TestParseOptionOOff;
    procedure TestParseOptionOOn;
    procedure TestParseOptionOBlank;
    procedure TestParseOptionOValue;
    procedure TestParseOptionP;
    procedure TestParseOptionPValue;
    procedure TestParseOptionQ;
    procedure TestParseOptionR;
    procedure TestParseOptionROff;
    procedure TestParseOptionROn;
    procedure TestParseOptionS;
    procedure TestParseOptionSBlank;
    procedure TestParseOptionSExtn;
    procedure TestParseOptionSFile;
    procedure TestParseOptionSOff;
    procedure TestParseOptionSOn;
    procedure TestParseOptionSSimple;
    procedure TestParseOptionT;
    procedure TestParseOptionTOff;
    procedure TestParseOptionTOn;
    procedure TestParseOptionU;
    procedure TestParseOptionUBlank;
    procedure TestParseOptionUExtn;
    procedure TestParseOptionUFile;
    procedure TestParseOptionUOff;
    procedure TestParseOptionUOn;
    procedure TestParseOptionV;
    procedure TestParseOptionVOff;
    procedure TestParseOptionVOn;
    procedure TestParseOptionW;
    procedure TestParseOptionWOff;
    procedure TestParseOptionWOn;
    procedure TestParseOptionWBlank;
    procedure TestParseOptionWValue;
    procedure TestParseOptionX;
    procedure TestParseOptionXOff;
    procedure TestParseOptionXOn;
    procedure TestParseOptionXBlank;
    procedure TestParseOptionXValue;
    procedure TestParseOptionY;
    procedure TestParseOptionZ;
  end;

  // Test methods for class TD2XOptions
  TestTD2XOptionGeneral = class(TestTD2XOptionBase)
  private
    procedure SetAllOptions;
  published
    procedure TestDefaultOptions;
    procedure TestReportOptions;
    procedure TestReportOptionsDefault;
    procedure TestReportOptionsFileDefault;
    procedure TestReportOptionsFile;
    procedure TestReportOptionsExtn;
    procedure TestReportOptionsDefines;
    procedure TestReportOptionsEmpty;
    procedure TestReportOptionsReset;
    procedure TestResetOptions;
    procedure TestShowOptions;
  end;

implementation

uses
  D2X,
  D2XUtils;

{ TestTD2XOptionBase }

procedure TestTD2XOptionBase.CheckLog(pMsg: string);
begin
  CheckEqualsString(pMsg, ReduceString(fLog.DataString), 'Log');
end;

procedure TestTD2XOptionBase.SetUp;
begin
  fOpts := TD2XOptions.Create;
  fLog := TStringStream.Create;
  fOpts.StartLog(fLog);
end;

procedure TestTD2XOptionBase.TearDown;
begin
  FreeAndNil(fLog);
  fOpts := nil;
end;

{ TestTD2XOptionFilenames }

procedure TestTD2XOptionFilenames.TestInputDirExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fOpts.ParseOption('-i:In');

  ReturnValue := fOpts.InputFileOrExtn(pExtn);

  CheckEqualsString('In\' + fOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestInputDirFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fOpts.ParseOption('-i:In');

  ReturnValue := fOpts.InputFileOrExtn(pFile);

  CheckEqualsString('In\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestInputOffExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fOpts.ParseOption('-i-');

  ReturnValue := fOpts.InputFileOrExtn(pExtn);

  CheckEqualsString(fOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestInputOffFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fOpts.ParseOption('-i-');

  ReturnValue := fOpts.InputFileOrExtn(pFile);

  CheckEqualsString('File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestInputOnExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fOpts.ParseOption('-i+');

  ReturnValue := fOpts.InputFileOrExtn(pExtn);

  CheckEqualsString('Config\' + fOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestInputOnFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fOpts.ParseOption('-i+');

  ReturnValue := fOpts.InputFileOrExtn(pFile);

  CheckEqualsString('Config\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestInputExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';

  ReturnValue := fOpts.InputFileOrExtn(pExtn);

  CheckEqualsString('Config\' + fOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestInputFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';

  ReturnValue := fOpts.InputFileOrExtn(pFile);

  CheckEqualsString('Config\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputDirExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fOpts.ParseOption('-o:Out');

  ReturnValue := fOpts.OutputFileOrExtn(pExtn);

  CheckEqualsString('Out\' + fOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputDirFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fOpts.ParseOption('-o:Out');

  ReturnValue := fOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Out\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';

  ReturnValue := fOpts.OutputFileOrExtn(pExtn);

  CheckEqualsString('Log\' + fOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';

  ReturnValue := fOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputNoTimestampExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fOpts.ParseOption('-t-');

  ReturnValue := fOpts.OutputFileOrExtn(pExtn);

  CheckEqualsString('Log\' + fOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputNoTimestampFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fOpts.ParseOption('-t-');

  ReturnValue := fOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputOffExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fOpts.ParseOption('-o-');

  ReturnValue := fOpts.OutputFileOrExtn(pExtn);

  CheckEqualsString(fOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputOffFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fOpts.ParseOption('-o-');

  ReturnValue := fOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputOnExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fOpts.ParseOption('-o+');

  ReturnValue := fOpts.OutputFileOrExtn(pExtn);

  CheckEqualsString('Log\' + fOpts.GlobalName + '.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputOnFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fOpts.ParseOption('-o+');

  ReturnValue := fOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Log\File.Extn', ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputTimestampExtn;
var
  ReturnValue: string;
  pExtn: string;
begin
  pExtn := '.Extn';
  fOpts.ParseOption('-t');

  ReturnValue := fOpts.OutputFileOrExtn(pExtn);

  CheckEqualsString('Log\' + fOpts.GlobalName + fOpts.OutputTimestamp + '.Extn', ReturnValue,
    'ReturnValue');
end;

procedure TestTD2XOptionFilenames.TestOutputTimestampFile;
var
  ReturnValue: string;
  pFile: string;
begin
  pFile := 'File.Extn';
  fOpts.ParseOption('-t');

  ReturnValue := fOpts.OutputFileOrExtn(pFile);

  CheckEqualsString('Log\File' + fOpts.OutputTimestamp + '.Extn', ReturnValue, 'ReturnValue');
end;

{ TestTD2XOptions }

procedure TestTD2XOptions.SetUp;
begin
  inherited;

  fOpts.Defines.CommaText := 'Alpha,Beta,Gamma';
end;

procedure TestTD2XOptions.TestParseOptionBlank;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '';
  StartExpectingException(ED2XOptionsException);
  ReturnValue := fOpts.ParseOption(pOpt);
  StopExpectingException;
  CheckFalse(ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptions.TestParseOptionA;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-A';
  ReturnValue := fOpts.ParseOption(pOpt);
  CheckFalse(ReturnValue, 'ReturnValue');
  CheckLog('Unknown option: -A');
end;

procedure TestTD2XOptions.TestParseOptionB;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-B';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.UseBase, 'UseBase');
  CheckEqualsString('', fOpts.BaseDirectory, 'BaseDirectory');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionBBlank;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-B:';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.UseBase, 'UseBase');
  CheckEqualsString('', fOpts.BaseDirectory, 'BaseDirectory');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionBOff;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-B-';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckFalse(fOpts.UseBase, 'UseBase');
  CheckEqualsString('', fOpts.BaseDirectory, 'BaseDirectory');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionBOn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-B+';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.UseBase, 'UseBase');
  CheckEqualsString('', fOpts.BaseDirectory, 'BaseDirectory');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionBValue;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-B:Base';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.UseBase, 'UseBase');
  CheckEqualsString('Base\', fOpts.BaseDirectory, 'BaseDirectory');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionC;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-C';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.CountChildren, 'CountChildren');
  CheckEqualsString('.cnt', fOpts.CountChildrenFoE, 'CountFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionCBlank;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-C:';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.CountChildren, 'CountChildren');
  CheckEqualsString('.cnt', fOpts.CountChildrenFoE, 'CountFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionCExtn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-C:Extn';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.CountChildren, 'CountChildren');
  CheckEqualsString('.Extn', fOpts.CountChildrenFoE, 'CountFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionCOff;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-C-';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckFalse(fOpts.CountChildren, 'CountChildren');
  CheckEqualsString('.cnt', fOpts.CountChildrenFoE, 'CountFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionCOn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-C+';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.CountChildren, 'CountChildren');
  CheckEqualsString('.cnt', fOpts.CountChildrenFoE, 'CountFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionCFile;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-C:File.Extn';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.CountChildren, 'CountChildren');
  CheckEqualsString('File.Extn', fOpts.CountChildrenFoE, 'CountFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionD;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-D';
  ReturnValue := fOpts.ParseOption(pOpt);
  CheckFalse(ReturnValue, 'ReturnValue');
  CheckLog('Invalid Defines option: -D');
end;

procedure TestTD2XOptions.TestParseOptionDAdd;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-D+Value';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.LoadDefines, 'Load Defines');
  CheckEqualsString('Alpha,Beta,Gamma,Value', fOpts.Defines.CommaText, 'Defines');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionDClear;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-D!';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckFalse(fOpts.LoadDefines, 'Load Defines');
  CheckEqualsString('', fOpts.Defines.CommaText, 'Defines');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionDDelete;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-D-Beta';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.LoadDefines, 'Load Defines');
  CheckEqualsString('Alpha,Gamma', fOpts.Defines.CommaText, 'Defines');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionDEmpty;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-D:';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.LoadDefines, 'Load Defines');
  CheckEqualsString('', fOpts.Defines.CommaText, 'Defines');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionDLoad;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-D:Test';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.LoadDefines, 'Load Defines');
  CheckEqualsString('Tango,Uniform', fOpts.Defines.CommaText, 'Defines');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionDMany;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fOpts.ParseOption('-D+Value1');
  Check(ReturnValue, 'ReturnValue1');
  ReturnValue := fOpts.ParseOption('-D+Value2');
  Check(ReturnValue, 'ReturnValue2');
  Check(fOpts.LoadDefines, 'Load Defines');
  CheckEqualsString('Alpha,Beta,Gamma,Value1,Value2', fOpts.Defines.CommaText, 'Defines');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionE;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-E';
  ReturnValue := fOpts.ParseOption(pOpt);
  CheckFalse(ReturnValue, 'ReturnValue');
  CheckLog('Invalid Show elapsed option: -E');
end;

procedure TestTD2XOptions.TestParseOptionEValue;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-ETotal';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.ElapsedMode = emTotal, 'ElapsedMode');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionF;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-F';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.FinalToken, 'FinalToken');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionFOff;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-F-';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckFalse(fOpts.FinalToken, 'FinalToken');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionFOn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-F+';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.FinalToken, 'FinalToken');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionG;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-G';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckEqualsString(ChangeFileExt(ExtractFileName(ParamStr(0)), ''), fOpts.GlobalName,
    'GlobalName');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionGValue;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-GGlobal';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckEqualsString('Global', fOpts.GlobalName, 'GlobalName');

  CheckEqualsString('Global\', fOpts.XmlDirectory, 'XmlDirectory');
  CheckEqualsString('Global\', fOpts.DefinesDirectory, 'DefinesDirectory');

  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionH;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-H';
  ReturnValue := fOpts.ParseOption(pOpt);
  CheckFalse(ReturnValue, 'ReturnValue');
  CheckLog('Unknown option: -H');
end;

procedure TestTD2XOptions.TestParseOptionI;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-I';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckEqualsString('Config\Input.File', fOpts.InputFileOrExtn('Input.File'),
    'InputFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionIBlank;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-I:';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckEqualsString('Input.File', fOpts.InputFileOrExtn('Input.File'), 'InputFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionIOff;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-I-';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckEqualsString('Input.File', fOpts.InputFileOrExtn('Input.File'), 'InputFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionIOn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-I+';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckEqualsString('Config\Input.File', fOpts.InputFileOrExtn('Input.File'),
    'InputFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionIValue;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-I:Input';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckEqualsString('Input\Input.File', fOpts.InputFileOrExtn('Input.File'),
    'InputFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionJ;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-J';
  ReturnValue := fOpts.ParseOption(pOpt);
  CheckFalse(ReturnValue, 'ReturnValue');
  CheckLog('Unknown option: -J');
end;

procedure TestTD2XOptions.TestParseOptionK;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-K';
  ReturnValue := fOpts.ParseOption(pOpt);
  CheckFalse(ReturnValue, 'ReturnValue');
  CheckLog('Unknown option: -K');
end;

procedure TestTD2XOptions.TestParseOptionL;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-L';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.LogErrors, 'LogErrors');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionLOff;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-L-';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckFalse(fOpts.LogErrors, 'LogErrors');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionLOn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-L+';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.LogErrors, 'LogErrors');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionM;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-M';
  ReturnValue := fOpts.ParseOption(pOpt);
  CheckFalse(ReturnValue, 'ReturnValue');
  CheckLog('Invalid Parse mode option: -M');
end;

procedure TestTD2XOptions.TestParseOptionMValue;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-MUses';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.ParseMode = pmUses, 'ParseMode');
  CheckEqualsString('Uses', fOpts.GlobalName, 'GlobalName');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionN;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-N';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.LogNotSupported, 'LogNotSupported');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionNOff;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-N-';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckFalse(fOpts.LogNotSupported, 'LogNotSupported');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionNOn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-N+';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.LogNotSupported, 'LogNotSupported');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionO;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-O';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckEqualsString('Log\Output.File', fOpts.OutputFileOrExtn('Output.File'),
    'OutputFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionOBlank;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-O:';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckEqualsString('Output.File', fOpts.OutputFileOrExtn('Output.File'), 'OutputFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionOOff;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-O-';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckEqualsString('Output.File', fOpts.OutputFileOrExtn('Output.File'), 'OutputFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionOOn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-O+';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckEqualsString('Log\Output.File', fOpts.OutputFileOrExtn('Output.File'),
    'OutputFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionOValue;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-O:Output';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckEqualsString('Output\Output.File', fOpts.OutputFileOrExtn('Output.File'),
    'OutputFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionP;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-P';
  ReturnValue := fOpts.ParseOption(pOpt);
  CheckFalse(ReturnValue, 'ReturnValue');
  CheckLog('Invalid Results per option: -P');
end;

procedure TestTD2XOptions.TestParseOptionPValue;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-PDir';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');

  Check(fOpts.ResultPer = rpDir, 'ResultPer');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionQ;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-Q';
  ReturnValue := fOpts.ParseOption(pOpt);
  CheckFalse(ReturnValue, 'ReturnValue');
  CheckLog('Unknown option: -Q');
end;

procedure TestTD2XOptions.TestParseOptionR;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-R';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.Recurse, 'Recurse');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionROff;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-R-';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckFalse(fOpts.Recurse, 'Recurse');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionROn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-R+';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.Recurse, 'Recurse');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionS;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-S';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.SkipMethods, 'SkipMethods');
  CheckEqualsString('.skip', fOpts.SkipMethodsFoE, 'SkipFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionSBlank;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-S:';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.SkipMethods, 'SkipMethods');
  CheckEqualsString('.skip', fOpts.SkipMethodsFoE, 'SkipFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionSExtn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-S:.Extn';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.SkipMethods, 'SkipMethods');
  CheckEqualsString('.Extn', fOpts.SkipMethodsFoE, 'SkipFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionSSimple;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-S:Extn';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.SkipMethods, 'SkipMethods');
  CheckEqualsString('Extn.skip', fOpts.SkipMethodsFoE, 'SkipFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionSFile;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-S:File.Extn';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.SkipMethods, 'SkipMethods');
  CheckEqualsString('File.Extn', fOpts.SkipMethodsFoE, 'SkipFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionSOff;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-S-';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckFalse(fOpts.SkipMethods, 'SkipMethods');
  CheckEqualsString('.skip', fOpts.SkipMethodsFoE, 'SkipFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionSOn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-S+';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.SkipMethods, 'SkipMethods');
  CheckEqualsString('.skip', fOpts.SkipMethodsFoE, 'SkipFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionT;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-T';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.TimestampFiles, 'TimestampFiles');
  CheckEqualsString(FormatDateTime('-HH-mm', Now), fOpts.OutputTimestamp, 'OutputTimestamp');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionTOff;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-T-';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckFalse(fOpts.TimestampFiles, 'TimestampFiles');
  CheckEqualsString(FormatDateTime('-HH-mm', Now), fOpts.OutputTimestamp, 'OutputTimestamp');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionTOn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-T+';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.TimestampFiles, 'TimestampFiles');
  CheckEqualsString(FormatDateTime('-HH-mm', Now), fOpts.OutputTimestamp, 'OutputTimestamp');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionU;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-U';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.DefinesUsed, 'DefinesUsed');
  CheckEqualsString('.used', fOpts.DefinesUsedFoE, 'UsedFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionUBlank;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-U:';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.DefinesUsed, 'DefinesUsed');
  CheckEqualsString('.used', fOpts.DefinesUsedFoE, 'UsedFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionUExtn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-U:Extn';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.DefinesUsed, 'DefinesUsed');
  CheckEqualsString('.Extn', fOpts.DefinesUsedFoE, 'UsedFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionUFile;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-U:File.Extn';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.DefinesUsed, 'DefinesUsed');
  CheckEqualsString('File.Extn', fOpts.DefinesUsedFoE, 'UsedFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionUOff;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-U-';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckFalse(fOpts.DefinesUsed, 'DefinesUsed');
  CheckEqualsString('.used', fOpts.DefinesUsedFoE, 'UsedFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionUOn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-U+';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.DefinesUsed, 'DefinesUsed');
  CheckEqualsString('.used', fOpts.DefinesUsedFoE, 'UsedFileOrExtn');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionV;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-V';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.Verbose, 'Verbose');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionVOff;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-V-';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckFalse(fOpts.Verbose, 'Verbose');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionVOn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-V+';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.Verbose, 'Verbose');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionW;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-W';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.WriteDefines, 'WriteDefines');
  CheckEqualsString('Defines\', fOpts.DefinesDirectory, 'DefinesDirectory');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionWBlank;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-W:';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.WriteDefines, 'WriteDefines');
  CheckEqualsString('', fOpts.DefinesDirectory, 'DefinesDirectory');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionWOff;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-W-';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckFalse(fOpts.WriteDefines, 'WriteDefines');
  CheckEqualsString('Defines\', fOpts.DefinesDirectory, 'DefinesDirectory');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionWOn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-W+';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.WriteDefines, 'WriteDefines');
  CheckEqualsString('Defines\', fOpts.DefinesDirectory, 'DefinesDirectory');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionX;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-X';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.WriteXml, 'Xml');
  CheckEqualsString('Xml\', fOpts.XmlDirectory, 'XmlDirectory');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionXBlank;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-X:';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.WriteXml, 'Xml');
  CheckEqualsString('', fOpts.XmlDirectory, 'XmlDirectory');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionXOff;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-X-';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckFalse(fOpts.WriteXml, 'Xml');
  CheckEqualsString('Xml\', fOpts.XmlDirectory, 'XmlDirectory');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionXOn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-X+';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.WriteXml, 'Xml');
  CheckEqualsString('Xml\', fOpts.XmlDirectory, 'XmlDirectory');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionXValue;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-X:Value';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.WriteXml, 'Xml');
  CheckEqualsString('Value\', fOpts.XmlDirectory, 'XmlDirectory');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionY;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-Y';
  ReturnValue := fOpts.ParseOption(pOpt);
  CheckFalse(ReturnValue, 'ReturnValue');
  CheckLog('Unknown option: -Y');
end;

procedure TestTD2XOptions.TestParseOptionZ;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-Z';
  ReturnValue := fOpts.ParseOption(pOpt);
  CheckFalse(ReturnValue, 'ReturnValue');
  CheckLog('Unknown option: -Z');
end;

procedure TestTD2XOptions.TestParseOptionWrongPrefix;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '=V';
  StartExpectingException(ED2XOptionsException);
  ReturnValue := fOpts.ParseOption(pOpt);
  StopExpectingException;
  CheckFalse(ReturnValue, 'ReturnValue');
end;

procedure TestTD2XOptions.TestParseOptionWValue;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-W:Write';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.WriteDefines, 'WriteDefines');
  CheckEqualsString('Write\', fOpts.DefinesDirectory, 'DefinesDirectory');
  CheckLog('');
end;

const
  ALTERED_REPORT_OPTIONS =
    'Current option settings: Verbose + Log Errors + Log Not Supp + Timestamp - ' +
    'Final Token + Recurse + Global name :Test Parse mode Full Results per File ' +
    'Show elapsed Quiet Base dir :Test\ Input dir :Test\ Output dir :Test\ ' +
    'Generate XML :Test\ Write Defines :Test\ Defines Used :.Test Count Children :.Test ' +
    'Skipped Methods :Test.skip Use these Defines: Tango, Uniform';
  BASE_REPORT_OPTIONS =
    'Current option settings: Verbose - Log Errors + Log Not Supp - Timestamp - ' +
    'Final Token + Recurse - Global name Delphi2XmlTests Parse mode Full Results per File ' +
    'Show elapsed Quiet Base dir - Input dir :Config\ Output dir :Log\ Generate XML :Xml\ ' +
    'Write Defines -(Defines\) Defines Used :.used Count Children :.cnt ' +
    'Skipped Methods :.skip ';
  DEFAULT_REPORT_OPTIONS =
    BASE_REPORT_OPTIONS + 'Use default Defines';
  EMPTY_REPORT_OPTIONS =
    BASE_REPORT_OPTIONS + 'Use NO Defines';
  DEFINED_REPORT_OPTIONS =
    BASE_REPORT_OPTIONS + 'Use these Defines: CPU32';

{ TestTD2XOptionGeneral }

procedure TestTD2XOptionGeneral.SetAllOptions;
var
  C: Char;
begin
  for C := 'A' to 'Z' do
    if not fOpts.ParseOption('-' + C + ':Test') then
      fOpts.ParseOption('-' + C + '+');
  fOpts.ParseOption('-T-');
  fLog.Clear;
end;

procedure TestTD2XOptionGeneral.TestDefaultOptions;
begin
  Check(fOpts.LogErrors, 'LogErrors');
  CheckFalse(fOpts.LogNotSupported, 'LogNotSupported');
  CheckFalse(fOpts.TimestampFiles, 'TimestampFiles');
  CheckFalse(fOpts.Verbose, 'Verbose');
  CheckFalse(fOpts.Recurse, 'Recurse');
  CheckFalse(fOpts.UseBase, 'UseBase');
  CheckEqualsString(ChangeFileExt(ExtractFileName(ParamStr(0)), ''), fOpts.GlobalName,
    'SkipFileOrExtn');
  CheckEqualsString('', fOpts.BaseDirectory, 'BaseDirectory');
  CheckFalse(fOpts.WriteDefines, 'WriteDefines');
  CheckEqualsString('Defines\', fOpts.DefinesDirectory, 'DefinesDirectory');
  Check(fOpts.WriteXml, 'Xml');
  CheckEqualsString('Xml\', fOpts.XmlDirectory, 'XmlDirectory');
  Check(fOpts.DefinesUsed, 'DefinesUsed');
  CheckEqualsString('.used', fOpts.DefinesUsedFoE, 'UsedFileOrExtn');
  Check(fOpts.LoadDefines, 'LoadDefines');
  CheckEqualsString('', fOpts.Defines.CommaText, 'Defines');
  Check(fOpts.CountChildren, 'CountChildren');
  CheckEqualsString('.cnt', fOpts.CountChildrenFoE, 'CountFileOrExtn');
  Check(fOpts.SkipMethods, 'SkipMethods');
  CheckEqualsString('.skip', fOpts.SkipMethodsFoE, 'SkipFileOrExtn');
  Check(fOpts.ParseMode = pmFull, 'ParseMode');
  Check(fOpts.ResultPer = rpFile, 'ResultPer');
  Check(fOpts.ElapsedMode = emQuiet, 'ElapsedMode');
  Check(fOpts.FinalToken, 'FinalToken');
end;

procedure TestTD2XOptionGeneral.TestReportOptions;
var
  ReturnValue: Boolean;
begin
  SetAllOptions;

  ReturnValue := fOpts.ParseOption('-@');

  Check(ReturnValue, 'ReturnValue');
  CheckLog(ALTERED_REPORT_OPTIONS);
end;

procedure TestTD2XOptionGeneral.TestReportOptionsDefault;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fOpts.ParseOption('-@');

  Check(ReturnValue, 'ReturnValue');
  CheckLog(EMPTY_REPORT_OPTIONS);
end;

procedure TestTD2XOptionGeneral.TestReportOptionsDefines;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fOpts.ParseOption('-D+CPU32');
  Check(ReturnValue, 'ReturnValue');

  ReturnValue := fOpts.ParseOption('-@');
  Check(ReturnValue, 'ReturnValue');
  CheckLog(DEFINED_REPORT_OPTIONS);
end;

procedure TestTD2XOptionGeneral.TestReportOptionsEmpty;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fOpts.ParseOption('-D:');
  Check(ReturnValue, 'ReturnValue');

  ReturnValue := fOpts.ParseOption('-@');
  Check(ReturnValue, 'ReturnValue');
  CheckLog(EMPTY_REPORT_OPTIONS);
end;

procedure TestTD2XOptionGeneral.TestReportOptionsExtn;
var
  ReturnValue: Boolean;
begin
  SetAllOptions;

  ReturnValue := fOpts.ParseOption('-@Test.tst');

  Check(ReturnValue, 'ReturnValue');
  CheckLog('');
end;

procedure TestTD2XOptionGeneral.TestReportOptionsFile;
var
  ReturnValue: Boolean;
begin
  SetAllOptions;

  ReturnValue := fOpts.ParseOption('-@Test');

  Check(ReturnValue, 'ReturnValue');
  CheckLog('');
end;

procedure TestTD2XOptionGeneral.TestReportOptionsFileDefault;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fOpts.ParseOption('-@Test');

  Check(ReturnValue, 'ReturnValue');
  CheckLog('');
end;

procedure TestTD2XOptionGeneral.TestReportOptionsReset;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fOpts.ParseOption('-D!');
  Check(ReturnValue, 'ReturnValue');

  ReturnValue := fOpts.ParseOption('-@');
  Check(ReturnValue, 'ReturnValue');
  CheckLog(DEFAULT_REPORT_OPTIONS);
end;

procedure TestTD2XOptionGeneral.TestResetOptions;
var
  ReturnValue: Boolean;

begin
  SetAllOptions;

  ReturnValue := fOpts.ParseOption('-@');
  Check(ReturnValue, 'ReturnValue');
  CheckLog(ALTERED_REPORT_OPTIONS);

  ReturnValue := fOpts.ParseOption('-!');
  Check(ReturnValue, 'ReturnValue');

  fLog.Clear;
  ReturnValue := fOpts.ParseOption('-@');
  Check(ReturnValue, 'ReturnValue');
  CheckLog(EMPTY_REPORT_OPTIONS);
end;

procedure TestTD2XOptionGeneral.TestShowOptions;
var
  ReturnValue: Boolean;

const
  EXPECTED_SHOW_OPTIONS =
    'Usage: Delphi2XmlTests [ Option | @Params | mFilename | Wildcard ] ... ' +
    'Options: Default Description ? Show valid options ' +
    '! Reset all options to defaults @<file> Report/Output Current options ' +
    'V[+|-] - Log all Parser methods called L[+|-] + Log Error messages ' +
    'N[+|-] - Log Not Supported messages T[+|-] - Timestamp global output files ' +
    'F[+|-] + Record Final Token R[+|-] - Recurse into subdirectories ' +
    'G<str> Delphi2XmlTests Sets global name ' +
    'M<mode> Full Set Parsing mode (F[ull], U[ses]) ' +
    'P<per> File Set Result per (F[ile], S[ubdir], D[ir], W[ildcard], P[aram], R[un]) ' +
    'E<mode> Quiet Set Elapsed time display to be (N[one], Q[uiet], T[otal], P[rocessing]) ' +
    'B[+-]:<dir> - Use <dir> as a base for all file lookups ' +
    'I[+-]:<dir> :Config\ Use <dir> as a base for all file input ' +
    'O[+-]:<dir> :Log\ Use <dir> as a base for all file output ' +
    'X[+-]:<dir> :Xml\ Generate XML files into current or given <dir> ' +
    'W[+-]:<dir> -(Defines\) Generate Final Defines files into current or given <dir> ' +
    'U[+-]:<f/e> :.used Report Defines Used into <f/e> ' +
    'C[+-]:<f/e> :.cnt Report Min/Max Children into <f/e> ' +
    'S[+-]:<f/e> :.skip Load Skipped Methods from <f/e> ' +
    'D[+-!:]<def> Add(+), Remove(-), Clear(!) or Load(:) Defines ' +
    'Definitions: <f/e> If value begins with "." is appended to global name to give file name';
begin
  ReturnValue := fOpts.ParseOption('-?');

  Check(ReturnValue, 'ReturnValue');
  CheckLog(EXPECTED_SHOW_OPTIONS);
end;

{ TestTD2XOptionEnums }

function TestTD2XOptionEnums.ConvertElapsedMode(pStr: string;
  pDflt: TD2XElapsedMode; out pVal: TD2XElapsedMode): boolean;
begin
  pVal := pDflt;
  Result := True;
end;

function TestTD2XOptionEnums.ConvertParsingMode(pStr: string;
  pDflt: TD2XParseMode; out pVal: TD2XParseMode): boolean;
begin
  pVal := pDflt;
  Result := True;
end;

function TestTD2XOptionEnums.ConvertResultPer(pStr: string;
  pDflt: TD2XResultPer; out pVal: TD2XResultPer): boolean;
begin
  pVal := pDflt;
  Result := True;
end;

function TestTD2XOptionEnums.FormatElapsedMode(pVal: TD2XElapsedMode): string;
begin
  Result := TD2X.ToLabel(pVal);
end;

function TestTD2XOptionEnums.FormatParsingMode(pVal: TD2XParseMode): string;
begin
  Result := TD2X.ToLabel(pVal);
end;

function TestTD2XOptionEnums.FormatResultPer(pVal: TD2XResultPer): string;
begin
  Result := TD2X.ToLabel(pVal);
end;

procedure TestTD2XOptionEnums.TestElapsedModeInvalidCreate;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<TD2XElapsedMode>.CreateReset('', '', '', '', nil, nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need to use correct constructor', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XOptionEnums.TestElapsedModeParam;
var
  lPrm : TD2XSingleParam<TD2XElapsedMode>;
begin
  lPrm := TD2XSingleParam<TD2XElapsedMode>.CreateParam('T', 'Test', '<tst>', 'Test Elapsed mode',
    emTotal, ConvertElapsedMode, FormatElapsedMode, nil);
  try
    CheckEqualsString('T<tst> Total Test Elapsed mode', ReduceString(lPrm.Describe), 'Describe Param');
    CheckEqualsString('Test Total', ReduceString(lPrm.Report), 'Report Default Value');
    Check(lPrm.IsDefault, 'Check is Default');

    Check(lPrm.Parse('T'), 'Parse right code with No value');
    Check(lPrm.Parse('TQuiet'), 'Parse right code with value');

    Check(emTotal = lPrm.Value, 'Returned value');
    CheckEqualsString('TTotal', lPrm.ToString, 'String representation');

    lPrm.Value := emQuiet;
    CheckEqualsString('TQuiet', lPrm.ToString, 'String representation');
  finally
    FreeAndNil(lPrm);
  end;
end;

procedure TestTD2XOptionEnums.TestParseModeInvalidCreate;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<TD2XParseMode>.CreateReset('', '', '', '', nil, nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need to use correct constructor', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XOptionEnums.TestParseModeParam;
var
  lPrm: TD2XSingleParam<TD2XParseMode>;
begin
  lPrm := TD2XSingleParam<TD2XParseMode>.CreateParam('T', 'Test', '<tst>', 'Test Parse mode',
    pmFull, ConvertParsingMode, FormatParsingMode, nil);
  try
    CheckEqualsString('T<tst> Full Test Parse mode', ReduceString(lPrm.Describe), 'Describe Param');
    CheckEqualsString('Test Full', ReduceString(lPrm.Report), 'Report Default Value');
    Check(lPrm.IsDefault, 'Check is Default');

    Check(lPrm.Parse('T'), 'Parse right code with No value');
    Check(lPrm.Parse('TUses'), 'Parse right code with value');

    Check(pmFull = lPrm.Value, 'Returned value');
    CheckEqualsString('TFull', lPrm.ToString, 'String representation');

    lPrm.Value := pmUses;
    CheckEqualsString('TUses', lPrm.ToString, 'String representation');
  finally
    FreeAndNil(lPrm);
  end;
end;

procedure TestTD2XOptionEnums.TestResultPerInvalidCreate;
begin
  StartExpectingException(EInvalidParam);
  try
    TD2XSingleParam<TD2XResultPer>.CreateReset('', '', '', '', nil, nil);
  except
    on E: EInvalidParam do
    begin
      CheckEqualsString('Need to use correct constructor', E.Message, 'Exception message');
      raise;
    end;
  end;
end;

procedure TestTD2XOptionEnums.TestResultPerParam;
var
  lPrm: TD2XSingleParam<TD2XResultPer>;
begin
  lPrm := TD2XSingleParam<TD2XResultPer>.CreateParam('T', 'Test', '<tst>', 'Test Parse mode',
    rpFile, ConvertResultPer, FormatResultPer, nil);
  try
    CheckEqualsString('T<tst> File Test Parse mode', ReduceString(lPrm.Describe), 'Describe Param');
    CheckEqualsString('Test File', ReduceString(lPrm.Report), 'Report Default Value');
    Check(lPrm.IsDefault, 'Check is Default');

    Check(lPrm.Parse('T'), 'Parse right code with No value');
    Check(lPrm.Parse('TDir'), 'Parse right code with value');

    Check(rpFile = lPrm.Value, 'Returned value');
    CheckEqualsString('TFile', lPrm.ToString, 'String representation');

    lPrm.Value := rpDir;
    CheckEqualsString('TDir', lPrm.ToString, 'String representation');
  finally
    FreeAndNil(lPrm);
  end;
end;

initialization

RegisterTests('Options', [TestTD2XOptionGeneral.Suite, TestTD2XOptionEnums.Suite,
    TestTD2XOptionFilenames.Suite, TestTD2XOptions.Suite]);

end.
