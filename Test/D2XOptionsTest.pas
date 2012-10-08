unit D2XOptionsTest;

interface

uses
  System.Classes,
  TestFramework,
  System.StrUtils,
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
    procedure TestParseOptionBValue;
    procedure TestParseOptionC;
    procedure TestParseOptionCExtn;
    procedure TestParseOptionCOff;
    procedure TestParseOptionCOn;
    procedure TestParseOptionCFile;
    procedure TestParseOptionD;
    procedure TestParseOptionDValue;
    procedure TestParseOptionDMany;
    procedure TestParseOptionE;
    procedure TestParseOptionEOff;
    procedure TestParseOptionEOn;
    procedure TestParseOptionF;
    procedure TestParseOptionFOff;
    procedure TestParseOptionFOn;
    procedure TestParseOptionG;
    procedure TestParseOptionGValue;
    procedure TestParseOptionH;
    procedure TestParseOptionI;
    procedure TestParseOptionIOff;
    procedure TestParseOptionIOn;
    procedure TestParseOptionIValue;
    procedure TestParseOptionJ;
    procedure TestParseOptionK;
    procedure TestParseOptionL;
    procedure TestParseOptionLOff;
    procedure TestParseOptionLOn;
    procedure TestParseOptionLValue;
    procedure TestParseOptionM;
    procedure TestParseOptionMValue;
    procedure TestParseOptionN;
    procedure TestParseOptionNOff;
    procedure TestParseOptionNOn;
    procedure TestParseOptionO;
    procedure TestParseOptionOOff;
    procedure TestParseOptionOOn;
    procedure TestParseOptionOValue;
    procedure TestParseOptionP;
    procedure TestParseOptionPValue;
    procedure TestParseOptionQ;
    procedure TestParseOptionR;
    procedure TestParseOptionROff;
    procedure TestParseOptionROn;
    procedure TestParseOptionS;
    procedure TestParseOptionSFile;
    procedure TestParseOptionSOff;
    procedure TestParseOptionSOn;
    procedure TestParseOptionSSimple;
    procedure TestParseOptionT;
    procedure TestParseOptionTOff;
    procedure TestParseOptionTOn;
    procedure TestParseOptionU;
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
    procedure TestParseOptionWValue;
    procedure TestParseOptionX;
    procedure TestParseOptionXOff;
    procedure TestParseOptionXOn;
    procedure TestParseOptionXValue;
    procedure TestParseOptionY;
    procedure TestParseOptionZ;
    procedure TestParseOptionZValue;
  end;

  // Test methods for class TD2XOptions
  TestTD2XOptionGeneral = class(TestTD2XOptionBase)
  published
    procedure TestDefaultOptions;
    procedure TestReportOptions;
    procedure TestShowOptions;
  end;

implementation

{ TestTD2XOptionBase }

procedure TestTD2XOptionBase.CheckLog(pMsg: string);
begin
  CheckEqualsString(pMsg, Trim(fLog.DataString), 'Log');
end;

procedure TestTD2XOptionBase.SetUp;
begin
  fOpts := TD2XOptions.Create;
  fLog := TStringStream.Create;
  fOpts.SetLog(fLog);
end;

procedure TestTD2XOptionBase.TearDown;
begin
  FreeAndNil(fLog);
  FreeAndNil(fOpts);
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
  CheckEqualsString('.cnt', fOpts.CountFileOrExtn, 'CountFileOrExtn');
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
  CheckEqualsString('.Extn', fOpts.CountFileOrExtn, 'CountFileOrExtn');
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
  CheckEqualsString('.cnt', fOpts.CountFileOrExtn, 'CountFileOrExtn');
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
  CheckEqualsString('.cnt', fOpts.CountFileOrExtn, 'CountFileOrExtn');
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
  CheckEqualsString('File.Extn', fOpts.CountFileOrExtn, 'CountFileOrExtn');
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
  CheckLog('Invalid Define option: -D');
end;

procedure TestTD2XOptions.TestParseOptionDMany;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fOpts.ParseOption('-D:Value1');
  Check(ReturnValue, 'ReturnValue1');
  ReturnValue := fOpts.ParseOption('-D:Value2');
  Check(ReturnValue, 'ReturnValue2');
  CheckEqualsString('Alpha,Beta,Gamma,Value1,Value2', fOpts.Defines.CommaText, 'Defines');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionDValue;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-D:Value';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckEqualsString('Alpha,Beta,Gamma,Value', fOpts.Defines.CommaText, 'Defines');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionE;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-E';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.LogErrors, 'LogErrors');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionEOff;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-E-';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckFalse(fOpts.LogErrors, 'LogErrors');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionEOn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-E+';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.LogErrors, 'LogErrors');
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
  Check(fOpts.LoadDefines, 'LoadDefines');
  CheckEqualsString('', fOpts.LoadFileOrExtn, 'LoadFileOrExtn');
  CheckEqualsString('', fOpts.Defines.CommaText, 'Defines');
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
  CheckFalse(fOpts.LoadDefines, 'LoadDefines');
  CheckEqualsString('', fOpts.LoadFileOrExtn, 'LoadFileOrExtn');
  CheckEqualsString('', fOpts.Defines.CommaText, 'Defines');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionLOn;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  fOpts.Defines.CommaText := 'Alpha,Beta,Value';
  pOpt := '-L+';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.LoadDefines, 'LoadDefines');
  CheckEqualsString('', fOpts.LoadFileOrExtn, 'LoadFileOrExtn');
  CheckEqualsString('', fOpts.Defines.CommaText, 'Defines');
  CheckLog('');
end;

procedure TestTD2XOptions.TestParseOptionLValue;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  fOpts.Defines.CommaText := 'Alpha,Beta,Value';
  pOpt := '-L:Load.def';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  Check(fOpts.LoadDefines, 'LoadDefines');
  CheckEqualsString('Load.def', fOpts.LoadFileOrExtn, 'LoadFileOrExtn');
  CheckEqualsString('Tango,Uniform', fOpts.Defines.CommaText, 'Defines');
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
  CheckEqualsString('.skip', fOpts.SkipFileOrExtn, 'SkipFileOrExtn');
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
  CheckEqualsString('Extn.skip', fOpts.SkipFileOrExtn, 'SkipFileOrExtn');
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
  CheckEqualsString('File.Extn', fOpts.SkipFileOrExtn, 'SkipFileOrExtn');
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
  CheckEqualsString('.skip', fOpts.SkipFileOrExtn, 'SkipFileOrExtn');
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
  CheckEqualsString('.skip', fOpts.SkipFileOrExtn, 'SkipFileOrExtn');
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
  CheckEqualsString('.used', fOpts.UsedFileOrExtn, 'UsedFileOrExtn');
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
  CheckEqualsString('.Extn', fOpts.UsedFileOrExtn, 'UsedFileOrExtn');
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
  CheckEqualsString('File.Extn', fOpts.UsedFileOrExtn, 'UsedFileOrExtn');
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
  CheckEqualsString('.used', fOpts.UsedFileOrExtn, 'UsedFileOrExtn');
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
  CheckEqualsString('.used', fOpts.UsedFileOrExtn, 'UsedFileOrExtn');
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
  Check(fOpts.Xml, 'Xml');
  CheckEqualsString('Xml\', fOpts.XmlDirectory, 'XmlDirectory');
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
  CheckFalse(fOpts.Xml, 'Xml');
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
  Check(fOpts.Xml, 'Xml');
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
  Check(fOpts.Xml, 'Xml');
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
  CheckLog('Invalid Undefine option: -Z');
end;

procedure TestTD2XOptions.TestParseOptionZValue;
var
  ReturnValue: Boolean;
  pOpt: string;
begin
  pOpt := '-Z:Beta';
  ReturnValue := fOpts.ParseOption(pOpt);
  Check(ReturnValue, 'ReturnValue');
  CheckEqualsString('Alpha,Gamma', fOpts.Defines.CommaText, 'Defines');
  CheckLog('');
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

{ TestTD2XOptionGeneral }

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
  Check(fOpts.Xml, 'Xml');
  CheckEqualsString('Xml\', fOpts.XmlDirectory, 'XmlDirectory');
  Check(fOpts.DefinesUsed, 'DefinesUsed');
  CheckEqualsString('.used', fOpts.UsedFileOrExtn, 'UsedFileOrExtn');
  Check(fOpts.LoadDefines, 'LoadDefines');
  CheckEqualsString('.def', fOpts.LoadFileOrExtn, 'LoadFileOrExtn');
  CheckEqualsString('', fOpts.Defines.CommaText, 'Defines');
  Check(fOpts.CountChildren, 'CountChildren');
  CheckEqualsString('.cnt', fOpts.CountFileOrExtn, 'CountFileOrExtn');
  Check(fOpts.SkipMethods, 'SkipMethods');
  CheckEqualsString(fOpts.GlobalName + '.skip', fOpts.SkipFileOrExtn, 'SkipFileOrExtn');
  Check(fOpts.ParseMode = pmFull, 'ParseMode');
  Check(fOpts.ResultPer = rpFile, 'ResultPer');
  Check(fOpts.FinalToken, 'FinalToken');
end;

procedure TestTD2XOptionGeneral.TestReportOptions;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fOpts.ReportOptions;

  Check(ReturnValue, 'ReturnValue');
  CheckNotEqualsString('', fLog.DataString, 'Log');
end;

procedure TestTD2XOptionGeneral.TestShowOptions;
var
  ReturnValue: Boolean;
begin
  ReturnValue := fOpts.ShowOptions;

  Check(ReturnValue, 'ReturnValue');
  CheckNotEqualsString('', fLog.DataString, 'Log');
end;

initialization

RegisterTests('Options', [TestTD2XOptionGeneral.Suite, TestTD2XOptionFilenames.Suite,
    TestTD2XOptions.Suite]);

end.
