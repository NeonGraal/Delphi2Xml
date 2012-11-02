unit D2XProcessorTest;

interface

implementation

uses
  TestFramework,
  CastaliaPasLexTypes,
  D2XProcessor,
  D2Xml,
  D2XTest,
  D2XHandler,
  D2XHandlerTest,
  D2XOptions,
  D2XParam,
  D2XParser,
  System.Classes,
  System.Diagnostics,
  System.Generics.Collections,
  System.SysUtils,
  System.Rtti;

type
  TTestBoolFlag = class(TInterfacedObject, IParamFlag)
  private
    fFlag: Boolean;

    function GetFlag: Boolean;
    procedure SetFlag(pVal: Boolean);
  end;

  TestTD2XProcessor = class(TStringTestCase)
  strict private
    FHandler: TD2XHandlerTester;
    FD2XProcessor: TD2XProcessor;

    fActive: IParamFlag;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSetProcessingInput;
    procedure TestSetProcessingOutput;
    procedure TestSetResultsOutput;
    procedure TestSetFileInput;
    procedure TestSetFileOutput;

    procedure TestBeginProcessing;
    procedure TestEndProcessing;

    procedure TestBeginFile;
    procedure TestEndFile;

    procedure TestBeginResults;
    procedure TestEndResults;

    procedure TestCheckBeforeMethod;
    procedure TestCheckAfterMethod;

    procedure TestBeginMethod;
    procedure TestEndMethod;
  end;

  TestTD2XParamProcessor = class(TStringTestCase)
  strict private
    FD2XParamProcessor: TD2XParamProcessor;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEndProcessing;

    procedure TestProcessParam;
    procedure TestProcessParamPasFiles;
    procedure TestProcessParamParamFile;

    procedure TestProcessCountChildren;
    procedure TestProcessVerbose;
  end;

const
  EXPECTED_PROCESSING = 'Processing Test.pas ... done';

  { TestTD2XProcessor }

procedure TestTD2XParamProcessor.SetUp;
begin
  inherited;

  FD2XParamProcessor := TD2XParamProcessor.Create(fSB);
end;

procedure TestTD2XParamProcessor.TearDown;
begin
  FD2XParamProcessor := nil;

  inherited;
end;

procedure TestTD2XParamProcessor.TestEndProcessing;
begin
  FD2XParamProcessor.EndProcessing;
  CheckString(fSB, '', 'Nothing');
end;

procedure TestTD2XParamProcessor.TestProcessCountChildren;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := '-!!';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);
  Check(ReturnValue, 'Return Value 1');

  pStr := '-C+';
  pIdx := 1;

  ReturnValue := FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);
  Check(ReturnValue, 'Return Value 2');

  pStr := 'Test.pas';
  pIdx := 2;

  ReturnValue := FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckString(fSB, '', 'Empty Log');
end;

procedure TestTD2XParamProcessor.TestProcessParam;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := '';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);

  CheckFalse(ReturnValue, 'Return Value');
  CheckString(fSB, '', 'Nothing');
end;

procedure TestTD2XParamProcessor.TestProcessParamParamFile;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
const
  EXPECTED_REPORT = 'Current option settings: Verbose - Log Errors + Log Not Supp - ' +
    'Timestamp - Final Token + Recurse - Global name Delphi2XmlTests ' +
    'Parse mode Full Results per File Show elapsed Quiet Base dir - ' +
    'Input dir :Config\ Output dir :Log\ Generate XML :Xml\ ' +
    'Write Defines -(Defines\) Defines Used :.used ' +
    'Count Children :.cnt Skipped Methods :.skip ' +
    'Use these Defines: CONDITIONALEXPRESSIONS, CPU386, MSWINDOWS, UNICODE, VER230, WIN32';
begin
  pStr := '@Test.prm';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckString(fSB, EXPECTED_REPORT, 'Nothing');
end;

procedure TestTD2XParamProcessor.TestProcessParamPasFiles;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := 'Test.pas';
  pFrom := 'Test';
  pIdx := 0;

  ReturnValue := FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckString(fSB, EXPECTED_PROCESSING, 'Nothing');
end;

procedure TestTD2XParamProcessor.TestProcessVerbose;
var
  ReturnValue: Boolean;
  pIdx: Integer;
  pFrom: string;
  pStr: string;
begin
  pStr := '-!!';
  pFrom := 'Test';
  pIdx := 0;
  FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);

  pStr := '-V+';
  pIdx := 1;
  FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);

  pStr := 'Test.pas';
  pIdx := 2;
  ReturnValue := FD2XParamProcessor.ProcessParam(pStr, pFrom, pIdx);

  Check(ReturnValue, 'Return Value');
  CheckNotEquals('', fSB.ToString, 'Log');
end;

{ TestTD2XProcessor }

procedure TestTD2XProcessor.SetUp;
begin
  inherited;

  FHandler := TD2XHandlerTester.Create;
  fActive := TTestBoolFlag.Create;
  FD2XProcessor := TD2XProcessor.Create(fActive, FHandler);
end;

procedure TestTD2XProcessor.TearDown;
begin
  FD2XProcessor.Free;
  FD2XProcessor := nil;
  FHandler.Free;
  FHandler := nil;

  inherited;
end;

procedure TestTD2XProcessor.TestBeginFile;
begin
  FD2XProcessor.SetFileInput(
    function: string
    begin
      Result := '';
    end);

  FD2XProcessor.BeginFile;
  CheckFalse(FHandler.CalledBeginFile, 'Ignored Begin File');

  fActive.Flag := True;
  FD2XProcessor.BeginFile;
  CheckTrue(FHandler.CalledBeginFile, 'Called Begin File');
end;

procedure TestTD2XProcessor.TestBeginMethod;
begin
  FD2XProcessor.BeginMethod('');
  CheckFalse(FHandler.CalledBeginMethod, 'Ignored Begin Method');

  fActive.Flag := True;
  FD2XProcessor.BeginMethod('');
  CheckTrue(FHandler.CalledBeginMethod, 'Called Begin Method');
end;

procedure TestTD2XProcessor.TestBeginProcessing;
begin
  FD2XProcessor.SetProcessingInput(
    function: string
    begin
      Result := '';
    end);

  FD2XProcessor.BeginProcessing;
  CheckFalse(FHandler.CalledBeginProcessing, 'Ignored Begin Processing');

  fActive.Flag := True;
  FD2XProcessor.BeginProcessing;
  CheckTrue(FHandler.CalledBeginProcessing, 'Called Begin Processing');
end;

procedure TestTD2XProcessor.TestBeginResults;
begin
  FD2XProcessor.BeginResults;
  CheckFalse(FHandler.CalledBeginResults, 'Ignored Begin Results');

  fActive.Flag := True;
  FD2XProcessor.BeginResults;
  CheckTrue(FHandler.CalledBeginResults, 'Called Begin Results');
end;

procedure TestTD2XProcessor.TestCheckAfterMethod;
begin
  FD2XProcessor.CheckAfterMethod('');
  CheckFalse(FHandler.CalledCheckAfterMethod, 'Ignored Check After Method');

  fActive.Flag := True;
  FD2XProcessor.CheckAfterMethod('');
  CheckTrue(FHandler.CalledCheckAfterMethod, 'Called Check After Method');
end;

procedure TestTD2XProcessor.TestCheckBeforeMethod;
begin
  FD2XProcessor.CheckBeforeMethod('');
  CheckFalse(FHandler.CalledCheckBeforeMethod, 'Ignored Check Before Method');

  fActive.Flag := True;
  FD2XProcessor.CheckBeforeMethod('');
  CheckTrue(FHandler.CalledCheckBeforeMethod, 'Called Check Before Method');
end;

procedure TestTD2XProcessor.TestEndFile;
begin
  FD2XProcessor.SetFileOutput(
    function: string
    begin
      Result := '';
    end);

  FD2XProcessor.EndFile;
  CheckFalse(FHandler.CalledEndFile, 'Ignored End File');

  fActive.Flag := True;
  FD2XProcessor.EndFile;
  CheckTrue(FHandler.CalledEndFile, 'Called End File');
end;

procedure TestTD2XProcessor.TestEndMethod;
begin
  FD2XProcessor.EndMethod('');
  CheckFalse(FHandler.CalledEndMethod, 'Ignored End Method');

  fActive.Flag := True;
  FD2XProcessor.EndMethod('');
  CheckTrue(FHandler.CalledEndMethod, 'Called End Method');
end;

procedure TestTD2XProcessor.TestEndProcessing;
begin
  FD2XProcessor.SetProcessingOutput(
    function: string
    begin
      Result := '';
    end);

  FD2XProcessor.EndProcessing;
  CheckFalse(FHandler.CalledEndProcessing, 'Ignored End Processing');

  fActive.Flag := True;
  FD2XProcessor.EndProcessing;
  CheckTrue(FHandler.CalledEndProcessing, 'Called End Processing');
end;

procedure TestTD2XProcessor.TestEndResults;
begin
  FD2XProcessor.SetResultsOutput(
    function(pFile: string): string
    begin
      Result := '';
    end);

  FD2XProcessor.EndResults('');
  CheckFalse(FHandler.CalledEndResults, 'Ignored End Results');

  fActive.Flag := True;
  FD2XProcessor.EndResults('');
  CheckTrue(FHandler.CalledEndResults, 'Called End Results');
end;

procedure TestTD2XProcessor.TestSetFileInput;
var
  lCalled: Boolean;
begin
  FHandler.CreateStreams := True;
  lCalled := False;

  FD2XProcessor.SetFileInput(
    function: string
    begin
      lCalled := True;
      Result := '';
    end);

  FD2XProcessor.BeginFile;
  CheckFalse(lCalled, 'Ignored File Input');

  fActive.Flag := True;
  FD2XProcessor.BeginFile;
  CheckTrue(lCalled, 'Called File Input');
end;

procedure TestTD2XProcessor.TestSetFileOutput;
var
  lCalled: Boolean;
begin
  FHandler.CreateStreams := True;
  lCalled := False;

  FD2XProcessor.SetFileOutput(
    function: string
    begin
      lCalled := True;
      Result := '';
    end);

  FD2XProcessor.EndFile;
  CheckFalse(lCalled, 'Ignored File Output');

  fActive.Flag := True;
  FD2XProcessor.EndFile;
  CheckTrue(lCalled, 'Called File Output');
end;

procedure TestTD2XProcessor.TestSetProcessingInput;
var
  lCalled: Boolean;
begin
  FHandler.CreateStreams := True;
  lCalled := False;
  FD2XProcessor.SetProcessingInput(
    function: string
    begin
      lCalled := True;
      Result := '';
    end);

  FD2XProcessor.BeginProcessing;
  CheckFalse(lCalled, 'Ignored Processing Input');

  fActive.Flag := True;
  FD2XProcessor.BeginProcessing;
  CheckTrue(lCalled, 'Called Processing Input');
end;

procedure TestTD2XProcessor.TestSetProcessingOutput;
var
  lCalled: Boolean;
begin
  FHandler.CreateStreams := True;
  lCalled := False;
  FD2XProcessor.SetProcessingOutput(
    function: string
    begin
      lCalled := True;
      Result := '';
    end);

  FD2XProcessor.EndProcessing;
  CheckFalse(lCalled, 'Ignored Processing Output');

  fActive.Flag := True;
  FD2XProcessor.EndProcessing;
  CheckTrue(lCalled, 'Called Processing Output');
end;

procedure TestTD2XProcessor.TestSetResultsOutput;
var
  lCalled: Boolean;
begin
  FHandler.CreateStreams := True;
  lCalled := False;
  FD2XProcessor.SetResultsOutput(
    function(pFile: string): string
    begin
      lCalled := True;
      Result := '';
    end);

  FD2XProcessor.EndResults('');
  CheckFalse(lCalled, 'Ignored Results Output');

  fActive.Flag := True;
  FD2XProcessor.EndResults('');
  CheckTrue(lCalled, 'Called Results Output');
end;

{ TTestBoolFlag }

function TTestBoolFlag.GetFlag: Boolean;
begin
  Result := fFlag;
end;

procedure TTestBoolFlag.SetFlag(pVal: Boolean);
begin
  fFlag := pVal;
end;

initialization

// Register any test cases with the test runner
RegisterTests('Processor', [TestTD2XProcessor.Suite, TestTD2XParamProcessor.Suite]);

end.
