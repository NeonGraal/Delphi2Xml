unit D2XHandlerTest;

interface

uses
  D2XHandler;

type
  TD2XHandlerTester = class(TD2XHandler)
  public
    CalledBeginProcessing: Boolean;
    CalledEndProcessing: Boolean;
    CalledBeginFile: Boolean;
    CalledEndFile: Boolean;
    CalledBeginResults: Boolean;
    CalledEndResults: Boolean;
    CalledCheckBeforeMethod: Boolean;
    CalledCheckAfterMethod: Boolean;
    CalledBeginMethod: Boolean;
    CalledEndMethod: Boolean;
    CalledCopy: Boolean;

    CreateStreams: Boolean;

    function Description: String; override;
    procedure BeginProcessing(pInput: TD2XHandler.ThStreamCreator); override;
    procedure EndProcessing(pOutput: TD2XHandler.ThStreamCreator); override;
    procedure BeginFile(pInput: TD2XHandler.ThStreamCreator); override;
    procedure EndFile(pOutput: TD2XHandler.ThStreamCreator); override;
    procedure BeginResults; override;
    procedure EndResults(pFile: String); override;
    function CheckBeforeMethod(pMethod: String): Boolean; override;
    function CheckAfterMethod(pMethod: String): Boolean; override;
    procedure BeginMethod(pMethod: String); override;
    procedure EndMethod(pMethod: String); override;

    procedure Copy(pFrom: TD2XHandler); override;

  end;

implementation

uses
  System.Classes,
  System.Generics.Collections,
  TestFramework;

type
  TestTD2XHandler = class(TTestCase)
  protected
    FD2XHandler: TD2XHandler;
    FD2XTester: TD2XHandlerTester;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
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
    procedure TestCopy;
  end;

{ TestTD2XHandler }

procedure TestTD2XHandler.SetUp;
begin
  FD2XTester := TD2XHandlerTester.Create;
  FD2XHandler := FD2XTester;
end;

procedure TestTD2XHandler.TearDown;
begin
  FD2XHandler.Free;
  FD2XHandler := nil;
end;

procedure TestTD2XHandler.TestBeginProcessing;
begin
  FD2XHandler.BeginProcessing(nil);

  CheckTrue(FD2XTester.CalledBeginProcessing, 'Called Begin Processing');
end;

procedure TestTD2XHandler.TestEndProcessing;
begin
  FD2XHandler.EndProcessing(nil);

  CheckTrue(FD2XTester.CalledEndProcessing, 'Called End Processing');
end;

procedure TestTD2XHandler.TestBeginResults;
begin
  FD2XHandler.BeginResults;

  CheckTrue(FD2XTester.CalledBeginResults, 'Called Begin Results');
end;

procedure TestTD2XHandler.TestCheckAfterMethod;
begin
  FD2XHandler.CheckAfterMethod('');

  CheckTrue(FD2XTester.CalledCheckAfterMethod, 'Called Check After Method');
end;

procedure TestTD2XHandler.TestCheckBeforeMethod;
begin
  FD2XHandler.CheckBeforeMethod('');

  CheckTrue(FD2XTester.CalledCheckBeforeMethod, 'Called Check Before Method');
end;

procedure TestTD2XHandler.TestCopy;
begin
  FD2XHandler.Copy(nil);

  CheckTrue(FD2XTester.CalledCopy, 'Called Copy');
end;

procedure TestTD2XHandler.TestBeginMethod;
begin
  FD2XHandler.BeginMethod('');

  CheckTrue(FD2XTester.CalledBeginMethod, 'Called Begin Method');
end;

procedure TestTD2XHandler.TestEndMethod;
begin
  FD2XHandler.EndMethod('');

  CheckTrue(FD2XTester.CalledEndMethod, 'Called End Method');
end;

procedure TestTD2XHandler.TestEndResults;
begin
  FD2XHandler.EndResults('');

  CheckTrue(FD2XTester.CalledEndResults, 'Called End Results');
end;

procedure TestTD2XHandler.TestBeginFile;
begin
  FD2XHandler.BeginFile(nil);

  CheckTrue(FD2XTester.CalledBeginFile, 'Called Begin File');
end;

procedure TestTD2XHandler.TestEndFile;
begin
  FD2XHandler.EndFile(nil);

  CheckTrue(FD2XTester.CalledEndFile, 'Called End File');
end;

{ TD2XHandlerTester }

procedure TD2XHandlerTester.BeginMethod(pMethod: String);
begin
  CalledBeginMethod := true;
end;

procedure TD2XHandlerTester.BeginFile(pInput: TD2XHandler.ThStreamCreator);
begin
  CalledBeginFile := true;
  if CreateStreams then
    pInput;
end;

procedure TD2XHandlerTester.BeginProcessing(pInput: TD2XHandler.ThStreamCreator);
begin
  CalledBeginProcessing := true;
  if CreateStreams then
    pInput;
end;

procedure TD2XHandlerTester.BeginResults;
begin
  CalledBeginResults := true;
end;

function TD2XHandlerTester.CheckAfterMethod(pMethod: String): Boolean;
begin
  CalledCheckAfterMethod := True;

  Result := inherited CheckAfterMethod(pMethod);
end;

function TD2XHandlerTester.CheckBeforeMethod(pMethod: String): Boolean;
begin
  CalledCheckBeforeMethod := True;

  Result := inherited CheckBeforeMethod(pMethod);
end;

procedure TD2XHandlerTester.Copy(pFrom: TD2XHandler);
begin
  CalledCopy := true;
end;

function TD2XHandlerTester.Description: String;
begin
  Result := 'Handler Tester';
end;

procedure TD2XHandlerTester.EndMethod(pMethod: String);
begin
  CalledEndMethod := true;
end;

procedure TD2XHandlerTester.EndFile(pOutput: TD2XHandler.ThStreamCreator);
begin
  CalledEndFile := true;
  if CreateStreams then
    pOutput;
end;

procedure TD2XHandlerTester.EndProcessing(pOutput: TD2XHandler.ThStreamCreator);
begin
  CalledEndProcessing := true;
  if CreateStreams then
    pOutput;
end;

procedure TD2XHandlerTester.EndResults(pFile: String);
begin
  CalledEndResults := true;
end;

initialization

// Register any test cases with the test runner
RegisterTests('Handlers', [TestTD2XHandler.Suite]);

end.
