unit Test.Parser.Tests;

interface

implementation

uses
  D2X.Parser,
  System.Classes,
  Test.Constants,
  Test.Parser,
  TestFramework;

type
  TestTD2XDefinesParser = class(TD2XParserTestCase)
  strict private
    FD2XDefinesParser: TD2XDefinesParser;

  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUsedUnitName;
    procedure TestMainUnitName;
    procedure TestMainUsedUnitExpression;
    procedure TestContainsExpression;
    procedure TestParseFile;

    procedure TestGetLexerDefines;
    procedure TestStartDefines;
    procedure TestHoldDefines;
    procedure TestProcessString;
  end;

  TestTD2XUsesParser = class(TD2XParserTestCase)
  strict private
    FD2XUsesParser: TD2XUsesParser;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUsedUnitName;
    procedure TestMainUnitName;
    procedure TestMainUsedUnit;
    procedure TestMainUsedUnitExpression;
  end;

  TestTD2XFullParser = class(TD2XParserTestCase)
  strict private
    FD2XFullParser: TD2XFullParser;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestUsedUnitName;
    procedure TestMainUnitName;
    procedure TestMainUsedUnit;
    procedure TestMainUsedUnitExpression;
  end;

  { TestTD2XDefinesParser }

procedure TestTD2XDefinesParser.SetUp;
begin
  FD2XDefinesParser := TD2XDefinesParser.Create;
  InitParser(FD2XDefinesParser);
end;

procedure TestTD2XDefinesParser.TearDown;
begin
  FD2XDefinesParser.Free;
  FD2XDefinesParser := nil;
end;

procedure TestTD2XDefinesParser.TestContainsExpression;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestContainsExpression';
  pContents := 'package Test; contains Test2 in ''Test2.pas''; end.';

  FD2XDefinesParser.ProcessString(pFilename, pContents);

  CheckText('Test2', 'Main Used Unit Name');
  CheckAttribute('file', '''Test2.pas''', 'Blank');
end;

procedure TestTD2XDefinesParser.TestGetLexerDefines;
var
  pDefs: TStringList;
begin
  pDefs := TStringList.Create;
  pDefs.Sorted := True;
  try
    FD2XDefinesParser.GetLexerDefines(pDefs);

    CheckEqualsString(EXPECTED_DEFINES, pDefs.CommaText, 'Default Lexer Defines');
  finally
    pDefs.Free;
  end;
end;

procedure TestTD2XDefinesParser.TestHoldDefines;
var
  pContents: string;
  pFilename: string;
begin
  FD2XDefinesParser.HeldDefines.CommaText := 'HELLO,TEST,VALUE';
  pFilename := 'TestProcessString';
  pContents := 'unit Test; {$UNDEF TEST} end.';

  FD2XDefinesParser.ProcessString(pFilename, pContents);
  CheckDefines('HELLO,TEST,VALUE', 'Current');
end;

procedure TestTD2XDefinesParser.TestMainUnitName;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestMainUnitName';
  pContents := 'unit Test; end.';

  FD2XDefinesParser.ProcessString(pFilename, pContents);

  CheckText('Test', 'Main Unit Name');
  CheckAttribute('', '', 'Blank');
end;

procedure TestTD2XDefinesParser.TestMainUsedUnitExpression;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestMainUsedUnitExpression';
  pContents := 'program Test; uses Test2 in ''Test2.pas''; begin end.';

  FD2XDefinesParser.ProcessString(pFilename, pContents);

  CheckText('Test2', 'Main Used Unit Name');
  CheckAttribute('file', '''Test2.pas''', 'Blank');
end;

procedure TestTD2XDefinesParser.TestParseFile;
var
  pContents: string;
  pFilename: string;
begin
  FD2XDefinesParser.StartDefines.Clear;
  pFilename := 'TestParseFile';
  pContents := 'unit Test; end.';

  FD2XDefinesParser.ProcessString(pFilename, pContents);

  CheckDefines('', 'Cleared Lexer');
end;

procedure TestTD2XDefinesParser.TestProcessString;
var
  pContents: string;
  pFilename: string;
begin
  FD2XDefinesParser.StartDefines.Clear;
  pFilename := 'TestProcessString';
  pContents := 'unit Test; {$DEFINE HELLO} end.';

  FD2XDefinesParser.ProcessString(pFilename, pContents);
  CheckDefines('HELLO', 'Current');
end;

procedure TestTD2XDefinesParser.TestStartDefines;
var
  pContents: string;
  pFilename: string;
begin
  FD2XDefinesParser.StartDefines.CommaText := 'HELLO,TEST,VALUE';
  pFilename := 'TestProcessString';
  pContents := 'unit Test; {$UNDEF TEST} end.';

  FD2XDefinesParser.ProcessString(pFilename, pContents);
  CheckDefines('HELLO,VALUE', 'Current');
end;

procedure TestTD2XDefinesParser.TestUsedUnitName;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestUsedUnitName';
  pContents := 'unit Test; interface uses Test1; end.';

  FD2XDefinesParser.ProcessString(pFilename, pContents);

  CheckText('Test1', 'Used Unit Name');
  CheckAttribute('', '', 'Blank');
end;

{ TestTD2XUsesParser }

procedure TestTD2XUsesParser.SetUp;
begin
  FD2XUsesParser := TD2XUsesParser.Create;
  InitParser(FD2XUsesParser);
end;

procedure TestTD2XUsesParser.TearDown;
begin
  FD2XUsesParser.Free;
  FD2XUsesParser := nil;
end;

procedure TestTD2XUsesParser.TestUsedUnitName;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestUsedUnitName';
  pContents := 'unit Test; interface uses Test1; implementation end.';

  FD2XUsesParser.ProcessString(pFilename, pContents);

  CheckText('Test1', 'Used Unit Name');
  CheckAttribute('', '', 'Blank');
end;

procedure TestTD2XUsesParser.TestMainUnitName;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestMainUnitName';
  pContents := 'unit Test; implementation end.';

  FD2XUsesParser.ProcessString(pFilename, pContents);

  CheckText('Test', 'Main Unit Name');
  CheckAttribute('', '', 'Blank');
end;

procedure TestTD2XUsesParser.TestMainUsedUnit;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestMainUsedUnitExpression';
  pContents := 'program Test; uses Test2; begin end.';

  FD2XUsesParser.ProcessString(pFilename, pContents);

  CheckText('Test2', 'Main Used File Name');
  CheckAttribute('', '', 'Blank');
end;

procedure TestTD2XUsesParser.TestMainUsedUnitExpression;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestMainUsedUnitExpression';
  pContents := 'program Test; uses Test2 in ''Test2.pas''; begin end.';

  FD2XUsesParser.ProcessString(pFilename, pContents);

  CheckText('Test2', 'Main Used File Name');
  CheckAttribute('file', '''Test2.pas''', 'Blank');
end;

{ TestTD2XFullParser }

procedure TestTD2XFullParser.SetUp;
begin
  FD2XFullParser := TD2XFullParser.Create;
  InitParser(FD2XFullParser);
end;

procedure TestTD2XFullParser.TearDown;
begin
  FD2XFullParser.Free;
  FD2XFullParser := nil;
end;

procedure TestTD2XFullParser.TestMainUnitName;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestMainUnitName';
  pContents := 'unit Test; implementation end.';

  FD2XFullParser.ProcessString(pFilename, pContents);

  CheckText('Test', 'Used Unit Name');
  CheckAttribute('', '', 'Blank');
end;

procedure TestTD2XFullParser.TestMainUsedUnit;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestMainUsedUnitExpression';
  pContents := 'program Test; uses Test2; begin end.';

  FD2XFullParser.ProcessString(pFilename, pContents);

  CheckText('Test2', 'Main Used File Name');
  CheckAttribute('', '', 'Blank');
end;

procedure TestTD2XFullParser.TestMainUsedUnitExpression;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestMainUsedUnitExpression';
  pContents := 'program Test; uses Test2 in ''Test2.pas''; begin end.';

  FD2XFullParser.ProcessString(pFilename, pContents);

  CheckText('Test2', 'Used Unit Name');
  CheckAttribute('file', '''Test2.pas''', 'Blank');
end;

procedure TestTD2XFullParser.TestUsedUnitName;
var
  pContents: string;
  pFilename: string;
begin
  pFilename := 'TestUsedUnitName';
  pContents := 'unit Test; interface uses Test1; implementation end.';

  FD2XFullParser.ProcessString(pFilename, pContents);

  CheckText('Test1', 'Used Unit Name');
  CheckAttribute('', '', 'Blank');
end;

initialization

// Register any test cases with the test runner
RegisterTests('Parser', [TestTD2XDefinesParser.Suite, TestTD2XUsesParser.Suite,
    TestTD2XFullParser.Suite]);

end.
