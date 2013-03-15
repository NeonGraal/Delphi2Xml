unit Test.Handlers;

interface

uses
  CastaliaPasLexTypes,

  Test.Global,

  D2X.Flag,
  D2X.Global,
  D2X.Handlers,
  D2X.Parser,
  D2X.Tree;

type
  TParserTestCase = class(TLoggerTestCase)
  protected
    fParser: TD2XDefinesParser;
    fActive: ID2XFlag;
    fFlag: TD2XBoolFlag;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestParserHandler = class(TD2XParserHandler)
  protected
    procedure OnParserMessage(const pTyp: TMessageEventType; const pMsg: string;
      pX, pY: Integer); override;
    procedure OnLexerInclude(const pFile: string; pX, pY: Integer); override;

  public
    fInitParserCalled: Boolean;
    fParserMessageCalled: Boolean;
    fLexerIncludeCalled: Boolean;

    procedure InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef); override;

    procedure TestOnParserMessage(const pTyp: TMessageEventType; const pMsg: string;
      pX, pY: Integer);
    procedure TestOnLexerInclude(const pFile: string; pX, pY: Integer);

    procedure TestDoParserMessage(const pTyp: TMessageEventType; const pMsg: string;
      pX, pY: Integer);
    procedure TestDoLexerInclude(const pFile: string; pX, pY: Integer);

    property Parser: TD2XDefinesParser read fParser;

  end;

  TTestCountDefinesUsedHandler = class(TD2XCountDefinesUsedHandler)
  public
    fInitParserCalled: Boolean;

    procedure InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef); override;

    procedure TestOnIf;
    procedure TestOnIfDef;

    procedure TestDefineUsed(pDef: string);
  end;

  TTestErrorHandler = class(TD2XErrorHandler)
  public
    fInitParserCalled: Boolean;

    procedure InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef); override;

    procedure TestParserMessage(const pTyp: TMessageEventType; const pMsg: string;
      pX, pY: Integer);
  end;

  TTestLogHandler = class(TD2XLogHandler)
  public
    fInitParserCalled: Boolean;

    procedure InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef); override;

    procedure TestParserMessage(const pTyp: TMessageEventType; const pMsg: string;
      pX, pY: Integer);
    procedure TestLexerInclude(const pFile: string; pX, pY: Integer);
  end;

  TTestTreeHandler = class(TD2XTreeHandler)
  public
    procedure TestParserMessage(const pTyp: TMessageEventType; const pMsg: string;
      pX, pY: Integer);
    procedure TestLexerInclude(const pFile: string; pX, pY: Integer);

    property TreeDoc: TD2XTreeDoc read fTreeDoc;
    property TreeNode: TD2XTreeNode read fTreeNode;

    property Parser: TD2XDefinesParser read fParser;
    property FinalToken: TD2XFlagRef read fFinalToken;
    property ParseMode: TD2XStringRef read fParseMode;

  end;

implementation

uses
  System.SysUtils;

{ TParserTestCase }

procedure TParserTestCase.SetUp;
begin
  inherited;

  fParser := TD2XDefinesParser.Create;
  fFlag := TD2XBoolFlag.Create;
  fFlag.SetFlag(True);
  fActive := fFlag;
end;

procedure TParserTestCase.TearDown;
begin
  fActive := nil;
  FreeAndNil(fFlag);
  FreeAndNil(fParser);

  inherited;
end;

{ TTestCountDefinesUsedHandler }

procedure TTestCountDefinesUsedHandler.InitParser(pParser: TD2XDefinesParser;
  pActive: TD2XFlagRef);
begin
  inherited;

  fInitParserCalled := True;
end;

procedure TTestCountDefinesUsedHandler.TestDefineUsed(pDef: string);
begin
  DefineUsed(pDef);
end;

procedure TTestCountDefinesUsedHandler.TestOnIf;
begin
  OnIf(nil);
end;

procedure TTestCountDefinesUsedHandler.TestOnIfDef;
begin
  OnIfDef(nil);
end;

{ TTestErrorHandler }

procedure TTestErrorHandler.InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef);
begin
  inherited;

  fInitParserCalled := True;
end;

procedure TTestErrorHandler.TestParserMessage(const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
begin
  OnParserMessage(pTyp, pMsg, pX, pY);
end;

{ TTestLogHandler }

procedure TTestLogHandler.InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef);
begin
  inherited;

  fInitParserCalled := True;
end;

procedure TTestLogHandler.TestLexerInclude(const pFile: string; pX, pY: Integer);
begin
  OnLexerInclude(pFile, pX, pY);
end;

procedure TTestLogHandler.TestParserMessage(const pTyp: TMessageEventType; const pMsg: string;
  pX, pY: Integer);
begin
  OnParserMessage(pTyp, pMsg, pX, pY);
end;

{ TTestTreeHandler }

procedure TTestTreeHandler.TestLexerInclude(const pFile: string; pX, pY: Integer);
begin
  OnLexerInclude(pFile, pX, pY);
end;

procedure TTestTreeHandler.TestParserMessage(const pTyp: TMessageEventType; const pMsg: string;
  pX, pY: Integer);
begin
  OnParserMessage(pTyp, pMsg, pX, pY);
end;

{ TTestParserHandler }

procedure TTestParserHandler.InitParser(pParser: TD2XDefinesParser; pActive: TD2XFlagRef);
begin
  inherited;

  fInitParserCalled := True;
end;

procedure TTestParserHandler.OnLexerInclude(const pFile: string; pX, pY: Integer);
begin
  fLexerIncludeCalled := True;
  inherited;
end;

procedure TTestParserHandler.OnParserMessage(const pTyp: TMessageEventType; const pMsg: string;
  pX, pY: Integer);
begin
  fParserMessageCalled := True;
  inherited;
end;

procedure TTestParserHandler.TestDoLexerInclude;
begin
  DoLexerInclude(nil);
end;

procedure TTestParserHandler.TestDoParserMessage(const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
begin
  DoParserMessage(nil, pTyp, pMsg, pX, pY);
end;

procedure TTestParserHandler.TestOnLexerInclude(const pFile: string; pX, pY: Integer);
begin
  OnLexerInclude(pFile, pX, pY);
end;

procedure TTestParserHandler.TestOnParserMessage(const pTyp: TMessageEventType;
  const pMsg: string; pX, pY: Integer);
begin
  OnParserMessage(pTyp, pMsg, pX, pY);
end;

end.
