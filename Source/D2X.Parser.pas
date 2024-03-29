unit D2X.Parser;

interface

uses
  System.Classes,
  System.Rtti,
  CastaliaPasLex,
  CastaliaPasLexTypes,
  CastaliaSimplePasPar;

type
  TD2XLexer = TmwBasePasLex;
  TD2XParser = TmwSimplePasPar;

  TD2XAddAttributeEvent = reference to procedure(pName, pValue: string);
  TD2XAddTextEvent = reference to procedure(pText: string);
  TD2XProgressEvent = reference to procedure(pProgress: Integer);
  TD2XTrimChildrenEvent = reference to procedure(pElement: string);
  TD2XMethodEvent = reference to procedure(pMethod: string);
  TD2XGetGroupEvent = reference to function(pName: string): string;

  TD2XDefinesParser = class(TD2XParser)
  private
    fLength: Cardinal;
    fProcessed: Byte;
    fLastTokens: string;
    fStartDefines: TStringList;
    fHeldDefines: TStringList;

    FAddAttribute: TD2XAddAttributeEvent;
    FAddText: TD2XAddTextEvent;
    fOnProgress: TD2XProgressEvent;
    fTrimChildren: TD2XTrimChildrenEvent;

    fVMI: TVirtualMethodInterceptor;

    fOnAfterMethod: TD2XMethodEvent;
    fOnBeforeMethod: TD2XMethodEvent;
    fOnGetGroup: TD2XGetGroupEvent;

    function GetStartDefines: TStringList;
    function GetHeldDefines: TStringList;

  protected
    fKeepTokens: Boolean;

    procedure NextToken; override;

    procedure DoAddAttribute(pName, pValue: string); overload;
    procedure DoAddAttribute(pName: string); overload;

    procedure DoAddText(pText: string); overload;
    procedure DoAddText; overload;

    procedure DoTrimChildren(pElement: string);

    procedure HandlePtUndefDirect(Sender: TmwBasePasLex); override;

    procedure ParseFile; override;
    procedure UnitName; override;
    procedure UsedUnitName; override;
    procedure MainUsedUnitExpression; override;
    procedure ContainsExpression; override;

  public
    constructor Create;
    destructor Destroy; override;

    procedure ContainsIdentifier; override;
    procedure ContainsStatement; override;
    procedure MainUnitName; override;

    procedure GetLexerDefines(pDefs: TStringList);

    procedure ProcessString(pFilename, pContents: string);

    property LastTokens: string read fLastTokens write fLastTokens;
    property KeepTokens: Boolean read fKeepTokens;

    property StartDefines: TStringList read GetStartDefines;
    property HeldDefines: TStringList read GetHeldDefines;

    property OnProgress: TD2XProgressEvent read fOnProgress write fOnProgress;
    property OnGetGroup: TD2XGetGroupEvent read fOnGetGroup write fOnGetGroup;

    property AddAttribute: TD2XAddAttributeEvent read FAddAttribute write FAddAttribute;
    property AddText: TD2XAddTextEvent read FAddText write FAddText;
    property TrimChildren: TD2XTrimChildrenEvent read fTrimChildren write fTrimChildren;

    property OnBeforeMethod: TD2XMethodEvent read fOnBeforeMethod write fOnBeforeMethod;
    property OnAfterMethod: TD2XMethodEvent read fOnAfterMethod write fOnAfterMethod;
  end;

  TD2XDefinesParserClass = class of TD2XDefinesParser;

  TD2XUsesParser = class(TD2XDefinesParser)
  private
    fCurrentSection: string;

    procedure ReadToFileEnd;
    procedure ReadTo(pSym: TptTokenKind);

  protected
    procedure ProgramBlock; override;

  public
    procedure ProgramFile; override;
    procedure PackageFile; override;
    procedure UnitFile; override;

    procedure InterfaceSection; override;
    procedure ImplementationSection; override;
    procedure MainUnitName; override;
    procedure MainUsedUnitStatement; override;
    procedure UsedUnitName; override;
    procedure ContainsExpression; override;

    property CurrentSection: string read fCurrentSection;
  end;

  TD2XFullParser = class(TD2XDefinesParser)
  protected
    procedure ConstantName; override;
    procedure ConstructorName; override;
    procedure DestructorName; override;
    procedure FieldName; override;
    procedure FunctionMethodName; override;
    procedure FunctionProcedureName; override;
    procedure MainUnitName; override;
    procedure ParameterName; override;
    procedure ProcedureMethodName; override;
    procedure PropertyName; override;
    procedure ResolutionInterfaceName; override;
    procedure TagFieldName; override;
    procedure TagFieldTypeName; override;
    procedure TypeName; override;
    procedure VarName; override; // !! Added VarName and VarNameList back in...

  public
    procedure AccessSpecifier; override;
    procedure AdditiveOperator; override;
    procedure AncestorIdList; override; // !! Added ancestorIdList back in...
    procedure AncestorId; override; // !! Added ancestorId back in...
    procedure AnonymousMethod; override;
    procedure AnonymousMethodType; override;
    procedure ArrayConstant; override;
    procedure ArrayType; override;
    procedure AsmStatement; override;
    procedure Block; override;
    procedure CaseLabel; override;
    procedure CaseSelector; override;
    procedure CaseStatement; override;
    procedure CharString; override;
    procedure ClassField; override;
    procedure ClassForward; override;
    procedure ClassFunctionHeading; override;
    procedure ClassHeritage; override;
    procedure ClassMemberList; override;
    procedure ClassMethodDirective; override;
    procedure ClassMethodHeading; override;
    procedure ClassMethodOrProperty; override;
    procedure ClassMethodResolution; override;
    procedure ClassProcedureHeading; override;
    procedure ClassClass; override;
    procedure ClassProperty; override;
    procedure ClassReferenceType; override;
    procedure ClassType; override;
    procedure ClassTypeEnd; override; // DR 2001-07-31
    procedure ClassVisibility; override;
    procedure CompoundStatement; override;
    procedure ConstantColon; override;
    procedure ConstantDeclaration; override;
    procedure ConstantEqual; override;
    procedure ConstantExpression; override;
    // JR added constant type
    procedure ConstantType; override;
    procedure ConstantValue; override;
    procedure ConstantValueTyped; override;
    procedure ConstParameter; override;
    procedure ConstructorHeading; override;
    procedure ConstSection; override;
    procedure ContainsExpression; override;
    procedure ContainsIdentifier; override;
    procedure ContainsStatement; override;
{$IFDEF D8_NEWER}
    procedure CustomAttribute; override; // JThurman 2004-03-03
{$ENDIF}
    procedure DeclarationSection; override;
    procedure Designator; override;
    procedure DestructorHeading; override;
    procedure Directive16Bit; override;
    procedure DirectiveBinding; override;
    procedure DirectiveCalling; override;
    procedure DirectiveDeprecated; override; // DR 2001-10-20
    procedure DirectiveLibrary; override; // DR 2001-10-20
    procedure DirectiveLocal; override; // DR 2001-11-14
    procedure DirectivePlatform; override; // DR 2001-10-20
    procedure DirectiveVarargs; override; // DR 2001-11-14
    procedure DispInterfaceForward; override;
    procedure DispIDSpecifier; override; // DR 2001-07-26
    procedure EmptyStatement; override;
    procedure EnumeratedType; override;
    procedure EnumeratedTypeItem; override; // DR 2001-10-29
    procedure ExceptBlock; override;
    procedure ExceptionBlockElseBranch; override;
    procedure ExceptionClassTypeIdentifier; override;
    procedure ExceptionHandler; override;
    procedure ExceptionHandlerList; override;
    procedure ExceptionIdentifier; override;
    procedure ExceptionVariable; override;
    procedure ExplicitType; override; // !! changed spelling to "Explicit"
    procedure ExportedHeading; override;
    procedure ExportsElement; override;
    procedure Expression; override;
    procedure ExpressionList; override;
    procedure ExternalDirective; override;
    procedure ExternalDirectiveThree; override;
    procedure ExternalDirectiveTwo; override;
    procedure Factor; override;
    procedure FieldDeclaration; override;
    procedure FieldList; override;
    procedure FieldNameList; override;
    procedure FileType; override;
    procedure FormalParameterList; override;
    procedure FormalParameterSection; override;
    procedure ForStatement; override;
    procedure ForwardDeclaration; override; { GLC: corrected spelling }
    procedure FunctionHeading; override;
    procedure FunctionMethodDeclaration; override;
    procedure FunctionProcedureBlock; override;
    procedure Identifier; override;
    procedure IdentifierList; override;
    procedure IfStatement; override;
    procedure ImplementationSection; override;
    procedure IncludeFile; override;
    procedure IndexSpecifier; override; // DR 2001-07-26
    procedure InheritedStatement; override;
    procedure InitializationSection; override;
    procedure InlineStatement; override;
    procedure InParameter; override;
    procedure InterfaceDeclaration; override;
    procedure InterfaceForward; override;
    procedure InterfaceGUID; override;
    procedure InterfaceHeritage; override;
    procedure InterfaceMemberList; override;
    procedure InterfaceSection; override;
    procedure InterfaceType; override;
    procedure LabelDeclarationSection; override;
    procedure LabeledStatement; override;
    procedure LabelId; override;
    procedure LibraryFile; override;
    procedure MainUsedUnitStatement; override;
    procedure MultiplicativeOperator; override;
    procedure NewFormalParameterType; override;
    procedure Number; override;
    procedure ObjectConstructorHeading; override;
    procedure ObjectDestructorHeading; override;
    procedure ObjectField; override;
    procedure ObjectForward; override;
    procedure ObjectFunctionHeading; override;
    procedure ObjectHeritage; override;
    procedure ObjectMemberList; override;
    procedure ObjectMethodDirective; override;
    procedure ObjectMethodHeading; override;
    procedure ObjectNameOfMethod; override;
    procedure ObjectProperty; override;
    procedure ObjectPropertySpecifiers; override;
    procedure ObjectProcedureHeading; override;
    procedure ObjectType; override;
    procedure ObjectTypeEnd; override; // DR 2001-08-07
    procedure ObjectVisibility; override;
    procedure OldFormalParameterType; override;
    procedure OrdinalIdentifier; override;
    procedure OrdinalType; override;
    procedure OutParameter; override;
    procedure PackageFile; override;
    procedure ParameterFormal; override;
    procedure ParameterNameList; override;
    procedure PointerType; override;
    procedure ProceduralDirective; override;
    procedure ProceduralType; override;
    procedure ProcedureDeclarationSection; override;
    procedure ProcedureHeading; override;
    procedure ProcedureMethodDeclaration; override;
    procedure ProgramBlock; override;
    procedure ProgramFile; override;
    procedure PropertyDefault; override;
    procedure PropertyInterface; override;
    procedure PropertyParameterConst; override;
    procedure PropertyParameterList; override;
    procedure PropertySpecifiers; override;
    procedure QualifiedIdentifier; override;
    procedure QualifiedIdentifierList; override;
    procedure RaiseStatement; override;
    procedure ReadAccessIdentifier; override;
    procedure RealIdentifier; override;
    procedure RealType; override;
    procedure RecordConstant; override;
    procedure RecordFieldConstant; override;
    procedure RecordType; override;
    procedure RecordVariant; override;
    procedure RelativeOperator; override;
    procedure RepeatStatement; override;
    procedure RequiresIdentifier; override;
    procedure ResourceDeclaration; override;
    procedure ReturnType; override;
    procedure SetConstructor; override;
    procedure SetElement; override;
    procedure SetType; override;
    procedure SimpleExpression; override;
    procedure SimpleStatement; override;
    procedure SimpleType; override;
    procedure SkipAnsiComment; override;
    procedure SkipBorComment; override;
    procedure SkipSlashesComment; override;
    procedure SkipSpace; override; // XM Jul-2000
    procedure SkipCRLFco; override; // XM Jul-2000
    procedure SkipCRLF; override; // XM Jul-2000
    procedure Statement; override;
    procedure StatementList; override;
    procedure StorageExpression; override;
    procedure StorageIdentifier; override;
    procedure StorageDefault; override;
    procedure StorageNoDefault; override;
    procedure StorageSpecifier; override;
    procedure StorageStored; override;
    procedure StringIdentifier; override;
    procedure StringStatement; override;
    procedure StringType; override;
    procedure StructuredType; override;
    procedure SubrangeType; override;
    procedure TagField; override;
    procedure Term; override;
    procedure TryStatement; override;
    procedure TypedConstant; override;
    procedure TypeDeclaration; override;
    procedure TypeId; override;
    procedure TypeKind; override;
    // generics
    procedure TypeArgs; override;
    procedure TypeParams; override;
    procedure TypeParamDecl; override;
    procedure TypeParamDeclList; override;
    procedure TypeParamList; override;
    procedure ConstraintList; override;
    procedure Constraint; override;
    // end generics
    procedure TypeSection; override;
    procedure UnitFile; override;
    procedure UnitId; override;
    procedure UsedUnitsList; override;
    procedure UsedUnitName; override;
    procedure VarAbsolute; override;
    procedure VarEqual; override;
    procedure VarDeclaration; override;
    procedure Variable; override;
    procedure VariableList; override;
    procedure VariableReference; override;
    procedure VariableTwo; override;
    procedure VariantIdentifier; override;
    procedure VariantSection; override;
    procedure VarParameter; override;
    procedure VarNameList; override;
    procedure VarSection; override;
    procedure VisibilityAutomated; override;
    procedure VisibilityPrivate; override;
    procedure VisibilityProtected; override;
    procedure VisibilityPublic; override;
    procedure VisibilityPublished; override;
    procedure VisibilityUnknown; override;
    procedure WhileStatement; override;
    procedure WithStatement; override;
    procedure WriteAccessIdentifier; override;
  end;

implementation

uses
  System.SysUtils;

{ TD2XParser }

procedure TD2XFullParser.AccessSpecifier;
begin
  inherited;
end;

procedure TD2XFullParser.AdditiveOperator;
begin
  inherited;
end;

procedure TD2XFullParser.AncestorId;
begin
  inherited;
end;

procedure TD2XFullParser.AncestorIdList;
begin
  inherited;
end;

procedure TD2XFullParser.AnonymousMethod;
begin
  inherited;
end;

procedure TD2XFullParser.AnonymousMethodType;
begin
  inherited;
end;

procedure TD2XFullParser.ArrayConstant;
begin
  inherited;
end;

procedure TD2XFullParser.ArrayType;
begin
  inherited;
end;

procedure TD2XFullParser.AsmStatement;
begin
  inherited;
end;

procedure TD2XFullParser.Block;
begin
  inherited;
end;

procedure TD2XFullParser.CaseLabel;
begin
  inherited;
end;

procedure TD2XFullParser.CaseSelector;
begin
  inherited;
end;

procedure TD2XFullParser.CaseStatement;
begin
  inherited;
end;

procedure TD2XFullParser.CharString;
begin
  inherited;
end;

procedure TD2XFullParser.ClassClass;
begin
  inherited;
end;

procedure TD2XFullParser.ClassField;
begin
  inherited;
end;

procedure TD2XFullParser.ClassForward;
begin
  inherited;
end;

procedure TD2XFullParser.ClassFunctionHeading;
begin
  inherited;
end;

procedure TD2XFullParser.ClassHeritage;
begin
  inherited;
end;

procedure TD2XFullParser.ClassMemberList;
begin
  inherited;
end;

procedure TD2XFullParser.ClassMethodDirective;
begin
  inherited;
end;

procedure TD2XFullParser.ClassMethodHeading;
begin
  inherited;
end;

procedure TD2XFullParser.ClassMethodOrProperty;
begin
  inherited;
end;

procedure TD2XFullParser.ClassMethodResolution;
begin
  inherited;
end;

procedure TD2XFullParser.ClassProcedureHeading;
begin
  inherited;
end;

procedure TD2XFullParser.ClassProperty;
begin
  inherited;
end;

procedure TD2XFullParser.ClassReferenceType;
begin
  inherited;
end;

procedure TD2XFullParser.ClassType;
begin
  inherited;
end;

procedure TD2XFullParser.ClassTypeEnd;
begin
  inherited;
end;

procedure TD2XFullParser.ClassVisibility;
begin
  inherited;
end;

procedure TD2XFullParser.CompoundStatement;
begin
  inherited;
end;

procedure TD2XFullParser.ConstantColon;
begin
  inherited;
end;

procedure TD2XFullParser.ConstantDeclaration;
begin
  inherited;
end;

procedure TD2XFullParser.ConstantEqual;
begin
  inherited;
end;

procedure TD2XFullParser.ConstantExpression;
begin
  inherited;
end;

procedure TD2XFullParser.ConstantName;
begin
  if not fKeepTokens then
    try
      fLastTokens := '';
      fKeepTokens := true;

      inherited;

      DoAddAttribute('name');
    finally
      fKeepTokens := False;
    end
  else
    inherited;
end;

procedure TD2XFullParser.ConstantType;
begin
  inherited;
end;

procedure TD2XFullParser.ConstantValue;
begin
  inherited;
end;

procedure TD2XFullParser.ConstantValueTyped;
begin
  inherited;
end;

procedure TD2XFullParser.ConstParameter;
begin
  inherited;
end;

procedure TD2XFullParser.Constraint;
begin
  inherited;
end;

procedure TD2XFullParser.ConstraintList;
begin
  inherited;
end;

procedure TD2XFullParser.ConstructorHeading;
begin
  inherited;
end;

procedure TD2XFullParser.ConstructorName;
begin
  if not fKeepTokens then
    try
      fLastTokens := '';
      fKeepTokens := true;

      inherited;

      DoAddAttribute('name');
    finally
      fKeepTokens := False;
    end
  else
    inherited;
end;

procedure TD2XFullParser.ConstSection;
begin
  inherited;
end;

procedure TD2XFullParser.ContainsExpression;
begin
  inherited;
end;

procedure TD2XFullParser.ContainsIdentifier;
begin
  inherited;
end;

procedure TD2XFullParser.ContainsStatement;
begin
  inherited;
end;

procedure TD2XFullParser.DeclarationSection;
begin
  inherited;
end;

procedure TD2XFullParser.Designator;
begin
  inherited;
end;

procedure TD2XFullParser.DestructorHeading;
begin
  inherited;
end;

procedure TD2XFullParser.DestructorName;
begin
  if not fKeepTokens then
    try
      fLastTokens := '';
      fKeepTokens := true;

      inherited;

      DoAddAttribute('name');
    finally
      fKeepTokens := False;
    end
  else
    inherited;
end;

procedure TD2XFullParser.Directive16Bit;
begin
  inherited;
end;

procedure TD2XFullParser.DirectiveBinding;
begin
  inherited;
end;

procedure TD2XFullParser.DirectiveCalling;
begin
  inherited;
end;

procedure TD2XFullParser.DirectiveDeprecated;
begin
  inherited;
end;

procedure TD2XFullParser.DirectiveLibrary;
begin
  inherited;
end;

procedure TD2XFullParser.DirectiveLocal;
begin
  inherited;
end;

procedure TD2XFullParser.DirectivePlatform;
begin
  inherited;
end;

procedure TD2XFullParser.DirectiveVarargs;
begin
  inherited;
end;

procedure TD2XFullParser.DispIDSpecifier;
begin
  inherited;
end;

procedure TD2XFullParser.DispInterfaceForward;
begin
  inherited;
end;

procedure TD2XFullParser.EmptyStatement;
begin
  inherited;
end;

procedure TD2XFullParser.EnumeratedType;
begin
  inherited;
end;

procedure TD2XFullParser.EnumeratedTypeItem;
begin
  inherited;
end;

procedure TD2XFullParser.ExceptBlock;
begin
  inherited;
end;

procedure TD2XFullParser.ExceptionBlockElseBranch;
begin
  inherited;
end;

procedure TD2XFullParser.ExceptionClassTypeIdentifier;
begin
  inherited;
end;

procedure TD2XFullParser.ExceptionHandler;
begin
  inherited;
end;

procedure TD2XFullParser.ExceptionHandlerList;
begin
  inherited;
end;

procedure TD2XFullParser.ExceptionIdentifier;
begin
  inherited;
end;

procedure TD2XFullParser.ExceptionVariable;
begin
  inherited;
end;

procedure TD2XFullParser.ExplicitType;
begin
  inherited;
end;

procedure TD2XFullParser.ExportedHeading;
begin
  inherited;
end;

procedure TD2XFullParser.ExportsElement;
begin
  inherited;
end;

procedure TD2XFullParser.Expression;
begin
  inherited;
end;

procedure TD2XFullParser.ExpressionList;
begin
  inherited;
end;

procedure TD2XFullParser.ExternalDirective;
begin
  inherited;
end;

procedure TD2XFullParser.ExternalDirectiveThree;
begin
  inherited;
end;

procedure TD2XFullParser.ExternalDirectiveTwo;
begin
  inherited;
end;

procedure TD2XFullParser.Factor;
begin
  inherited;
end;

procedure TD2XFullParser.FieldDeclaration;
begin
  inherited;
end;

procedure TD2XFullParser.FieldList;
begin
  inherited;
end;

procedure TD2XFullParser.FieldName;
begin
  if not fKeepTokens then
    try
      fLastTokens := '';
      fKeepTokens := true;

      inherited;

      DoAddAttribute('name');
    finally
      fKeepTokens := False;
    end
  else
    inherited;
end;

procedure TD2XFullParser.FieldNameList;
begin
  inherited;
end;

procedure TD2XFullParser.FileType;
begin
  inherited;
end;

procedure TD2XFullParser.FormalParameterList;
begin
  inherited;
end;

procedure TD2XFullParser.FormalParameterSection;
begin
  inherited;
end;

procedure TD2XFullParser.ForStatement;
begin
  inherited;
end;

procedure TD2XFullParser.ForwardDeclaration;
begin
  inherited;
end;

procedure TD2XFullParser.FunctionHeading;
begin
  inherited;
end;

procedure TD2XFullParser.FunctionMethodDeclaration;
begin
  inherited;
end;

procedure TD2XFullParser.FunctionMethodName;
begin
  if not fKeepTokens then
    try
      fLastTokens := '';
      fKeepTokens := true;

      inherited;

      DoAddAttribute('name');
    finally
      fKeepTokens := False;
    end
  else
    inherited;
end;

procedure TD2XFullParser.FunctionProcedureBlock;
begin
  inherited;
end;

procedure TD2XFullParser.FunctionProcedureName;
begin
  if not fKeepTokens then
    try
      fLastTokens := '';
      fKeepTokens := true;

      inherited;

      DoAddAttribute('name');
    finally
      fKeepTokens := False;
    end
  else
    inherited;
end;

procedure TD2XFullParser.Identifier;
begin
  inherited;
end;

procedure TD2XFullParser.IdentifierList;
begin
  inherited;
end;

procedure TD2XFullParser.IfStatement;
begin
  inherited;
end;

procedure TD2XFullParser.ImplementationSection;
begin
  inherited;
end;

procedure TD2XFullParser.IncludeFile;
begin
  inherited;
end;

procedure TD2XFullParser.IndexSpecifier;
begin
  inherited;
end;

procedure TD2XFullParser.InheritedStatement;
begin
  inherited;
end;

procedure TD2XFullParser.InitializationSection;
begin
  inherited;
end;

procedure TD2XFullParser.InlineStatement;
begin
  inherited;
end;

procedure TD2XFullParser.InParameter;
begin
  inherited;
end;

procedure TD2XFullParser.InterfaceDeclaration;
begin
  inherited;
end;

procedure TD2XFullParser.InterfaceForward;
begin
  inherited;
end;

procedure TD2XFullParser.InterfaceGUID;
begin
  inherited;
end;

procedure TD2XFullParser.InterfaceHeritage;
begin
  inherited;
end;

procedure TD2XFullParser.InterfaceMemberList;
begin
  inherited;
end;

procedure TD2XFullParser.InterfaceSection;
begin
  inherited;
end;

procedure TD2XFullParser.InterfaceType;
begin
  inherited;
end;

procedure TD2XFullParser.LabelDeclarationSection;
begin
  inherited;
end;

procedure TD2XFullParser.LabeledStatement;
begin
  inherited;
end;

procedure TD2XFullParser.LabelId;
begin
  inherited;
end;

procedure TD2XFullParser.LibraryFile;
begin
  inherited;
end;

procedure TD2XFullParser.MainUnitName;
begin
  if not fKeepTokens then
    try
      fLastTokens := '';
      fKeepTokens := true;

      inherited;

      DoAddAttribute('name');
    finally
      fKeepTokens := False;
    end
  else
    inherited;
end;

procedure TD2XFullParser.MainUsedUnitStatement;
begin
  inherited;
end;

procedure TD2XFullParser.MultiplicativeOperator;
begin
  inherited;
end;

procedure TD2XFullParser.NewFormalParameterType;
begin
  inherited;
end;

procedure TD2XFullParser.Number;
begin
  inherited;
end;

procedure TD2XFullParser.ObjectConstructorHeading;
begin
  inherited;
end;

procedure TD2XFullParser.ObjectDestructorHeading;
begin
  inherited;
end;

procedure TD2XFullParser.ObjectField;
begin
  inherited;
end;

procedure TD2XFullParser.ObjectForward;
begin
  inherited;
end;

procedure TD2XFullParser.ObjectFunctionHeading;
begin
  inherited;
end;

procedure TD2XFullParser.ObjectHeritage;
begin
  inherited;
end;

procedure TD2XFullParser.ObjectMemberList;
begin
  inherited;
end;

procedure TD2XFullParser.ObjectMethodDirective;
begin
  inherited;
end;

procedure TD2XFullParser.ObjectMethodHeading;
begin
  inherited;
end;

procedure TD2XFullParser.ObjectNameOfMethod;
begin
  inherited;
end;

procedure TD2XFullParser.ObjectProcedureHeading;
begin
  inherited;
end;

procedure TD2XFullParser.ObjectProperty;
begin
  inherited;
end;

procedure TD2XFullParser.ObjectPropertySpecifiers;
begin
  inherited;
end;

procedure TD2XFullParser.ObjectType;
begin
  inherited;
end;

procedure TD2XFullParser.ObjectTypeEnd;
begin
  inherited;
end;

procedure TD2XFullParser.ObjectVisibility;
begin
  inherited;
end;

procedure TD2XFullParser.OldFormalParameterType;
begin
  inherited;
end;

procedure TD2XFullParser.OrdinalIdentifier;
begin
  inherited;
end;

procedure TD2XFullParser.OrdinalType;
begin
  inherited;
end;

procedure TD2XFullParser.OutParameter;
begin
  inherited;
end;

procedure TD2XFullParser.PackageFile;
begin
  inherited;
end;

procedure TD2XFullParser.ParameterFormal;
begin
  inherited;
end;

procedure TD2XFullParser.ParameterName;
begin
  if not fKeepTokens then
    try
      fLastTokens := '';
      fKeepTokens := true;

      inherited;

      DoAddAttribute('name');
    finally
      fKeepTokens := False;
    end
  else
    inherited;
end;

procedure TD2XFullParser.ParameterNameList;
begin
  inherited;
end;

procedure TD2XFullParser.PointerType;
begin
  inherited;
end;

procedure TD2XFullParser.ProceduralDirective;
begin
  inherited;
end;

procedure TD2XFullParser.ProceduralType;
begin
  inherited;
end;

procedure TD2XFullParser.ProcedureDeclarationSection;
begin
  inherited;
end;

procedure TD2XFullParser.ProcedureHeading;
begin
  inherited;
end;

procedure TD2XFullParser.ProcedureMethodDeclaration;
begin
  inherited;
end;

procedure TD2XFullParser.ProcedureMethodName;
begin
  if not fKeepTokens then
    try
      fLastTokens := '';
      fKeepTokens := true;

      inherited;

      DoAddAttribute('name');
    finally
      fKeepTokens := False;
    end
  else
    inherited;
end;

procedure TD2XFullParser.ProgramBlock;
begin
  inherited;
end;

procedure TD2XFullParser.ProgramFile;
begin
  inherited;
end;

procedure TD2XFullParser.PropertyDefault;
begin
  inherited;
end;

procedure TD2XFullParser.PropertyInterface;
begin
  inherited;
end;

procedure TD2XFullParser.PropertyName;
begin
  if not fKeepTokens then
    try
      fLastTokens := '';
      fKeepTokens := true;

      inherited;

      DoAddAttribute('name');
    finally
      fKeepTokens := False;
    end
  else
    inherited;
end;

procedure TD2XFullParser.PropertyParameterConst;
begin
  inherited;
end;

procedure TD2XFullParser.PropertyParameterList;
begin
  inherited;
end;

procedure TD2XFullParser.PropertySpecifiers;
begin
  inherited;
end;

procedure TD2XFullParser.QualifiedIdentifier;
begin
  inherited;
end;

procedure TD2XFullParser.QualifiedIdentifierList;
begin
  inherited;
end;

procedure TD2XFullParser.RaiseStatement;
begin
  inherited;
end;

procedure TD2XFullParser.ReadAccessIdentifier;
begin
  inherited;
end;

procedure TD2XFullParser.RealIdentifier;
begin
  inherited;
end;

procedure TD2XFullParser.RealType;
begin
  inherited;
end;

procedure TD2XFullParser.RecordConstant;
begin
  inherited;
end;

procedure TD2XFullParser.RecordFieldConstant;
begin
  inherited;
end;

procedure TD2XFullParser.RecordType;
begin
  inherited;
end;

procedure TD2XFullParser.RecordVariant;
begin
  inherited;
end;

procedure TD2XFullParser.RelativeOperator;
begin
  inherited;
end;

procedure TD2XFullParser.RepeatStatement;
begin
  inherited;
end;

procedure TD2XFullParser.RequiresIdentifier;
begin
  inherited;
end;

procedure TD2XFullParser.ResolutionInterfaceName;
begin
  if not fKeepTokens then
    try
      fLastTokens := '';
      fKeepTokens := true;

      inherited;

      DoAddAttribute('name');
    finally
      fKeepTokens := False;
    end
  else
    inherited;
end;

procedure TD2XFullParser.ResourceDeclaration;
begin
  inherited;
end;

procedure TD2XFullParser.ReturnType;
begin
  inherited;
end;

procedure TD2XFullParser.SetConstructor;
begin
  inherited;
end;

procedure TD2XFullParser.SetElement;
begin
  inherited;
end;

procedure TD2XFullParser.SetType;
begin
  inherited;
end;

procedure TD2XFullParser.SimpleExpression;
begin
  inherited;
end;

procedure TD2XFullParser.SimpleStatement;
begin
  inherited;
end;

procedure TD2XFullParser.SimpleType;
begin
  inherited;
end;

procedure TD2XFullParser.SkipAnsiComment;
begin
  inherited;
end;

procedure TD2XFullParser.SkipBorComment;
begin
  inherited;
end;

procedure TD2XFullParser.SkipCRLF;
begin
  inherited;
end;

procedure TD2XFullParser.SkipCRLFco;
begin
  inherited;
end;

procedure TD2XFullParser.SkipSlashesComment;
begin
  inherited;
end;

procedure TD2XFullParser.SkipSpace;
begin
  inherited;
end;

procedure TD2XFullParser.Statement;
begin
  inherited;
end;

procedure TD2XFullParser.StatementList;
begin
  inherited;
end;

procedure TD2XFullParser.StorageDefault;
begin
  inherited;
end;

procedure TD2XFullParser.StorageExpression;
begin
  inherited;
end;

procedure TD2XFullParser.StorageIdentifier;
begin
  inherited;
end;

procedure TD2XFullParser.StorageNoDefault;
begin
  inherited;
end;

procedure TD2XFullParser.StorageSpecifier;
begin
  inherited;
end;

procedure TD2XFullParser.StorageStored;
begin
  inherited;
end;

procedure TD2XFullParser.StringIdentifier;
begin
  inherited;
end;

procedure TD2XFullParser.StringStatement;
begin
  inherited;
end;

procedure TD2XFullParser.StringType;
begin
  inherited;
end;

procedure TD2XFullParser.StructuredType;
begin
  inherited;
end;

procedure TD2XFullParser.SubrangeType;
begin
  inherited;
end;

procedure TD2XFullParser.TagField;
begin
  inherited;
end;

procedure TD2XFullParser.TagFieldName;
begin
  if not fKeepTokens then
    try
      fLastTokens := '';
      fKeepTokens := true;

      inherited;

      DoAddAttribute('name');
    finally
      fKeepTokens := False;
    end
  else
    inherited;
end;

procedure TD2XFullParser.TagFieldTypeName;
begin
  if not fKeepTokens then
    try
      fLastTokens := '';
      fKeepTokens := true;

      inherited;

      DoAddAttribute('name');
    finally
      fKeepTokens := False;
    end
  else
    inherited;
end;

procedure TD2XFullParser.Term;
begin
  inherited;
end;

procedure TD2XFullParser.TryStatement;
begin
  inherited;
end;

procedure TD2XFullParser.TypeArgs;
begin
  inherited;
end;

procedure TD2XFullParser.TypedConstant;
begin
  inherited;
end;

procedure TD2XFullParser.TypeDeclaration;
begin
  inherited;
end;

procedure TD2XFullParser.TypeId;
begin
  inherited;
end;

procedure TD2XFullParser.TypeKind;
begin
  inherited;
end;

procedure TD2XFullParser.TypeName;
begin
  if not fKeepTokens then
    try
      fLastTokens := '';
      fKeepTokens := true;

      inherited;

      DoAddAttribute('name');
    finally
      fKeepTokens := False;
    end
  else
    inherited;
end;

procedure TD2XFullParser.TypeParamDecl;
begin
  inherited;
end;

procedure TD2XFullParser.TypeParamDeclList;
begin
  inherited;
end;

procedure TD2XFullParser.TypeParamList;
begin
  inherited;
end;

procedure TD2XFullParser.TypeParams;
begin
  inherited;
end;

procedure TD2XFullParser.TypeSection;
begin
  inherited;
end;

procedure TD2XFullParser.UnitFile;
begin
  inherited;
end;

procedure TD2XFullParser.UnitId;
begin
  inherited;
end;

procedure TD2XFullParser.UsedUnitName;
begin
  inherited;
end;

procedure TD2XFullParser.UsedUnitsList;
begin
  inherited;
end;

procedure TD2XFullParser.VarAbsolute;
begin
  inherited;
end;

procedure TD2XFullParser.VarDeclaration;
begin
  inherited;
end;

procedure TD2XFullParser.VarEqual;
begin
  inherited;
end;

procedure TD2XFullParser.Variable;
begin
  inherited;
end;

procedure TD2XFullParser.VariableList;
begin
  inherited;
end;

procedure TD2XFullParser.VariableReference;
begin
  inherited;
end;

procedure TD2XFullParser.VariableTwo;
begin
  inherited;
end;

procedure TD2XFullParser.VariantIdentifier;
begin
  inherited;
end;

procedure TD2XFullParser.VariantSection;
begin
  inherited;
end;

procedure TD2XFullParser.VarName;
begin
  if not fKeepTokens then
    try
      fLastTokens := '';
      fKeepTokens := true;

      inherited;

      DoAddAttribute('name');
    finally
      fKeepTokens := False;
    end
  else
    inherited;
end;

procedure TD2XFullParser.VarNameList;
begin
  inherited;
end;

procedure TD2XFullParser.VarParameter;
begin
  inherited;
end;

procedure TD2XFullParser.VarSection;
begin
  inherited;
end;

procedure TD2XFullParser.VisibilityAutomated;
begin
  inherited;
end;

procedure TD2XFullParser.VisibilityPrivate;
begin
  inherited;
end;

procedure TD2XFullParser.VisibilityProtected;
begin
  inherited;
end;

procedure TD2XFullParser.VisibilityPublic;
begin
  inherited;
end;

procedure TD2XFullParser.VisibilityPublished;
begin
  inherited;
end;

procedure TD2XFullParser.VisibilityUnknown;
begin
  inherited;
end;

procedure TD2XFullParser.WhileStatement;
begin
  inherited;
end;

procedure TD2XFullParser.WithStatement;
begin
  inherited;
end;

procedure TD2XFullParser.WriteAccessIdentifier;
begin
  inherited;
end;

{ TD2XDefinesParser }

procedure TD2XDefinesParser.ContainsExpression;
begin
  if not fKeepTokens then
    try
      fLastTokens := '';
      fKeepTokens := true;
      inherited;
      DoAddAttribute('file');
      DoTrimChildren('ConstantExpression');
    finally
      fKeepTokens := False;
    end
  else
    inherited;
end;

procedure TD2XDefinesParser.ContainsIdentifier;
begin
  fLastTokens := '';
  inherited;
end;

procedure TD2XDefinesParser.ContainsStatement;
begin
  inherited;
end;

constructor TD2XDefinesParser.Create;
begin
  inherited;

  fStartDefines := nil;
  fHeldDefines := nil;
  fLength := 0;
  fProcessed := 0;
  fOnProgress := nil;
  fKeepTokens := False;

  fVMI := TVirtualMethodInterceptor.Create(TObject(Self).ClassType);
  fVMI.Proxify(Self);

  fVMI.OnBefore :=
      procedure(pInst: TObject; pMethod: TRttiMethod; const pArgs: TArray<TValue>;
      out pDoInvoke: Boolean; out pResult: TValue)
    begin
      pDoInvoke := true;
      if Assigned(fOnBeforeMethod) then
        fOnBeforeMethod(pMethod.Name);
    end;
  fVMI.OnAfter :=
      procedure(pInst: TObject; pMethod: TRttiMethod; const pArgs: TArray<TValue>;
      var pResult: TValue)
    begin
      if Assigned(fOnAfterMethod) then
        fOnAfterMethod(pMethod.Name);
    end;
end;

destructor TD2XDefinesParser.Destroy;
begin
  fVMI.Unproxify(Self);
  FreeAndNil(fVMI);

  FAddAttribute := nil;
  FAddText := nil;
  fOnProgress := nil;
  fTrimChildren := nil;

  FreeAndNil(fHeldDefines);
  FreeAndNil(fStartDefines);

  inherited;
end;

procedure TD2XDefinesParser.DoAddAttribute(pName: string);
begin
  DoAddAttribute(pName, fLastTokens);
  fLastTokens := '';
end;

procedure TD2XDefinesParser.DoAddText(pText: string);
begin
  if Assigned(FAddText) and (pText > '') then
    FAddText(pText);
end;

procedure TD2XDefinesParser.DoAddText;
begin
  DoAddText(fLastTokens);
  fLastTokens := '';
end;

procedure TD2XDefinesParser.DoTrimChildren(pElement: string);
begin
  if Assigned(fTrimChildren) then
    fTrimChildren(pElement);
end;

procedure TD2XDefinesParser.DoAddAttribute(pName, pValue: string);
begin
  if Assigned(FAddAttribute) and (pValue > '') then
    FAddAttribute(pName, pValue);
end;

function TD2XDefinesParser.GetHeldDefines: TStringList;
begin
  if not Assigned(fHeldDefines) then
  begin
    fHeldDefines := TStringList.Create;
    fHeldDefines.Sorted := true;
  end;

  Result := fHeldDefines;
end;

procedure TD2XDefinesParser.GetLexerDefines(pDefs: TStringList);
begin
  Lexer.GetDefines(pDefs);
  pDefs.Sorted := true;
end;

function TD2XDefinesParser.GetStartDefines: TStringList;
begin
  if not Assigned(fStartDefines) then
  begin
    fStartDefines := TStringList.Create;
    fStartDefines.Sorted := true;
  end;

  Result := fStartDefines;
end;

procedure TD2XDefinesParser.HandlePtUndefDirect(Sender: TmwBasePasLex);
var
  lS: string;
begin
  if Assigned(fHeldDefines) then
  begin
    lS := UpperCase(Lexer.DirectiveParam);
    if fHeldDefines.IndexOf(lS) >= 0 then
      Lexer.AddDefine(lS);
  end;

  inherited;
end;

procedure TD2XDefinesParser.MainUnitName;
begin
  fLastTokens := '';
  inherited;
end;

procedure TD2XDefinesParser.MainUsedUnitExpression;
begin
  if not fKeepTokens then
    try
      fLastTokens := '';
      fKeepTokens := true;
      inherited;
      DoAddAttribute('file');
      DoTrimChildren('ConstantExpression');
    finally
      fKeepTokens := False;
    end
  else
    inherited;
end;

procedure TD2XDefinesParser.NextToken;
var
  lProcessed: Byte;
begin
  if not (Lexer.TokenID in [ptAnsiComment, ptBorComment, ptSlashesComment,
    ptSpace, ptCRLF, ptCRLFCo,
    ptIfDirect, ptIfEndDirect, ptElseIfDirect, ptIfDefDirect, ptIfNDefDirect,
    ptElseDirect, ptEndIfDirect, ptIfOptDirect, ptDefineDirect, ptUndefDirect]) then
    fLastTokens := fLastTokens + Lexer.Token;

  if Assigned(fOnProgress) and (fLength > 0) then
  begin
    lProcessed := Cardinal(Lexer.RunPos) * 100 div fLength;
    if lProcessed > fProcessed then
    begin
      fProcessed := lProcessed;
      fOnProgress(fProcessed);
    end;
  end;

  inherited;
end;

procedure TD2XDefinesParser.ParseFile;
var
  lS: string;
begin
  if Assigned(fStartDefines) or Assigned(fHeldDefines) then
  begin
    Lexer.ClearDefines;
    if Assigned(fHeldDefines) then
      for lS in fHeldDefines do
        Lexer.AddDefine(lS);
    if Assigned(fStartDefines) then
      for lS in fStartDefines do
        Lexer.AddDefine(lS);
  end;

  inherited;
end;

procedure TD2XDefinesParser.ProcessString(pFilename, pContents: string);
var
  lMS: TMemoryStream;
begin
  lMS := TMemoryStream.Create;
  try
    fLength := Length(pContents);
    fProcessed := 0;
    lMS.Size := fLength * Sizeof(Char);
    lMS.Write(PChar(pContents)^, lMS.Size);

    Run(pFilename, lMS);
    if Assigned(fOnProgress) and (fProcessed < 100) then
      fOnProgress(100);
  finally
    lMS.Free;
  end;
end;

procedure TD2XDefinesParser.UnitName;
var
  lGroup: string;
begin
  inherited;

  if Assigned(fOnGetGroup) then
  begin
    lGroup := fOnGetGroup(fLastTokens);
    if lGroup > '' then
      DoAddAttribute('group', lGroup);
  end;

  DoAddText;
end;

procedure TD2XDefinesParser.UsedUnitName;
begin
  fLastTokens := '';
  inherited;
end;

{ TD2XUsedParser }

procedure TD2XUsesParser.ContainsExpression;
begin
  inherited;
end;

procedure TD2XUsesParser.ImplementationSection;
begin
  Expected(ptImplementation);
  fCurrentSection := 'Implementation';
  if TokenID = ptUses then
    UsesClause;

  ReadToFileEnd;
end;

procedure TD2XUsesParser.InterfaceSection;
begin
  Expected(ptInterface);
  fCurrentSection := 'Interface';
  if TokenID = ptUses then
    UsesClause;

  ReadTo(ptImplementation);
end;

procedure TD2XUsesParser.MainUnitName;
begin
  inherited;
end;

procedure TD2XUsesParser.MainUsedUnitStatement;
begin
  inherited;
end;

procedure TD2XUsesParser.PackageFile;
begin
  inherited;
end;

procedure TD2XUsesParser.ProgramBlock;
begin
  if TokenID = ptUses then
    MainUsesClause;

  ReadToFileEnd;
  NextToken;
end;

procedure TD2XUsesParser.ProgramFile;
begin
  inherited;
end;

procedure TD2XUsesParser.ReadTo(pSym: TptTokenKind);
begin
  while (TokenID <> pSym) and (TokenID <> ptNull) do
  begin
    fLastTokens := '';
    NextToken;
  end;
end;

procedure TD2XUsesParser.ReadToFileEnd;
begin
  while not((TokenID = ptEnd) and (Lexer.CharAhead = '.')) and (TokenID <> ptNull) do
  begin
    fLastTokens := '';
    NextToken;
  end;
end;

procedure TD2XUsesParser.UnitFile;
begin
  inherited;
end;

procedure TD2XUsesParser.UsedUnitName;
begin
  inherited;
end;

end.
