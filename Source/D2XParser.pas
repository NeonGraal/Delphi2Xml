unit D2XParser;

interface

uses
  System.Classes,
  CastaliaPasLex,
  CastaliaPasLexTypes,
  CastaliaSimplePasPar;

type
  TD2XLexer = TmwBasePasLex;
  TD2XParser = TmwSimplePasPar;

  TD2XAddAttributeEvent = reference to procedure(pName, pValue: string);
  TD2XAddTextEvent = reference to procedure(pText: string);
  TD2XProgressEvent = reference to procedure(pProgress: Integer);

  TD2XDefinesParser = class(TD2XParser)
  private
    fLength: Cardinal;
    fProcessed: Byte;
    fLastTokens: string;
    fStartDefines: TStringList;
    FAddAttribute: TD2XAddAttributeEvent;
    FAddText: TD2XAddTextEvent;
    fOnProgress: TD2XProgressEvent;

    function GetStartDefines: TStringList;

  protected
    procedure NextToken; override;

    procedure DoAddAttribute(pName, pValue: string); overload;
    procedure DoAddAttribute(pName: string); overload;

    procedure DoAddText(pText: string); overload;
    procedure DoAddText; overload;

  public
    constructor Create;
    destructor Destroy; override;

    procedure UsedUnitName; override;
    procedure MainUnitName; override;
    procedure MainUsedUnitExpression; override;

    procedure ParseFile; override;
    procedure GetLexerDefines(pDefs: TStringList);

    procedure ProcessString(pFilename, pContents: string);

    property LastTokens: string read fLastTokens write fLastTokens;
    property StartDefines: TStringList read GetStartDefines;

    property OnProgress: TD2XProgressEvent read fOnProgress write fOnProgress;

    property AddAttribute: TD2XAddAttributeEvent read FAddAttribute write FAddAttribute;
    property AddText: TD2XAddTextEvent read FAddText write FAddText;
  end;

  TD2XUsesParser = class(TD2XDefinesParser)
  private
    fCurrentSection: string;

    procedure ReadToFileEnd;
    procedure ReadTo(pSym: TptTokenKind);
  public
    procedure InterfaceSection; override;
    procedure ImplementationSection; override;
    procedure ProgramBlock; override;
    procedure MainUsedUnitStatement; override;

    procedure UsedUnitName; override;
    procedure MainUnitName; override;
    procedure MainUsedUnitExpression; override;

    property CurrentSection: string read fCurrentSection;
  end;

  TD2XFullParser = class(TD2XDefinesParser)
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
    procedure ConstantName; override;
    // JR added constant type
    procedure ConstantType; override;
    procedure ConstantValue; override;
    procedure ConstantValueTyped; override;
    procedure ConstParameter; override;
    procedure ConstructorHeading; override;
    procedure ConstructorName; override;
    procedure ConstSection; override;
    procedure ContainsClause; override;
    procedure ContainsExpression; override;
    procedure ContainsIdentifier; override;
    procedure ContainsStatement; override;
{$IFDEF D8_NEWER}
    procedure CustomAttribute; override; // JThurman 2004-03-03
{$ENDIF}
    procedure DeclarationSection; override;
    procedure Designator; override;
    procedure DestructorHeading; override;
    procedure DestructorName; override;
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
    procedure ExportsClause; override;
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
    procedure FieldName; override;
    procedure FileType; override;
    procedure FormalParameterList; override;
    procedure FormalParameterSection; override;
    procedure ForStatement; override;
    procedure ForwardDeclaration; override; { GLC: corrected spelling }
    procedure FunctionHeading; override;
    procedure FunctionMethodDeclaration; override;
    procedure FunctionMethodName; override;
    procedure FunctionProcedureBlock; override;
    procedure FunctionProcedureName; override;
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
    procedure MainUnitName; override;
    procedure MainUsedUnitExpression; override;
    procedure MainUsedUnitName; override;
    procedure MainUsedUnitStatement; override;
    procedure MainUsesClause; override;
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
    procedure ParameterName; override;
    procedure ParameterNameList; override;
    procedure ParseFile; override;
    procedure PointerType; override;
    procedure ProceduralDirective; override;
    procedure ProceduralType; override;
    procedure ProcedureDeclarationSection; override;
    procedure ProcedureHeading; override;
    procedure ProcedureMethodDeclaration; override;
    procedure ProcedureMethodName; override;
    procedure ProgramBlock; override;
    procedure ProgramFile; override;
    procedure PropertyDefault; override;
    procedure PropertyInterface; override;
    procedure PropertyName; override;
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
    procedure RequiresClause; override;
    procedure RequiresIdentifier; override;
    procedure ResolutionInterfaceName; override;
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
    procedure TagFieldName; override;
    procedure TagFieldTypeName; override;
    procedure Term; override;
    procedure TryStatement; override;
    procedure TypedConstant; override;
    procedure TypeDeclaration; override;
    procedure TypeId; override;
    procedure TypeKind; override;
    procedure TypeName; override;
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
    procedure UnitName; override;
    procedure UsedUnitName; override;
    procedure UsedUnitsList; override;
    procedure UsesClause; override;
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
    procedure VarName; override; // !! Added VarName and VarNameList back in...
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
  System.Math,
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
  inherited;
end;

procedure TD2XFullParser.ConstSection;
begin
  inherited;
end;

procedure TD2XFullParser.ContainsClause;
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

procedure TD2XFullParser.ExportsClause;
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
  inherited;
end;

procedure TD2XFullParser.FunctionProcedureBlock;
begin
  inherited;
end;

procedure TD2XFullParser.FunctionProcedureName;
begin
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
  inherited;
end;

procedure TD2XFullParser.MainUsedUnitExpression;
begin
  inherited;
end;

procedure TD2XFullParser.MainUsedUnitName;
begin
  inherited;
end;

procedure TD2XFullParser.MainUsedUnitStatement;
begin
  inherited;
end;

procedure TD2XFullParser.MainUsesClause;
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
  inherited;
end;

procedure TD2XFullParser.ParameterNameList;
begin
  inherited;
end;

procedure TD2XFullParser.ParseFile;
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

procedure TD2XFullParser.RequiresClause;
begin
  inherited;
end;

procedure TD2XFullParser.RequiresIdentifier;
begin
  inherited;
end;

procedure TD2XFullParser.ResolutionInterfaceName;
begin
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
  inherited;
end;

procedure TD2XFullParser.TagFieldTypeName;
begin
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

procedure TD2XFullParser.UnitName;
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

procedure TD2XFullParser.UsesClause;
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

constructor TD2XDefinesParser.Create;
begin
  inherited;

  fStartDefines := nil;
  fLength := 0;
  fProcessed := 0;
  fOnProgress := nil;
end;

destructor TD2XDefinesParser.Destroy;
begin
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
  if Assigned(FAddText) then
    FAddText(pText);
end;

procedure TD2XDefinesParser.DoAddText;
begin
  DoAddText(fLastTokens);
  fLastTokens := '';
end;

procedure TD2XDefinesParser.DoAddAttribute(pName, pValue: string);
begin
  if Assigned(FAddAttribute) then
    FAddAttribute(pName, pValue);
end;

procedure TD2XDefinesParser.GetLexerDefines(pDefs: TStringList);
begin
  Lexer.GetDefines(pDefs);
end;

function TD2XDefinesParser.GetStartDefines: TStringList;
begin
  if not Assigned(fStartDefines) then
    fStartDefines := TStringList.Create;

  Result := fStartDefines;
end;

procedure TD2XDefinesParser.MainUnitName;
begin
  fLastTokens := '';
  inherited;
  DoAddText;
end;

procedure TD2XDefinesParser.MainUsedUnitExpression;
begin
  fLastTokens := '';
  inherited;
  DoAddText;
end;

procedure TD2XDefinesParser.NextToken;
var
  lProcessed: Byte;
begin
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
  if Assigned(fStartDefines) then
  begin
    Lexer.ClearDefines;
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
    lMS.Write(PChar(pContents)^, fLength * Sizeof(Char));

    Run(pFilename, lMS);
  finally
    lMS.Free;
  end;
end;

procedure TD2XDefinesParser.UsedUnitName;
begin
  fLastTokens := '';
  inherited;
  DoAddText;
end;

{ TD2XUsedParser }

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

procedure TD2XUsesParser.MainUsedUnitExpression;
begin
  inherited;
end;

procedure TD2XUsesParser.MainUsedUnitStatement;
begin
  inherited;
end;

procedure TD2XUsesParser.ProgramBlock;
begin
  if TokenID = ptUses then
  begin
    MainUsesClause;
  end;

  ReadToFileEnd;
  NextToken;
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

procedure TD2XUsesParser.UsedUnitName;
begin
  inherited;
end;

end.
