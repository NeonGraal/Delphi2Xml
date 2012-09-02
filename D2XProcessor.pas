unit D2XProcessor;

interface

uses
  System.Rtti,
  Xml.XMLIntf,
  CastaliaPasLexTypes,
  CastaliaPasLex,
  CastaliaSimplePasPar;

type
  TD2XOptions = class
  private
    fVerbose: Boolean;
  public
    property Verbose: Boolean read fVerbose write fVerbose default False;
  end;

  TD2XParser = class(TmwSimplePasPar)
  private
    fLastTokens: string;

  protected
    procedure NextToken; override;

  public
    property LastTokens: string read fLastTokens write fLastTokens;

    procedure HandlePtCompDirect(Sender: TmwBasePasLex); override;
    procedure HandlePtDefineDirect(Sender: TmwBasePasLex); override;
    procedure HandlePtElseDirect(Sender: TmwBasePasLex); override;
    procedure HandlePtEndIfDirect(Sender: TmwBasePasLex); override;
    procedure HandlePtIfDefDirect(Sender: TmwBasePasLex); override;
    procedure HandlePtIfNDefDirect(Sender: TmwBasePasLex); override;
    procedure HandlePtIfOptDirect(Sender: TmwBasePasLex); override;
    procedure HandlePtIncludeDirect(Sender: TmwBasePasLex); override;
    procedure HandlePtResourceDirect(Sender: TmwBasePasLex); override;
    procedure HandlePtUndefDirect(Sender: TmwBasePasLex); override;
    procedure HandlePtIfDirect(Sender: TmwBasePasLex); override;
    procedure HandlePtIfEndDirect(Sender: TmwBasePasLex); override;
    procedure HandlePtElseIfDirect(Sender: TmwBasePasLex); override;

//    procedure AccessSpecifier; override;
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
//    procedure ClassMethodOrProperty; override;
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
//JR added constant type
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
    procedure CustomAttribute; override; //JThurman 2004-03-03
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
    procedure ForwardDeclaration; override; {GLC: corrected spelling}
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
//    procedure InterfaceDeclaration; override;
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
//    procedure OrdinalIdentifier; override;
    procedure OrdinalType; override;
    procedure OutParameter; override;
    procedure PackageFile; override;
    procedure ParameterFormal; override;
    procedure ParameterName; override;
    procedure ParameterNameList; override;
//    procedure ParseFile; override;
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
//    procedure QualifiedIdentifier; override;
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
    procedure SkipSpace; override; //XM Jul-2000
    procedure SkipCRLFco; override; //XM Jul-2000
    procedure SkipCRLF; override; //XM Jul-2000
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
//    procedure StringType; override;
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
    //generics
    procedure TypeArgs; override;
    procedure TypeParams; override;
    procedure TypeParamDecl; override;
    procedure TypeParamDeclList; override;
    procedure TypeParamList; override;
    procedure ConstraintList; override;
    procedure Constraint; override;
    //end generics
    procedure TypeSection; override;
    procedure UnitFile; override;
    procedure UnitId; override;
    procedure UnitName; override;
    procedure UsedUnitName; override;
    procedure UsedUnitsList; override;
//    procedure UsesClause; override;
    procedure VarAbsolute; override;
    procedure VarEqual; override;
    procedure VarDeclaration; override;
    procedure Variable; override;
    procedure VariableList; override;
    procedure VariableReference; override;
//    procedure VariableTwo; override;
    procedure VariantIdentifier; override;
    procedure VariantSection; override;
    procedure VarParameter; override;
    procedure VarName; override; //!! Added VarName and VarNameList back in...
    procedure VarNameList; override;
    procedure VarSection; override;
//    procedure VisibilityAutomated; override;
//    procedure VisibilityPrivate; override;
//    procedure VisibilityProtected; override;
//    procedure VisibilityPublic; override;
//    procedure VisibilityPublished; override;
//    procedure VisibilityUnknown; override;
    procedure WhileStatement; override;
    procedure WithStatement; override;
    procedure WriteAccessIdentifier; override;

  end;

  TD2XProcessor = class
  private
    fOpts: TD2XOptions;

    fParser: TD2XParser;
    fVMI: TVirtualMethodInterceptor;

    fXmlDoc: IXMLDocument;
    fXmlNode: IXMLNode;

    procedure SetProxy;

  public
    constructor Create;
    destructor Destroy; override;

    procedure ProcessFile(pFilename: String);

    procedure RemoveProxy;
    procedure SetSimpleProxy;
    procedure SetXmlProxy;

    property Options: TD2XOptions read fOpts;

  end;

implementation

uses
  Xml.XMLDoc,
  System.Classes,
  System.SysUtils;

{ TD2XParser }

(*
  procedure TD2XParser.AccessSpecifier;
  begin
    inherited;

  end;
*)
procedure TD2XParser.AdditiveOperator;
begin
  inherited;

end;

procedure TD2XParser.AncestorId;
begin
  inherited;

end;

procedure TD2XParser.AncestorIdList;
begin
  inherited;

end;

procedure TD2XParser.AnonymousMethod;
begin
  inherited;

end;

procedure TD2XParser.AnonymousMethodType;
begin
  inherited;

end;

procedure TD2XParser.ArrayConstant;
begin
  inherited;

end;

procedure TD2XParser.ArrayType;
begin
  inherited;

end;

procedure TD2XParser.AsmStatement;
begin
  inherited;

end;

procedure TD2XParser.Block;
begin
  inherited;

end;

procedure TD2XParser.CaseLabel;
begin
  inherited;

end;

procedure TD2XParser.CaseSelector;
begin
  inherited;

end;

procedure TD2XParser.CaseStatement;
begin
  inherited;

end;

procedure TD2XParser.CharString;
begin
  inherited;

end;

procedure TD2XParser.ClassClass;
begin
  inherited;

end;

procedure TD2XParser.ClassField;
begin
  inherited;

end;

procedure TD2XParser.ClassForward;
begin
  inherited;

end;

procedure TD2XParser.ClassFunctionHeading;
begin
  inherited;

end;

procedure TD2XParser.ClassHeritage;
begin
  inherited;

end;

procedure TD2XParser.ClassMemberList;
begin
  inherited;

end;

procedure TD2XParser.ClassMethodDirective;
begin
  inherited;

end;

procedure TD2XParser.ClassMethodHeading;
begin
  inherited;

end;
(*
  procedure TD2XParser.ClassMethodOrProperty;
  begin
    inherited;

  end;
*)
procedure TD2XParser.ClassMethodResolution;
begin
  inherited;

end;

procedure TD2XParser.ClassProcedureHeading;
begin
  inherited;

end;

procedure TD2XParser.ClassProperty;
begin
  inherited;

end;

procedure TD2XParser.ClassReferenceType;
begin
  inherited;

end;

procedure TD2XParser.ClassType;
begin
  inherited;

end;

procedure TD2XParser.ClassTypeEnd;
begin
  inherited;

end;

procedure TD2XParser.ClassVisibility;
begin
  inherited;

end;

procedure TD2XParser.CompoundStatement;
begin
  inherited;

end;

procedure TD2XParser.ConstantColon;
begin
  inherited;

end;

procedure TD2XParser.ConstantDeclaration;
begin
  inherited;

end;

procedure TD2XParser.ConstantEqual;
begin
  inherited;

end;

procedure TD2XParser.ConstantExpression;
begin
  inherited;

end;

procedure TD2XParser.ConstantName;
begin
  inherited;

end;

procedure TD2XParser.ConstantType;
begin
  inherited;

end;

procedure TD2XParser.ConstantValue;
begin
  inherited;

end;

procedure TD2XParser.ConstantValueTyped;
begin
  inherited;

end;

procedure TD2XParser.ConstParameter;
begin
  inherited;

end;

procedure TD2XParser.Constraint;
begin
  inherited;

end;

procedure TD2XParser.ConstraintList;
begin
  inherited;

end;

procedure TD2XParser.ConstructorHeading;
begin
  inherited;

end;

procedure TD2XParser.ConstructorName;
begin
  inherited;

end;

procedure TD2XParser.ConstSection;
begin
  inherited;

end;

procedure TD2XParser.ContainsClause;
begin
  inherited;

end;

procedure TD2XParser.ContainsExpression;
begin
  inherited;

end;

procedure TD2XParser.ContainsIdentifier;
begin
  inherited;

end;

procedure TD2XParser.ContainsStatement;
begin
  inherited;

end;

procedure TD2XParser.DeclarationSection;
begin
  inherited;

end;

procedure TD2XParser.Designator;
begin
  inherited;

end;

procedure TD2XParser.DestructorHeading;
begin
  inherited;

end;

procedure TD2XParser.DestructorName;
begin
  inherited;

end;

procedure TD2XParser.Directive16Bit;
begin
  inherited;

end;

procedure TD2XParser.DirectiveBinding;
begin
  inherited;

end;

procedure TD2XParser.DirectiveCalling;
begin
  inherited;

end;

procedure TD2XParser.DirectiveDeprecated;
begin
  inherited;

end;

procedure TD2XParser.DirectiveLibrary;
begin
  inherited;

end;

procedure TD2XParser.DirectiveLocal;
begin
  inherited;

end;

procedure TD2XParser.DirectivePlatform;
begin
  inherited;

end;

procedure TD2XParser.DirectiveVarargs;
begin
  inherited;

end;

procedure TD2XParser.DispIDSpecifier;
begin
  inherited;

end;

procedure TD2XParser.DispInterfaceForward;
begin
  inherited;

end;

procedure TD2XParser.EmptyStatement;
begin
  inherited;

end;

procedure TD2XParser.EnumeratedType;
begin
  inherited;

end;

procedure TD2XParser.EnumeratedTypeItem;
begin
  inherited;

end;

procedure TD2XParser.ExceptBlock;
begin
  inherited;

end;

procedure TD2XParser.ExceptionBlockElseBranch;
begin
  inherited;

end;

procedure TD2XParser.ExceptionClassTypeIdentifier;
begin
  inherited;

end;

procedure TD2XParser.ExceptionHandler;
begin
  inherited;

end;

procedure TD2XParser.ExceptionHandlerList;
begin
  inherited;

end;

procedure TD2XParser.ExceptionIdentifier;
begin
  inherited;

end;

procedure TD2XParser.ExceptionVariable;
begin
  inherited;

end;

procedure TD2XParser.ExplicitType;
begin
  inherited;

end;

procedure TD2XParser.ExportedHeading;
begin
  inherited;

end;

procedure TD2XParser.ExportsClause;
begin
  inherited;

end;

procedure TD2XParser.ExportsElement;
begin
  inherited;

end;

procedure TD2XParser.Expression;
begin
  inherited;

end;

procedure TD2XParser.ExpressionList;
begin
  inherited;

end;

procedure TD2XParser.ExternalDirective;
begin
  inherited;

end;

procedure TD2XParser.ExternalDirectiveThree;
begin
  inherited;

end;

procedure TD2XParser.ExternalDirectiveTwo;
begin
  inherited;

end;

procedure TD2XParser.Factor;
begin
  inherited;

end;

procedure TD2XParser.FieldDeclaration;
begin
  inherited;

end;

procedure TD2XParser.FieldList;
begin
  inherited;

end;

procedure TD2XParser.FieldName;
begin
  inherited;

end;

procedure TD2XParser.FieldNameList;
begin
  inherited;

end;

procedure TD2XParser.FileType;
begin
  inherited;

end;

procedure TD2XParser.FormalParameterList;
begin
  inherited;

end;

procedure TD2XParser.FormalParameterSection;
begin
  inherited;

end;

procedure TD2XParser.ForStatement;
begin
  inherited;

end;

procedure TD2XParser.ForwardDeclaration;
begin
  inherited;

end;

procedure TD2XParser.FunctionHeading;
begin
  inherited;

end;

procedure TD2XParser.FunctionMethodDeclaration;
begin
  inherited;

end;

procedure TD2XParser.FunctionMethodName;
begin
  inherited;

end;

procedure TD2XParser.FunctionProcedureBlock;
begin
  inherited;

end;

procedure TD2XParser.FunctionProcedureName;
begin
  inherited;

end;

procedure TD2XParser.HandlePtCompDirect(Sender: TmwBasePasLex);
begin
  inherited;

end;

procedure TD2XParser.HandlePtDefineDirect(Sender: TmwBasePasLex);
begin
  inherited;

end;

procedure TD2XParser.HandlePtElseDirect(Sender: TmwBasePasLex);
begin
  inherited;

end;

procedure TD2XParser.HandlePtElseIfDirect(Sender: TmwBasePasLex);
begin
  inherited;

end;

procedure TD2XParser.HandlePtEndIfDirect(Sender: TmwBasePasLex);
begin
  inherited;

end;

procedure TD2XParser.HandlePtIfDefDirect(Sender: TmwBasePasLex);
begin
  inherited;

end;

procedure TD2XParser.HandlePtIfDirect(Sender: TmwBasePasLex);
begin
  inherited;

end;

procedure TD2XParser.HandlePtIfEndDirect(Sender: TmwBasePasLex);
begin
  inherited;

end;

procedure TD2XParser.HandlePtIfNDefDirect(Sender: TmwBasePasLex);
begin
  inherited;

end;

procedure TD2XParser.HandlePtIfOptDirect(Sender: TmwBasePasLex);
begin
  inherited;

end;

procedure TD2XParser.HandlePtIncludeDirect(Sender: TmwBasePasLex);
begin
  inherited;

end;

procedure TD2XParser.HandlePtResourceDirect(Sender: TmwBasePasLex);
begin
  inherited;

end;

procedure TD2XParser.HandlePtUndefDirect(Sender: TmwBasePasLex);
begin
  inherited;

end;

procedure TD2XParser.Identifier;
begin
  inherited;

end;

procedure TD2XParser.IdentifierList;
begin
  inherited;

end;

procedure TD2XParser.IfStatement;
begin
  inherited;

end;

procedure TD2XParser.ImplementationSection;
begin
  inherited;

end;

procedure TD2XParser.IncludeFile;
begin
  inherited;

end;

procedure TD2XParser.IndexSpecifier;
begin
  inherited;

end;

procedure TD2XParser.InheritedStatement;
begin
  inherited;

end;

procedure TD2XParser.InitializationSection;
begin
  inherited;

end;

procedure TD2XParser.InlineStatement;
begin
  inherited;

end;

procedure TD2XParser.InParameter;
begin
  inherited;

end;
(*
  procedure TD2XParser.InterfaceDeclaration;
  begin
    inherited;

  end;
*)
procedure TD2XParser.InterfaceForward;
begin
  inherited;

end;

procedure TD2XParser.InterfaceGUID;
begin
  inherited;

end;

procedure TD2XParser.InterfaceHeritage;
begin
  inherited;

end;

procedure TD2XParser.InterfaceMemberList;
begin
  inherited;

end;

procedure TD2XParser.InterfaceSection;
begin
  inherited;

end;

procedure TD2XParser.InterfaceType;
begin
  inherited;

end;

procedure TD2XParser.LabelDeclarationSection;
begin
  inherited;

end;

procedure TD2XParser.LabeledStatement;
begin
  inherited;

end;

procedure TD2XParser.LabelId;
begin
  inherited;

end;

procedure TD2XParser.LibraryFile;
begin
  inherited;

end;

procedure TD2XParser.MainUsedUnitExpression;
begin
  inherited;

end;

procedure TD2XParser.MainUsedUnitName;
begin
  inherited;

end;

procedure TD2XParser.MainUsedUnitStatement;
begin
  inherited;

end;

procedure TD2XParser.MainUsesClause;
begin
  inherited;

end;

procedure TD2XParser.MultiplicativeOperator;
begin
  inherited;

end;

procedure TD2XParser.NewFormalParameterType;
begin
  inherited;

end;

procedure TD2XParser.NextToken;
begin
  fLastTokens := fLastTokens + Lexer.Token;

  inherited;
end;

procedure TD2XParser.Number;
begin
  inherited;

end;

procedure TD2XParser.ObjectConstructorHeading;
begin
  inherited;

end;

procedure TD2XParser.ObjectDestructorHeading;
begin
  inherited;

end;

procedure TD2XParser.ObjectField;
begin
  inherited;

end;

procedure TD2XParser.ObjectForward;
begin
  inherited;

end;

procedure TD2XParser.ObjectFunctionHeading;
begin
  inherited;

end;

procedure TD2XParser.ObjectHeritage;
begin
  inherited;

end;

procedure TD2XParser.ObjectMemberList;
begin
  inherited;

end;

procedure TD2XParser.ObjectMethodDirective;
begin
  inherited;

end;

procedure TD2XParser.ObjectMethodHeading;
begin
  inherited;

end;

procedure TD2XParser.ObjectNameOfMethod;
begin
  inherited;

end;

procedure TD2XParser.ObjectProcedureHeading;
begin
  inherited;

end;

procedure TD2XParser.ObjectProperty;
begin
  inherited;

end;

procedure TD2XParser.ObjectPropertySpecifiers;
begin
  inherited;

end;

procedure TD2XParser.ObjectType;
begin
  inherited;

end;

procedure TD2XParser.ObjectTypeEnd;
begin
  inherited;

end;

procedure TD2XParser.ObjectVisibility;
begin
  inherited;

end;

procedure TD2XParser.OldFormalParameterType;
begin
  inherited;

end;
(*
  procedure TD2XParser.OrdinalIdentifier;
  begin
    inherited;

  end;
*)
procedure TD2XParser.OrdinalType;
begin
  inherited;

end;

procedure TD2XParser.OutParameter;
begin
  inherited;

end;

procedure TD2XParser.PackageFile;
begin
  inherited;

end;

procedure TD2XParser.ParameterFormal;
begin
  inherited;

end;

procedure TD2XParser.ParameterName;
begin
  inherited;

end;

procedure TD2XParser.ParameterNameList;
begin
  inherited;

end;
(*
  procedure TD2XParser.ParseFile;
  begin
    inherited;

  end;
*)
procedure TD2XParser.PointerType;
begin
  inherited;

end;

procedure TD2XParser.ProceduralDirective;
begin
  inherited;

end;

procedure TD2XParser.ProceduralType;
begin
  inherited;

end;

procedure TD2XParser.ProcedureDeclarationSection;
begin
  inherited;

end;

procedure TD2XParser.ProcedureHeading;
begin
  inherited;

end;

procedure TD2XParser.ProcedureMethodDeclaration;
begin
  inherited;

end;

procedure TD2XParser.ProcedureMethodName;
begin
  inherited;

end;

procedure TD2XParser.ProgramBlock;
begin
  inherited;

end;

procedure TD2XParser.ProgramFile;
begin
  inherited;

end;

procedure TD2XParser.PropertyDefault;
begin
  inherited;

end;

procedure TD2XParser.PropertyInterface;
begin
  inherited;

end;

procedure TD2XParser.PropertyName;
begin
  inherited;

end;

procedure TD2XParser.PropertyParameterConst;
begin
  inherited;

end;

procedure TD2XParser.PropertyParameterList;
begin
  inherited;

end;

procedure TD2XParser.PropertySpecifiers;
begin
  inherited;

end;
(*
procedure TD2XParser.QualifiedIdentifier;
begin
  inherited;

end;
*)
procedure TD2XParser.QualifiedIdentifierList;
begin
  inherited;

end;

procedure TD2XParser.RaiseStatement;
begin
  inherited;

end;

procedure TD2XParser.ReadAccessIdentifier;
begin
  inherited;

end;

procedure TD2XParser.RealIdentifier;
begin
  inherited;

end;

procedure TD2XParser.RealType;
begin
  inherited;

end;

procedure TD2XParser.RecordConstant;
begin
  inherited;

end;

procedure TD2XParser.RecordFieldConstant;
begin
  inherited;

end;

procedure TD2XParser.RecordType;
begin
  inherited;

end;

procedure TD2XParser.RecordVariant;
begin
  inherited;

end;

procedure TD2XParser.RelativeOperator;
begin
  inherited;

end;

procedure TD2XParser.RepeatStatement;
begin
  inherited;

end;

procedure TD2XParser.RequiresClause;
begin
  inherited;

end;

procedure TD2XParser.RequiresIdentifier;
begin
  inherited;

end;

procedure TD2XParser.ResolutionInterfaceName;
begin
  inherited;

end;

procedure TD2XParser.ResourceDeclaration;
begin
  inherited;

end;

procedure TD2XParser.ReturnType;
begin
  inherited;

end;

procedure TD2XParser.SetConstructor;
begin
  inherited;

end;

procedure TD2XParser.SetElement;
begin
  inherited;

end;

procedure TD2XParser.SetType;
begin
  inherited;

end;

procedure TD2XParser.SimpleExpression;
begin
  inherited;

end;

procedure TD2XParser.SimpleStatement;
begin
  inherited;

end;

procedure TD2XParser.SimpleType;
begin
  inherited;

end;

procedure TD2XParser.SkipAnsiComment;
begin
  inherited;

end;

procedure TD2XParser.SkipBorComment;
begin
  inherited;

end;

procedure TD2XParser.SkipCRLF;
begin
  inherited;

end;

procedure TD2XParser.SkipCRLFco;
begin
  inherited;

end;

procedure TD2XParser.SkipSlashesComment;
begin
  inherited;

end;

procedure TD2XParser.SkipSpace;
begin
  inherited;

end;

procedure TD2XParser.Statement;
begin
  inherited;

end;

procedure TD2XParser.StatementList;
begin
  inherited;

end;

procedure TD2XParser.StorageDefault;
begin
  inherited;

end;

procedure TD2XParser.StorageExpression;
begin
  inherited;

end;

procedure TD2XParser.StorageIdentifier;
begin
  inherited;

end;

procedure TD2XParser.StorageNoDefault;
begin
  inherited;

end;

procedure TD2XParser.StorageSpecifier;
begin
  inherited;

end;

procedure TD2XParser.StorageStored;
begin
  inherited;

end;

procedure TD2XParser.StringIdentifier;
begin
  inherited;

end;

procedure TD2XParser.StringStatement;
begin
  inherited;

end;
(*
  procedure TD2XParser.StringType;
  begin
    inherited;

  end;
*)
procedure TD2XParser.StructuredType;
begin
  inherited;

end;

procedure TD2XParser.SubrangeType;
begin
  inherited;

end;

procedure TD2XParser.TagField;
begin
  inherited;

end;

procedure TD2XParser.TagFieldName;
begin
  inherited;

end;

procedure TD2XParser.TagFieldTypeName;
begin
  inherited;

end;

procedure TD2XParser.Term;
begin
  inherited;

end;

procedure TD2XParser.TryStatement;
begin
  inherited;

end;

procedure TD2XParser.TypeArgs;
begin
  inherited;

end;

procedure TD2XParser.TypedConstant;
begin
  inherited;

end;

procedure TD2XParser.TypeDeclaration;
begin
  inherited;

end;

procedure TD2XParser.TypeId;
begin
  inherited;

end;

procedure TD2XParser.TypeKind;
begin
  inherited;

end;

procedure TD2XParser.TypeName;
begin
  inherited;

end;

procedure TD2XParser.TypeParamDecl;
begin
  inherited;

end;

procedure TD2XParser.TypeParamDeclList;
begin
  inherited;

end;

procedure TD2XParser.TypeParamList;
begin
  inherited;

end;

procedure TD2XParser.TypeParams;
begin
  inherited;

end;

procedure TD2XParser.TypeSection;
begin
  inherited;

end;

procedure TD2XParser.UnitFile;
begin
  inherited;

end;

procedure TD2XParser.UnitId;
begin
  inherited;

end;

procedure TD2XParser.UnitName;
begin
  inherited;

end;

procedure TD2XParser.UsedUnitName;
begin
  inherited;

end;

procedure TD2XParser.UsedUnitsList;
begin
  inherited;

end;
(*
  procedure TD2XParser.UsesClause;
  begin
    inherited;

  end;
*)
procedure TD2XParser.VarAbsolute;
begin
  inherited;

end;

procedure TD2XParser.VarDeclaration;
begin
  inherited;

end;

procedure TD2XParser.VarEqual;
begin
  inherited;

end;

procedure TD2XParser.Variable;
begin
  inherited;

end;

procedure TD2XParser.VariableList;
begin
  inherited;

end;

procedure TD2XParser.VariableReference;
begin
  inherited;

end;
(*
procedure TD2XParser.VariableTwo;
begin
  inherited;

end;
*)
procedure TD2XParser.VariantIdentifier;
begin
  inherited;

end;

procedure TD2XParser.VariantSection;
begin
  inherited;

end;

procedure TD2XParser.VarName;
begin
  inherited;

end;

procedure TD2XParser.VarNameList;
begin
  inherited;

end;

procedure TD2XParser.VarParameter;
begin
  inherited;

end;

procedure TD2XParser.VarSection;
begin
  inherited;

end;
(*
  procedure TD2XParser.VisibilityAutomated;
  begin
    inherited;

  end;

  procedure TD2XParser.VisibilityPrivate;
  begin
    inherited;

  end;

  procedure TD2XParser.VisibilityProtected;
  begin
    inherited;

  end;

  procedure TD2XParser.VisibilityPublic;
  begin
    inherited;

  end;

  procedure TD2XParser.VisibilityPublished;
  begin
    inherited;

  end;

  procedure TD2XParser.VisibilityUnknown;
  begin
    inherited;

  end;
*)
procedure TD2XParser.WhileStatement;
begin
  inherited;

end;

procedure TD2XParser.WithStatement;
begin
  inherited;

end;

procedure TD2XParser.WriteAccessIdentifier;
begin
  inherited;

end;

{ TD2XProcessor }

constructor TD2XProcessor.Create;
begin
  inherited Create;

  fOpts := TD2XOptions.Create;
  fParser := TD2XParser.Create;
end;

destructor TD2XProcessor.Destroy;
begin
  RemoveProxy;

  FreeAndNil(fParser);
  FreeAndNil(fOpts);

  inherited;
end;

procedure TD2XProcessor.ProcessFile(pFilename: String);
var
  lSS: TStringStream;
  lMS: TMemoryStream;
begin
  lMS := nil;
  lSS := TStringStream.Create;
  try
    lSS.LoadFromFile(pFilename);
    lMS := TMemoryStream.Create;
    lMS.Write(PChar(lSS.DataString)^, lSS.Size * Sizeof(PChar));
    fParser.Run(pFilename, lMS);

    if Assigned(fXmlDoc) then begin
      fXmlDoc.XML.SaveToFile(ChangeFileExt(pFilename, '.xml'));
    end;
  finally
    FreeAndNil(lMS);
    FreeAndNil(lSS);
  end;
end;

procedure TD2XProcessor.RemoveProxy;
begin
  if Assigned(fVMI) then
  begin
    fXmlDoc := nil;
    fXmlNode := nil;

    fVMI.Unproxify(fParser);
    FreeAndNil(fVMI);
  end;
end;

procedure TD2XProcessor.SetProxy;
begin
  RemoveProxy;
  fVMI := TVirtualMethodInterceptor.Create(TObject(fParser).ClassType);
  fVMI.Proxify(fParser);
end;

procedure TD2XProcessor.SetSimpleProxy;
begin
  SetProxy;
  fVMI.OnBefore := procedure(pInst: TObject; pMethod: TRttiMethod;
      const pArgs: TArray<TValue>; out pDoInvoke: Boolean; out pResult: TValue)
    begin
      pDoInvoke := True;
      Writeln('BEFORE ', pMethod.Name, ' @ ', TD2XParser(pInst).Lexer.Token);
    end;
  fVMI.OnAfter := procedure(pInst: TObject; pMethod: TRttiMethod;
      const pArgs: TArray<TValue>; var pResult: TValue)
    begin
      Writeln('AFTER  ', pMethod.Name);
    end;

end;

procedure TD2XProcessor.SetXmlProxy;
var
  lAttr: IXMLNode;
begin
  SetProxy;
  fXmlDoc := NewXmlDocument;
  fXmlDoc.Options := fXmlDoc.Options + [doNodeAutoIndent];
  fVMI.OnBefore := procedure(pInst: TObject; pMethod: TRttiMethod;
      const pArgs: TArray<TValue>; out pDoInvoke: Boolean; out pResult: TValue)
    begin
      pDoInvoke := True;
      if Assigned(fXmlDoc) then
      begin
        if Assigned(fXmlNode) then
          fXmlNode := fXmlNode.AddChild(pMethod.Name)
        else
          fXmlNode := fXmlDoc.AddChild(pMethod.Name);
        fParser.LastTokens := '';
      end;
    end;
  fVMI.OnAfter := procedure(pInst: TObject; pMethod: TRttiMethod;
      const pArgs: TArray<TValue>; var pResult: TValue)
    begin
      if Assigned(fXmlNode) then begin
        if Length(fParser.LastTokens) > 1 then
        begin
          lAttr := fXmlDoc.CreateNode('lastToken', ntAttribute);
          lAttr.Text := fParser.LastTokens;
          fXmlNode.AttributeNodes.Add(lAttr);
          fParser.LastTokens := '';
        end;

        fXmlNode := fXmlNode.ParentNode;
      end;
    end;
end;

end.
