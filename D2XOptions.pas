unit D2XOptions;

interface

uses
  System.SysUtils,
  System.StrUtils,
  System.Classes;

type
  ED2XOptionsException = class(Exception);

  TD2XParseMode = (pmFull, pmUses);

  TD2XResultPer = (rpFile, rpWildcard, rpSubDir, rpDir, rpParam, rpRun);


  TD2X = class
    class function EnumLabel<T: record>(pVal: T): String;
  end;

  TD2XOptions = class
  private
    fVerbose: boolean;
    fXml: boolean;
    fCountChildren: boolean;
    fCountExtension: string;
    fXmlDirectory: string;
    fSkipExtension: string;
    fSkipMethods: boolean;
    fUseBase: boolean;
    fBaseDirectory: string;
    fRecurse: boolean;
    fLogErrors: boolean;
    fLogNotSupported: boolean;
    fTimestampFiles: boolean;
    fDefinesUsed: boolean;
    fUsedExtension: string;
    fLoadExtension: string;
    fLoadDefines: boolean;
    fDefines: TStringList;
    fWriteDefines: boolean;
    fDefinesDirectory: string;
    fUseOutput: boolean;
    fOutputDirectory: string;
    fUseInput: boolean;
    fInputDirectory: string;
    fParseMode: TD2XParseMode;
    fResultPer: TD2XResultPer;
    fFinalToken: Boolean;

    procedure AddDefine(pDef: string);
    procedure DeleteDefine(pDef: string);
    procedure LoadDefinesFile(pFile: string);

    function SetParseMode(pVal: string): Boolean;
    function SetResultPer(pVal: string): Boolean;

  public
    property LogErrors: boolean read fLogErrors;
    property LogNotSupported: boolean read fLogNotSupported;
    property TimestampFiles: boolean read fTimestampFiles;
    property Verbose: boolean read fVerbose;
    property Recurse: boolean read fRecurse;
    property UseBase: boolean read fUseBase;
    property BaseDirectory: string read fBaseDirectory;
    property UseInput: boolean read fUseInput;
    property InputDirectory: string read fInputDirectory;
    property UseOutput: boolean read fUseOutput;
    property OutputDirectory: string read fOutputDirectory;
    property WriteDefines: boolean read fWriteDefines;
    property DefinesDirectory: string read fDefinesDirectory;
    property Xml: boolean read fXml;
    property XmlDirectory: string read fXmlDirectory;
    property DefinesUsed: boolean read fDefinesUsed;
    property UsedExtension: string read fUsedExtension;
    property LoadDefines: boolean read fLoadDefines;
    property LoadExtension: string read fLoadExtension;
    property Defines: TStringList read fDefines;
    property CountChildren: boolean read fCountChildren;
    property CountExtension: string read fCountExtension;
    property SkipMethods: boolean read fSkipMethods;
    property SkipExtension: string read fSkipExtension;
    property ParseMode: TD2XParseMode read fParseMode;
    property ResultPer: TD2XResultPer read fResultPer;
    property FinalToken: Boolean read fFinalToken;

    constructor Create;
    destructor Destroy; override;

    function ParseOption(pOpt: string): boolean;
    function ReportOptions: boolean;
    procedure ShowOptions;
  end;

implementation

uses
  System.Rtti,
  System.TypInfo;

type
  TD2XSetterFunc = function(pVal: string): Boolean of object;

{ TD2XOptions }

procedure TD2XOptions.AddDefine(pDef: string);
begin
  fLoadDefines := True;
  if fDefines.IndexOf(pDef) < 0 then
    fDefines.Add(pDef);
end;

constructor TD2XOptions.Create;
begin
  inherited;
  fLogErrors := True;
  fLogNotSupported := False;
  fTimestampFiles := False;
  fVerbose := False;
  fUseBase := False;
  fBaseDirectory := '';
  fUseInput := False;
  fInputDirectory := '';
  fUseOutput := True;
  fOutputDirectory := 'Log\';
  fXml := True;
  fXmlDirectory := '';
  fWriteDefines := False;
  fDefinesDirectory := '';
  fDefinesUsed := True;
  fUsedExtension := '.used';
  fCountChildren := True;
  fCountExtension := '.cnt';
  fLoadDefines := True;
  fLoadExtension := '.def';
  fSkipMethods := True;
  fSkipExtension := '.skip';
  fDefines := TStringList.Create;
  fDefines.Sorted := True;
  fParseMode := pmFull;
  fResultPer := rpFile;
  fFinalToken := False;
end;

procedure TD2XOptions.DeleteDefine(pDef: string);
var
  lIdx: Integer;
begin
  lIdx := fDefines.IndexOf(pDef);
  if lIdx >= 0 then
  begin
    fDefines.Delete(lIdx);
    fLoadDefines := True;
  end;
end;

destructor TD2XOptions.Destroy;
begin
  FreeAndNil(fDefines);
  inherited;
end;

procedure TD2XOptions.LoadDefinesFile(pFile: string);
begin
  if pFile = '' then
    fDefines.Clear
  else
    if StartsText('.', pFile) then
      fDefines.LoadFromFile(ChangeFileExt(ParamStr(0), pFile))
    else
      fDefines.LoadFromFile(pFile);
end;

function TD2XOptions.ParseOption(pOpt: string): boolean;
  function ErrorUnlessSet(out pFlag: Boolean): Boolean;
  begin
    Result := False;
    if (Length(pOpt) = 2) or (pOpt[3] = '+') then
      pFlag := True
    else
      if pOpt[3] = '-' then
        pFlag := False
      else
        Result := True;
  end;
  function ErrorUnlessValue(out pVal: string): Boolean;
  begin
    Result := False;
    if (Length(pOpt) > 2) and (pOpt[3] = ':') then
      pVal := Copy(pOpt, 4, 99)
    else
      Result := True;
  end;
  function ErrorUnlessSetter(pFunc: TD2XSetterFunc): Boolean;
  begin
    Result := False;
    if (Length(pOpt) > 2) and (pOpt[3] = ':') then
      Result := not pFunc(Copy(pOpt, 4, 99))
    else
      Result := True;
  end;
  function ErrorUnlessSetValue(out pFlag: Boolean; out pVal: string): Boolean;
  begin
    Result := False;
    if ErrorUnlessSet(pFlag) then
      if pOpt[3] = ':' then
      begin
        pFlag := True;
        pVal := Copy(pOpt, 4, 99)
      end
      else
        Result := True;
  end;
  function ErrorUnlessSetExtension(out pFlag: Boolean; out pExtn: string;
    pDflt: string): Boolean;
  begin
    Result := False;
    if ErrorUnlessSetValue(pFlag, pExtn) then
      Result := True
    else
      if pExtn = '' then
        pExtn := pDflt
      else
        if pExtn[1] <> '.' then
          pExtn := '.' + pExtn;
  end;
  function ErrorUnlessSetDir(out pFlag: Boolean; out pDir: string): Boolean;
  begin
    Result := False;
    if ErrorUnlessSetValue(pFlag, pDir) then
      Result := True
    else
      if pDir > '' then
        pDir := IncludeTrailingPathDelimiter(pDir);
  end;

var
  lDefine: string;
begin
  Result := False;
  if (Length(pOpt) < 2) or not CharInSet(pOpt[1], ['-', '/']) then
    Writeln('Invalid option: ' + pOpt)
  else
    case pOpt[2] of
      '?':
        Result := False;
      '!':
        Result := ReportOptions;
      'E', 'e':
        if ErrorUnlessSet(fLogErrors) then
          Writeln('Invalid Log Error messages option: ' + pOpt)
        else
          Result := True;
      'N', 'n':
        if ErrorUnlessSet(fLogNotSupported) then
          Writeln('Invalid Log Not Supported messages option: ' + pOpt)
        else
          Result := True;
      'T', 't':
        if ErrorUnlessSet(fTimestampFiles) then
          Writeln('Invalid Timestamp Files option: ' + pOpt)
        else
          Result := True;
      'V', 'v':
        if ErrorUnlessSet(fVerbose) then
          Writeln('Invalid Verbose option: ' + pOpt)
        else
          Result := True;
      'F', 'f':
        if ErrorUnlessSet(fFinalToken) then
          Writeln('Invalid Final token option: ' + pOpt)
        else
          Result := True;
      'R', 'r':
        if ErrorUnlessSet(fRecurse) then
          Writeln('Invalid Recurse Directories option: ' + pOpt)
        else
          Result := True;
      'M', 'm':
        if ErrorUnlessSetter(SetParseMode) then
          Writeln('Invalid Parse mode option: ' + pOpt)
        else
          Result := True;
      'P', 'p':
        if ErrorUnlessSetter(SetResultPer) then
          Writeln('Invalid Result per option: ' + pOpt)
        else
          Result := True;
      'D', 'd':
        if ErrorUnlessValue(lDefine) then
          Writeln('Invalid Define option: ' + pOpt)
        else
        begin
          AddDefine(lDefine);
          Result := True;
        end;
      'Z', 'z':
        if ErrorUnlessValue(lDefine) then
          Writeln('Invalid Undefine option: ' + pOpt)
        else
        begin
          DeleteDefine(lDefine);
          Result := True;
        end;
      'L', 'l':
        if ErrorUnlessSetValue(fLoadDefines, fLoadExtension) then
          Writeln('Invalid Load Defines option: ' + pOpt)
        else
        begin
          LoadDefinesFile(fLoadExtension);
          Result := True;
        end;
      'W', 'w':
        if ErrorUnlessSetDir(fWriteDefines, fDefinesDirectory) then
          Writeln('Invalid Write Defines option: ' + pOpt)
        else
          Result := True;
      'B', 'b':
        if ErrorUnlessSetDir(fUseBase, fBaseDirectory) then
          Writeln('Invalid Use Base Directory option: ' + pOpt)
        else
          Result := True;
      'I', 'i':
        if ErrorUnlessSetDir(fUseInput, fInputDirectory) then
          Writeln('Invalid Use Input Directory option: ' + pOpt)
        else
          Result := True;
      'O', 'o':
        if ErrorUnlessSetDir(fUseOutput, fOutputDirectory) then
          Writeln('Invalid Use Output Directory option: ' + pOpt)
        else
          Result := True;
      'X', 'x':
        if ErrorUnlessSetDir(fXml, fXmlDirectory) then
          Writeln('Invalid Xml option: ' + pOpt)
        else
          Result := True;
      'U', 'u':
        if ErrorUnlessSetExtension(fDefinesUsed, fUsedExtension, '.used') then
          Writeln('Invalid Count Defines Used option: ' + pOpt)
        else
          Result := True;
      'C', 'c':
        if ErrorUnlessSetExtension(fCountChildren, fCountExtension, '.cnt') then
          Writeln('Invalid Count Children option: ' + pOpt)
        else
          Result := True;
      'S', 's':
        if ErrorUnlessSetExtension(fSkipMethods, fSkipExtension, '.skip') then
          Writeln('Invalid Load Skipped Methods option: ' + pOpt)
        else
          Result := True;
    else
      Writeln('Unknown option: ' + pOpt);
    end;
end;

function TD2XOptions.ReportOptions: boolean;
  function ShowEnabled(pOpt: Boolean; pLabel, pVal: string): string;
  begin
    if pOpt then
    begin
      if pVal > '' then
        Result := 'Enabled  ' + pLabel + pVal
      else
        Result := 'Enabled  ';
    end
    else
      Result := 'Disabled ';
  end;

var
  lS: string;
  w: Integer;
  procedure WriteWidth(pStr: string);
  begin
    write(pStr);
    Inc(w, Length(pStr));
  end;

begin
  Result := True;
  Writeln('Current option settings:');
  Writeln('  Parse Mode              ', TD2X.EnumLabel(fParseMode));
  Writeln('  Result per              ', TD2X.EnumLabel(fResultPer));
  Writeln('  Errors                  ', ShowEnabled(fLogErrors, '', ''));
  Writeln('  Not Supported           ', ShowEnabled(fLogNotSupported, '', ''));
  Writeln('  Timestamp Files         ', ShowEnabled(fTimestampFiles, '', ''));
  Writeln('  Verbose                 ', ShowEnabled(fVerbose, '', ''));
  Writeln('  Recurse                 ', ShowEnabled(fRecurse, '', ''));
  Writeln('  Show Final token        ', ShowEnabled(fFinalToken, '', ''));
  Writeln('  Write defines           ', ShowEnabled(fWriteDefines, 'Dir  ',
      fDefinesDirectory));
  Writeln('  Directory base          ', ShowEnabled(fUseBase, 'Dir  ', fBaseDirectory));
  Writeln('  Input base              ', ShowEnabled(fUseInput, 'Dir  ', fInputDirectory));
  Writeln('  Output base             ', ShowEnabled(fUseOutput, 'Dir  ', fOutputDirectory));
  Writeln('  Xml output              ', ShowEnabled(fXml, 'Dir  ', fXmlDirectory));
  Writeln('  Count defines used      ', ShowEnabled(fDefinesUsed, 'Extn ', fUsedExtension));
  Writeln('  Count min/max children  ', ShowEnabled(fCountChildren, 'Extn ', fCountExtension));
  Writeln('  Skip methods in         ', ShowEnabled(fSkipMethods, 'Extn ', fSkipExtension));
  if fLoadDefines then
    if fDefines.Count < 1 then
      Write('Use NO Defines')
    else
    begin
      Writeln('Use these Defines:');
      w := 0;
      for lS in fDefines do
      begin
        if w = 0 then
          WriteWidth('    ')
        else
          if (w + Length(lS)) > 78 then
          begin
            Writeln;
            w := 0;
            WriteWidth('    ');
          end
          else
            WriteWidth(', ');
        WriteWidth(lS);
      end;
    end
  else
    Write('Use default Defines');
  Writeln;
end;

function TD2XOptions.SetParseMode(pVal: string): Boolean;
begin
  case pVal[1] of
    'U', 'u':
      fParseMode := pmUses;
    else
      fParseMode := pmFull;
  end;
end;

function TD2XOptions.SetResultPer(pVal: string): Boolean;
begin
  case pVal[1] of
    'R', 'r':
      fResultPer := rpRun;
    'P', 'p':
      fResultPer := rpParam;
    'W', 'w':
      fResultPer := rpWildcard;
    'S', 's':
      fResultPer := rpSubDir;
    'D', 'd':
      fResultPer := rpDir;
    else
      fResultPer := rpFile;
  end;
end;

procedure TD2XOptions.ShowOptions;
var
  lBase: string;
begin
  lBase := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  Writeln('Usage: ', lBase, ' [ Option | @Params | mFilename | Wildcard ] ... ');
  Writeln('  Options:        Default   Description');
  Writeln('    E[+-]         -         Log Error messages');
  Writeln('    F[+-]         -         Record Final Token');
  Writeln('    N[+-]         -         Log Not Supported messages');
  Writeln('    T[+-]         -         Timestamp global output files');
  Writeln('    V[+-]         -         Log all Parser methods called');
  Writeln('    R[+-]         +         Recurse into subdirectories');
  Writeln('    D:<define>              Define <define> (also enables "Load Defines")');
  Writeln('    Z:<define>              Undefine <define> (also enables "Load Defines")');
  Writeln('    M:<mode>                Set Parsing mode (F[ull], U[ses])');
  Writeln('    P:<Per>                 Set Result per (F[ile], [S]ubdir, D[ir], W[ildcard], P[aram], R[un])');
  Writeln('    L[+-]|:<file> -         Load Defines from <file> (no <file> clears all defines)');
  Writeln('    W[+-]|:<dir>  -         Generate Final Defines files into current or given <dir>');
  Writeln('    B[+-]|:<dir>  -         Use <dir> as a base for all file lookups');
  Writeln('    I[+-]|:<dir>  -         Use <dir> as a base for all file input');
  Writeln('    O[+-]|:<dir>  -         Use <dir> as a base for all file output');
  Writeln('    X[+-]|:<dir>  +         Generate XML files into current or given <dir>');
  Writeln('    C[+-]|:<ext>  +:cnt     Report Min/Max Children into ', lBase, '.<ext>');
  Writeln('    U[+-]|:<ext>  +:used    Report Defines Used into ', lBase, '.<ext>');
  Writeln('    S[+-]|:<ext>  +:skip    Load Skipped Methods from ', lBase, '.<ext>');
  // Available option letters: AGHJKQY
end;

{ TD2X }

class function TD2X.EnumLabel<T>(pVal: T): String;
var
  lV: TValue;
begin
  lV := TValue.From<T>(pVal);
  Result := Copy(lV.ToString, 3, 99);
end;

end.
