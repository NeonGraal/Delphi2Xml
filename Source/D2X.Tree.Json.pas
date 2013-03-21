unit D2X.Tree.Json;

interface

uses
  D2X.Tree;

function JsonTreeWriter: TD2XTreeWriterClass;

implementation

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils;

type
  TProcRef = reference to procedure;

  TD2XJsonWriter = class(TD2XTreeWriter)
  private
    fSeps: TStack<string>;

    procedure WS(pNode: TD2XTreeElement);

  protected
    procedure WriteAttribute(pNode: TD2XTreeElement; pAttr: TD2XTreeNode); override;

    procedure WriteHead(pNode: TD2XTreeElement); override;
    procedure WriteElHead(pNode: TD2XTreeElement); override;
    procedure WriteChildLine(pNode: TD2XTreeElement; pLine: string; pFirst: Boolean); override;
    procedure WriteElText(pNode: TD2XTreeElement); override;
    procedure WriteElFoot(pNode: TD2XTreeElement); override;
    procedure WriteText(pNode: TD2XTreeElement); override;
    procedure WriteRawText(pNode: TD2XTreeElement); override;

  public
    constructor Create(pStream: TStringStream); override;
    destructor Destroy; override;

  end;

function JsonTreeWriter: TD2XTreeWriterClass;
begin
  Result := TD2XJsonWriter;
end;

{ TD2XJsonWriter }

constructor TD2XJsonWriter.Create(pStream: TStringStream);
begin
  inherited;

  fSeps := TStack<string>.Create;
end;

destructor TD2XJsonWriter.Destroy;
begin
  FreeAndNil(fSeps);

  inherited;
end;

procedure TD2XJsonWriter.WriteAttribute(pNode: TD2XTreeElement; pAttr: TD2XTreeNode);
begin
  WS(pNode);
  fW.Write('_');
  fW.Write(pAttr.LocalName);
  fW.Write(':"');
  //  fW.Write(JsonEscape(pNode.Text));
  fW.Write(pAttr.Text);
  fW.Write('"');
end;

procedure TD2XJsonWriter.WriteChildLine(pNode: TD2XTreeElement; pLine: string;
  pFirst: Boolean);
begin
  if pFirst then
    WS(pNode)
  else
  begin
    if HasOption(pNode, toAutoIndent) then
      fW.WriteLine;
    fW.Write('  ');
  end;
  fW.Write(pLine);
end;

procedure TD2XJsonWriter.WriteElFoot(pNode: TD2XTreeElement);
begin
  if HasOption(pNode, toAutoIndent) then
    fW.WriteLine;
  fSeps.Pop;
  fW.Write('}');
end;

procedure TD2XJsonWriter.WriteElHead(pNode: TD2XTreeElement);
begin
  fW.Write(pNode.LocalName);
  fW.Write(':{');
  if HasOption(pNode, toAutoIndent) then
    fSeps.Push('  ')
  else
    fSeps.Push('');
  WriteAttributes(pNode);
end;

procedure TD2XJsonWriter.WriteElText(pNode: TD2XTreeElement);
begin
  WS(pNode);
  fW.Write('_:"');
  fW.Write(pNode.Text);
  fW.Write('"');
end;

procedure TD2XJsonWriter.WriteHead(pNode: TD2XTreeElement);
begin
  fW.Write(pNode.LocalName);
  fW.Write(':{}');
end;

procedure TD2XJsonWriter.WriteRawText(pNode: TD2XTreeElement);
begin
  fW.Write(pNode.Text);
end;

procedure TD2XJsonWriter.WriteText(pNode: TD2XTreeElement);
begin
  if pNode.HasAttributes then
  begin
    WriteElHead(pNode);
    WriteElText(pNode);
    WriteElFoot(pNode);
  end
  else
  begin
    fW.Write(pNode.LocalName);
    fW.Write(':"');
    fW.Write(pNode.Text);
    fW.Write('"');
  end;
end;

procedure TD2XJsonWriter.WS(pNode: TD2XTreeElement);
var
  lSep: string;
begin
  lSep := fSeps.Pop;
  if HasOption(pNode, toAutoIndent) then
  begin
    fW.WriteLine;
    fSeps.Push(', ');
  end
  else
    fSeps.Push(',');
  fW.Write(lSep);
end;

end.
