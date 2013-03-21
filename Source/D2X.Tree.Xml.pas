unit D2X.Tree.Xml;

interface

uses
  D2X.Tree;

function XmlTreeWriter: TD2XTreeWriterClass;

implementation

uses
  Data.Cloud.CloudAPI;

type
  TD2XXmlWriter = class(TD2XTreeWriter)
  protected
    procedure WriteAttribute(pNode: TD2XTreeElement; pAttr: TD2XTreeNode); override;
    procedure WriteDoc(pNode: TD2XTreeDoc); override;

    procedure WriteHead(pNode: TD2XTreeElement); override;
    procedure WriteElHead(pNode: TD2XTreeElement); override;
    procedure WriteChildLine(pNode: TD2XTreeElement; pLine: string; pFirst: Boolean); override;
    procedure WriteElText(pNode: TD2XTreeElement); override;
    procedure WriteElFoot(pNode: TD2XTreeElement); override;
    procedure WriteText(pNode: TD2XTreeElement); override;
    procedure WriteAttrHead(pNode: TD2XTreeElement); override;
    procedure WriteRawText(pNode: TD2XTreeElement); override;

  end;

function XmlTreeWriter: TD2XTreeWriterClass;
begin
  Result := TD2XXmlWriter;
end;

{ TD2XXmlWriter }

procedure TD2XXmlWriter.WriteAttrHead(pNode: TD2XTreeElement);
begin
  fW.Write('<');
  fW.Write(pNode.LocalName);
  WriteAttributes(pNode);
  fW.Write(' />');
  if HasOption(pNode, toAutoIndent) then
    fW.WriteLine;
end;

procedure TD2XXmlWriter.WriteAttribute(pNode: TD2XTreeElement; pAttr: TD2XTreeNode);
begin
  fW.Write(' ');
  fW.Write(pAttr.LocalName);
  fW.Write('="');
  fW.Write(XmlEscape(pAttr.Text));
  fW.Write('"');
end;

procedure TD2XXmlWriter.WriteChildLine(pNode: TD2XTreeElement; pLine: string; pFirst: Boolean);
begin
  if HasOption(pNode, toAutoIndent) then
    fW.WriteLine('  ' + pLine)
  else
    fW.Write(pLine);
end;

procedure TD2XXmlWriter.WriteDoc(pNode: TD2XTreeDoc);
begin
  fW.WriteLine('<?xml version="1.0"?>');
  inherited;
end;

procedure TD2XXmlWriter.WriteElFoot(pNode: TD2XTreeElement);
begin
  fW.Write('</');
  fW.Write(pNode.LocalName);
  fW.Write('>');
  if HasOption(pNode, toAutoIndent) then
    fW.WriteLine;
end;

procedure TD2XXmlWriter.WriteElHead(pNode: TD2XTreeElement);
begin
  fW.Write('<');
  fW.Write(pNode.LocalName);
  WriteAttributes(pNode);
  fW.Write('>');
  if HasOption(pNode, toAutoIndent) then
    fW.WriteLine;
end;

procedure TD2XXmlWriter.WriteElText(pNode: TD2XTreeElement);
begin
  fW.Write(pNode.Text)
end;

procedure TD2XXmlWriter.WriteHead(pNode: TD2XTreeElement);
begin
  fW.Write('<');
  fW.Write(pNode.LocalName);
  fW.Write(' />');
  if HasOption(pNode, toAutoIndent) then
    fW.WriteLine;
end;

procedure TD2XXmlWriter.WriteRawText(pNode: TD2XTreeElement);
begin
  fW.Write(pNode.Text);
end;

procedure TD2XXmlWriter.WriteText(pNode: TD2XTreeElement);
begin
  fW.Write('<');
  fW.Write(pNode.LocalName);
  WriteAttributes(pNode);
  fW.Write('>');
  fW.Write(pNode.Text);
  WriteElFoot(pNode);
end;

end.
