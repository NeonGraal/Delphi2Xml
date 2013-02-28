unit D2X.Tree.Json;

interface

uses
  D2X.Tree,
  System.Classes;

type
  TD2XJsonWriter = class(TD2XTreeWriter)
  protected
    procedure WriteAttribute(pNode: TD2XTreeNode; pW: TTextWriter); override;
    procedure WriteElement(pNode: TD2XTreeElement; pW: TTextWriter); override;
    //    procedure WriteDoc(pNode: TD2XTreeDoc; pW: TTextWriter); override;

  end;

implementation

uses
  System.SysUtils;

{ TD2XJsonWriter }

procedure TD2XJsonWriter.WriteAttribute(pNode: TD2XTreeNode; pW: TTextWriter);
begin
  pW.Write('_');
  pW.Write(pNode.LocalName);
  pW.Write(':"');
  //  pW.Write(JsonEscape(pNode.Text));
  pW.Write(pNode.Text);
  pW.Write('"');
end;

type
  TProcRef = reference to procedure;

procedure TD2XJsonWriter.WriteElement(pNode: TD2XTreeElement; pW: TTextWriter);
var
  lSep: string;
  lWS: TProcRef;

begin
  if pNode.LocalName > '' then
  begin
    pW.Write(pNode.LocalName);
    pW.Write(':');
    if pNode.HasChildren or pNode.HasAttributes then
    begin
      pW.Write('{');
      lWS := procedure
        begin
          if HasOption(pNode, toAutoIndent) then
            pW.WriteLine;
          pW.Write(lSep);
          if HasOption(pNode, toAutoIndent) then
            lSep := ', '
          else
            lSep := ',';
        end;
      if HasOption(pNode, toAutoIndent) then
        lSep := '  '
      else
        lSep := '';
      ForeachAttribute(pNode,
          procedure(pAttr: TD2XTreeNode)
        begin
          lWS;
          WriteAttribute(pAttr, pW);
        end);

      if pNode.Text > '' then
      begin
        lWS;
        pW.Write('_:"');
        pW.Write(pNode.Text);
        pW.Write('"');
      end;
      if HasOption(pNode, toAutoIndent) then
      begin
        //        pW.WriteLine;
        ForeachChild(pNode,
          procedure(pChild: TD2XTreeNode)
          var
            lR: TStreamReader;
            lL, lS: string;
          begin
            lWS;
            try
              lR := TStreamReader.Create(pChild.GetStream);
              lR.BaseStream.Seek(0, soBeginning);
              lS := '';
              while not lR.EndOfStream do
              begin
                if HasOption(pNode, toAutoIndent) and (lS > '') then
                  pW.WriteLine;
                lL := lR.ReadLine;
                pW.Write(lS);
                pW.Write(lL);
                lS := '  ';
              end;
            finally
              FreeAndNil(lR);
            end;
          end);
      end
      else
        ForeachChild(pNode,
          procedure(pChild: TD2XTreeNode)
          begin
            lWS;
            pW.Write(pChild.GetStream.DataString);
          end);
      if HasOption(pNode, toAutoIndent) then
        pW.WriteLine;
      pW.Write('}');
      lWS := nil;
    end
    else
    begin
      pW.Write('"');
      pW.Write(pNode.Text);
      pW.Write('"');
    end;
    //    if HasOption(pNode, toAutoIndent) then
    //      pW.WriteLine;
  end
  else
    pW.Write(pNode.Text);
end;

end.
