unit uXML;

interface

uses
  // Delphi
  XMLIntf; // for IXMLNode

procedure XML_WriteString(const aXMLNode: IXMLNode; const aElementName: string; const aString: string);
function XML_ReadString(const aXMLNode: IXMLNode; const aElementName: string): string;

implementation

procedure XML_WriteString(const aXMLNode: IXMLNode; const aElementName: string; const aString: string);
var
  SubNode: IXMLNode;
begin
  SubNode := aXMLNode.AddChild(aElementName);
  SubNode.Text := aString;
end;

function XML_ReadString(const aXMLNode: IXMLNode; const aElementName: string): string;
var
  SubNode: IXMLNode;
begin
  SubNode := aXMLNode.ChildNodes.FindNode(aElementName);
  if Assigned(SubNode) then
    Result := SubNode.Text
  else
    Result := '';
end;

end.
