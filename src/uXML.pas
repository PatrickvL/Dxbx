unit uXML;

interface

uses
  // Delphi
  SysUtils, XMLIntf;

procedure XML_WriteString(const aXMLNode: IXMLNode; const aElementName: string; const aString: string);
function XML_ReadString(const aXMLNode: IXMLNode; const aElementName: string): string;
procedure XML_WriteDateTime(const aXMLNode: IXMLNode; const aElementName: string; const aDateTime: TDateTime);


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

procedure XML_WriteDateTime(const aXMLNode: IXMLNode; const aElementName: string; const aDateTime: TDateTime);
begin
  XML_WriteString(aXMLNode, aElementName, DateTimeToStr(aDateTime));
end;


end.
