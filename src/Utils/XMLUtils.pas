unit XMLUtils;

interface

uses
  System.Classes, Winapi.msxml;

type
  TStoreFormStruct = record
    Width: Integer;
    Height: Integer;
    Left: Integer;
    Top: Integer;
    Maximized: Boolean;
  end;

function GetArrayIndexByName(const AName: string; const Arr: array of string): Integer;
procedure StringsToXML(AStrings: TStrings; ANode: IXMLDOMNode);
procedure XMLToStrings(AStrings: TStrings; ANode: IXMLDOMNode);
procedure FormToXML(const AStore: TStoreFormStruct; ANode: IXMLDOMNode);
procedure XMLToForm(var AStore: TStoreFormStruct; ANode: IXMLDOMNode);
procedure SaveXMLToFile(const AFileName: string; ADomDoc: IXMLDOMDocument);
procedure AddCDataNodeName(const AName: string);

implementation

uses
  SysUtils, JobConsts, Windows;

const
  CDataXMLFormat = '<xsl:output indent="yes" method="xml" encoding="UTF-8" cdata-section-elements="%s"/>';
  XMLFormat = '<?xml version="1.0" encoding="UTF-8"?>' +
              '<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">' +
              '  <xsl:output indent="yes" method="xml" encoding="UTF-8"/> %s ' +
              '    <xsl:template match="/ | @* | node()">' +
              '  <xsl:copy>' +
              '  <xsl:apply-templates select="@* | node()"/>' +
              '    </xsl:copy>' +
              '  </xsl:template>' +
              '</xsl:stylesheet>';

var
  CDataNodeNames: TStrings = nil;

procedure AddCDataNodeName(const AName: string);
begin
  if (CDataNodeNames = nil) then
  begin
    CDataNodeNames := TStringList.Create();
  end;
  if (CDataNodeNames.IndexOf(AName) < 0) then
  begin
    CDataNodeNames.Add(AName);
  end;
end;
              
procedure SaveXMLToFile(const AFileName: string; ADomDoc: IXMLDOMDocument);
  function GetXMLFormat: string;
  var
    i: Integer;
  begin
    Result := '';
    if (CDataNodeNames <> nil) then
    begin
      for i := 0 to CDataNodeNames.Count - 1 do
      begin
        Result := Result + #32 + Format(CDataXMLFormat, [CDataNodeNames[i]]);
      end;
    end;
    Result := Format(XMLFormat, [Result]);
  end;
  
var
  b: Boolean;
  TransDoc, ResDoc: IXMLDOMDocument;
begin
  TransDoc := CoDOMDocument.Create();
  ResDoc := CoDOMDocument.Create();
  TransDoc.loadXML(GetXMLFormat());
  try
    ADomDoc.transformNodeToObject(TransDoc, ResDoc);
    b := (ResDoc.xml <> '');
  except
    b := False;
  end;
  if b then
  begin
    ResDoc.save(AFileName);
  end else
  begin
    ADomDoc.save(AFileName);
  end;
end;

function GetArrayIndexByName(const AName: string; const Arr: array of string): Integer;
begin
  for Result := Low(Arr) to High(Arr) do
  begin
    if (CompareText(Arr[Result], AName) = 0) then Exit;
  end;
  Result := -1;
end;

procedure StringsToXML(AStrings: TStrings; ANode: IXMLDOMNode);
var
  ChildNode: IXMLDOMNode;
begin
  ChildNode := ANode.ownerDocument.createCDATASection(AStrings.Text);
  ANode.appendChild(ChildNode);
end;

procedure XMLToStrings(AStrings: TStrings; ANode: IXMLDOMNode);
begin
  AStrings.Text := ANode.text;
end;

procedure FormToXML(const AStore: TStoreFormStruct; ANode: IXMLDOMNode);
var
  ChildNode: IXMLDOMNode;
begin
  ChildNode := ANode.ownerDocument.createElement('Height');
  ANode.appendChild(ChildNode);
  ChildNode.text := IntToStr(AStore.Height);

  ChildNode := ANode.ownerDocument.createElement('Width');
  ANode.appendChild(ChildNode);
  ChildNode.text := IntToStr(AStore.Width);

  ChildNode := ANode.ownerDocument.createElement('Left');
  ANode.appendChild(ChildNode);
  ChildNode.text := IntToStr(AStore.Left);

  ChildNode := ANode.ownerDocument.createElement('Top');
  ANode.appendChild(ChildNode);
  ChildNode.text := IntToStr(AStore.Top);

  ChildNode := ANode.ownerDocument.createElement('Maximized');
  ANode.appendChild(ChildNode);
  ChildNode.text := cStoreBoolean[AStore.Maximized];
end;

procedure XMLToForm(var AStore: TStoreFormStruct; ANode: IXMLDOMNode);
var
  v: Integer;
  ChildNode: IXMLDOMNode;
begin
  ZeroMemory(@AStore, SizeOf(AStore));

  ChildNode := ANode.selectSingleNode('Height');
  if ChildNode <> nil then
  begin
    AStore.Height := StrToIntDef(ChildNode.text, 0);
  end;

  ChildNode := ANode.selectSingleNode('Width');
  if ChildNode <> nil then
  begin
    AStore.Width := StrToIntDef(ChildNode.text, 0);
  end;

  ChildNode := ANode.selectSingleNode('Left');
  if ChildNode <> nil then
  begin
    AStore.Left := StrToIntDef(ChildNode.text, 0);
  end;

  ChildNode := ANode.selectSingleNode('Top');
  if ChildNode <> nil then
  begin
    AStore.Top := StrToIntDef(ChildNode.text, 0);
  end;

  ChildNode := ANode.selectSingleNode('Maximized');
  if ChildNode <> nil then
  begin
    v := GetArrayIndexByName(ChildNode.text, cStoreBoolean);
    if (v > -1) then AStore.Maximized := Boolean(v);
  end;
end;

initialization

finalization
  CDataNodeNames.Free();

end.
