unit ufrmMAin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ImgList, ComCtrls, ToolWin, ActnList, xmldom, XMLIntf,
  msxmldom, XMLDoc, ExtCtrls, StdCtrls, FileCtrl;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    SaveAs1: TMenuItem;
    Open1: TMenuItem;
    ActionList1: TActionList;
    actClose: TAction;
    actOpenStatusXml: TAction;
    OpenDialog: TOpenDialog;
    actTreeExpand: TAction;
    actTreeCollapse: TAction;
    PageControl1: TPageControl;
    ToolButton3: TToolButton;
    actGenerateCharts: TAction;
    XMLDocument1: TXMLDocument;
    ToolButton4: TToolButton;
    actSaveCharts: TAction;
    StatusBar1: TStatusBar;
    procedure actCloseExecute(Sender: TObject);
    procedure actGenerateChartsExecute(Sender: TObject);
    procedure actOpenStatusXmlExecute(Sender: TObject);
    procedure actSaveChartsExecute(Sender: TObject);
  private
    procedure CreateHtmlPages ( aMemo : TMemo; XmlName : String; Items : Integer );
    procedure CreateBeginBlockChart( aMemo : TMemo );
    procedure CreateEndBlockChart( aMemo : TMemo; ItemCount : Integer; ReportCaption : String );
    Function CreateMainStatusChart ( aMemo : TMemo ) : Integer;
    procedure CreateSubItemCharts;
  end;

  TChartTab = class ( TTabSheet )
  private
    aChartMemo : TMemo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ChartMemo : TMemo read aChartMemo write aChartMemo;
 end;

var Form1 : TForm1;

implementation

{$R *.dfm}

procedure TForm1.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.actGenerateChartsExecute(Sender: TObject);
var
  Height : Integer;
begin
  with TChartTab.Create( PageControl1 ) do begin
    Caption := 'Main.Xml';
    Height := CreateMainStatusChart ( aChartMemo );
  end;

  with TChartTab.Create( PageControl1 ) do begin
    Caption := 'Main.Html';
    CreateHtmlPages ( aChartMemo, 'Main.Xml', Height );
  end;

  CreateSubItemCharts;
end;

procedure TForm1.actOpenStatusXmlExecute(Sender: TObject);
begin
  if OpenDialog.Execute then begin
    XMLDocument1.LoadFromFile( OpenDialog.FileName );
    XMLDocument1.Active := true;
    StatusBar1.SimpleText := OpenDialog.FileName + ' loaded...';
  end;
end;

procedure TForm1.actSaveChartsExecute(Sender: TObject);
var Cnt : Integer;
  Dir: string;
begin
  if SelectDirectory('Select directory to save files to', '', Dir) then begin
    for Cnt := 0 to PageControl1.PageCount - 1 do begin
      TChartTab (PageControl1.Pages[Cnt]).aChartMemo.Lines.SaveToFile( Dir + '\' + PageControl1.Pages[Cnt].Caption );
    end;    
  end;
  ShowMessage ( 'Files saved!' );
end;

procedure TForm1.CreateBeginBlockChart(aMemo: TMemo);
begin
  aMemo.Lines.Add ('<chart>');
  aMemo.Lines.Add ('');
	aMemo.Lines.Add ('<axis_category shadow=''low'' size=''14'' color=''88bb77'' alpha=''100'' orientation=''horizontal'' />');
	aMemo.Lines.Add ('<axis_ticks value_ticks=''false'' category_ticks=''true'' major_thickness=''2'' minor_thickness=''1'' minor_count=''1'' major_color=''222222'' minor_color=''222222'' position=''centered'' />');
	aMemo.Lines.Add ('<axis_value min=''0'' max=''100'' alpha=''0'' />');
  aMemo.Lines.Add ('');
	aMemo.Lines.Add ('<chart_border color=''000000'' top_thickness=''0'' bottom_thickness=''0'' left_thickness=''4'' right_thickness=''0'' />');
end;

procedure TForm1.CreateEndBlockChart(aMemo: TMemo; ItemCount : Integer; ReportCaption : String);
begin
	aMemo.Lines.Add ('<chart_grid_h alpha=''5'' color=''000000'' thickness=''30'' />');
	aMemo.Lines.Add ('<chart_label shadow=''low'' color=''ffffff'' alpha=''75'' size=''12'' position=''center'' suffix=''%''  />');
	aMemo.Lines.Add ('<chart_rect shadow=''high'' x=''600'' y=''105'' width=''320'' height=''' + IntToStr(ItemCount * 20) + '''positive_color=''dba34c'' negative_color=''ff0000'' positive_alpha=''65'' negative_alpha=''25'' corner_tl=''0'' corner_tr=''30'' corner_br=''30'' corner_bl=''0'' />');
	aMemo.Lines.Add ('<chart_type>bar</chart_type>');
  aMemo.Lines.Add ('');
	aMemo.Lines.Add ('<draw>');
	aMemo.Lines.Add ('	<text shadow=''low'' color=''ff8800'' alpha=''100'' size=''20'' x=''35'' y=''65'' width=''500'' height=''200'' h_align=''left'' v_align=''top''>' + ReportCaption + '</text>');
	aMemo.Lines.Add ('</draw>');
	aMemo.Lines.Add ('<filter>');
	aMemo.Lines.Add ('	<bevel id=''data'' angle=''90'' blurX=''10'' blurY=''10'' distance=''5'' highlightAlpha=''10'' shadowAlpha=''20'' type=''full'' />');
	aMemo.Lines.Add ('	<bevel id=''small'' angle=''45'' blurX=''2'' blurY=''2'' distance=''1'' highlightAlpha=''35'' highlightColor=''ffffff'' shadowColor=''000000'' shadowAlpha=''35'' type=''inner'' />');
	aMemo.Lines.Add ('	<shadow id=''high'' distance=''5'' angle=''45'' alpha=''35'' blurX=''15'' blurY=''15'' />');
	aMemo.Lines.Add ('	<shadow id=''low'' distance=''2'' angle=''45'' alpha=''50'' blurX=''5'' blurY=''5'' />');
	aMemo.Lines.Add ('</filter>');
  aMemo.Lines.Add ('');
	aMemo.Lines.Add ('<legend layout=''hide'' />');
  aMemo.Lines.Add ('');
	aMemo.Lines.Add ('<series_color>');
	aMemo.Lines.Add ('	<color>5a4b6e</color>');
	aMemo.Lines.Add ('</series_color>');
  aMemo.Lines.Add ('');
  aMemo.Lines.Add ('</chart>');
end;

procedure TForm1.CreateHtmlPages(aMemo: TMemo; XmlName : String; Items : Integer);
begin
  aMemo.Lines.Add ('<HTML>');
  aMemo.Lines.Add ('');
  aMemo.Lines.Add ('	<HEAD>');
	aMemo.Lines.Add ('	<link rel="stylesheet" type="text/css" href="../dxbx.css"/>');
	aMemo.Lines.Add ('	<TITLE>Shadow_tj - DXBX</TITLE>');
	aMemo.Lines.Add ('	<META NAME="description" CONTENT="DXBX - XBOX EMULATION">');
	aMemo.Lines.Add ('	<META NAME="keywords" CONTENT="xbox, xbox emu, xbox emulation, cxbx, dxbx">');
	aMemo.Lines.Add ('	<META NAME="robot" CONTENT="index,follow">');
	aMemo.Lines.Add ('	<META NAME="author" CONTENT="shadowtj@shadowtj.org">');
	aMemo.Lines.Add ('	<META NAME="revisit-after" CONTENT="20">');
	aMemo.Lines.Add ('</HEAD>');

  aMemo.Lines.Add ('<script language="javascript">AC_FL_RunContent = 0;</script>');
  aMemo.Lines.Add ('<script language="javascript"> DetectFlashVer = 0; </script>');
  aMemo.Lines.Add ('<script src="AC_RunActiveContent.js" language="javascript"></script>');
  aMemo.Lines.Add ('<script language="JavaScript" type="text/javascript">');
  aMemo.Lines.Add ('<!--' );
  aMemo.Lines.Add ('var requiredMajorVersion = 9;');
  aMemo.Lines.Add ('var requiredMinorVersion = 0;');
  aMemo.Lines.Add ('var requiredRevision = 45;');
  aMemo.Lines.Add ('-->');
  aMemo.Lines.Add ('</script>');
  aMemo.Lines.Add ('<BODY bgcolor="#000000">');


  aMemo.Lines.Add ('<CENTER>');
  aMemo.Lines.Add ('<p>');
	aMemo.Lines.Add ('<img alt="" src="../images/dxbxlogo.jpg" width="398" height="77"></p>');
	aMemo.Lines.Add ('<p><span><a href="../index.html">News&nbsp;</a>&nbsp;&nbsp;');
	aMemo.Lines.Add ('<a href="../faq.html">Faq</a>&nbsp;&nbsp;&nbsp; <a href="status.html">Status</a>&nbsp;&nbsp;</span>&nbsp;');
	aMemo.Lines.Add ('<a href="../download.html">Downloads</a>&nbsp;&nbsp;&nbsp;');
	aMemo.Lines.Add ('<a href="../translationguide.html">Translation guide</a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; </p>');
  aMemo.Lines.Add ('');

  aMemo.Lines.Add ('<script language="JavaScript" type="text/javascript">');
  aMemo.Lines.Add ('<!--');
  aMemo.Lines.Add ('if (AC_FL_RunContent == 0 || DetectFlashVer == 0) {');
	aMemo.Lines.Add ('alert("This page requires AC_RunActiveContent.js.");');
  aMemo.Lines.Add ('} else {');
	aMemo.Lines.Add ('var hasRightVersion = DetectFlashVer(requiredMajorVersion, requiredMinorVersion, requiredRevision);');
	aMemo.Lines.Add ('if(hasRightVersion) {');
	aMemo.Lines.Add ('	AC_FL_RunContent(');
	aMemo.Lines.Add ('		''codebase'', ''http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab#version=9,0,45,0'',');
  aMemo.Lines.Add (' 		''width'', ''800'',');
	aMemo.Lines.Add (' 		''height'', '''+ IntToStr((Items *20) + 200) +''',');
	aMemo.Lines.Add ('		''scale'', ''noscale'',');
	aMemo.Lines.Add ('    ''salign'', ''TL'',');
	aMemo.Lines.Add ('		''bgcolor'', ''#000000'',');
	aMemo.Lines.Add ('		''wmode'', ''opaque'',');
	aMemo.Lines.Add ('		''movie'', ''charts'',');
	aMemo.Lines.Add ('		''src'', ''charts'',');
	aMemo.Lines.Add ('		''FlashVars'', ''library_path=charts_library&xml_source=' + XmlName + ''',');
	aMemo.Lines.Add ('		''id'', ''my_chart'',');
	aMemo.Lines.Add ('		''name'', ''my_chart'',');
	aMemo.Lines.Add ('		''menu'', ''true'',');
	aMemo.Lines.Add ('		''allowFullScreen'', ''true'',');
	aMemo.Lines.Add ('		''allowScriptAccess'',''sameDomain'',');
	aMemo.Lines.Add ('		''quality'', ''high'',');
	aMemo.Lines.Add ('		''align'', ''middle'',');
	aMemo.Lines.Add ('		''pluginspage'', ''http://www.macromedia.com/go/getflashplayer'',');
	aMemo.Lines.Add ('		''play'', ''true'',');
	aMemo.Lines.Add ('		''devicefont'', ''false''');
	aMemo.Lines.Add ('		);');
	aMemo.Lines.Add ('} else {');
	aMemo.Lines.Add ('	var alternateContent = ''This content requires the Adobe Flash Player. ''');
	aMemo.Lines.Add ('	+ ''<u><a href=http://www.macromedia.com/go/getflash/>Get Flash</a></u>.'';');
	aMemo.Lines.Add ('	document.write(alternateContent);');
	aMemo.Lines.Add ('}');
  aMemo.Lines.Add ('}');
  aMemo.Lines.Add ('// -->');
  aMemo.Lines.Add ('</script>');
  aMemo.Lines.Add ('</CENTER>');
  aMemo.Lines.Add ('<noscript>');
  aMemo.Lines.Add ('	<P>This content requires JavaScript.</P>');
  aMemo.Lines.Add ('</noscript>');
  aMemo.Lines.Add ('');
  aMemo.Lines.Add ('</BODY>');
  aMemo.Lines.Add ('</HTML>');
end;

Function TForm1.CreateMainStatusChart(aMemo: TMemo) : Integer;
var XMLRootNode, XMLNode : IXMLNode;
  ItemList, ProgressList : TStrings;
  cnt : integer;
begin
  aMemo.Lines.Clear;
  CreateBeginBlockChart ( aMemo );

  // Write Chart Data
  XMLRootNode := XMLDocument1.DocumentElement;
  XMLNode := XMLRootNode.ChildNodes.FindNode('UNIT');

  ItemList := TStringList.Create;
  ProgressList := TStringList.Create;

  aMemo.Lines.add ( '<chart_data>' );

  while Assigned ( XmlNode ) do begin
    ItemList.Add( XmlNode.AttributeNodes.First.Text );
    ProgressList.Add( '0' );
    XmlNode := XmlNode.NextSibling;
  end;

  // Section names
  aMemo.Lines.add ( '<row>' );
  for cnt := 0 to ItemList.Count - 1 do begin
    aMemo.Lines.add ('<string>' + ItemList.Strings[cnt] + '</string>' );
  end;
  aMemo.Lines.add ( '</row>' );

  // percentage
  aMemo.Lines.add ( '<row>' );
  for cnt := 0 to ProgressList.Count - 1 do begin
    aMemo.Lines.add ('<number bevel=''data''>' + ProgressList.Strings[cnt] + '</number>' );
  end;
  aMemo.Lines.add ( '</row>' );

  aMemo.Lines.Add ('</chart_data>');

  CreateEndBlockChart ( aMemo, ItemList.Count, 'Status report DXBX' );
  Result := ItemList.Count;
  ItemList.free;
  ProgressList.free;
end;

procedure TForm1.CreateSubItemCharts;
var XMLRootNode, XmlNode, XmlSubItem : iXmlNode;
  ItemList, ProgressList : TStrings;
  ChartTab : TChartTab;
  cnt : Integer;
begin
  XMLRootNode := XMLDocument1.DocumentElement;
  XMLNode := XMLRootNode.ChildNodes.FindNode('UNIT');

  while Assigned ( XmlNode ) do begin
    XmlSubItem := XmlNode.ChildNodes.First;

    ChartTab := TChartTab.Create( PageControl1 );
    ChartTab.Caption := XmlNode.AttributeNodes.First.Text + '.Xml';
    CreateBeginBlockChart( ChartTab.aChartMemo );

    ItemList := TStringList.Create;
    ProgressList := TStringList.Create;
    while Assigned ( XmlSubItem ) do begin
      ItemList.Add ( XmlSubItem.AttributeNodes.First.Text );
      ProgressList.Add ( XmlSubItem.AttributeNodes.Last.Text );
      XmlSubItem := XmlSubItem.NextSibling;
    end;

    ChartTab.aChartMemo.Lines.add ( '<chart_data>' );

    ChartTab.aChartMemo.Lines.add ( '<row>' );
    for cnt := 0 to ItemList.Count - 1 do begin
      ChartTab.aChartMemo.Lines.add ('<string>' + ItemList.Strings[cnt] + '</string>' );
    end;
    ChartTab.aChartMemo.Lines.add ( '</row>' );

    ChartTab.aChartMemo.Lines.add ( '<row>' );
    for cnt := 0 to ProgressList.Count - 1 do begin
      ChartTab.aChartMemo.Lines.add ('<number bevel=''data''>' + ProgressList.Strings[cnt] + '</number>' );
    end;
    ChartTab.aChartMemo.Lines.add ( '</row>' );
    ChartTab.aChartMemo.Lines.add ( '</chart_data>' );


    CreateEndBlockChart( ChartTab.aChartMemo, ItemList.Count, 'Status report ' + XmlNode.AttributeNodes.First.Text );

    with TChartTab.Create( PageControl1 ) do begin
      Caption := XmlNode.AttributeNodes.First.Text + '.Html';
      CreateHtmlPages ( aChartMemo, XmlNode.AttributeNodes.First.Text + '.Xml', ItemList.Count );
    end;           

    ItemList.Free;
    ProgressList.Free;
    XmlNode := XmlNode.NextSibling;
  end;
end;

{ TChartTab }

constructor TChartTab.Create(AOwner: TComponent);
begin
  inherited;
  PageControl := TPageControl(AOwner);
  ChartMemo := TMemo.Create(self);
  ChartMemo.parent := self;
  ChartMemo.Align := alClient;
  ChartMemo.ScrollBars := ssBoth;
end;

destructor TChartTab.Destroy;
begin
  ChartMemo.Free;
  inherited;
end;

end.
