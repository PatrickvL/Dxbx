unit clienteftp;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdFTP, Psock, NMFtp, Buttons, ImgList;

type
  TForm6 = class(TForm)
    ListView1: TListView;
    StatusBar1: TStatusBar;
    GroupBox1: TGroupBox;
    EditUsuario: TEdit;
    EditPass: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    EditRuta: TEdit;
    Button1: TButton;
    Button2: TButton;
    Label4: TLabel;
    EditIP: TEdit;
    SpeedButton1: TSpeedButton;
    ImageList1: TImageList;
    Label3: TLabel;
    EditPuerto: TEdit;
    FTP: TIdFTP;
    procedure Button1Click(Sender: TObject);
    procedure IdFTP1Connected(Sender: TObject);
    procedure IdFTP1Disconnected(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FTPConnected(Sender: TObject);
    procedure FTPDisconnected(Sender: TObject);
  private
    procedure LeerLista();
  public
    Carpeta: string;
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

uses Textos;

// GetFilename
//
// Return the filename of a FTP response line.
//
// Added by Yursoft
// 2/15/2003
function GetFilename(Line: string): string;
var
   i,j: integer;
begin
        i := 0;
        // I suppose that 8 elements exists before the name.
        while i < 8 do
        begin
          j := pos(' ',Line);
          if j <> 0 then
          begin
            Line := Copy(Line,j,Length(Line));
            i := i+1;
            Line := TrimLeft(Line);
          end;
        end;
        Result := Line;
end;

function GetSize(Line: string): string;
var
   i,j: integer;
begin
        i := 0;
        Trim(Line);
        while i < 4 do
        begin
          j := pos(' ',Line);
          if j <> 0 then
          begin
            Line := Copy(Line,j,Length(Line));
            i := i+1;
            Line := TrimLeft(Line);
          end;
        end;
        j := pos(' ',Line);
        Line := Copy(Line,1,j-1);
        Result := Line;
end;

function GetAttribs(Line: string): string;
var
   j: integer;
begin
        Trim(Line);
        j := pos(' ',Line);
        if j <> 0 then
        begin
          Line := Copy(Line,1,j-1);
          Line := Trim(Line);
        end;
        Result := Line;
end;

procedure TForm6.LeerLista();
var
   L: TStringList;
   i: integer;
   Row: TListItem;
   Nombre,Tamano,Atributos: string;
begin
        if not FTP.Connected then Exit;
        L := TStringList.Create;
        FTP.List(L,'',True);

        Listview1.Items.BeginUpdate;
        Listview1.Items.Clear;
        for i := 0 to L.Count-1 do
        begin

          Nombre := GetFilename(L[i]);
          Tamano := GetSize(L[i]);
          Atributos := GetAttribs(L[i]);
          if Nombre[1] = '.' then
            Continue;

          Row := Listview1.Items.Add;
          Row.Caption := Nombre;
          Row.SubItems.Add(Tamano);
          Row.SubItems.Add(Atributos);
          if Lowercase(Atributos[1]) = 'd' then
            Row.ImageIndex := 0
          else
            Row.ImageIndex := 2;
        end;
        Listview1.Items.EndUpdate;
        EditRuta.Text := FTP.RetrieveCurrentDir;
        L.Free;        
end;

procedure TForm6.Button1Click(Sender: TObject);
begin
        if (EditIP.Text = '') or (EditPuerto.Text = '') then Exit;
        ListView1.Items.Clear;
        if FTP.Connected then FTP.Disconnect;
        FTP.User := EditUsuario.Text;
        FTP.Password := EditPass.Text;
        FTP.Host := EditIP.Text;
        FTP.Port := StrToIntDef(EditPuerto.Text,21);
        FTP.Connect(True);
        LeerLista();
end;

procedure TForm6.IdFTP1Connected(Sender: TObject);
begin
        Button1.Enabled := False;
        Button2.Enabled := True;
end;

procedure TForm6.IdFTP1Disconnected(Sender: TObject);
begin
        Button1.Enabled := True;
        Button2.Enabled := False;
end;

procedure TForm6.Button2Click(Sender: TObject);
begin
        if FTP.Connected then
          FTP.Disconnect();
end;

procedure TForm6.ListView1DblClick(Sender: TObject);
begin
        if Listview1.Selected = nil then Exit;
        if Lowercase(Listview1.Selected.SubItems[1][1]) = 'd' then
        begin
          FTP.ChangeDir(Listview1.Selected.Caption);
          LeerLista;
        end;
end;

procedure TForm6.SpeedButton1Click(Sender: TObject);
begin
        FTP.ChangeDirUp;
        LeerLista;
     {   if not FTP.Connected then Exit;
        Dir := FTP.CurrentDir;
        if (Dir <> '/') or (Length(Dir) > 0) then
        begin
          if Dir[Length(Dir)] = '/' then
            Dir := Copy(Dir,1,Length(Dir)-1);
          Dir := Copy(Dir,1,LastDelimiter('/',Dir));
          FTP.ChangeDir(Dir);
        end; }
end;

procedure TForm6.FTPConnected(Sender: TObject);
begin
        StatusBar1.Panels[0].Text := rcConectado;
        Button1.Enabled := False;
        Button2.Enabled := True;
end;

procedure TForm6.FTPDisconnected(Sender: TObject);
begin
        StatusBar1.Panels[0].Text := rcDesconectado;
        Button1.Enabled := True;
        Button2.Enabled := False;
end;

end.
