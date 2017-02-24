unit FOProgramu;

{ 
  formular O programu
  -----------------
  modul, ktery obsahuje zdrojovy kod formulare O programu
  -----------------
  projekt:         Sachy
  autor:           Miloslav Ciz
  posledni uprava: 19.03.2010
}

INTERFACE

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm3 = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

//------------------------------------------------------------------------------

IMPLEMENTATION

uses Main;

{$R *.dfm}

  procedure TForm3.Button1Click(Sender: TObject);

  begin
    Self.Close;
  end;

//------------------------------------------------------------------------------

  procedure TForm3.FormClose(Sender: TObject; var Action: TCloseAction);

  begin
    Main.Form1.Enabled := true;
    Self.Visible := false;
  end;

//------------------------------------------------------------------------------

end.
