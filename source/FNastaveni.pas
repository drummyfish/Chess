unit FNastaveni;

{
  formular nastaveni hry
  -----------------
  modul obsahujici formular pro nastaveni vzhledu, barvy zvyraznovani apod.
  -----------------
  projekt:         Sachy
  autor:           Miloslav Ciz
  posledni uprava: 20.03.2010
}

INTERFACE

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

//------------------------------------------------------------------------------

IMPLEMENTATION

uses Main;

{$R *.dfm}

  procedure Obnov;

  begin
    case Main.Hra.Nastaveni.Vzhled of
      1: Form2.RadioButton1.Checked := true;
      2: Form2.RadioButton2.Checked := true;
      3: Form2.RadioButton3.Checked := true;
    end;
    Form2.CheckBox1.Checked := Hra.Nastaveni.ZobrazovatTahy;
    Main.Form1.ImageList1.Draw(Form2.Image1.Canvas,1,1,0);
    Main.Form1.ImageList1.Draw(Form2.Image1.Canvas,1,1,1);
    Main.Form1.ImageList2.Draw(Form2.Image2.Canvas,1,1,0);
    Main.Form1.ImageList2.Draw(Form2.Image2.Canvas,1,1,1);
    Main.Form1.ImageList3.Draw(Form2.Image3.Canvas,1,1,0);
    Main.Form1.ImageList3.Draw(Form2.Image3.Canvas,1,1,1);
  end;

//------------------------------------------------------------------------------

  procedure TForm2.FormCreate(Sender: TObject);

  begin
    Self.Visible := false;
    Obnov;
  end;

//------------------------------------------------------------------------------

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);

  begin
    Main.Form1.Enabled := true;
    Self.Visible := false;
  end;

//------------------------------------------------------------------------------

procedure TForm2.Button1Click(Sender: TObject);                                 // tlacitko OK

  begin
    Main.Hra.Nastaveni.ZobrazovatTahy := CheckBox1.Checked;
    if RadioButton1.Checked then begin
      Main.Hra.Nastaveni.Vzhled := 1;
      Main.Vykreslovac.ZmenVzhled(Main.Form1.ImageList1,Main.Form1.Image2.Picture.Bitmap);
      end
      else if RadioButton2.Checked then begin
        Main.Hra.Nastaveni.Vzhled := 2;
        Main.Vykreslovac.ZmenVzhled(Main.Form1.ImageList2,Main.Form1.Image2.Picture.Bitmap);
        end
          else if RadioButton3.Checked then begin
          Main.Hra.Nastaveni.Vzhled := 3;
          Main.Vykreslovac.ZmenVzhled(Main.Form1.ImageList3,Main.Form1.Image2.Picture.Bitmap);
          end;
          
    Main.Vykreslovac.Vykresli(Main.Hra.Sachovnice,Main.Form1.Image1.Canvas);
    Self.Close;
  end;

//------------------------------------------------------------------------------

procedure TForm2.Button2Click(Sender: TObject);                                 // tlacitko zrusit

  begin
    Obnov;
    Self.Close;
  end;

//------------------------------------------------------------------------------

end.
