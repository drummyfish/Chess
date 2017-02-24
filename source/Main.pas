unit Main;

{
  hlavni modul projektu Sachy
  -----------------
  hlavni modul, ktery pracuje s moduly trid a obsahuje formular hry
  -----------------
  projekt:         Sachy
  autor:           Miloslav Ciz
  posledni uprava: 19.03.2010
}

INTERFACE

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons, ImgList, ComCtrls, Menus, CHra, Dalsi,
  CVykreslovac, CTahy;

type
  TForm1 = class(TForm)           // hlavni formular
    ImageList1: TImageList;
    Image1: TImage;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    S1: TMenuItem;
    Novhra1: TMenuItem;
    Konec1: TMenuItem;
    Nastaven1: TMenuItem;
    Pomoc1: TMenuItem;
    Npovda1: TMenuItem;
    Oprogramu1: TMenuItem;
    Image2: TImage;
    ImageList2: TImageList;
    ImageList3: TImageList;
    Zznamhry1: TMenuItem;
    StatusBar1: TStatusBar;
    RichEdit1: TRichEdit;
    Button1: TButton;
    SaveDialog1: TSaveDialog;
    nastaven2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Konec1Click(Sender: TObject);
    procedure Novhra1Click(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Zznamhry1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure nastaven2Click(Sender: TObject);
    procedure RichEdit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Oprogramu1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Hra: THra;
  Vykreslovac: TVykreslovac;

//------------------------------------------------------------------------------

IMPLEMENTATION

uses FNastaveni, FOProgramu;

var
  Retezec: String;    // zde se zaznamenava, co uzivatel napsal kvuli eastereggu

{$R *.dfm}

  procedure TForm1.FormCreate(Sender: TObject);

  begin
    Retezec := '';
    Hra := THra.Create;
    StatusBar1.Panels[2].Text := 'bílý na tahu';
    Vykreslovac := TVykreslovac.Create(ImageList1,Image2.Picture.Bitmap);
    Vykreslovac.Vykresli(Hra.Sachovnice,Image1.Canvas);
  end;

//------------------------------------------------------------------------------

  procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  var
    VystupTahu: String;       // pro ulozeni zaznamu provedeneho tahu ve forme retezce a naslednou analyzu nebo zapis do zaznamu hry
    Tahy: TTahy;

  begin
    X := PixelyNaPole(X);
    Y := 9 - PixelyNaPole(Y);       // prevraceni Ypsylonove souradnice kvuli zobrazeni, ktere pocita odspodu

    VystupTahu := Hra.Tah(X,Y);     // provedeni vyberu kamene, pripadne tahu

    if VystupTahu[1] <> ';' then begin                    // zapis do zaznamu hry
      RichEdit1.Lines.Add(VystupTahu);
      if Hra.BilyNaTahu then
        RichEdit1.Lines.Add('--------');
      if VystupTahu[Length(VystupTahu)] = '+' then
        ShowMessage('šach')
        else
          if VystupTahu[Length(VystupTahu)] = '#' then
            ShowMessage('šach mat');
      Vykreslovac.Vykresli(Hra.Sachovnice,Image1.Canvas);
      end else if VystupTahu[2] = 's' then begin         // oznaceni kamene (zvyrazneni tahu)
        Vykreslovac.Zvyrazni(Image1.Canvas,StrToInt(VystupTahu[8]),StrToInt(VystupTahu[10]),Hra.Nastaveni.BarvaVyberu[1],Hra.Nastaveni.BarvaVyberu[2],Hra.Nastaveni.BarvaVyberu[3]);
        if Hra.Nastaveni.ZobrazovatTahy then begin
          Tahy := TTahy.Create;
          Hra.Sachovnice.ZjistiTahy(StrToInt(VystupTahu[8]),StrToInt(VystupTahu[10]),Tahy);
          Vykreslovac.ZvyrazniTahy(Image1.Canvas,Tahy,Hra.Nastaveni.BarvaTahu[1],Hra.Nastaveni.BarvaTahu[2],Hra.Nastaveni.BarvaTahu[3]);
          Tahy.Destroy;
          end;
        end else                                             // odznaceni kamene
          Vykreslovac.Vykresli(Hra.Sachovnice,Image1.Canvas);

    if Hra.BilyNaTahu then
      StatusBar1.Panels[2].Text := 'bílý na tahu'
      else
        StatusBar1.Panels[2].Text := 'èerný na tahu';
  end;

//------------------------------------------------------------------------------

  procedure TForm1.Konec1Click(Sender: TObject);

  begin
    Hra.Free;
    Halt;
  end;

//------------------------------------------------------------------------------

  procedure TForm1.Novhra1Click(Sender: TObject);

  begin
    if MessageDlg('Ukonèit aktuální hru?',mtConfirmation,[mbYes,mbNo],0) = mrYes then begin // dotaz na zahajeni nove hry
      Hra.NovaHra;
      RichEdit1.Text := '';
      end;
    Vykreslovac.Vykresli(Hra.Sachovnice,Image1.Canvas);
  end;

//------------------------------------------------------------------------------

  procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
    Y: Integer);        // prekreslovani stavoveho radku

  var
    PomX,
    PomY: Byte;

  begin
    PomX := PixelyNaPole(X);
    PomY := PixelyNaPole(Y);
    if (PomX <> 0) and (PomY <> 0) then begin
      PomY := 9 - PomY;
      StatusBar1.Panels[0].Text := Chr(64 + PomX) + IntToStr(PomY);
      StatusBar1.Panels[1].Text := Hra.KamenJmeno(PomX,PomY) + ' ';
      case Hra.KamenBarva(PomX,PomY) of
        Zadna: StatusBar1.Panels[1].Text := 'prázdné pole';
        Bila: StatusBar1.Panels[1].Text := StatusBar1.Panels[1].Text + 'bílá';
        Cerna: StatusBar1.Panels[1].Text := StatusBar1.Panels[1].Text + 'èerná';
        end;
      end
      else begin
        StatusBar1.Panels[0].Text := '';
        StatusBar1.Panels[1].Text := '';
        end;
  end;

//------------------------------------------------------------------------------

  procedure TForm1.Zznamhry1Click(Sender: TObject);

  begin
    if RichEdit1.Visible then begin
      RichEdit1.Visible := false;
      Form1.Width := Form1.Width - RichEdit1.Width;
      MainMenu1.Items[1].Items[0].Checked := false;
      Button1.Visible := false;
      end
      else begin
        RichEdit1.Visible := true;
        Form1.Width := Form1.Width + RichEdit1.Width;
        MainMenu1.Items[1].Items[0].Checked := true;
        Button1.Visible := true;
        end;
  end;

//------------------------------------------------------------------------------

  procedure TForm1.Button1Click(Sender: TObject);

  begin
    if SaveDialog1.Execute then
      RichEdit1.Lines.SaveToFile(SaveDialog1.FileName);
  end;

//------------------------------------------------------------------------------

procedure TForm1.nastaven2Click(Sender: TObject);

  begin
    FNastaveni.Form2.Visible := true;
    Self.Enabled := false;
  end;

//------------------------------------------------------------------------------

  procedure TForm1.RichEdit1KeyDown(Sender: TObject; var Key: Word;             // easter egg (po napsani 'ahoj' se zobrazi 'cau')
  Shift: TShiftState);

  begin
    case key of
      65: Retezec := Retezec + 'a';
      72: Retezec := Retezec + 'h';
      79: Retezec := Retezec + 'o';
      74: if Retezec = 'aho' then begin
            ShowMessage('èau');
            Retezec := '';
            end
      else Retezec := '';
    end;
  end;

//------------------------------------------------------------------------------

  procedure TForm1.Oprogramu1Click(Sender: TObject);

  begin
    FOProgramu.Form3.Visible := true;
    Self.Enabled := false;
  end;

//------------------------------------------------------------------------------

end.
