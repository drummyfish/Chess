unit CHra;

{
  trida hra
  ------------------
  implementuje tridu sachove hry, ktera umozni provadet tahy, uchovavat a
  menit nastaveni apod.
  ------------------
  projekt:         Sachy
  autor:           Miloslav Ciz
  posledni uprava: 19.03.2010
}

INTERFACE

uses
  CSachovnice, CTahy, Graphics, Controls, SysUtils, Dalsi;

type
  TNastaveni = record                                    // obsahuje nastaveni hry a vzhledu
                 ZobrazovatTahy: Boolean;
                 BarvaTahu,                              // barva zvyrazneni tahu
                 BarvaVyberu: array[1..3] of Integer;    // barva zvyrazneni vyberu
                 Vzhled: Byte;
               end;

  THra = class                                           // sjednocuje sachovnici a promenne potrebne pro hru
           private
             OdkudX,                                     // pro uchovavani vychozi Y souradnice na sachovnici, pokud = 0, pak se vybira kamen, kterym bude tazeno
             OdkudY: Byte;                               // pro uchovavani vychozi X souradnice na sachovnici
             Tahy: TTahy;                                // mnozina pro zjistovani tahu
             Pozadi: TBitmap;                            // pozadi sachovnice pro vykreslovani
             Platno: TCanvas;                            // platno, kam se bude sachovnice vykreslovat
             CisloTahu: Word;                            // pocet tahu od zacatku hry
           public
             Sachovnice: TSachovnice;                    // sachovnice, na ktere se hraje
             BilyNaTahu: Boolean;                        // true, pokud je bily na tahu, jinak false
             Nastaveni: TNastaveni;                      // nastaveni hry
             constructor Create;
             destructor Free;
             function Tah(X,Y: Byte): String;            // necha hrace hrat, X;Y je zvolene pole (muze se tykat vyberu kamene nebo tahu), Vraci zaznamenany tah (v pripade ze se nejdena o tah '')
             function KamenJmeno(X,Y: TRozmer): String;  // vraci jmeno kamene na dane pozici na sachovnici
             function KamenBarva(X,Y: TRozmer): TBarva;  // vraci barvu kamene na dane pozici na sachovnici
             procedure NovaHra;                          // zahaji novou hru (rozestavi sachovnici, bily je na tahu, atd.)
           end;

  function PixelyNaPole(Souradnice: Integer): Byte;      // prevede souradnice v pixelech na souradnice v polich sachovnice

//------------------------------------------------------------------------------

IMPLEMENTATION

  function PixelyNaPole(Souradnice: Integer): Byte;

  var
    Vysledek: Byte;

  begin
    Vysledek := (Souradnice - 14) div (53) + 1;
    if (Vysledek < 1) or (Vysledek > 8) then
      Result := 0
      else
        Result := Vysledek;
  end;

//------------------------------------------------------------------------------

  constructor THra.Create;

  begin
    Self.Nastaveni.Vzhled := 1;
    Self.Nastaveni.ZobrazovatTahy := true;
    Self.Pozadi := Pozadi;
    Self.Platno := Platno;
    Self.Nastaveni.BarvaTahu[1] := 50;
    Self.Nastaveni.BarvaTahu[2] := -50;
    Self.Nastaveni.BarvaTahu[3] := -50;
    Self.Nastaveni.BarvaVyberu[1] := 100;
    Self.Nastaveni.BarvaVyberu[2] := -100;
    Self.Nastaveni.BarvaVyberu[3] := -100;
    Self.Tahy := TTahy.Create;
    Self.Sachovnice := TSachovnice.Create;
    Self.NovaHra;
  end;

//------------------------------------------------------------------------------

  destructor THra.Free;

  begin
    Self.Tahy.Free;
    Self.Sachovnice.Free;
  end;

//------------------------------------------------------------------------------

  procedure THra.NovaHra;

  begin
    Self.Sachovnice.Rozestav;   // zakladni rozestaveni
    Self.BilyNaTahu := true;    // bily zacina
    Self.OdkudX := 0;           // zacina se vyberem kamene
    Self.CisloTahu := 1;
  end;

//------------------------------------------------------------------------------

  function THra.Tah(X,Y: Byte): String;

  var
    Vysledek: String;
    var I: Byte;

  begin
    Vysledek := ';';
    if Self.OdkudX = 0 then begin                                                // faze vyberu kamene, kterym bude tazeno
      if Ord(Self.Sachovnice.ZjistiBarvu(X,Y)) = Ord(Self.BilyNatahu) then begin // aby nebylo mozne vybrat kamen souperovy barvy
        Self.OdkudX := X;                                                        // nastaveni vychozich souradnic
        Self.OdkudY := Y;
        Self.Sachovnice.ZjistiTahy(Self.OdkudX,Self.OdkudY,Self.Tahy);           // zjisteni moznych tahu vybraneho kamene
        Vysledek := ';select' + IntToStr(Self.OdkudX) + ';' + IntToStr(OdkudY);
        end;
      end
      else begin                                                                 // faze tahu vybranym kamene
        if Self.Tahy.JeTah(X,Y) then begin

          if Self.BilyNaTahu then begin
            Vysledek := IntToStr(Self.CisloTahu) + '. ';                         // zvyseni cisla tahu od zacatku hry
            end
            else begin
              Vysledek := '';
              for I := 1 to Length(IntToStr(Self.CisloTahu)) + 2 do
              Vysledek := Vysledek + ' ';
              Self.CisloTahu := Self.CisloTahu + 1;
              end;

          Vysledek := Vysledek + Chr(64 + OdkudX) + IntTostr(OdkudY) + ' ' +
                      Self.Sachovnice.ZjistiJmeno(OdkudX,OdkudY)[1];

          if Self.Sachovnice.ZjistiJmeno(X,Y) <> '' then
            Vysledek := Vysledek + ' x '
            else
              Vysledek := Vysledek + ' - ';

          Vysledek := Vysledek + Chr(64 + X) + IntTostr(Y);

          if Self.Sachovnice.ZjistiJmeno(X,Y) <> '' then
            Vysledek := Vysledek + ' ' + Self.Sachovnice.ZjistiJmeno(X,Y)[1];

          Self.Sachovnice.Tah(OdkudX,OdkudY,X,Y);
          Self.BilyNaTahu := not Self.BilyNaTahu;                                // prostridani tahu
          Self.OdkudX := 0;                                                      // zacina se opet vyberem kamene

          if Self.Sachovnice.JeSach(Bila) or Self.Sachovnice.JeSach(Cerna) then  // overeni sachu, pripadne matu
            case Self.Sachovnice.JeMat of
              Zadna: Vysledek := Vysledek + ' +';
              Bila: Vysledek := Vysledek + ' #';
              Cerna: Vysledek := Vysledek + ' #';
            end;

          end else                                                               // pokud bylo vybrano stejne pole, kamen se odznaci
            if (Self.OdkudX = X) and (Self.OdkudY = Y) then begin
              Self.OdkudX := 0;
              Vysledek := ';deselect';
              end;
        end;

    Result := Vysledek;
  end;

//------------------------------------------------------------------------------

  function THra.KamenJmeno(X,Y: TRozmer): String;

  begin
    Result := Self.Sachovnice.ZjistiJmeno(X,Y);
  end;

//------------------------------------------------------------------------------

  function THra.KamenBarva(X,Y: TRozmer): TBarva;

  begin
    Result := Self.Sachovnice.ZjistiBarvu(X,Y);
  end;
  
//------------------------------------------------------------------------------

end.