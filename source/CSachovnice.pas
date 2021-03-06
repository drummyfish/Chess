unit CSachovnice;

{ 
  trida sachovnice
  -----------------
  implementace tridy sachovnice, ktera obsahuje prostredky pro uchovavani a
  manipulaci s daty sachovnice, ale take napr. jeji vykreslovani, dale pak
  tridu kamen a jeji potomky, umoznujici overovani moznych tahu, presunu, apod.
  -----------------
  projekt:         Sachy
  autor:           Miloslav Ciz
  posledni uprava: 19.03.2010
}

INTERFACE

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, CTahy, Dalsi;
  
type

  TKamen = class;                                                               // dopredna deklarace

  TSachovnice = class
                  private                                                       // trida sachovnice
                    Sachovnice: array[1..8,1..8] of TKamen;                     // vnitrni reprezentace sachovnice
                  public
                    constructor Create;
                    destructor Free;
                    function ZjistiBarvu(X,Y: TRozmer): TBarva;
                    function ZjistiJmeno(X,Y: TRozmer): String;                 // vraci jmeno kamene na dane pozici (cesky s diakritikou)
                    function ByloTazeno(X,Y: TRozmer): Boolean;                 // vraci true, pokud bylo kamenem na pozici jiz tazeno, jinak false
                    function JeSach(Barva: TBarva): Boolean;                    // overuje zda je hrac v sachu
                    function JeMat: TBarva;                                     // vraci barvu hrace ktery ma mat (Zadna = neni mat)
                    function LzeEnPssant(X,Y: TRozmer): Boolean;                // vraci true, pokud je mozne vzit kamen na pozici tahem en passant (brani mimochodem), jinak false
                    procedure Vycisti;                                          // vyprazdni sachovnici
                    procedure Rozestav;                                         // obnovi zakladni sachove rozestaveni
                    procedure Presun(OdkudX,OdkudY,KamX,KamY: TRozmer);         // presune kamen z jedne pozice na jinou, nevynuluje moznost tahu en passant a kamen se bere jako by jim nebylo tazeno
                    procedure Tah(OdkudX,OdkudY,KamX,KamY: TRozmer);            // presune kamen z jedne pozice na jinou s ohledem na specialni tahy (rosada, en passant, promena...), vynuluje moznost tahu en passant a zmeni ByloTazeno na true
                    procedure Rosada(Bila, Mala: Boolean);                      // presune kameny jako pri rosade dle parametru (netestuje zda je rosada mozna)
                    procedure EnPassant(X: TRozmer; Bily, Vpravo: Boolean);     // provede tah en passant (brani mimochodem) pesakem na potici X, barvy urcene prom. Bily a smerem urcenym prom. Vpravo
                    procedure ZjistiVsechnyTahy(X,Y: TRozmer; var Tahy: TTahy); // zjisti tahy bez ohledu na vlastni sach
                    procedure ZjistiTahy(X,Y: TRozmer; var Tahy: TTahy);        // zjisti tahy a odebere ty, ktere znamenaji vlastni sach
                    procedure VemKamen(X,Y: Trozmer; var Kamen: TKamen);        // sejme kamen ze sachovnice, odkaz na nej zustane v promenne Kamen
                    procedure PostavKamen(X,Y: Trozmer; Kamen: TKamen);         // polozi kamen z promenne Kamen na sachovnici
                end;

  TKamen = class                                                                // trida reprezentujici kamen (figurku) na sachovnici
             private
               Sachovnice: TSachovnice;                                         // sachovnice, na niz se kamen nachazi
               PoziceX,                                                         // X pozice na sachovnici
               PoziceY: TRozmer;                                                // Y pozice na sachovnici
             public
               EnPassant,                                                       // zda je mozne vzit kamen (pesaka) tahem en passant (brani mimochodem)
               ByloTazeno: Boolean;                                             // zda bylo kamenem jiz tazeno (pro potrebu rosady)
               Barva: TBarva;                                                   // vlastni barva
               BarvaSouper: TBarva;                                             // barva soupere
               constructor Create(Barva: TBarva; Sachovnice: TSachovnice; X,Y: TRozmer);
               function JeVOhrozeni: Boolean;                                   // vraci true, pokud je kamen v konkretni pozici ohrozeni, jinak false
               procedure ZjistiTahy(var Tahy: TTahy); Virtual; Abstract;        // zjisti mozne tahy kamene (bez ohledu na vlastni sach, ten testuje metoda tridy TSachovnice)
               procedure Presun(X,Y: TRozmer);                                  // presune sam sebe na urcenou pozici na sachovnici, nevynuluje moznost enpassant a kamen se bere, jako by jim nebylo tazeno
           end;

  TPesak = class(TKamen)                                                        // pesak
             procedure ZjistiTahy(var Tahy: TTahy); Override;
           end;

  TVez = class(TKamen)                                                          // vez
           procedure ZjistiTahy(var Tahy: TTahy); Override;
         end;

  TJezdec = class(TKamen)                                                       // jezdec
              procedure ZjistiTahy(var Tahy: TTahy); Override;
            end;

  TStrelec = class(TKamen)                                                      // strelec
               procedure ZjistiTahy(var Tahy: TTahy); Override;
             end;

  TDama = class(TKamen)                                                         // dama
            procedure ZjistiTahy(var Tahy: TTahy); Override;
          end;

  TKral = class(TKamen)                                                         // kral
            procedure ZjistiTahy(var Tahy: TTahy); Override;
          end;

//------------------------------------------------------------------------------

IMPLEMENTATION

  constructor TSachovnice.Create;

  var
    I,J: TRozmer;

  begin
    for J := 1 to 8 do            // vycisteni sachovnice
      for I := 1 to 8 do
        Self.Sachovnice[I,J] := NIL;
  end;

//------------------------------------------------------------------------------

  destructor TSachovnice.Free;

  begin
    Self.Vycisti;
  end;

//------------------------------------------------------------------------------

  function TSachovnice.ZjistiBarvu(X,Y: TRozmer): TBarva;

  begin
    if JePlatnyTah(X,Y) then
      if Self.Sachovnice[X,Y] = NIL then // prazne pole
        Result := Zadna
        else
          if Self.Sachovnice[X,Y].Barva = Bila then
            Result := Bila
            else
              Result := Cerna;
  end;

//------------------------------------------------------------------------------

  function TSachovnice.ZjistiJmeno(X,Y: TRozmer): String;

  var
    Vysledek: String;

  begin
    Vysledek := '';   // nelze pouzit case, protoze string neni ordinalni typ :/
    if JePlatnyTah(X,Y) and (Self.Sachovnice[X,Y] <> NIL) then
      if Self.Sachovnice[X,Y].ClassName = 'TKral' then
        Vysledek := 'kr�l'
        else
          if Self.Sachovnice[X,Y].ClassName = 'TPesak' then
            Vysledek := 'p��k'
            else
              if Self.Sachovnice[X,Y].ClassName = 'TStrelec' then
                Vysledek := 'st�elec'
                else
                  if Self.Sachovnice[X,Y].ClassName = 'TJezdec' then
                    Vysledek := 'jezdec'
                    else
                      if Self.Sachovnice[X,Y].ClassName = 'TVez' then
                        Vysledek := 'v�'
                        else
                          if Self.Sachovnice[X,Y].ClassName = 'TDama' then
                            Vysledek := 'd�ma';
    Result := Vysledek;
  end;

//------------------------------------------------------------------------------

  function TSachovnice.ByloTazeno(X,Y: TRozmer): Boolean;

  begin
    if JePlatnyTah(X,Y) and (Self.Sachovnice[X,Y] <> NIL) then
      Result := Self.Sachovnice[X,Y].ByloTazeno
      else
        Result := false;
  end;

//------------------------------------------------------------------------------

  function TSachovnice.JeSach(Barva: TBarva): Boolean;

  var
    I,J: TRozmer;

  begin
    for J := 1 to 8 do     // prochazeni vsech poli
      for I := 1 to 8 do
        if (Self.ZjistiBarvu(I,J) = Barva) and (Self.ZjistiJmeno(I,J) = 'kr�l') and (Self.Sachovnice[I,J].JeVOhrozeni) then begin // pokud je na poli kral zvolene barvy v ohrozeni
          Result := true;  // je sach
          Exit;
          end;
    Result := false;
  end;

//------------------------------------------------------------------------------

  function TSachovnice.JeMat: TBarva;

  var
    I,J: TRozmer;
    Tahy: TTahy;
    BilyOK,            // zda je bily mimo mat
    CernyOK: Boolean;  // zda je cerny mimo mat

  begin
    BilyOK := false;
    CernyOK := false;
    Tahy := TTahy.Create;

    for J := 1 to 8 do
      for I := 1 to 8 do
        if (not BilyOK) and (Self.ZjistiBarvu(I,J) = Bila) then begin
          Self.ZjistiTahy(I,J,Tahy);
          if not Tahy.ZadnyTah then   // pokud existuje mozny tah (s ohledem na vlastni sach), pak neni mat (a dal se neoveruje)
            BilyOK := true;
          end else
            if (not CernyOK) and (Self.ZjistiBarvu(I,J) = Cerna) then begin // stejny zpusob overeni pro cerneho
              Self.ZjistiTahy(I,J,Tahy);
              if not Tahy.ZadnyTah then
                CernyOK := true;
              end;

    Tahy.Free;
    if not BilyOK then
      Result := Bila
      else
        if not CernyOK then
          Result := Cerna
          else
            Result := Zadna;
  end;

//------------------------------------------------------------------------------

  function TSachovnice.LzeEnPssant(X,Y: TRozmer): Boolean;

  begin
    if JePlatnyTah(X,Y) and (Self.Sachovnice[X,Y] <> NIL) then
      Result := Self.Sachovnice[X,Y].EnPassant
      else
        Result := false;
  end;

//------------------------------------------------------------------------------

  procedure TSachovnice.Presun(OdkudX,OdkudY,KamX,KamY: TRozmer);

  begin
    if Self.Sachovnice[OdkudX,OdkudY] <> NIL then
      Self.Sachovnice[OdkudX,OdkudY].Presun(KamX,KamY);
  end;

//------------------------------------------------------------------------------

  procedure TSachovnice.Tah(OdkudX,OdkudY,KamX,KamY: TRozmer);

  var
    I,J: TRozmer;

  begin
    if (not JePlatnyTah(OdkudX,OdkudY)) or (not JePlatnyTah(KamX,KamY)) then
      Exit;

    for J := 1 to 8 do      // vynulovani moznosti tahu en passant
      for I := 1 to 8 do
        if Self.Sachovnice[I,J] <> NIL then
          Self.Sachovnice[I,J].EnPassant := false;

    if (Self.ZjistiJmeno(OdkudX,OdkudY) = 'kr�l') and (OdkudX = 5) and ((KamX=7) or (KamX=3)) then // rosada
      case Self.ZjistiBarvu(OdkudX,OdkudY) of
        Bila: Self.Rosada(true,KamX = 7);
        Cerna: Self.Rosada(false,KamX = 7);
      end
      else
        if (Self.ZjistiJmeno(OdkudX,OdkudY) = 'p��k') and (KamX <> OdkudX) and (Self.ZjistiBarvu(KamX,KamY) = Zadna) then // en passant (brani mimochodem)
          case Self.ZjistiBarvu(OdkudX,OdkudY) of
            Bila: Self.EnPassant(OdkudX,true,KamX > OdkudX);
            Cerna: Self.EnPassant(OdkudX,false,KamX > OdkudX);
          end
          else
            Self.Presun(OdkudX,OdkudY,KamX,KamY);

    Self.Sachovnice[KamX,KamY].ByloTazeno := true;
  end;

//------------------------------------------------------------------------------

  procedure TSachovnice.Rosada(Bila, Mala: Boolean);

  var
    Y: TRozmer;

  begin
    if Bila then
      Y := 1       // Ypsylonova pozice bileho krale
      else
        Y := 8;    // Ypsylonova pozice cerneho krale
    if Mala then begin            // mala rosada
      Self.Presun(5,Y,7,Y);       // presun krale
      Self.Presun(8,Y,6,Y);       // presun veze
      end else begin              // velka rosada
        Self.Presun(5,Y,3,Y);     // presun krale
        Self.Presun(1,Y,4,Y);     // presun veze
        end;
  end;

//------------------------------------------------------------------------------

  procedure TSachovnice.EnPassant(X: TRozmer; Bily, Vpravo: Boolean);

  var
    Y: TRozmer;
    Pom: ShortInt;

  begin
    if Bily then begin
      Y := 5;       // bily provadi en passant z Ypsylonove pozice 5
      Pom := 1;     // pro smer pohybu vpred
      end
      else begin
        Y := 4;     // cerny provadi en passant z Ypsylonove pozice 6
        Pom := -1;  // pro smer pohybu vzad
        end;

    if Vpravo then begin
      Self.Presun(X,Y,X + 1,Y + Pom);     // presun pesaka
      Self.Sachovnice[X + 1,Y].Free;      // vyhozeni soupere
      Self.Sachovnice[X + 1,Y] := NIL;
      end else begin
        Self.Presun(X,Y,X - 1,Y + Pom);
        Self.Sachovnice[X - 1,Y].Free;
        Self.Sachovnice[X - 1,Y] := NIL;
        end;
  end;

//------------------------------------------------------------------------------

  procedure TSachovnice.Vycisti;

  var
    I,J: TRozmer;

  begin
    for J := 1 to 8 do
      for I := 1 to 8 do
        if Self.Sachovnice[I,J] <> NIL then begin
          Self.Sachovnice[I,J].Free;
          Self.Sachovnice[I,J] := NIL;
          end;
  end;

//------------------------------------------------------------------------------

  procedure TSachovnice.Rozestav;

  var
    I: TRozmer;

  begin
    with Self do begin
      Vycisti;
      for I := 1 to 8 do // bili
        Sachovnice[I,2] := TPesak.Create(Bila,Self,I,2);
      Sachovnice[1,1] := TVez.Create(Bila,Self,1,1);
      Sachovnice[8,1] := TVez.Create(Bila,Self,8,1);
      Sachovnice[2,1] := TJezdec.Create(Bila,Self,2,1);
      Sachovnice[7,1] := TJezdec.Create(Bila,Self,7,1);
      Sachovnice[6,1] := TStrelec.Create(Bila,Self,6,1);
      Sachovnice[3,1] := TStrelec.Create(Bila,Self,3,1);
      Sachovnice[4,1] := TDama.Create(Bila,Self,4,1);
      Sachovnice[5,1] := TKral.Create(Bila,Self,5,1);

      for I := 1 to 8 do // cerni
        Sachovnice[I,7] := TPesak.Create(Cerna,Self,I,7);
      Sachovnice[1,8] := TVez.Create(Cerna,Self,1,8);
      Sachovnice[8,8] := TVez.Create(Cerna,Self,8,8);
      Sachovnice[2,8] := TJezdec.Create(Cerna,Self,2,8);
      Sachovnice[7,8] := TJezdec.Create(Cerna,Self,7,8);
      Sachovnice[6,8] := TStrelec.Create(Cerna,Self,6,8);
      Sachovnice[3,8] := TStrelec.Create(Cerna,Self,3,8);
      Sachovnice[4,8] := TDama.Create(Cerna,Self,4,8);
      Sachovnice[5,8] := TKral.Create(Cerna,Self,5,8);
    end;
  end;

//------------------------------------------------------------------------------

  procedure TSachovnice.ZjistiVsechnyTahy(X,Y: TRozmer; var Tahy: TTahy);

  begin
    if (Self.Sachovnice[X,Y] = NIL) or not(JePlatnyTah(X,Y)) then
      Tahy.Vycisti
      else
        Self.Sachovnice[X,Y].ZjistiTahy(Tahy);
  end;

//------------------------------------------------------------------------------

  procedure TSachovnice.ZjistiTahy(X,Y: TRozmer; var Tahy: TTahy);

  var
    I,J: TRozmer;
    PomKamen: TKamen;
    PomBarva: TBarva;

  begin
    Self.ZjistiVsechnyTahy(X,Y,Tahy);                // zjisteni tahu
    PomBarva := Self.ZjistiBarvu(X,Y);

    for J := 1 to 8 do                               // vyzkouseni vsech tahu
      for I := 1 to 8 do
        if Tahy.JeTah(I,J) then begin
          Self.VemKamen(I,J,PomKamen);
          Self.Presun(X,Y,I,J);
          if Self.JeSach(PomBarva) = true then       // pokud tah znamena vlastni sach
            Tahy.OdeberTah(I,J);                     // pak je odebran
          Self.Presun(I,J,X,Y);
          Self.Sachovnice[X,Y].ByloTazeno := false;  // kamenem jeste nebylo tazeno z hlediska hry
          Self.PostavKamen(I,J,PomKamen);
          PomKamen := NIL;
          end;

    if (Self.ZjistiJmeno(X,Y) = 'kr�l') and (not Self.ByloTazeno(X,Y)) and (Tahy.JeTah(7,Y)) then // zjisteni, zda se kral snzi o malo rosadu a jeji overeni
      if Self.JeSach(Self.ZjistiBarvu(X,Y)) then    // rosadou se nelze dostat z sachu
        Tahy.OdeberTah(7,Y)
        else begin
          Self.Presun(X,Y,6,Y);                     // testovaci presun na vedlejsi pole pro overeni ohrozeni
          if Self.Sachovnice[6,Y].JeVOhrozeni then
            Tahy.OdeberTah(7,Y);
          Self.Presun(6,Y,X,Y);                     // presun zpet
          end;

    if (Self.ZjistiJmeno(X,Y) = 'kr�l') and (not Self.ByloTazeno(X,Y)) and (Tahy.JeTah(3,Y)) then // overeni velke rosady
      if Self.JeSach(Self.ZjistiBarvu(X,Y)) then
        Tahy.OdeberTah(3,Y)
        else begin
          Self.Presun(X,Y,4,Y);
          if Self.Sachovnice[4,Y].JeVOhrozeni then
            Tahy.OdeberTah(3,Y);
          Self.Presun(4,Y,X,Y);
          end;
  end;

//------------------------------------------------------------------------------

  procedure TSachovnice.VemKamen(X,Y: Trozmer; var Kamen: TKamen);

  begin
    Kamen := Self.Sachovnice[X,Y];
    Self.Sachovnice[X,Y] := NIL;
  end;

//------------------------------------------------------------------------------

  procedure TSachovnice.PostavKamen(X,Y: Trozmer; Kamen: TKamen);

  begin
    if Self.Sachovnice[X,Y] <> NIL then
      Self.Sachovnice[X,Y].Free;
    Self.Sachovnice[X,Y] := Kamen;
  end;

//------------------------------------------------------------------------------

  constructor TKamen.Create(Barva: TBarva; Sachovnice: TSachovnice; X,Y: TRozmer);

  begin
    Self.Sachovnice := Sachovnice;      // nastaveni sachovnice, kde se kamen nachazi
    Self.PoziceX := X;                  // nastaveni pozice X
    Self.PoziceY := Y;                  // nastaveni pozice Y
    Self.Barva := Barva;                // nastaveni vlastni barvy
    Self.ByloTazeno := false;           // kamenem zatim nebylo tazeno
    Self.EnPassant := false;
    if Barva = Bila then
      Self.BarvaSouper := Cerna         // nastaveni barvy soupere
      else
        Self.BarvaSouper := Bila;       // nastaveni barva soupere
  end;

//------------------------------------------------------------------------------

  function TKamen.JeVOhrozeni: Boolean;

  var
    I,J: TRozmer;
    Tahy: TTahy;

  begin
    Tahy := TTahy.Create;

    for J := 1 to 8 do                                                     // testovani cele sachovnice
      for I := 1 to 8 do
        if Self.Sachovnice.ZjistiBarvu(I,J) = Self.BarvaSouper then begin  // pokud je nalezen souperuv kamen
          Self.Sachovnice.ZjistiVsechnyTahy(I,J,Tahy);                     // pak se zjisti jeho mozne tahy
          if Tahy.JeTah(Self.PoziceX,Self.PoziceY) then begin              // a pokud je vlastni pozice jednim z nalezenych tahu
            Result := true;                                                // pak je kamen v ohrozeni
            Tahy.Free;
            Exit;
            end;
          end;

    Tahy.Free;
    Result := false;  // kamen neni v ohrozeni pokud funkce dojde az sem
  end;

//------------------------------------------------------------------------------

  procedure TKamen.Presun(X,Y: TRozmer);

  var
    PomKamen: TKamen;

  begin
    if (Self.ClassName = 'TPesak') and ( ((Self.PoziceY = 2) and (Y = 4)) or ((Self.PoziceY = 7) and (Y = 5)) ) then // povoleni tahu en passant
      Self.EnPassant := true;

    Self.Sachovnice.VemKamen(Self.PoziceX,Self.PoziceY,PomKamen); // sejmuti kamene
    Self.Sachovnice.PostavKamen(X,Y,PomKamen);                    // postaveni na cilovou pozici
    Self.PoziceX := X;       // nastaveni nove pozice
    Self.PoziceY := Y;
  end;

//------------------------------------------------------------------------------

  procedure TPesak.ZjistiTahy(var Tahy: TTahy);

  var
    Pom: ShortInt;      // kvuli kladnemu/zapornemu smeru overovani smeru (dopredu/dozadu)
    StartPoz: TRozmer;  // pesakova Ypsylonova startovni pozice (pro bile 2, pro cerne 7)

  begin
    if Self.Barva = Bila then begin
      Pom := 1;
      StartPoz := 2;
      end
      else begin
        Pom := -1;
        StartPoz := 7;
        end;

    Tahy.Vycisti;

    if Self.Sachovnice.ZjistiBarvu(Self.PoziceX,Self.PoziceY + Pom) = Zadna then begin // overeni pozice o jedno vpred
      Tahy.PridejTah(PoziceX,PoziceY + Pom);
      if (Self.PoziceY = StartPoz) and (Sachovnice.ZjistiBarvu(Self.PoziceX,PoziceY + 2 * Pom) = Zadna) then // overeni mozneho tahu o 2 dopredu ze startovni pozice
        Tahy.PridejTah(Self.PoziceX,Self.PoziceY + 2 * Pom);
      end;

    if Self.Sachovnice.ZjistiBarvu(Self.PoziceX + 1,Self.PoziceY + Pom) = Self.BarvaSouper then // overeni moznosti vzit soupere sikmo vpravo
      Tahy.PridejTah(Self.PoziceX + 1,Self.PoziceY + Pom);

    if Self.Sachovnice.ZjistiBarvu(Self.PoziceX - 1,Self.PoziceY + Pom) = Self.BarvaSouper then // overeni moznosti vzit soupere sikmo vlevo
      Tahy.PridejTah(Self.PoziceX - 1,Self.PoziceY + Pom);

    if (Self.Sachovnice.ZjistiBarvu(Self.PoziceX + 1,Self.PoziceY) = Self.BarvaSouper) and // overeni moznosti tahu en passant (vpravo)
       (Self.Sachovnice.LzeEnPssant(Self.PoziceX + 1,Self.PoziceY)) then
      Tahy.PridejTah(Self.PoziceX + 1,Self.PoziceY + Pom);

    if (Self.Sachovnice.ZjistiBarvu(Self.PoziceX - 1,Self.PoziceY) = Self.BarvaSouper) and // overeni moznosti tahu en passant (vlevo)
       (Self.Sachovnice.LzeEnPssant(Self.PoziceX - 1,Self.PoziceY)) then
      Tahy.PridejTah(Self.PoziceX - 1,Self.PoziceY + Pom);

  end;

//------------------------------------------------------------------------------

  procedure TVez.ZjistiTahy(var Tahy: TTahy);

  var
   I,J,K: ShortInt;

  begin
    Tahy.Vycisti;

    for I := -1 to 1 do
      for J := -1 to 1 do
        if not((I = 0) and (J = 0)) and ((I = 0) or (J = 0)) then begin  // aby se overoval pouze horizontalni a vertikalni smer tahu
        K := 1;
        while Self.Sachovnice.ZjistiBarvu(Self.PoziceX + I * K,Self.PoziceY + J * K) = Zadna do begin // postupujeme po polich dokud nenarazime na kamen (vlastni nebo souperuv)
          Tahy.PridejTah(Self.PoziceX + I * K,Self.PoziceY + J * K);
          K := K + 1;
          end;
        if Self.Sachovnice.ZjistiBarvu(Self.PoziceX + I * K,Self.PoziceY + J * K) = Self.BarvaSouper then // pokud je kamen souperuv, pak jej lze vzit (pridame dalsi tah)
          Tahy.PridejTah(Self.PoziceX + I * K,Self.PoziceY + J * K);
        end;
  end;

//------------------------------------------------------------------------------

  procedure TJezdec.ZjistiTahy(var Tahy: TTahy);

  var
    I,J: ShortInt;

  begin
    Tahy.Vycisti;

    for I := -1 to 1 do
      for J := -1 to 1 do
        if not((I = 0) and (J = 0)) then begin  // aby nebyl mezi tahy zarazen tah na vychozi pozici
          if I = 0 then begin
            if Self.Sachovnice.ZjistiBarvu(Self.PoziceX + 1,Self.PoziceY + 2 * J) <> Self.Barva then
              Tahy.PridejTah(Self.PoziceX + 1,Self.PoziceY + 2 * J);
            if Self.Sachovnice.ZjistiBarvu(Self.PoziceX - 1,Self.PoziceY + 2 * J) <> Self.Barva then
              Tahy.PridejTah(Self.PoziceX - 1,Self.PoziceY + 2 * J);
            end
            else
              if J = 0 then begin
                if Self.Sachovnice.ZjistiBarvu(Self.PoziceX + 2 * I,Self.PoziceY + 1) <> Self.Barva then
                  Tahy.PridejTah(Self.PoziceX + 2 * I,Self.PoziceY + 1);
                if Self.Sachovnice.ZjistiBarvu(Self.PoziceX + 2 * I,Self.PoziceY - 1) <> Self.Barva then
                  Tahy.PridejTah(Self.PoziceX + 2 * I,Self.PoziceY - 1);
                end;
        end;
  end;

//------------------------------------------------------------------------------

  procedure TStrelec.ZjistiTahy(var Tahy: TTahy);

  var
    I,J,K: ShortInt;

  begin
    Tahy.Vycisti;

    for I := -1 to 1 do
      for J := -1 to 1 do
        if (I <> 0) and (J <> 0) then begin   // aby se overovali pouze diagonalni tahy
        K := 1;
        while Self.Sachovnice.ZjistiBarvu(Self.PoziceX + I * K,Self.PoziceY + J * K) = Zadna do begin // postupujeme po polich dokud nenarazime na kamen (vlastni nebo souperuv)
          Tahy.PridejTah(Self.PoziceX + I * K,Self.PoziceY + J * K);
          K := K + 1;
          end;
        if Self.Sachovnice.ZjistiBarvu(Self.PoziceX + I * K,Self.PoziceY + J * K) = Self.BarvaSouper then // pokud je kamen souperuv, lze jej vzit (pridame dalsi tah)
          Tahy.PridejTah(Self.PoziceX + I * K,Self.PoziceY + J * K);
        end;
  end;
  
//------------------------------------------------------------------------------

  procedure TDama.ZjistiTahy(var Tahy: TTahy);

  var
    I,J,K: ShortInt;

  begin
    Tahy.Vycisti;

    for I := -1 to 1 do
      for J := -1 to 1 do
        if not((I = 0) and (J = 0)) then begin  // aby nebyl mezi tahy zarazen tah na vychozi pozici
        K := 1;
        while Self.Sachovnice.ZjistiBarvu(Self.PoziceX + I * K,Self.PoziceY + J * K) = Zadna do begin // postupujeme po polich dokud nenarazime na kamen (vlastni nebo souperuv)
          Tahy.PridejTah(Self.PoziceX + I * K,Self.PoziceY + J * K);
          K := K + 1;
          end;
        if Self.Sachovnice.ZjistiBarvu(Self.PoziceX + I * K,Self.PoziceY + J * K) = Self.BarvaSouper then // pokud je kamen souperuv, lze jej vzit (pridame dalsi tah)
          Tahy.PridejTah(Self.PoziceX + I * K,Self.PoziceY + J * K);
        end;
  end;

//------------------------------------------------------------------------------

  procedure TKral.ZjistiTahy(var Tahy: TTahy);

  var
    I,J: ShortInt;

  begin
    Tahy.Vycisti;

    for I := -1 to 1 do
      for J := -1 to 1 do
        if not((I = 0) and (J = 0)) then  // aby nebyl mezi tahy zarazen tah na vychozi pozici
          if Self.Sachovnice.ZjistiBarvu(Self.PoziceX + I,Self.PoziceY + J) <> Self.Barva then
            Tahy.PridejTah(Self.PoziceX + I,Self.PoziceY + J);

    if (Self.ByloTazeno = false)                                  and           // overeni male rosady
       (Self.Sachovnice.ZjistiJmeno(8,Self.PoziceY) = 'v�')      and           // zda je v rohu vez
       (not Self.Sachovnice.ByloTazeno(8,Self.PoziceY))           and           // zda ji nebylo tazeno
       (Self.Sachovnice.ZjistiBarvu(8,Self.PoziceY) = Self.Barva) and           // zda je stejne barvy
       (Self.Sachovnice.ZjistiBarvu(6,Self.PoziceY) = Zadna)      and           // zda jsou pole mezi kralem a vezi prazdna
       (Self.Sachovnice.ZjistiBarvu(7,Self.PoziceY) = Zadna)      then
         Tahy.PridejTah(7,Self.PoziceY);

    if (Self.ByloTazeno = false)                                  and           // overeni velke rosady
       (Self.Sachovnice.ZjistiJmeno(1,Self.PoziceY) = 'v�')      and           // zda je v rohu vez
       (not Self.Sachovnice.ByloTazeno(1,Self.PoziceY))           and           // zda ji nebylo tazeno
       (Self.Sachovnice.ZjistiBarvu(1,Self.PoziceY) = Self.Barva) and           // zda je stejne barvy
       (Self.Sachovnice.ZjistiBarvu(2,Self.PoziceY) = Zadna)      and           // zda jsou pole mezi kralem a vezi prazdna
       (Self.Sachovnice.ZjistiBarvu(3,Self.PoziceY) = Zadna)      and
       (Self.Sachovnice.ZjistiBarvu(4,Self.PoziceY) = Zadna)      then
         Tahy.PridejTah(3,Self.PoziceY);
  end;

//------------------------------------------------------------------------------

end.

  