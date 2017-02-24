unit CVykreslovac;

{
  trida vykreslovac sachovnice
  -----------------
  implementace tridy pro vykreslovani sachovnice
  -----------------
  projekt:         Sachy
  autor:           Miloslav Ciz
  posledni uprava: 19.03.2010
}

INTERFACE

uses
  Controls, Graphics, Classes, CSachovnice, Dalsi, CTahy;

type
  TVykreslovac = class
                   private
                     Obrazky: TImageList;
                     Pozadi: TBitmap;
                   public
                     constructor Create(Obrazky: TImageList; Pozadi: TBitmap);
                     procedure Vykresli(Sachovnice: TSachovnice; Platno: TCanvas);
                     procedure Zvyrazni(Platno: TCanvas; X,Y: TRozmer; Cervena, Zelena, Modra: Integer);
                     procedure ZvyrazniTahy(Platno: TCanvas; Tahy: TTahy; Cervena, Zelena, Modra: Integer);
                     procedure ZmenVzhled(Obrazky: TImageList; Pozadi: TBitmap);
                 end;

//------------------------------------------------------------------------------

IMPLEMENTATION

  constructor TVykreslovac.Create(Obrazky: TImageList; Pozadi: TBitmap);

  begin
    Self.Obrazky := Obrazky;
    Self.Pozadi := Pozadi;
  end;

//------------------------------------------------------------------------------

  procedure TVykreslovac.Vykresli(Sachovnice: TSachovnice; Platno: TCanvas);

  var
    I,J: TRozmer;
    X,Y: Integer;
    Index: Byte;

  begin
    Platno.Brush.Bitmap := Self.Pozadi;
    Platno.FillRect(Rect(1,1,Pozadi.Width,Pozadi.Height));

    for J := 1 to 8 do
      for I := 1 to 8 do begin
        X := 13 + I + (I - 1) * Self.Obrazky.Width;
        Y := 14 + (8 - J) + (8 - J) * Self.Obrazky.Height;
        if ((I mod 2 = 0) and (J mod 2 = 0)) or ((I mod 2 <> 0) and (J mod 2 <> 0)) then
          Self.Obrazky.Draw(Platno,X,Y,7,true)                     // vykresleni cerneho pole
          else
            Self.Obrazky.Draw(Platno,X,Y,0,true);                  // vykresleni bileho pole
        if Sachovnice.ZjistiBarvu(I,J) <> Zadna then begin         // pokud neni pole prazdne
          if Sachovnice.ZjistiBarvu(I,J) = Bila then
            Index := 0
              else
                Index := 7;

          if Sachovnice.ZjistiJmeno(I,J) = 'pìšák' then
            Self.Obrazky.Draw(Platno,X,Y,1 + Index,true)
            else if Sachovnice.ZjistiJmeno(I,J) = 'jezdec' then
              Self.Obrazky.Draw(Platno,X,Y,2 + Index,true)
              else if Sachovnice.ZjistiJmeno(I,J) = 'støelec' then
                Self.Obrazky.Draw(Platno,X,Y,3 + Index,true)
                else if Sachovnice.ZjistiJmeno(I,J) = 'vìž' then
                  Self.Obrazky.Draw(Platno,X,Y,4 + Index,true)
                  else if Sachovnice.ZjistiJmeno(I,J) = 'dáma' then
                    Self.Obrazky.Draw(Platno,X,Y,5 + Index,true)
                    else
                      Self.Obrazky.Draw(Platno,X,Y,6 + Index,true)

          end;
        end;
  end;

//------------------------------------------------------------------------------

  procedure TVykreslovac.Zvyrazni(Platno: TCanvas; X,Y: TRozmer; Cervena, Zelena, Modra: Integer);

  var
    I,J,OdkudX,OdkudY,R,G,B: Integer;

  begin
    OdkudX := 13 + X + (X - 1) * Self.Obrazky.Width;       // prevod rozmeru z poli na sachovnici na pixely na platne
    OdkudY := 14 + (8 - Y) + (8 - Y) * Self.Obrazky.Height;
 
    for I := OdkudY to OdkudY + Self.Obrazky.Height do
      for J := OdkudX to OdkudX + Self.Obrazky.Width do begin
        R := Platno.Pixels[J,I] and $ff + Cervena;         // upraveni vsech slozek pixelu dle parametru
        if R > 255 then
          R := 255
          else if R < 0 then
            R := 0;
        G := (Platno.Pixels[J,I] and $ff00) shr 8 + Zelena;
        if G > 255 then
          G := 255
          else if G < 0 then
            G := 0;
        B := (Platno.Pixels[J,I] and $ff0000) shr 16 + Modra;
        if B > 255 then
          B := 255
          else if B < 0 then
            B := 0;
        Platno.Pixels[J,I] := B Shl 16 Or G Shl 8  Or R;;  // prirazeni novzch slozek pixelu
        end;
  end;

//------------------------------------------------------------------------------

  procedure TVykreslovac.ZvyrazniTahy(Platno: TCanvas; Tahy: TTahy; Cervena, Zelena, Modra: Integer);

  var
    I,J: TRozmer;

  begin
    for J := 1 to 8 do
      for I := 1 to 8 do
        if Tahy.JeTah(I,J) then
          Self.Zvyrazni(Platno,I,J,Cervena,Zelena,Modra);
  end;

//------------------------------------------------------------------------------

  procedure TVykreslovac.ZmenVzhled(Obrazky: TImageList; Pozadi: TBitmap);

  begin
    Self.Obrazky := Obrazky;
    Self.Pozadi := Pozadi;
  end;

//------------------------------------------------------------------------------

end.
 