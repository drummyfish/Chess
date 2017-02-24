unit CTahy;

{
  trida mnoziny tahu
  ------------------
  implementace tridy, jez dokaze uchovavat mnozinu jednotlivych poli na
  sachovnici (napr moznych tahu)
  ------------------
  projekt:         Sachy
  autor:           Miloslav Ciz
  posledni uprava: 19.03.2010
}

INTERFACE

uses
  Dalsi;

type
  TTahy = class                                       // trida umoznujici umoznujici pracovat s mnozinou poli (tahu) na sachovnici
            private
              Tahy: set of Byte;                      // samotna mnozina
              function TahNaByte(X,Y: Trozmer): Byte; // prevede tah na byte ulozitelny do mnoziny
            public
              constructor Create;
              function JeTah(X,Y: TRozmer): Boolean;  // overeni existence tahu v mnozine
              function ZadnyTah: Boolean;             // testuje prazdnost mnoziny tahu
              procedure PridejTah(X,Y: TRozmer);      // prida tah do mnoziny
              procedure OdeberTah(X,Y: TRozmer);      // odebere tah z mnoziny
              procedure Vycisti;                      // vyprazdni mnozinu
          end;

//------------------------------------------------------------------------------

IMPLEMENTATION

  constructor TTahy.Create;

  begin
    Self.Tahy := [];
  end;

//------------------------------------------------------------------------------

  function TTahy.TahNaByte(X,Y: Trozmer): byte;

  begin
    Result := X - 1 + (Y - 1) * 8;
  end;
  
//------------------------------------------------------------------------------

  function TTahy.ZadnyTah: Boolean;

  begin
    Result := Self.Tahy = [];
  end;

//------------------------------------------------------------------------------

  function TTahy.JeTah(X,Y: TRozmer): boolean;

  begin
    Result := TahNaByte(X,Y) in Self.Tahy;
  end;

//------------------------------------------------------------------------------

  procedure TTahy.PridejTah(X,Y: TRozmer);

  begin
    if JePlatnyTah(X,Y) then
      Self.Tahy := Self.Tahy + [TahNaByte(X,Y)];
  end;
  
//------------------------------------------------------------------------------

  procedure TTahy.OdeberTah(X,Y: TRozmer);

  begin
    if JePlatnyTah(X,Y) then
      Self.Tahy := Self.Tahy - [Self.TahNaByte(X,Y)];
  end;

//------------------------------------------------------------------------------

  procedure TTahy.Vycisti;

  begin
    Self.Tahy := [];
  end;

//------------------------------------------------------------------------------

end.