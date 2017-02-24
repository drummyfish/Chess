unit Dalsi;

{ 
  dalsi datove typy
  -----------------
  modul, ktery obsahuje ostatni prostredky potrebne pro beh programu Sachy
  -----------------
  projekt:         Sachy
  autor:           Miloslav Ciz
  posledni uprava: 19.03.2010
}

INTERFACE

type
  TRozmer = 1..8;                 // rozmer sachovnice
  TBarva  = (Cerna, Bila, Zadna); // mozne barvy

  function JePlatnyTah(X,Y: Integer): Boolean;  // overuje, zda je tah v mezich sachovnice

//------------------------------------------------------------------------------

IMPLEMENTATION

  function JePlatnyTah(X,Y: Integer): Boolean;

  begin
    Result := (X >= 1) and (X <= 8) and (Y >= 1) and (Y <= 8);
  end;

//------------------------------------------------------------------------------

end.