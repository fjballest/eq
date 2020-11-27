Unit Fun_Mat;

     (*
      * Funciones matematicas generales: Para no incluir todo el codigo de
      *                                  La unidad de las Tool-Box
      *)

Interface


  Const Infinito_Real = 1e30;

    Function Log( n: Real ) : Real;
    (*
     * Log base 10
     *)


  Function Unidad( r : Real ): Integer;
  (* Devuelve la unidad de r.
   *)

  Function Decena( r : Real ): Integer;
  (* Devuelve la decena de r.
   *)

  Function Trunc_por_u( r: Real ): Real;
  (* Devuelve r con ceros por unidad y decimales.
   *)

  Function Trunc_por_d( r: Real ): Real;
  (* Devuelve r con ceros por dec., unid. y decimales.
   *)

  (*
   * Las siguientes funciones dan informacion sobre lineas en un plano.
   *   Las acabadas en ...2p dan informacion tomando para cada linea 2 ptos.
   *   Las acabadas en pc, dan informacion segun y = p * x + c
   *)

   (*y = Pendiente x + constante
    *)
  Function Pendiente_2p( x1,y1,x2,y2 : Real ): Real;
  Function Constante_2p( x1,y1,x2,y2   : Real ) : Real;

  Function Y_2p( x, x1,y1,x2,y2 : Real ) : Real;
  Function Y_pc( x, p,c : Real ) : Real;

  Function X_corte_pc( p1,c1, p2,c2 : Real ): Real;
  Function Y_corte_pc( p1,c1, p2,c2 : Real ): Real;



Implementation


    Function Log( n : Real ) : Real;
    (*
     * Log decimal
     *)
    Const Log_e = 2.3025851;
    Begin
       Log := Ln( n ) / Log_e
    End;

  Function Decena( r : Real ): Integer;
  (* Devuelve la decena de r.
   *)
  Begin
      Decena := (Trunc(r) Div 10) - (Trunc(r) Div 100)*10
  End;

  Function Unidad( r : Real ): Integer;
  (* Devuelve la unidad de r.
   *)
  Begin
      Unidad := Trunc(r) - (Trunc(r) Div 10)*10
  End;

  Function Trunc_por_u( r: Real ): Real;
  (* Devuelve r con ceros por unidad y decimales.
   *)
  Begin
     Trunc_por_u := Int( r )
  End;

  Function Trunc_por_d( r: Real ): Real;
  (* Devuelve r con ceros por dec., unid. y decimales.
   *)
  Begin
     Trunc_por_d := Int( r ) - Unidad( r )
  End;


  Function Pendiente_2p( x1,y1,x2,y2 : Real ): Real;
  Begin
      Pendiente_2p := (y2-y1) / (x2-x1)
  End;

  Function Constante_2p( x1,y1,x2,y2   : Real ) : Real;
  Begin
      Constante_2p := y1 - Pendiente_2p(x1,y1,x2,y2) * x1
  End;

  Function Y_2p( x, x1,y1,x2,y2 : Real ) : Real;
  Begin
       Y_2p := Pendiente_2p(x1,y1,x2,y2) * x + Constante_2p(x1,y1,x2,y2)

  End;

  Function Y_pc( x, p,c : Real ) : Real;
  Begin
       Y_pc := p * x + c
  End;

  Function X_corte_pc( p1,c1, p2,c2 : Real ): Real;
  Begin
       X_corte_pc := ( c2 - c1 ) / (p1-p2)
  End;

  Function Y_corte_pc( p1,c1, p2,c2 : Real ): Real;
  Begin
      Y_corte_pc := p1 * X_corte_pc(p1,c1,p2,c2) + c1
  End;



End.