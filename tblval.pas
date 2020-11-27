(*
 * Fuente TPascal + ToolBox -- (c) 1991,1992  Esther Li#an y Nemo
 *
 * Modulo : [TblVal] Tabla de valoracion: Almacenam. y dibujo tabla val.
 * Rev    : 1.0
 * O.S.   : DOS
 *
 *
 *)


Unit TblVal;

Interface (*TblVal*)

   Uses Graph,     (* Unidades std ... *)
        Printer,

        TpString,  (* Tool-Box ... *)

        Opcs,      (* Opciones  *)
        GrIO;      (* IO Grafico*)

   Const Num_Digits : Integer = 8; (* Numero de digitos de cada col. *)
                                   (* Solo modificable antes de inicializar*)
         TBL_ERR = -1;


   Type Tipo_vvals = Array[1..Max_ptos_normal] Of Record
                                                     v,c,d,p : Real
                                                    End;
        Tipo_tbl = Record
           num_lineas   : Integer; (* Numero de lineas *)
           cx_linea1    : Integer; (* Coord. *)
           cy_linea1    : Integer; (*    de la primera linea (%) *)
           cx_linea2    : Integer; (* Coord. x del pH (der. si no ph)*)
           cx_linea3    : Integer; (* Coord. x de la der. si ph *)
           li_actual    : Integer; (* Numero de linea actual *)
           num_vals     : Integer;
           vals         : Tipo_vvals;

        End;

  Procedure Iniciar_Tabla( min_y, max_y : Integer;
                           Var max_x    : Integer;
                           Var salida   : Tipo_opc_salida;
                           Var t        : Tipo_tbl        );
  (*
   * Inicia el iterador de tabla (t) y dibuja la tabla en pantalla
   * en los limites min/max_y.
   * Devuelve en max_x el punto maximo dibujado.
   * Dependiendo de salida abre el fichero de salida/impresora etc.
   *)

  Procedure Indicar_en_tabla( Var op           : Tipo_opc_salida;
                              val1, val2, val3, val4 : Real;
                              Var t            : Tipo_tbl;
                              valor_man        : Boolean );
  (*
   * Indica los valores ( segun op ) en t.
   * Si valor_man es True lo toma como valor manual, almacenandolo y
   * actualizandolo en t solo si op asi lo indica.
   *)

  Procedure Terminar_Tabla( Var o : Tipo_opc_salida;
                            Var t : Tipo_tbl );
  (*
   * Da oportunidad de terminacion para las actividades relacionadas
   * con la tabla de pHs. [Cerrar ficheros,etc.]
   *)

  Procedure Consultar_vals( Var t: Tipo_tbl; pto : Integer;
                            Var r1,r2,r3,r4 : Real            );
  (*
   * Devuelve los tres pto-esimos valores almacenados en t en r1,r2,r3
   * Si no estan devuelve en ellos TBL_ERR
   *)


Implementation (*TblVal *)


  Procedure Iniciar_Tabla( min_y, max_y : Integer;
                           Var max_x    : Integer;
                           Var salida   : Tipo_opc_salida;
                           Var t        : Tipo_tbl        );
  (*
   * Inicia el iterador de tabla (t) y dibuja la tabla en pantalla
   * en los limites min/max_y.
   * Devuelve en max_x el punto maximo dibujado.
   * Dependiendo de salida abre el fichero de salida/impresora etc.
   *)

    Procedure Iniciar_Tabla_Grafica( Var salida   : Tipo_opc_salida;
                                     min_y, max_y : Integer;
                                     Var max_x    : Integer;
                                     Var t        : Tipo_tbl  );
    (*
     * Dibuja la tabla y define en t aquellos campos dependientes de
     * dicho dibujo.
     *)



     Const borde = 3;
           min_x = 0;

     Function Numero_columnas ( Var s : Tipo_opc_salida ): Integer;
     Begin
        If s.curva And s.derivada Then
           Numero_columnas := 3
        Else If s.curva Or s.derivada Then
           Numero_columnas := 2
        Else
           Numero_columnas := 1
     End;

     Var num_cols : Integer;

    Begin(*Iniciar_Tabla_Grafica*)

     num_cols := Numero_columnas( salida );

     (* Coords. graf. *)
     max_x := min_x + borde * 2 * num_cols +
              ( num_digits * TextWidth('X')) * num_cols + borde;
     t.cx_linea1 := min_x+borde+borde Div 2;
     t.cx_linea2 := t.cx_linea1 +
                    borde  + ( num_digits * TextWidth('X'));
     t.cx_linea3 := t.cx_linea2 +
                    borde  + ( num_digits * TextWidth('X'));

     (* Dibujar tabla *)
     Line( min_x+borde,min_y+1,max_x-1,min_y+1 );
     Line( min_x+borde,min_y+1,min_x+borde,max_y-1 );
     Line( max_x-1,min_y+1,max_x-1,max_y-1); (* Recuadro exterior *)
     Line( min_x+borde,max_y-1,max_x-1,max_y-1);
     Line( min_x+borde,min_y+borde+TextHeight('X')+1,
           max_x-1,    min_y+borde+TextHeight('X')+1);
     If num_cols > 1 Then
       Line( t.cx_linea2 - Borde Div 2, min_y+1,
             t.cx_linea2 - Borde Div 2, max_y-1);
     If num_cols = 3 Then
       Line( t.cx_linea3 - Borde Div 2, min_y+1,
             t.cx_linea3 - Borde Div 2, max_y-1);

     (* Poner Titulos *)
     OutTextXY( t.cx_linea1, min_y+borde,' % Val.' );
     If salida.curva Then Begin
       OutTextXY( t.cx_linea2, min_y+borde,'   pH' );
       If salida.derivada Then
          OutTextXY( t.cx_linea3, min_y+borde,'  d pH' );
       End
     Else if salida.derivada Then
       OutTextXY( t.cx_linea2,     min_y+borde,   '  d pH' );

     (* Parametros para insercion dependientes de la grafica... *)
     t.cy_linea1 := min_y+2*borde+TextHeight('X');
     t.num_lineas:= (max_y - t.cy_linea1) DIV (TextHeight('X')+1) ;
     t.li_actual := 0

    End;(*Iniciar_Tabla_Grafica*)

    Procedure Iniciar_tabla_fichero( Var f : Text; Var done: Boolean );
    Begin
      {$I-}
        WriteLn(f,'# %val.     pH      d pH #');
        WriteLn(f,'#------   ------   ------#');
        Done := IOResult = 0
      {$I+}
    End;(*Iniciar_tabla_fichero*)

    Var done : Boolean;


  Begin(*Iniciar_tabla*)

    t.num_vals := 0;

    Iniciar_Tabla_Grafica( salida, min_y, max_y, max_x,t );
    If salida.a_impresora Then Begin (* estado de la impresora ? *)

    End;
    If salida.nombre_fich <> Path_Nulo Then Begin(* a fichero *)
      Iniciar_tabla_fichero( salida.fichero_dos, done );
      If Not done Then
        salida.nombre_fich := Path_Nulo
    End;
    If salida.a_impresora Then Begin
      Iniciar_tabla_fichero( lst, done );
      If Not done Then
          salida.a_impresora := False
    End
  End;(*Iniciar_tabla*)

  Procedure Indicar_en_tabla( Var op           : Tipo_opc_salida;
                              val1, val2, val3, val4 : Real;
                              Var t            : Tipo_tbl;
                              valor_man        : Boolean );
  (*
   * Indica los valores ( segun op ) en t.
   * Si valor_man es True lo toma como valor manual, almacenandolo y
   * actualizandolo en t solo si op asi lo indica.
   *)

     Procedure Indicar_tbl_grf( Var op : Tipo_opc_salida;
                                val1,val2,val3 : Real;
                                Var t  : Tipo_tbl        );
        Var x,y : Integer;
     Begin
        x := t.cx_linea1;
        y := t.cy_linea1+t.li_actual*(TextHeight('X')+1);
        Imprimir_str_xyl( x, y, num_digits, ' '+Form('###.##',val1));

        If op.curva Or op.derivada Then Begin
           x := t.cx_linea2;
           If val2 = MaxLongInt Then
             Imprimir_str_xyl( x, y, num_digits-1,' infin.')
           Else If val2 = -MaxLongInt Then
             Imprimir_str_xyl( x, y, num_digits-1,'-infin.')
           Else
             Imprimir_str_xyl( x,y, num_digits-1,' '+Form('###.##',val2));

           If op.curva And op.derivada Then Begin
              x := t.cx_linea3;
              If val3 = MaxLongInt Then
                Imprimir_str_xyl( x, y, num_digits-1,' infin.')
              Else If val3 = -MaxLOngInt Then
                Imprimir_str_xyl( x, y, num_digits-1,'-infin.')
              Else
                Imprimir_str_xyl( x,y, num_digits-1,' '+Form('###.##',val3));
           End

        End;

        Inc( t.li_actual );
        If  t.li_actual >= t.num_lineas Then (* Llena! *)
          t.li_actual := 0;

        x := t.cx_linea1;
        y := t.cy_linea1+t.li_actual*(TextHeight('X')+1);
        Imprimir_str_xyl(x,y,num_digits,'');

        If op.curva Or op.derivada Then Begin
          x := t.cx_linea2;
          Imprimir_str_xyl(x,y,num_digits-1,'');
          If op.curva And op.derivada Then Begin
             x := t.cx_linea3;
             Imprimir_str_xyl(x,y,num_digits-1,'');
          End
        End;
     End; (*Indicar_tbl_grf*)

     Procedure Fijar_valores( Var op : Tipo_opc_salida; Var r1,r2,r3 : Real );
     (*
      * Dependiendo de op. ordena r1,r2 y r3 como %,ph,d ph. Eliminando los
      * que no existan.
      *)
     Begin
       If op.derivada And ( Not op.curva ) Then (* d pH el 2a. col. *)
         val2 := val3;
     End;

     Procedure Indicar_tbl_fich( Var f: Text; r1,r2,r3 : Real;
                                 Var done: Boolean            );
     Begin
       {$I-}
         WriteLn( f, Form('###.##',r1),'   ',
                     Form('###.##',r2),'   ',
                     Form('###.##',r3)          );
         done := IOResult = 0
       {I+}
     End;

     Procedure Incluir_vals( Var t : Tipo_tbl; r1,r2,r3,r4 : Real );
      (*
       * Almacena los valores para recuperacion.
       *)
     Begin
         If t.num_vals < Max_ptos_normal Then Begin
            Inc(t.num_vals);
            With t.vals[t.num_vals] Do Begin
               p := r1; c := r2; d := r3; v := r4
            End

         End
     End;

     Var x,y  : Integer;
         done : Boolean;

  Begin (*Indicar_en_tabla*)

     If valor_man Then (* Solo incluir los manuales...*)
       Incluir_vals( t, val1,val2,val3,val4 );

     If Not (valor_man Xor (op.precision=Prec_Normal)) Then Begin
         Fijar_valores( op, val1, val2, val3 );
         Indicar_tbl_grf( op, val1, val2, val3, t );

         If op.nombre_Fich <> Path_Nulo Then Begin
            Indicar_tbl_fich( op.fichero_dos, val1, val2, val3, done );
            If Not done Then
               op.nombre_fich := Path_Nulo
         End;

         If op.a_impresora Then Begin
            Indicar_tbl_fich( lst, val1, val2, val3, done );
            If Not done Then
              op.a_impresora := False
          {$I+}
         End
     End

  End;(*Indicar_en_tabla*)

  Procedure Terminar_Tabla( Var o : Tipo_opc_salida;
                            Var t : Tipo_tbl );
  (*
   * Da oportunidad de terminacion para las actividades relacionadas
   * con la tabla de pHs. [Cerrar ficheros,etc.]
   *)

     Procedure Terminar_tbl_fich( Var f: Text; Var done : Boolean );
     Begin
        {$I-}
          WriteLn(o.fichero_dos,'#------   ------   ------#');
        {$I+}
        done := IOResult = 0
     End;

     Var done: Boolean;
  Begin
     If o.nombre_Fich <> Path_Nulo Then Begin
        Terminar_tbl_fich( o.fichero_dos, done );
        If Not done Then
          o.nombre_Fich := Path_Nulo
        Else
          {$I-} Close( o.fichero_dos );
          {$I+}
     End;

     If o.a_impresora Then Begin
        Terminar_tbl_fich( lst, done );
        If Not done Then
          o.a_impresora := False
     End

  End;(*Terminar_Tabla*)

  Procedure Consultar_vals( Var t: Tipo_tbl; pto : Integer;
                            Var r1,r2,r3,r4 : Real            );
  (*
   * Devuelve los tres pto-esimos valores almacenados en t en r1,r2,r3.
   * Si no estan devuelve en ellos TBL_ERR
   *)
  Begin
      If ( pto > 0 ) And ( pto <= t.num_vals ) Then
        With t.vals[pto] Do Begin
           r1 := p; r2 := c; r3 := d; r4 := v
        End
      Else Begin
         r1 := TBL_ERR; r2 := TBL_ERR; r3 := TBL_ERR; r4 := TBL_ERR
      End
  End;

Begin

End. (*TblVal*)

