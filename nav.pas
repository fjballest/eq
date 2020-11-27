(*
 * Fuente TPascal + ToolBox -- (c) 1991,1992  Esther Li#an y Nemo
 *
 * Modulo : [Nav] Navegador: Viaje a traves de una serie de puntos.
 * Rev    : 1.0
 * O.S.   : DOS
 *
 *
 *)


Unit Nav;

Interface (*Nav*)

  Uses  Graph;  (* Unid. std. *)

  Const Max_Ptos_Nav   = 50;
        Dim_Cursor_Nav : Integer = 16;


  Type  Tipo_vnav= Array[1..Max_Ptos_Nav] Of PointType;

        Tipo_nav  = Record
                       pto       : Integer;
                       ptos      : Tipo_vnav;
                       num_ptos  : Integer;
                       cursor    : Pointer
                    End;

        (* Comandos de navegacion *)
        Tipo_cmd_nav = ( NIniciar,      (* empezar a navegar...*)
                         NAvanzar,      (* navegar...*)
                         NRetroceder,
                         NTerminar,     (* Fin de navegacion *)
                         NNada          (* no hace nada *)
                       );

  Procedure Iniciar_nav( Var n : Tipo_nav; c : Pointer );
  (*
   * Devuelve un navegador vacio con cursor c.
   *)

  Procedure Vaciar_nav( Var n : Tipo_nav );
  (*
   * Vacia el navegador.
   *)

  Procedure Incluir_en_nav( Var n : Tipo_nav; px, py : Integer );
  (*
   * Incluye el pto. para navegar.
   *)

  Procedure Incluir_pto_en_nav( Var n : Tipo_nav; p : PointType );
  (*
   * Incluye p en nav
   *)

  Procedure Navegar( Var n : Tipo_nav; cmd : Tipo_cmd_nav );
  (*
   * Ejecuta el cmd.
   *)


Implementation (*Nav*)

  Procedure Vaciar_nav( Var n : Tipo_nav );
  (*
   * Vacia el navegador.
   *)
  Begin
    n.num_ptos := 0
  End;

  Procedure Iniciar_nav( Var n : Tipo_nav; c: Pointer );
  (*
   * Devuelve un navegador vacio
   *)
  Begin
     Vaciar_nav(n);
     n.cursor   := c
  End;

  Procedure Incluir_en_nav( Var n : Tipo_nav; px, py : Integer );
  (*
   * Incluye el pto. para navegar.
   *)
  Begin
     If ( n.num_ptos < Max_Ptos_Nav ) Then Begin
       Inc( n.num_ptos );
       With n.ptos[n.num_ptos] Do Begin
         x := px;
         y := py
       End
     End
  End;

  Procedure Incluir_pto_en_nav( Var n : Tipo_nav; p : PointType );
  (*
   * Incluye p en nav
   *)
  Begin
     Incluir_en_nav( n, p.x, p.y )
  End;

  Procedure Marcar( Var nav_cursor : Pointer;
                    p : PointType );
  Begin
      PutImage( p.x - Dim_Cursor_Nav Div 2 + 1,
                p.y - Dim_Cursor_Nav Div 2 + 1,
                nav_cursor^,
                XorPut);
  End;

  Procedure Desmarcar( Var nav_cursor : Pointer;
                       p : PointType );
  Begin
    Marcar(nav_cursor,p)
  End;


  Procedure Navegar( Var n : Tipo_nav; cmd : Tipo_cmd_nav );
  (*
   * Ejecuta el cmd.
   *)
  Begin
    If (n.cursor <> NIL) And ( cmd <> Nnada ) Then
      Case cmd Of
        NIniciar: Begin
          n.pto := 1;
          Marcar( n.cursor, n.ptos[n.pto] )
          End;

        NAvanzar: Begin
          Desmarcar( n.cursor, n.ptos[n.pto] );
          Inc( n.pto );
          If n.pto > n.num_ptos Then
            n.pto := 1;
          Marcar(n.cursor,n.ptos[n.pto])
          End;

        NRetroceder: Begin
          Desmarcar( n.cursor, n.ptos[n.pto] );
          Dec( n.pto );
          If n.pto < 1 Then
            n.pto := n.num_ptos;
          Marcar(n.cursor,n.ptos[n.pto])
          End;

        NTerminar: Desmarcar( n.cursor, n.ptos[n.pto] )

      End

  End;


Begin

End.(*Nav*)

