(*
 * Fuente TPascal + ToolBox -- (c) 1991,1992  Esther Li#an y Nemo
 *
 * Modulo : [ItrVal] Iterador de valoraciones: Metodos para simular
 *                   el proceso de la valoracion.
 * Rev    : 1.0
 * O.S.   : DOS
 *
 *
 *)


Unit ItrVal;

Interface (*ItrVal*)

   Uses  Graph,   (* Unid. std. *)

         Opcs,
         Fun_mat, (* Funciones matematicas..*)
         Val;     (* Calculos sobre valoraciones *)

   Const (* Los dos simbolos siguientes definen el intervalo para el cual
          * El calculo de pH solo se puede hacer con la formula de equilibrio.
          * Al alcanzar dicho intervalo, se calcula el pH en el pto. de eq.
          * y se fija la iteracion para continuar PASADO el limite superior
          * del mismo
          *)
         Cien_Pcent_Inf : Real = 99.99;     (* 100% +/- 0.01 *)
         Cien_Pcent_Sup : Real = 100.01;

         Final_Pcent    : Real = 199.99; (* Indica el final de la iteracion*)

   Type Tipo_itr = Record
           (* Condiciones de iteracion...
            *)
           max_vol_bureta : Real; (* Vol. de bureta *)
           vol       : Real;      (* Vol. a valorar *)
           vol_step  : Real;

           (* Valores de eq...
            *)
           eq_ph,eq_vol : Real;
           max_der_ph,max_der_vol : Real;
           eq_preph, eq_postph : Real;

           valor_ins : Boolean; (* A true cuando se debe devolver el ultimo*)
                                (* valor calculado por haberse insertado uno*)
           ign_valorm: Boolean; (* Se pone a True para evitar considerar el*)
                                (* ultimo valor computado como manual.     *)

           (* Estado...
            *)

           ant_tmp,tmp : Tipo_momento; (* Tiempo de la ultima iteracion *)


           ult_pcent: Real;    (* Ultimo porcentaje de valoracion *)
           ult_pH   : Real;    (* Ulimo pH *)
           ult_vol  : Real;    (* Ultimo volumen de valoracion *)
           ult_der  : Real;

           ant_pH   : Real;    (* ph y vol anteriores al ultimo(actual)*)
           ant_vol  : Real;    (* para calculo de derivada             *)
           ant_der  : Real;
           ant_pcent: Real
        End;

  (*
   * Las siguientes funciones permiten activar el iterador.
   *)

  Procedure Iniciar_itr( Var i : Tipo_itr; Var val : Tipo_valoracion;
                         num_steps: Integer);
  (*
   * inicializa i para iterar con val como valoracion,
   * num_steps el No. de pasos en que se debe completar la valoracion.
   *
   *)

  Procedure Continuar_itr( Var i : Tipo_itr; Var val: Tipo_valoracion );
  (*
   * Continua la iteracion un paso.
   * Actualiza i.
   *)


  (*
   * Las siguientes funciones dan respuestas sobre el estado de la
   * ultima iteracion.
   *)

  Function Estimar_indicador_opt( Var i : Tipo_itr;
                                  Var v : Tipo_valoracion ) : Integer;

  Function Indicador_optimo_i( Var i: Tipo_itr;
                               Var v: Tipo_valoracion ): Integer;
  (*
   * Devuelve el indice el indicador optimo en inds para i.
   *)

  Function Tiempo_i( Var i : Tipo_itr ): Tipo_momento;
  (*
   * Informa del tiempo de la iteracion.
   *)


  Function Max_dph_ph_i( Var i : Tipo_itr ) : Real;
  Function Max_dph_vol_i(Var i : Tipo_itr ) : Real;
  (*
   * Devuelven (vol,ph) para el max/min de la dph.
   *)

  Function Eq_ph_i( Var i : Tipo_itr ) : Real;
  Function Eq_vol_i(Var i : Tipo_itr ) : Real;
  (*
   * Devuelven (vol,ph) para el pto. de eq. de la curva de ph.
   *)


  Function Pcent_i( Var i : Tipo_itr ) : Real;
  Function Ph_i( Var i : Tipo_itr ) : Real;
  Function Vol_i( Var i : Tipo_itr ): Real;
  Function Dph_i( Var i : Tipo_itr ): Real;
  (*
   * Devuelven los valores en el ultimo paso para el % de valoracion,
   * pH, vol y d pH. ( o bien los de eq. si valor_ins es True )
   *)

  Function Hay_paso_im( Var i : Tipo_itr ): Boolean;
  (*
   * Devuelve True cuando la ultima iteracion tiene valores manuales.
   *)

  Function Pcent_im( Var i : Tipo_itr ) : Real;
  Function Ph_im( Var i : Tipo_itr ) : Real;
  Function Vol_im( Var i : Tipo_itr ): Real;
  Function Dph_im( Var i : Tipo_itr ): Real;
  (*
   * Devuelven los valores en el ultimo paso para el % de valoracion,
   * pH, vol y d pH correspondientes a una iteracion manual:
   *   [ %vol.:0,1,10,20,...90,91,...99,100,101,...,109,110,120...190,200]
   * ( o bien los de eq. si valor_ins es True )
   *)





Implementation (*ItrVal*)


  Function Calcular_dph( Var i : Tipo_itr ) : Real;
  (*
   * Calcula la derivada.
   *)
  Const Cero = 0.0000001; (* Cualquier valor inferior se redondea a 0 *)
        Min_Cero = 0.009; (* Valores inf. de d pH se redondean a 0    *)

  Begin
    If i.valor_ins Then
      If Abs(i.eq_vol-i.ant_vol) <= Cero Then (* Infinito *)
         If i.eq_vol-i.ant_vol < 0 Then
          Calcular_dph := -MaxLongInt
         Else
          Calcular_dph := MaxLongInt
      Else If Abs((i.eq_pH-i.ant_pH)/(i.eq_vol-i.ant_vol)) <= Min_Cero Then
         Calcular_dph := 0
      Else
         Calcular_dph := (i.eq_pH - i.ant_pH)/(i.eq_vol-i.ant_vol)
    Else
      If Abs(i.ult_vol-i.ant_vol) <= Cero Then (* Infinito *)
        If i.ult_vol-i.ant_vol < 0 Then
         Calcular_dph := -MaxLongInt
        Else
         Calcular_dph := MaxLongInt
      Else If Abs((i.ult_pH-i.ant_pH)/(i.ult_vol-i.ant_vol)) <= Min_Cero Then
         Calcular_dph := 0
      Else
         Calcular_dph := (i.ult_pH-i.ant_pH)/(i.ult_vol-i.ant_vol)
  End;


  Function Interpolar( x, x1, x2, y1, y2 : Real ): Real;
  (*
   * Devuelve el valor de interp. lineal para y en ( x,y) conocidos
   * (x1,y1) y (x2,y2) entre los que esta (x,y).
   *)
  Begin
      If Abs(x1 - x2) > 0 Then
        Interpolar := y1 + (x-x1)*(y2-y1)/(x2-x1)
      Else If Abs( y1 - y2 ) = 0  Then
        Interpolar := (y1 + y2) / 2
      Else
        Interpolar := Infinito_Real
  End;

  Procedure Fijar_dpH( Var v: Tipo_valoracion;
                       Var i: Tipo_Itr      );
  Begin
     i.ant_der := i.ult_der;
     If (i.tmp > Antes) And (i.tmp < Final) Then Begin
       i.ult_der := Calcular_dph(i);
       If i.tmp = Equilibrio Then    (* En el eq. es infinito. hay que *)
         If i.ult_der > 0 THen       (* forzarlo pues no computamos el valor*)
           i.ult_der := MaxLongInt   (* exacto al tomar solo un pto. para la*)
         Else                        (* linea de eq. *)
           i.ult_der := -MaxLongInt;
       If Abs(i.max_der_ph) < Abs( i.ult_der ) Then Begin
         i.max_der_ph := i.ult_der;
         i.max_der_vol:= i.ult_vol
       End
     End
  End;


  Procedure Fijar_ph( Var v: Tipo_valoracion;
                      Var i: Tipo_Itr      );
  Begin
     i.ant_ph := i.ult_ph;
     If i.tmp < Despues Then
       i.ult_ph := Calcular_ph( v,i.tmp,i.ult_vol,0 )
     Else
       i.ult_ph := Calcular_ph( v,i.tmp,i.ult_vol,i.ult_vol- i.eq_vol )

  End;

  Procedure Iniciar_itr( Var i : Tipo_itr; Var val : Tipo_valoracion;
                         num_steps: Integer);
  (*
   * inicializa i para iterar con val como valoracion,
   * num_steps el No. de pasos en que se debe completar la valoracion.
   *
   *)

  Begin

     With i Do Begin
       max_vol_bureta := Dimension_bureta( val );
       vol := val.vol;
       vol_step := max_vol_bureta / num_steps;

       eq_pH    := 0;
       eq_vol   := 0;

       valor_ins := False;

       max_der_vol := 0;
       max_der_ph  := 0;

       tmp      := Antes;
       ant_tmp  := Antes;

       ult_pcent:= 0;
       ult_pH   := 0;
       ult_vol  := 0;
       ult_der  := 0;

       ant_vol  := 0;
       ant_der  := 0;
       ant_pcent:= 0;
       ant_pH   := 0
     End;

     Fijar_pH( val, i );
     i.ant_ph := i.ult_ph

  End;(*Iniciar_itr*)


  Procedure Continuar_itr( Var i : Tipo_itr; Var val: Tipo_valoracion );
  (*
   * Continua la iteracion un paso.
   * Actualiza i.
   *)
     Procedure Inc_vol( Var i : Tipo_itr );
     (*
      * Incrementa el vol. recordando el anterior y actualizando
      * los porcentajes de vol. actual y anterior.
      *)
     Begin
        i.ant_pcent:= i.ult_pcent;
        i.ant_vol  := i.ult_vol;
        i.ult_vol  := i.ult_vol + i.vol_step;
        i.ult_pcent:= Vol_neut( val, i.ult_vol )
     End;

     Procedure Fijar_tiempo( Var i: Tipo_itr );
     (*
      * Actualiza el tiempo de i, basandose en el % de valoracion.
      * Lo hace DURANTE la valoracion ( no antes ).
      *)
     Begin
        i.ant_tmp := i.tmp;

        If i.ult_pcent <= Cien_Pcent_Inf Then
           i.tmp := Durante
        Else If i.ant_pcent <= Cien_pcent_Sup Then
           i.tmp := Equilibrio
        Else If i.ult_pcent < Final_Pcent Then
           i.tmp := Despues
        Else
           i.tmp := Final
     End;


  Begin (*Continuar_itr*)

     i.ign_valorm := False;
     If (i.tmp <> Final) And (Not i.valor_ins) Then Begin
       Inc_vol( i );
       Fijar_tiempo( i );
       If i.tmp <> Equilibrio Then Begin (* Normal *)
         Fijar_ph( val, i );
         If i.ant_der = 0 Then Begin (* 1a.Itrac: d pH *)
            Fijar_dph( val,i );
            i.ant_der := i.ult_der
            End
         Else
            Fijar_dph( val, i );
         If (i.tmp <> i.ant_tmp ) And (i.tmp = Despues) Then
           i.eq_postph := i.ult_ph
       End
       Else Begin (* Eq.!*)

          i.eq_preph := i.ult_ph;
          i.eq_vol:= Interpolar( 100 , i.ant_pcent, i.ult_pcent,
                                          i.ant_vol,i.ult_vol );
          Fijar_ph( val, i );
          i.eq_ph := i.ult_pH;
          i.valor_ins := True;
          FIjar_dph( val, i );
          While i.ant_pcent <= Cien_pcent_Sup Do
             Inc_vol(i)
      End
     End
     Else if i.valor_ins Then Begin
       i.ant_tmp := i.tmp;
       i.tmp := Despues;
       i.eq_postph := i.ult_ph;
       i.valor_ins := False;
     End


  End; (*Continuar_itr*)


  Procedure Cambiar( Var a,b : Real );
    Var x : Real;
  Begin
    x := a; a:= b; b:= x
  End;

  Function Mejor_ind( Var v : Tipo_valoracion;
                      ind,opt : Integer;
                      Var i : Tipo_itr ):Boolean;
  (* ind es mejor
   * que opt.
   *)
     Function Max( a,b : real) : Real;
     Begin
       If a > b Then Max := a Else Max := b
     End;

     Function Sale_de_eq( Var v: Tipo_valoracion; i: Integer;
                          Var it : Tipo_itr ): Real;
        Var t : Real;
     Begin
        Sale_de_eq := Max( PhSup_ind(v, i ) - it.eq_postpH,
                           it.eq_preph - PhInf_ind(v, i )   );

     End;

     Function Eq_en_medio( Var v: Tipo_valoracion;
                           ind  : Integer;
                           Var i: Tipo_itr ) : Boolean;
     Begin
        Eq_en_medio := (PhSup_ind(v,ind) > i.eq_ph) And
                       (PhInf_ind(v,ind) < i.eq_ph)
     End;

  Begin
      If Eq_en_medio(v,opt,i) Then
        Mejor_ind := (Sale_de_eq( v,ind,i) < Sale_de_eq( v,opt, i )) And
                     Eq_en_medio(v,ind,i)
      Else
        Mejor_ind := Eq_en_medio(v,ind,i) Or
                     (Sale_de_eq( v,ind,i) < Sale_de_eq( v,opt, i ))
  End;


  Function Indicador_optimo_i( Var i: Tipo_itr;
                               Var v: Tipo_valoracion ): Integer;
  (*
   * Devuelve el indice el indicador optimo en inds para i.
   *  -> Encuentra indicadores entre i.eq_preph y i.eq_postph
   *  -> Se queda con el mas centrado en i.eq_ph
   *)

      Var ind, opt_ind : Integer;
  Begin
      ind := 1; opt_ind := 1;
      If i.eq_prepH > i.eq_postpH Then
         Cambiar( i.eq_prepH, i.eq_postpH );
      While (Phinf_ind(v,ind) <> 0) Or (Phsup_ind(v,ind)<>0) Do Begin
          If Mejor_ind( v,ind,opt_ind,i)  Then
             opt_ind := ind;
          Inc(ind)
      End;

      Indicador_optimo_i := opt_ind

  End;(*Indicador_optimo_i*)

  Function Estimar_indicador_opt( Var i : Tipo_itr;
                                  Var v : Tipo_valoracion ) : Integer;
     Var ind, opt_ind : Integer;
  Begin
      ind := 1; opt_ind := 1;
      i.eq_ph := Estimar_ph_eq( v );
      i.eq_preph := i.eq_ph - 2;
      i.eq_postph:= i.eq_ph + 2;
      While (Phinf_ind(v,ind) <> 0) Or (Phsup_ind(v,ind)<>0) Do Begin
          If Mejor_ind( v,ind,opt_ind,i)  Then
             opt_ind := ind;
          Inc(ind)
      End;
      Estimar_indicador_opt := opt_ind
  End;

  Function Tiempo_i( Var i : Tipo_itr ): Tipo_momento;
  (*
   * Informa del tiempo de la iteracion.
   *)
  Begin
     Tiempo_i := i.tmp
  End;


  Function Max_dph_ph_i( Var i : Tipo_itr ) : Real;
  Begin
     Max_dph_ph_i := i.max_der_ph
  End;

  Function Max_dph_vol_i(Var i : Tipo_itr ) : Real;
  (*
   * Devuelven (vol,ph) para el max/min de la dph.
   *)
  Begin
     Max_dph_vol_i := i.max_der_vol
  End;

  Function Eq_ph_i( Var i : Tipo_itr ) : Real;
  Begin
    Eq_ph_i := i.eq_ph
  End;

  Function Eq_vol_i(Var i : Tipo_itr ) : Real;
  (*
   * Devuelven (vol,ph) para el pto. de eq. de la curva de ph.
   *)
  Begin
    Eq_vol_i := i.eq_vol
  End;


  Function Pcent_i( Var i : Tipo_itr ) : Real;
  Begin
    If Not i.valor_ins Then
      Pcent_i := i.ult_pcent
    Else
      Pcent_i := 100
  End;

  Function Ph_i( Var i : Tipo_itr ) : Real;
  Begin
    If Not i.valor_ins Then
      Ph_i := i.ult_ph
    Else
      Ph_i := i.eq_ph
  End;

  Function Vol_i( Var i : Tipo_itr ): Real;
  Begin
    If Not i.valor_ins Then
      Vol_i := i.ult_vol
    Else
      Vol_i := i.eq_vol
  End;

  Function Dph_i( Var i : Tipo_itr ): Real;
  (*
   * Devuelven los valores en el ultimo paso para el % de valoracion,
   * pH, vol y d pH.
   *)
  Begin
    If Not i.valor_ins Then
      Dph_i := i.ult_der
    Else
      Dph_i := i.max_der_ph
  End;


  Function Hay_paso_im( Var i : Tipo_itr ): Boolean;
  (*
   * Devuelve True cuando la ultima iteracion tiene valores manuales.
   *)
      Function Cambio_signf_nrm( r1,r2 : Real ): Boolean;
      (*
       * Devuelve True si r1 y r2 difieren significativamente para
       * precision normal ( a nivel de decenas )
       *)
      Begin
         Cambio_signf_nrm := ((r1 < 1) And (r2 >1)) (* 1 debe aparecer*)
                             Or ( Decena(r1) <> Decena(r2))
      End;

      Function Cambio_signf_eq( r1,r2 : Real ) : Boolean;
      (*
       * Devuelve True cuando hay cambio sig. entre r1 y r2 a nivel
       * de unidades ( precision para intervalo de eq.
       *)
      Begin
         Cambio_signf_eq := Unidad( r1 ) <> Unidad( r2 )
      End;

  Begin (*Hay_paso_im*)

      Hay_paso_im := Not i.ign_valorm And
                     (( i.tmp = Antes )          (* Ptos. singulares...*)
                     Or ( i.valor_ins )
                     Or ( i.tmp = Final )
                     Or ( (i.ult_pcent >= Lim_Inf_TPrec)
                          And ( i.ult_pcent <= Lim_Sup_TPrec)
                          And Cambio_signf_eq( i.ant_pcent,i.ult_pcent) )
                     Or ( ((i.ult_pcent  < Lim_Inf_TPrec) Or
                           (i.ult_pcent  > Lim_Sup_TPrec))
                          And Cambio_signf_nrm( i.ant_pcent, i.ult_pcent) ))
  End;(*Hay_paso_im*)

  Function Valor_manual( r1, r2 : Real ): Real;
  (*
   * Devuelve el valor manual entre r1 y r2.
   *)
  Begin
     If r1 = r2 Then    (* Principio o final *)
        Valor_manual := r1
     Else If (r1 < 1) And ( r2 > 1 ) Then
        Valor_manual := 1
     Else If (r2 < Lim_Inf_TPrec) Or ( r1 > Lim_Sup_TPrec ) Then
        Valor_manual := Trunc_por_d( r2 )
     Else
        Valor_manual := Trunc_por_u( r2 )
  End;

  Function Pcent_im( Var i : Tipo_itr ) : Real;
  Begin
      If i.valor_ins Then
         Pcent_im := 100
      Else
         Pcent_im := Valor_manual(i.ant_pcent,i.ult_pcent)

  End;

  Function Ph_im( Var i : Tipo_itr ) : Real;
  Begin
      If i.valor_ins Then
         Ph_im := i.eq_ph
      Else
         Ph_im := Interpolar( Valor_manual(i.ant_pcent,i.ult_pcent),
                              i.ant_pcent, i.ult_pcent,
                              i.ant_ph,    i.ult_ph    )

  End;

  Function Vol_im( Var i : Tipo_itr ): Real;
  Begin
      If i.valor_ins Then
         Vol_im := i.eq_vol
      Else
         Vol_im:= Interpolar( Valor_manual(i.ant_pcent,i.ult_pcent),
                              i.ant_pcent, i.ult_pcent,
                              i.ant_vol,   i.ult_vol    )

  End;

  Function Dph_im( Var i : Tipo_itr ): Real;
  (*
   * Devuelven los valores en el ultimo paso para el % de valoracion,
   * pH, vol y d pH correspondientes a una iteracion manual:
   *   [ %vol.:0,1,10,20,...90,91,...99,100,101,...,109,110,120...190,200]
   *)
  Begin
      If i.valor_ins Then
         Dph_im := i.max_der_ph
      Else
         Dph_im := Interpolar( Valor_manual(i.ant_pcent,i.ult_pcent),
                               i.ant_pcent,i.ult_pcent,
                               i.ant_der,  i.ult_der    )
  End;



Begin

End. (*ItrVal*)

