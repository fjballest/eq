(*
 * Fuente TPascal + ToolBox -- (c) 1991,1992  Esther Li#an y Nemo
 *
 * Modulo : [Val] Valoracion: Objetos y calculos involucrados en una val.
 * Rev    : 1.0
 * O.S.   : DOS
 *
 *
 *)


Unit Val;

Interface (*Val*)


  Uses React, (* Reactivos *)
       PHFun, (* Funciones para calcular pHs *)
       GrIO;  (* Colores *)

  Const Num_Acds = 10; (* Numero de ac. bas. e ind. conocidos *)
        Num_Bass = 10;
        Num_Inds = 30;

        Done     : Boolean = True; (* Operacion ok. *)

  Type  RealPt = ^Real;

        Tipo_momento = ( antes, durante, equilibrio, despues, final );


        Tipo_acds = Array [1..Num_Acds] Of Tipo_acido;
        Tipo_bass = Array [1..Num_Bass] Of Tipo_base;
        Tipo_inds = Array [1..Num_Inds] Of Tipo_indicador;

        Tipo_clase_valoracion = ( Acido_con_base, Base_con_acido );


        Tipo_denominacion_val = ( Af_bf, Af_bd, Ad_bf, Ad_bd );

        Tipo_valoracion = Record
                            acds  : Tipo_acds;    (* Acidos, bases, inds *)
                            bass  : Tipo_bass;
                            inds  : Tipo_inds;
                            acido : Integer;      (* Utilizados en esta..*)
                            base  : Integer;
                            ind   : Integer;
                            vol   : Real;       (* Volumen a valorar *)
                            clase_val   : Tipo_clase_valoracion;
                          End;

    Function Nombre_momento( t : Tipo_momento ) : Tipo_Nombre;
    (*
     * Devuelve una cadena con el nombre de t.
     *)

    Procedure Crear_val( Var v : Tipo_valoracion );
    (*
     * Crea una valoracion vacia.
     *)

    Function Vol_neut( Var v : Tipo_valoracion;
                       vol   : Real            ): Real;
    (*
     * Calcula el % neutralizado con vol de bureta ya echado.
     *)


    Procedure Leer_mundo_de_val( Var f: Text; Var v: Tipo_valoracion );
    (*
     * Lee v de f.
     *)


    Function Calcular_ph(  Var v  : Tipo_valoracion;
                           t      : Tipo_momento;
                           vol    : Real;
                           vol_exc: Real             ): Real;
    (*
     * Calcula el ph en t con vol echado y vol_exc el exceso.
     * vv es el volumen que se valora. vb es el de la bureta ya echado.
     * ve es el volumen en exceso.
     *)

    Function Dimension_bureta( Var v : Tipo_valoracion ) : Real;
    (*
     * Devuelve el maximo volumen de bureta utilizado.
     * Se calcula como el doble de lo necesario.
     *)

    Function Valoracion_posible( Var v : Tipo_valoracion ) : Boolean;
    (*
     * Devuelve True si la valoracion v es simulable.
     *)

    Procedure Fijar_indicador( Var v : Tipo_valoracion;
                               ind   : Integer         );
    (*
     * Fija en v ind como el indicador utilizado
     *)
    Function Indicador_de_Valoracion ( Var v: Tipo_valoracion ):Integer;


    Function Estimar_ph_eq( Var v : Tipo_valoracion ): Real;


    (*
     * Las siguientes funciones dan datos sobre ac.bas. e ind. utilizados.
     *)

    Function Norm_ac( Var v : Tipo_valoracion )  : Real;
    Function Cte_ac( Var v : Tipo_valoracion )   : Real;
    Function Nombre_ac( Var v : Tipo_valoracion ): Tipo_nombre;
    Function Fuerza_ac( Var v : Tipo_valoracion ): Tipo_fuerza;

    Function Norm_ba( Var v : Tipo_valoracion )  : Real;
    Function Cte_ba( Var v : Tipo_valoracion )   : Real;
    Function Nombre_ba( Var v : Tipo_valoracion ): Tipo_nombre;
    Function Fuerza_ba( Var v : Tipo_valoracion ): Tipo_fuerza;

    Function Nombre_ind( Var v : Tipo_valoracion; i:Integer ) : Tipo_nombre;
    Function Phinf_ind( Var v : Tipo_valoracion; i:Integer ) : Real;
    Function Phsup_ind( Var v : Tipo_valoracion; i:Integer ) : Real;
    Function Colorinf_ind( Var v : Tipo_valoracion; i:Integer ) : Tipo_Color;
    Function Colorsup_ind( Var v : Tipo_valoracion; i:Integer ) : Tipo_Color;


    (*
     * Las siguientes funciones devuelven punteros a valores modificables.
     *)

    Function Norm_ac_pt( Var v: Tipo_valoracion ) : RealPt;
    Function Norm_ba_pt( Var v: Tipo_valoracion ) : RealPt;
    Function Vol_pt( Var v: Tipo_valoracion )     : RealPt;

Implementation (*Val*)


    Function Nombre_momento( t : Tipo_momento ) : Tipo_Nombre;
    (*
     * Devuelve una cadena que adjetive a 'valoracion' con el nombre de t.
     *)
    Begin
       Case t Of
         Antes:     Nombre_momento := 'antes de empezar';
         Durante:   Nombre_momento := 'antes del equilibrio';
         Equilibrio:Nombre_momento := 'en el equilibrio';
         Despues:   Nombre_momento := 'tras el equilibrio';
         Final:     Nombre_momento := 'despues de terminar'
       End
    End;(*Nombre_momento*)

    Function Vol_neut( Var v : Tipo_valoracion;
                       vol   : Real            ): Real;
    (*
     * Calcula el % neutralizado con vol de bureta ya echado.
     *)
    Begin
        If v.clase_val = Acido_con_base Then
          Vol_neut :=  vol* v.bass[v.base].n * 100 /
                       (v.vol*v.acds[v.acido].n)
        Else
          Vol_neut :=  vol* v.acds[v.acido].n * 100 /
                       (v.vol*v.bass[v.base].n)
    End;



    Function Dimension_bureta( Var v : Tipo_valoracion ) : Real;
    (*
     * Devuelve el maximo volumen de bureta utilizado.
     * Se calcula como el doble de lo necesario.
     *)
    Begin
      If v.clase_val = Acido_con_Base Then
        Dimension_bureta := 2 * v.vol * v.acds[v.acido].n
                            / v.bass[v.base].n
      Else
        Dimension_bureta := 2 * v.vol * v.bass[v.base].n
                            / v.acds[v.acido].n

    End;


    Function Estimar_ph_eq( Var v : Tipo_valoracion ): Real;
    Begin
       Estimar_ph_eq :=
           Calcular_ph( v, Equilibrio, Dimension_bureta(v) / 2,  0)
    End;

    Function Calcular_ph(  Var v  : Tipo_valoracion;
                           t      : Tipo_momento;
                           vol    : Real;
                           vol_exc: Real             ): Real;
    (*
     * Calcula el ph en t con vol echado y vol_exc el exceso.
     * vv es el volumen que se valora. vb es el de la bureta ya echado.
     * ve es el volumen en exceso.
     *)
    Begin
       If v.clase_val = Acido_con_base Then Begin
         If (Fuerza_ac(v) = Fuerte) And (Fuerza_ba(v) = Fuerte ) Then
            Case t Of
              Antes:
                Calcular_ph := Ph_afbf_antes( Norm_ac(v) );
              Durante:
                Calcular_ph :=
                   Ph_afbf_durante(v.vol,Norm_ac(v),vol,Norm_ba(v));
              Equilibrio:
                Calcular_ph := Ph_afbf_eq;
              Despues:
                Calcular_ph := Ph_afbf_despues(v.vol,vol,Norm_ba(v),vol_exc)
            End
         Else If (Fuerza_ac(v) = Debil) And ( Fuerza_ba(v) = Fuerte ) Then
            Case t Of
              Antes:
                Calcular_ph := Ph_adbf_antes( Norm_ac(v),Cte_ac(v) );
              Durante:
                Calcular_ph :=
                   Ph_adbf_durante(v.vol,Norm_ac(v),Cte_ac(v),vol,Norm_ba(v));
              Equilibrio:
                Calcular_ph := Ph_adbf_eq(v.vol,Norm_ac(v),Cte_ac(v),vol);
              Despues:
                Calcular_ph := Ph_adbf_despues(v.vol,vol,Norm_ba(v),vol_exc)
            End
         End
       Else Begin
         If ( Fuerza_ac(v) = Fuerte) And ( Fuerza_ba(v) = Debil  ) Then
            Case t Of
              Antes:
                Calcular_ph := Ph_bdaf_antes( Norm_ba(v),Cte_ba(v) );
              Durante:
                Calcular_ph :=
                  Ph_bdaf_durante(vol,Norm_ac(v),v.vol,Norm_ba(v),Cte_ba(v));
              Equilibrio:
                Calcular_ph := Ph_bdaf_eq(vol,v.vol,Norm_ba(v),Cte_ba(v));
              Despues:
                Calcular_ph :=
                  Ph_bdaf_despues(vol,Norm_ac(v),vol_exc,Norm_ba(v))
            End
         Else if ( Fuerza_ac(v) = Fuerte) And ( Fuerza_ba(v) = Fuerte) Then
            Case t Of
              Antes:
                Calcular_ph := Ph_bfaf_antes( Norm_ba(v) );
              Durante:
                Calcular_ph :=
                  Ph_bfaf_durante(vol,Norm_ac(v),v.vol,Norm_ba(v));
              Equilibrio:
                Calcular_ph := Ph_bfaf_eq;
              Despues:
                Calcular_ph :=
                  Ph_bfaf_despues(vol,Norm_ac(v),vol_exc,Norm_ba(v))
            End
         End
    End;



    Procedure Leer_mundo_de_val( Var f : Text;
                                 Var v : Tipo_valoracion );
    (*
     * Define los ejemplos de acidos, bases e  predefinidos.
     *)
       Var num_ac, num_ba, num_ind : Integer;    (* Acidos y Bases Leidos *)
           aux   : Char;
           aux_n : Tipo_Nombre;
    Begin
       (* Empezar a leer *)
       {$I-}Reset( f );{$I+} (* Que no aborte la eje.*)
       num_ac := 0; num_ba := 0; num_ind := 0;
       While Not SeekEof( f ) And (num_ac < Num_Acds )
             And ( num_ba < Num_Bass ) And ( num_ind < Num_Inds ) Do Begin
          While SeekEoLn( f ) Do
            ReadLn(f);
          Read( f, aux );
          Case aux Of
          'a','A': Begin (* Tengo un acido *)
             Inc( num_ac );
             Leer_ac( f, v.acds[num_ac] )
             End;

          'b','B': Begin (* Tengo una base *)
             Inc( num_ba );
             Leer_ba( f, v.bass[num_ba] )
             End;
          'i','I': Begin (* Indicador *)
             Inc( num_ind );
             Leer_ind( f, v.inds[num_ind] )
             End;
          '#': ReadLn( f )    (* Comentario     *)
          End
       End;
       If num_ac < Num_Acds Then
          v.acds[num_ac+1] := Ac_Nulo;
       If num_ba < Num_Bass Then
          v.bass[num_ba+1] := Ba_Nula;
       If num_ind < Num_Inds Then
          v.inds[num_ind+1] := Ind_Nulo;
       {$I-}
        Close( f );
       {$I+}
       Val.Done := IOResult = 0;
    End; (*Leer_mundo_de_val*)




    Function Valoracion_posible( Var v : Tipo_valoracion ) : Boolean;
    (*
     * Devuelve True si la valoracion v es simulable.
     *)
    Begin
       Valoracion_posible :=
           ( (v.clase_val = Acido_Con_Base) And (Fuerza_ba(v)=Fuerte) )
           Or
           ( (v.clase_val = Base_Con_Acido) And (Fuerza_ac(v)=Fuerte) )
    End;

    Function Norm_ac( Var v : Tipo_valoracion )  : Real;
    Begin
       Norm_ac := v.acds[v.acido].n
    End;

    Function Cte_ac( Var v : Tipo_valoracion )   : Real;
    Begin
       Cte_ac := v.acds[v.acido].cte
    End;

    Function Nombre_ac( Var v : Tipo_valoracion ): Tipo_nombre;
    Begin
       Nombre_ac := v.acds[v.acido].nombre
    End;

    Function Fuerza_ac( Var v : Tipo_valoracion ): Tipo_fuerza;
    Begin
       Fuerza_ac := v.acds[v.acido].fuerza
    End;

    Function Norm_ba( Var v : Tipo_valoracion )  : Real;
    Begin
       Norm_ba := v.bass[v.base].n
    End;

    Function Cte_ba( Var v : Tipo_valoracion )   : Real;
    Begin
       Cte_ba := v.bass[v.base].cte
    End;

    Function Nombre_ba( Var v : Tipo_valoracion ): Tipo_nombre;
    Begin
       Nombre_ba := v.bass[v.base].nombre
    End;

    Function Fuerza_ba( Var v : Tipo_valoracion ): Tipo_fuerza;
    Begin
       Fuerza_ba := v.bass[v.base].fuerza
    End;

    Function Norm_ac_pt( Var v: Tipo_valoracion ) : RealPt;
    Begin
       Norm_ac_pt := @v.acds[v.acido].n
    End;

    Function Norm_ba_pt( Var v: Tipo_valoracion ) : RealPt;
    Begin
       Norm_ba_pt := @v.bass[v.base].n
    End;

    Function Vol_pt( Var v: Tipo_valoracion )     : RealPt;
    Begin
       Vol_pt := @v.vol
    End;


    Procedure Crear_val( Var v : Tipo_valoracion );
    (*
     * Crea una valoracion vacia.
     *)
    Begin
       With v Do Begin
          acido := 1; base := 1; ind := 1;
          vol := 0;
          clase_val := Acido_Con_Base
       End
    End;(*Crear_val*)

    Procedure Fijar_indicador( Var v : Tipo_valoracion;
                               ind   : Integer         );
    (*
     * Fija en v ind como el indicador utilizado
     *)
    Begin
       v.ind := ind
    End;(*Fijar_indicador*)

    Function Indicador_de_val( Var v : Tipo_valoracion ) : Integer;
    Begin
       Indicador_de_val := v.ind
    End;(*Indicador_de_val*)

    Function Nombre_ind( Var v : Tipo_valoracion; i:Integer) : Tipo_nombre;
    Begin
        Nombre_ind := v.inds[i].nombre
    End;(*Nombre_ind*)

    Function Phinf_ind( Var v : Tipo_valoracion; i:Integer ) : Real;
    Begin
        Phinf_ind := v.inds[i].min_vph
    End;(*Phinf_ind*)

    Function Phsup_ind( Var v : Tipo_valoracion; i:Integer ) : Real;
    Begin
        Phsup_ind := v.inds[i].max_vph
    End;(*Phsup_ind*)

    Function Colorinf_ind( Var v : Tipo_valoracion; i:Integer ) : Tipo_Color;
    Begin
        Colorinf_ind := Nombre2Color(v.inds[i].color1)
    End;(*Colorinf_ind*)

    Function Colorsup_ind( Var v : Tipo_valoracion; i:Integer ) : Tipo_Color;
    Begin
        Colorsup_ind := Nombre2Color(v.inds[i].color2)
    End;(*Colorsup_ind*)

    Function Indicador_de_valoracion( Var v : Tipo_valoracion ) : Integer;
    Begin
      Indicador_de_valoracion := v.ind
    End;

Begin

End. (*Val*)

