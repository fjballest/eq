Unit Simular;


Interface


    Uses Graph, (* Unidades Std ... *)
         Printer,

         TPCrt, (* Tool-Box *)
         TPString,
         TPEdit,
         TPWindow,

         Fun_Mat,  (* Para funciones geometricas (tgs.) *)
         Opcs,     (* Opciones *)
         React,    (* Reactivos*)
         Val,      (* Valoraciones *)
         TblVal,   (* Tabla de ''  *)
         GrIO,
         GrfVal,   (* Grafica de ''*)
         DbjSim,   (* Dibujo de montaje de la simulacion *)
         ItrVal,   (* Metodos para simular la valoracion *)
         Nav;      (* Navegador grf*)




    Procedure Valorar( Var v  : Tipo_valoracion;
                       Var opc_sal : Tipo_opc_salida   );

    (*
     * Realiza la valoracion
     *)


Implementation


    Var cur_nav1, cur_nav2 : Pointer; (* Cursores para navegacion *)


    Procedure Valorar( Var v  : Tipo_valoracion;
                       Var opc_sal : Tipo_opc_salida   );

    (*
     * Realiza la valoracion
     *)

    Var dr_g, md_g : Integer;
        min_y        : Integer; (* Alto de la pantalla (excluido titulo)*)
        info_y       : Integer; (* Comienzo linea de Informacion *)
        stat_y       : Integer; (* Comienzo linea de Status *)
        tit1,tit2    : String;
        nav_done     : Boolean; (* True si los cursores se inicial.*)
        c1,c2        : Pointer; (* Cursores de navegacion *)
        t : Tipo_tbl;       (* Informacion para iteracion *)
        g : Tipo_grf;
        i : Tipo_itr;
        n1,n2        : Tipo_nav;
        fin_tbl_x    : Integer;
        intr         : Boolean;
        void         : Char;
        ind_est      : Integer;

       Procedure Iniciar_cursores( Var c1,c2 : Pointer;
                                   hacer1,hacer2 : Boolean;
                                   Var  done: Boolean);
       (*
        * Asigna pixmaps a c1 y c2. devuelve done=True si lo consigue.
        *)
         Var clr : Word;
       Begin
         If hacer1 And ( cur_nav1 = NIL ) Then Begin
              GetMem( cur_nav1,
                    ImageSize( 0, 0, 15, 15 ) );
              clr := GetColor;
              SetColor( color_crs_c );
              Line( 0, 0, 15, 15 );
              Line( 0, 15, 15, 0 );
              Circle( 7,7,7);
              SetColor( clr );
              If cur_nav1 <> Nil Then
                 GetImage( 0,0,15,15, cur_nav1^ )
         End;
         If hacer2 And ( cur_nav2 = NIL ) Then Begin
              GetMem( cur_nav2,
                    ImageSize( 20, 20, 35, 35 ));
              clr := GetColor;
              SetColor( color_crs_d );
              Line( 27,20, 27, 35);
              Line( 20,27, 35, 27);
              Circle( 27,27, 7 );
              SetColor( clr );
              If cur_nav2 <> Nil Then
                 GetImage( 20,20,35,35, cur_nav2^ )
              Else
                 cur_nav2 := cur_nav1  (* En cualquier caso si 'hacer1' se *)
                                       (*  reutilizara el cursor 1         *)
         End;

         c1 := cur_nav1; c2 := cur_nav2;
         done := Not ( hacer1 And (c1 = NIL)) And
                 Not ( hacer2 And (c2 = NIL))

       End;(* Iniciar_cursores *)


       Procedure A_graficos( Var dr_g, md_g : Integer );
       (*
        * Activa el modo grafico con dr_g y md_g.
        * CAMBIA los valores de COLORES en el modulo OPCS si la tarjeta
        * es monocromo.
        *)
        Var ErrCode : Integer;
       Begin
          dr_g := Detect; md_g := Detect;
          InitGraph( dr_g, md_g, '' ); ErrCode := GraphResult;
          If ErrCode <> grOk Then Begin
             ClrScr;
             WriteLn('Eq : Error grafico : ',GraphErrorMsg(ErrCode));
             Halt(2);
          End;
          If (dr_g  = Integer(MCGA)) Or
             ( dr_g = Integer(EGAMONO)) Or
             (dr_g  = Integer(HercMono)) Then Begin
              Color_curva   := White;
              Color_der     := White;
              Color_eq      := White;
              Color_der_eq  := White;
              Color_titulo  := White;
              Color_info    := White

          End;
       End;

       Procedure Componer_titulo( Var v : Tipo_valoracion;
                                  Var s1,s2 : String       );
       (*
        * forma los titulos s1 ( Triplex arriba ) y s2 ( normal bajo s1 ).
        * a partir de v
        *)
       Begin
             s1 := 'Valoracion de ';
             If v.clase_val = Acido_con_Base Then
               s1 := s1 + Nombre_ac(v) + ' con ' + Nombre_ba(v)
             Else
               s1 := s1 + Nombre_ba(v) + ' con ' + Nombre_ac(v);

             s2 := 'N acido:'+ Form( '###.##',Norm_ac(v)) + '   ' +
                        'N base:' + Form( '###.##',Norm_ba(v)) + '   ' +
                        'Volumen a valorar: '+ Form('###.##',v.vol) + 'mL';
       End;

       Procedure Encabezar_fich( Var f:Text;
                                 n    : Tipo_path;
                                 Var s1,s2: String;
                                 Var done: Boolean );
       (*
        * Imprime la cabezera correspondiente a s1 y s2 en f.
        * Devuelve done a True si no hubo error.
        * Si n es distinto de Path_Nulo intenta asignarlo a f y lo
        * abre para escritura.
        *)
       Begin
          If n <> Path_Nulo Then Begin
            {$I-}
            Assign(f,n);
            ReWrite(f);
            {$I+}
            done := IOresult = 0
          End;
          {$I-}
            WriteLn(f,'# eq: Resultados de simulaci¢n. #');
            WriteLn(f,'#     '+s1+' #');
            WriteLn(f,'#     '+s2+' #');
          {$I+}
          done := done And (IOResult = 0)
       End;


       Procedure Pantalla_grafica ( s1     : String;
                                    s2     : String;
                                    Var min_y : Integer;
                                    Var txy_1,txy_2 : Integer );
       (*
        * Dibuja la pantalla con modo grafico.
        * Imprime en una linea ( arriba ) s1 ( Triplex ) y debajo s2.
        * Devuelve el minimo valor aceptable para coordenada y en min_y
        * ( El resto se utiliza con el titulo ).
        * Devuelve asi mismo lugares apropiados para imprimir dos lineas
        * de msgs. ( Esos lugares no  estan excluidos por min_y ). Para
        * ello devuelve las coords. y en txy_1/2
        *)

          Var ErrCode: Integer;
              cadena1,cadena2 : String;
              col    : Word;

       Begin
          ClearViewPort;
          col := GetColor;
          SetColor(Color_titulo);
          SetTextStyle( TriplexFont, HorizDir, 1 );
          MoveTo( (GetMaxX - Integer(TextWidth(s1))) Div 2, 5 );
          OutText( s1 );
          min_y  := TextHeight(s1)+7;
          SetTextStyle( DefaultFont, HorizDir, 1 );
          MoveTo( (GetMaxX - Integer(TextWidth(s2))) Div 2, min_y+3 );
          OutText(s2);
          min_y := min_y+3+TextHeight(s2) + TextHeight('X');
          SetColor(col);
          txy_1 := GetMaxY - TextHeight('X');
          txy_2 := txy_1 - 2 - TextHeight('X')

       End;(*Pantalla_grafica*)


       Procedure Mostrar_indicadores( Var it: Tipo_itr;
                                      Var v: Tipo_valoracion;
                                      Var g: Tipo_grf;
                                      info_y, stat_y: Integer );
       (*
        * Muestra el iterador mas optimo y lo fija como optimo en v.
        *)
        Var i : Integer;
       Begin
           i := Indicador_optimo_i(it,v);
           Fijar_indicador( v,i);
           Marcar_pH(g,PhInf_ind(v,i),ColorInf_ind(v,i));
           Marcar_pH(g,PhSup_ind(v,i),ColorSup_ind(v,i));

       End; (*Mostrar_indicadores*)


       Procedure Simular_Val( Var n1,n2 : Tipo_nav;
                              Var v : Tipo_valoracion;
                              Var i : Tipo_itr;
                              Var t : Tipo_tbl;
                              Var g : Tipo_grf;
                              Var op: Tipo_opc_salida;
                              info_y, stat_y : Integer;
                              Var intr : Boolean );

          Const Retardo_Inicial = 500;

          Procedure Fijar_velocidad( Var dly : LongInt; Var intr: Boolean );
          (*
           * Varia la velocidad.
           *)

           Const Ch_Ret = #13;
                 Ch_Esc = #27;
                 Ch_Spc = #32;

                 MAX_DLY= 10000; (* Retardo maximo permitido *)

          Begin
                If TpCrt.KeyPressed Then
                   Case tpCrt.ReadKey Of
                    '+': Begin
                         dly := dly-250;
                         If dly < 0 Then
                            dly := 0
                         End;
                    '-': Begin
                         dly := dly+250;
                         If dly > MAX_DLY Then
                            dly := MAX_DLY
                         End;
                    Ch_Ret,Ch_Spc : dly := 0;
                    Ch_Esc        : intr:=True
                   End
          End;(*Fijar_velocidad*)

          Procedure Dar_msg( info_y, stat_y: Integer; t : Tipo_momento );
          Begin
              Dar_info( info_y, 'Pulse: [+] Acelerar, [-] Decelerar, ' +
                                '[INTRO] Lo mas rapido, [ESC] Abortar.'  );
              Dar_info( stat_y, 'Simulando valoracion '+
                                Nombre_momento(t) + '.' );
          End;(*Dar_msg*)

          Var dly  : LongInt;
              cuando: Tipo_momento;

       Begin(*Simular_Val*)
          intr := False;
          dly := Retardo_Inicial;
          cuando:= Final;
          Repeat
             If cuando <> Tiempo_i(i) Then Begin
               Dar_msg( info_y, stat_y, Tiempo_i(i) );
               cuando := Tiempo_i(i)
             End;
             Fijar_velocidad( dly, intr );
             Delay( dly );
             If op.dibujo Then Begin
               Vaciar_bureta( Dimension_bureta(v) / 2 + i.ult_vol );
               Goteo( Yellow, dly );
               Llenar_Matraz( v.vol + i.ult_vol, Tiempo_i(i) < Equilibrio )
             End;
             Indicar_en_grafica( op.curva, Ph_i(i),
                                 Tiempo_i(i)=Antes, Tiempo_i(i) <> Equilibrio,
                                 g);
             Indicar_en_derivada( op.derivada, Dph_i(i),
                                 Tiempo_i(i)=Antes, Tiempo_i(i) <> Equilibrio,
                                 g);
             If op.curva Then
               Indicar_eq( Tiempo_i(i) = Equilibrio, g,Eq_vol_i(i),Eq_ph_i(i) );
             If Hay_paso_im( i ) Then Begin
                Indicar_en_tabla( op, Pcent_im(i),
                                  Ph_im(i), Dph_im(i), Vol_im(i),t, True );
                If (Tiempo_i(i) <> Antes) And (Tiempo_i(i)<>Final) Then Begin
                  Incluir_pto_en_nav( n1, g.pix_actual );
                  Incluir_pto_en_nav( n2, g.der_pix_actual )
                  End
                End
             Else
                Indicar_en_tabla( op, Pcent_i(i),
                                      Ph_i(i), Dph_i(i), Vol_i(i), t, False );

             Delay( dly );
             Continuar_itr( i, v )
          Until intr Or (Tiempo_i( i ) = Final);
          If op.derivada Then
            Indicar_deq( True, g,
                         Max_dph_vol_i(i),Max_dph_ph_i(i) );


       End;(*Simular_Val*)

       Procedure Preparar_nav( c1,c2: Pointer;
                               Var n1,n2 : Tipo_nav );
       Begin
          Iniciar_nav(n1,c1);
          Iniciar_nav(n2,c2)
       End;


       Procedure Bip;
       Begin
          If Opcs.Sonido Then Begin
            Sound(200);
            Delay(1000);
            NoSound
          End
       End;

       Function Stroke : Char;
          Const Esc_Ch = #27;
                Dr_Ch  = #77;
                Iz_Ch  = #75;
                Ar_Ch  = #72;
                Ab_Ch  = #80;
                Ext_ch = #00;
                Spc_ch = #32;

          Var ch : Char;
       Begin
          ch := TpCrt.ReadKey;
          If ch = Ext_ch Then
             ch := TpCrt.ReadKey;
          Stroke := ch
       End;


       Procedure Navegaciones( Var n1,n2 : Tipo_nav;
                               Var o : Tipo_opc_salida;
                               Var v : Tipo_valoracion;
                               Var t : Tipo_tbl;
                               info_y: Integer;
                               stat_y: Integer );

          Const Esc_Ch = #27;
                Dr_Ch  = #77;
                Iz_Ch  = #75;
                Ar_Ch  = #72;
                Ab_Ch  = #80;
                Ext_ch = #00;
                Spc_ch = #32;

          Var pto : Integer;
              r1,r2,r3,r4 : Real;
              ch  : Char;
              void: Word;
              cmd : Tipo_cmd_nav;
              nomb: Tipo_Nombre;

       Begin(*Navegaciones*)
           pto := 1;
           Dar_info( info_y,'Teclas del Cursor -> moverse'+
                            ' [ESC/Spc]->terminar');
           nomb := Trim(Nombre_ind(v,Indicador_de_valoracion(v)));
           cmd := NIniciar;
           If o.curva Then
             Navegar( n1, NIniciar);
           If o.derivada Then
           Navegar( n2, NIniciar);
           cmd := Nnada;
           Repeat
              Consultar_vals( t, pto+1, r1,r2,r3,r4 );
              If o.curva Then
                Navegar( n1, cmd);
              If o.derivada Then
                Navegar( n2, cmd);
              If o.indicadores Then
                Dar_info( stat_y, 'Con '+nomb+', Al '+Form('###.#',r1)+'% : pH='+
                                Form('##.##',r2)+'; d PH='+
                                Form('####.###',r3) +'; Vol='+
                                Form('####.##',r4 ))
              Else
                Dar_info( stat_y, 'Al '+Form('###.#',r1)+'% : pH='+
                                Form('##.##',r2)+'; d PH='+
                                Form('####.###',r3) +'; Vol='+
                                Form('####.##',r4 ));

              ch := Stroke;
              Case ch Of
                Dr_ch,Ab_ch: Begin
                              Inc(pto);
                              cmd := NAvanzar;
                              If pto = t.num_vals Then pto := 1
                             End;
                Iz_ch,Ar_ch: Begin
                              Dec(pto);
                              cmd := NRetroceder;
                              If pto < 1 Then
                                 pto := t.num_vals -1 (* t tiene 1 pto mas *)
                             End;                     (* que n1,n2         *)
                Esc_ch,Spc_ch : cmd := NTerminar
              End;
           Until cmd = NTerminar;
           If o.curva Then
             Navegar(n1,cmd);
           If o.derivada Then
             Navegar(n2,cmd)

       End;(*Navegaciones*)


       Procedure Simular_tgs( g : Tipo_grf;
                              t : tipo_tbl;
                              info_y, stat_y: Integer );
       (*
        * Simula paso a paso el metodo de las tangentes, deja pasar
        * Tgs_Dly/1000 segundos entre paso y paso. ( 1 sg).
        *)
        Const Tgs_Dly = 1000;
        Type Tipo_pto = Record
                          v,ph : Real
                        End;

        Procedure Buscar_pto( pcent : Real;
                              Var t : Tipo_tbl; Var p : Tipo_pto );
           Var i : Integer;
               v1,v2,v3,v4: Real;
        Begin
            i := 1;
            Consultar_vals( t, i, v1,v2,v3,v4);
            While ( i < t.num_vals ) And ( pcent > v1 ) Do Begin
               Inc(i);
               Consultar_vals( t, i, v1,v2,v3,v4)
            End;
            p.v := v4;
            p.ph:= v2
        End;

        Var pto10,pto20   : Tipo_pto;  (* Puntos para rectas segun %val. *)
            pto_aux       : Tipo_pto;
            m10_20,c10_20 : Real;      (*  pendte. y cte. y = m*x+c *)

            pto160,pto180 : Tipo_pto;
            m160_180,c160_180 : Real;

            pto_eq        : Tipo_pto;
            si,sd,ii,id   : Tipo_pto; (* Sup.Izq.,... ptos para el 'cruce'*)
            m1,m2,c1,c2   : Real;     (* Las diagonales.*)
            y_corte1,y_corte2 : Real; (* pH's de corte de tgs. con el eq.*)
                                      (* 1 -> tg. antes eq. 2 -> despues. *)
            delta_v       : Real;     (* Longitud segmentos horizontales.*)
            c             : Tipo_Color;
            void          : Char;
       Begin
           Dar_info( info_y, 'Pulse una Tecla para continuar...');
           c := GetColor;
           SetColor(Brown);
           (* Ptos. interpolacion tangentes.*)
           Buscar_pto( 20, t, pto10 );
           Buscar_pto( 50, t, pto20 );
           Buscar_pto( 150,t, pto160);
           Buscar_pto( 190,t, pto180);
           Buscar_pto( 100,t, pto_eq);
           delta_v := pto_eq.v / 2;

           (* tangentes...*)
           Dar_info(stat_y,'Trazo tangentes a la curva');
           m10_20 := Pendiente_2p( pto10.v, pto10.ph,
                                   pto20.v, pto20.ph );
           c10_20 := Constante_2p( pto10.v, pto10.ph,
                                   pto20.v, pto20.ph );
           m160_180 := Pendiente_2p( pto160.v, pto160.ph,
                                   pto180.v, pto180.ph );
           c160_180 := Constante_2p( pto160.v, pto160.ph,
                                   pto180.v, pto180.ph );
           Marcar_vol(g, pto_eq.v, Brown);
           Linea( g, pto10.v,pto10.ph,
                  pto180.v, Y_pc( pto180.v, m10_20,c10_20),Brown,DottedLn );
           Linea( g, pto180.v,pto180.ph,
                  pto10.v, Y_pc( pto10.v, m160_180,c160_180),Brown,DottedLn );

           (* Corte con vertical en eq. *)
           void := Stroke;
           Dar_info(stat_y,'Trazo Paralelas a eje X en ptos. de corte');
           y_corte1 := Y_pc( pto_eq.v, m10_20, c10_20 );
           Marcar_pH( g, y_corte1,Brown);
           y_corte2 := Y_pc( pto_eq.v, m160_180,c160_180);
           Marcar_pH( g, y_corte2,Brown);


           (* 4 puntos para las 2 diagonales *)
           void := Stroke;
           Dar_info(stat_y,'Trazo Diagonales ...');
           Delay( Tgs_Dly );
           si.pH := y_corte2; sd.pH := y_corte2;
           ii.pH := y_corte1; id.pH := y_corte1;
           si.v := pto_eq.v - delta_v; ii.v := si.v;
           sd.v := pto_eq.v + delta_v; id.v := sd.v;
           m1 := Pendiente_2p( si.v,si.ph,id.v,id.ph );
           m2 := Pendiente_2p( sd.v,sd.ph,ii.v,ii.ph );
           c1 := Constante_2p( si.v,si.ph,id.v,id.ph );
           c2 := Constante_2p( sd.v,sd.ph,ii.v,ii.ph );
           Linea(g,si.v,si.ph,id.v,id.ph,Brown,CenterLn);
           Linea(g,ii.v,ii.ph,sd.v,sd.ph,Brown,CenterLn);

           (* Cruce de diagonales...eq *)
           pto_eq.v := X_corte_pc( m1,c1,m2,c2);
           pto_eq.ph:= Y_corte_pc( m1,c1,m2,c2);
           Dar_info(stat_y,'Trazo Diagonales : eq en pH '+
                           Form('##.##',pto_eq.ph) +
                           ' con vol.' + Form('###.##',pto_eq.v) );

           void := Stroke;
           SetColor(c)
       End;(*Simular_tgs*)


       Procedure Msg_inicial( sy, iy : Integer );
       (*                    8
        * Informa de que se esta inicializando.
        *)
       Begin
         Dar_info( sy, 'Inicializando...');
         Dar_info( iy, 'Espere por favor.');
       End;

       Procedure Iniciar_Dibujo( min_y,max_y : Integer; Var fin_x : Integer;
                                 volb,volb2, volm : Real;
                                 colm_ac, colm_ba : Word );
       Begin
            
          Situar_Bureta( fin_x + ((min_y + (max_y-min_y) * 70 Div 100) * 20 Div 100) Div 4,
                         min_y,
                         fin_x + (min_y + (max_y-min_y) * 70 Div 100) * 20 Div 100 +
                             ((min_y + (max_y-min_y) * 70 Div 100) * 20 Div 100) Div 4,
                         min_y + ( max_y - min_y ) * 70 Div 100,
                         volb);
          Situar_Matraz( fin_x,min_y + ( max_y - min_y ) * 70 Div 100,
                         (max_y - min_y - ( max_y - min_y ) * 70 Div 100)
                         * 80 Div 100 + fin_x,
                         max_y, volm + volb,
                         colm_ac,
                         colm_ba );

          Dibujar_Bureta( volb, Yellow );
          Vaciar_Bureta( volb2 );
          Dibujar_Matraz;
          Llenar_Matraz( volm, True );

          fin_x := (max_y - min_y - ( max_y - min_y ) * 70 Div 100)
                         * 80 Div 100 + fin_x
       End;

    Begin (* Valorar *)

      (* Iniciar pantalla y comunicar inicializacion....
       *)
      Componer_titulo( v, tit1, tit2 );
      A_graficos( dr_g, md_g );
      Iniciar_cursores( c1,c2,opc_sal.curva, opc_sal.derivada, nav_done );
      Pantalla_grafica( tit1, tit2, min_y, info_y, stat_y );
      Msg_inicial( stat_y, info_y);
      If opc_sal.nombre_fich <> Path_Nulo Then Begin
         Encabezar_fich( opc_sal.fichero_dos, opc_sal.nombre_fich,
                         tit1,tit2, done );
         If Not Done Then
            opc_sal.nombre_fich := Path_Nulo
      End;
      If opc_sal.a_impresora Then Begin
         Encabezar_fich( lst, Path_Nulo, tit1,tit2, done );
         If Not Done Then
            opc_sal.a_impresora := False
      End;

      (* Preparar salidas...
       *)

      Iniciar_tabla( min_y, stat_y-TextHeight('X'), fin_tbl_x, opc_sal, t );
      If opc_sal.dibujo Then Begin{ dibujar el montaje: Desplazar grafica}
          ind_est := Estimar_indicador_opt( i, v );
          Iniciar_dibujo( min_y,stat_y-TextHeight('X'), fin_tbl_x,
                          Dimension_bureta(v)*2,
                          Dimension_bureta(v) / 2,
                          v.vol, ColorInf_ind(v,ind_est),
                                 ColorSup_ind(v,ind_est)  )
      End;
      Iniciar_grafica( min_y, stat_y-TextHeight('X'),
                       fin_tbl_x, Dimension_bureta(v), g);
      Iniciar_derivada( g );
      Iniciar_itr( i, v, Numero_ptos_g( g ) );
      Preparar_nav( c1,c2,  n1, n2 );

      (*Valoracion
       *)
      If Valoracion_posible( v ) Then Begin
         Simular_val( n1,n2, v, i, t, g, opc_sal, info_y, stat_y, intr );
         If Not intr Then Begin
            If opc_sal.tangentes Then
              Simular_tgs( g, t, info_y, stat_y );
            If opc_sal.indicadores Then
              Mostrar_indicadores( i, v, g,  info_y, stat_y );
            Navegaciones( n1,n2,opc_sal, v, t, info_y, stat_y )
         End
      End
      Else Begin
         Dar_info(stat_y,'Valoracion imposible.');
         Dar_info(info_y,'Pulse una tecla.');
         void := Stroke
      End;

      RestoreCrtMode

    End;(*Valorar*)

Begin(* Inicializar variables globales *)

    cur_nav1 := NIL;
    cur_nav2 := NIL
End.