
Program Eq( datos, input, output );
(*
 *
 * Eq: Simulacion de equilibrios. version 2.0
 *     (c)-1991,1992 Esther Li#an Veganzones.
 *
 *     Se leen datos sobre acidos/bases y se pueden simular valoraciones
 *     entre ellos. ( Solo monoproticos ).
 *     Formato del fichero de datos:
 *     Linea de comentarios:
 *           # ......
 *     Lineas de datos:
 *           <tipo> <formula> <fuerza> <constante> <nombre>
 *     Donde <tipo> es "a" o "b" y <fuerza> es "fuerte" o "debil".
 *
 *     Ejemplo:
 *       # Acidos...
 *       a ClH  fuerte 1 Acido Clorh°drico
 *
 *     Nota: '#', 'a' y 'b' han de estar al principio de la linea.
 *
 *     Cambios respecto a la version 1.0:
 *
 *
 *)

Uses

     (*
      *Unidades de las Turbo-Pascal Tool Box.
      *)
     TpDOS,    (* TB-MsDOS procs.*)
     TpString, (* TB-Strings     *)
     TpCrt,    (* TB-Terminal I/O*)
     TpEdit,   (* TB-Input       *)
     TpMenu,   (* TB-Menus       *)
     TpWindow, (* TB-Windowing   *)

     Opcs,
     GrIO,
     React,
     Val,
     Simular;    (* Funciones de calculo de pH *)



Const Nombre_datos  = 'eq.dat';

   (*
    * Opciones de los menus...
    *    Formato 'xxyy' xx -> Numero de menu ; yy-> Numero de opcion
    *)
      Op_acidos   = 0101; (* Principal *)
      Op_bases    = 0102;
      Op_tipoVal  = 0103;
      Op_opc      = 0104;
      Op_val      = 0105;
      Op_salir    = 0106;

      Op_opc_prn  = 0501; (* Selecciones del submenu de opciones *)
      Op_opc_fich = 0502;
      Op_opc_val  = 0503;
      Op_opc_val_curva = 0701;
      Op_opc_val_der   = 0702;
      Op_opc_val_tang  = 0703;
      Op_opc_val_ind   = 0704;
      Op_opc_val_dib   = 0705;

      Op_opc_prec_norm = 0801;
      Op_opc_prec_total= 0802;

      Op_ent_explotar  = 0901;
      Op_ent_sonido    = 0902;

      Op_tv_ab    = 0401; (* Selecciones del submenu de tipo de valoracion *)
      Op_tv_ba    = 0402;

   (*
    * Posicion de la ventana de info. valoracion e input.
    *)

      Val_xi       = 5;
      Val_yi       = 19;
      Val_xs       = 75;
      Val_ys       = 24;
      Marco        = 2;
      Ancho        = Val_xs - Val_xi - Marco;





    (*
     * Variables relacionadas con la valoracion ******************************
     *)

    Var valoracion : Tipo_valoracion;

    (*
     * Variables auxiliares **************************************************
     *)
        datos      : Text;
        ventana_val: WindowPtr; (* Info. sobre valoracion *)
        ventana_cop: WindowPtr; (* Ventana (c)    *)
        ventana_input:WindowPtr;(* Ventana para leer nombres ficheros, etc*)
        ask_win    : WindowPtr;

        menu_pr    : Menu;      (* Menu principal *)
        tecla      : Char;
        opcion     : MenuKey;
        opc_entrada: Tipo_opc_entrada;
        opc_salida : Tipo_opc_salida; (* Opciones para salida de datos *)
        abort      : Boolean;

    (*
     * Procedimientos auxiliares *********************************************
     *)



    procedure InitMenu( var M : Menu;
                        var ej_a : Tipo_acds;
                        var ej_b : Tipo_bass );
    const
      Color1 : MenuColorArray = ($09, $09, $03, $1E, $0B, $0E);
      Color2 : MenuColorArray = ($06, $2E, $03, $1E, $0B, $0E);
      Color3 : MenuColorArray = ($04, $4E, $03, $1E, $0B, $0E);
      Color4 : MenuColorArray = ($04, $2E, $03, $1E, $0B, $0E);
      Frame1 : FrameArray = '…»ªºÕ∫';
      Frame2 : FrameArray = '’‘∏æÕ≥';
      Var cont : Integer;

    begin
      {Customize this call for special exit characters and custom item displays}
      M := NewMenu([], nil);

      SubMenu(1,3,1,Horizontal,Frame1,Color1,'Menu Principal');
        MenuItem('Acidos',2,1,101,'Elegir un †cido para la valoraci¢n.');
        SubMenu(3,5,1,Vertical,Frame1,Color2,'');
          cont := 1;
          While (ej_a[cont].nombre <> Ac_Nulo.nombre) And
                (cont<Num_Acds)              Do Begin
             With ej_a[cont] Do
               MenuItem(formula,cont,1,200+cont,
                        nombre+Nombre_fuerza(fuerza));
             Inc(cont )
          End;
          PopSublevel;
        MenuItem('Bases',14,1,102,'Elegir una base para la valoraci¢n.');
        SubMenu(15,5,1,Vertical,Frame1,Color2,'');
          cont := 1;
          While (ej_b[cont].nombre <> Ba_Nula.nombre) And
                (cont<Num_Bass)              Do Begin
             With ej_b[cont] Do
               MenuItem(formula,cont,1,300+cont,
                        nombre+Nombre_fuerza(fuerza));
             Inc(cont )
          End;
          PopSublevel;
        MenuItem('Tipo de Valoraci¢n',24,1,103,'Seleccionar a quiÇn se valora.');
        SubMenu(28,5,1,Vertical,Frame1,Color2,'');
          MenuItem('Acido con Base',1,1,401,'Valorar el †cido con la base.');
          MenuItem('Base con Acido',2,1,402,'Valorar la base con el †cido.');
          PopSublevel;
        MenuItem('Opciones',47,1,104,'Cambiar opciones para la simulaci¢n');
        SubMenu(48,5,1,Vertical,Frame1,Color2,'');
          MenuItem('Salida a Impresora',1,10,501,'Imprimir o no la tabla de valoraci¢n.');
          MenuItem('Salida a fichero',2,10,502,'Copiar la tabla de valoraci¢n a un fichero');
          MenuItem('MÇtodos de valoraci¢n',3,1,503,'Elegir los mÇtodos gr†ficos para la valoraci¢n');
          SubMenu(50,9,1,Vertical,Frame2,Color3,'Dibujar');
            MenuItem('Curva de valoraci¢n',1,1,701,'Dibujar o no la curva de valoraci¢n.');
            MenuItem('Primera derivada',2,1,702,'Dibujar o no la primera derivada de la curva.');
            MenuItem('Tangentes',3,1,703,'Dibujar o no el mÇtodo de las tangentes.');
            MenuItem('Indicador',4,1,704,'Dibujar o no el area de aplicaci¢n del indicador.');
            MenuItem('Dibujo del Montaje',5,1,705,'Grafica del dispositivo de laboratorio');
            PopSublevel;
          MenuItem('Precisi¢n',4,1,504,'Elegir precisi¢n para la simulaci¢n');
          SubMenu(60,9,1,Vertical,Frame1,Color4,'');
            MenuItem('Normal',1,1,801,'Precisi¢n usual para simulaci¢n manual');
            MenuItem('Maxima',2,1,802,'Utilizar la m†xima precisi¢n disponible');
            PopSublevel;
{         MenuItem('Configuraci¢n',5,1,505,'Elegir opiones del entorno');
          SubMenu(53,10,1,Vertical,Frame1,Color3,'Entorno');
            MenuItem('Explotar ventanas',1,1,901,'Explotar ventanas al crearlas/cerrarlas o no hacerlo');
            MenuItem('Sonido',2,1,902,'Activar/Desactivar sonido');
            PopSublevel;}
          PopSublevel;
        MenuItem('Valorar',60,1,105,'Simular la valoraci¢n.');
        MenuItem('Salir',73,1,106,'Terminar la ejecuci¢n del programa.');
        PopSublevel;

      ResetMenu(M);
    end;





    Procedure Mostrar_cop( Var cop_win : WindowPtr );
    (*
     * Dice que es todo esto. Devuelve la ventana utilizada.
     *)
      Const Info_xi       = 5;
            Info_yi       = 15;
            Info_xs       = 75;
            Info_ys       = 18;
            Marco         = 2;
            Ancho         = Info_xs - Info_xi - Marco;

      Var void      : Boolean;
    Begin
      (*
       * Dar ayuda
       *)
      void := SelectTiledWindow( cop_win );
      void := DisplayWindow( cop_win );
      Window( Info_xi+1, Info_yi+1, Info_xs-1, Info_ys-1 );
      ClrScr;
      Write( Center(
                'Programa para simulaci¢n de valoraciones †cido-base',
                Ancho) );
      GotoXY(1,2);
      Write( Center( '(c)-1991,1992 Esther Li§†n, Franc.J.Ballesteros',
                       ancho));

      Window(0,0,79,24);

    End;(*Mostrar_cop*)


    Procedure Init_cop( Var cop_win : WindowPtr );
    (*
     * Dice que es todo esto. Devuelve la ventana utilizada.
     *)
      Const Info_xi       = 5;
            Info_yi       = 15;
            Info_xs       = 75;
            Info_ys       = 18;
            Info_attr     = 7;
            Info_bor_attr = 5;
            Info_cab_attr = 10;
            Marco         = 2;
            Ancho         = Info_xs - Info_xi - Marco;

      Var void      : Boolean;
    Begin
      (*
       * Dar ayuda
       *)
      ClrScr;
      If Not MakeWindow( cop_win, Info_xi, Info_yi, Info_xs, Info_ys,
                         True, True, True,
                         Info_attr, Info_bor_attr, Info_cab_attr,
                         'Eq v3.0: Simulaci¢n de equilibrios' )  Then Begin
         WriteLn(' Eq : Error al inicializar ventanas...');
         Halt( 1 );
      End;
      Mostrar_cop( cop_win );

    End;(*Init_cop*)



    Procedure Pedir_opc_fich( Var opc : Tipo_opc_salida;
                              Var win : WindowPtr       );
    (*
     * Pregunta en win si se desea salida a fichero. Lo deja indicado
     * en opc. ( opc.nombre_fich )
     *)
      Const w_attr     = 8;
            w_bor_attr = 6;
            w_cab_attr = 11;
            marco      = 2;
            prompt     = 'Vac°o si ninguno:';
            Nombre_por_def = 'tabla.eq';

      Var void : Boolean;
          w_xi,w_yi, w_xs, w_ys : Byte;
          viejo_fich,path: Tipo_nombre;
    Begin
      viejo_fich := opc.nombre_fich;
      If viejo_fich = Path_nulo Then
        opc.nombre_fich := Nombre_por_def;
      w_xi := 10; w_yi := 9;
      w_xs := 70; w_ys := 12;
      If win = NIL Then
         If Not MakeWindow( win, w_xi, w_yi, w_xs, w_ys,
                            True, True, True,
                            w_attr, w_bor_attr, w_cab_attr,
                            'Nombre del Fichero?' )  Then Begin
            WriteLn(' Eq : Error al inicializar ventanas...');
            Halt( 1 );
         End;

      void := SelectTiledWindow( win );
      Window( w_xi+1, w_yi+1, w_xs-1, w_ys-1 );
      ClrScr;
      Window(0,0,79,24);
      ReadString( prompt,
                  w_yi+1,w_xi+1,30,
                  w_cab_attr,w_bor_attr,w_attr,
                  void, opc.nombre_fich );
      If void Then
         opc.nombre_fich := viejo_fich;
      If (opc.nombre_fich = CharStr(' ',Length(opc.nombre_fich))) Then
         opc.nombre_fich := Path_nulo;
      win := EraseTopWindow;
    End;

    Procedure Mostrar_info_val(val_win  : WindowPtr;
                               Var v    : Tipo_valoracion;
                               Var opc  : Tipo_opc_salida  );
    (*
     * Mostrar valoracion
     *)

      Var void : Boolean;
          n    : Tipo_nombre;
    Begin
      void := SelectTiledWindow( val_win );
      void := DisplayWindow( val_win );
      Window( Val_xi+1, Val_yi+1, Val_xs-1, Val_ys-1 );
      ClrScr;

      Write( Center( '*** Se valora ***',Ancho Div 2));
      Write( 'Salidas: ' );
      If opc.a_impresora Then
        Write('[Impresora]');
      If opc.nombre_fich <> Path_nulo Then Begin
        IF Length( opc.nombre_fich ) > 12 Then Begin
           n :=Copy(opc.nombre_fich,Length(opc.nombre_fich) - 9,10);
           n := '..' + n;
           End
        Else n := opc.nombre_Fich;
        Write('[',n,']');
      End;
      GotoXY(1,2);

      If v.clase_val = Acido_con_base Then
            Write(Center(Nombre_ac(v),Ancho Div 2))
      Else  Write(Center(Nombre_ba(v),Ancho  Div 2));
      Write(CenterCh('MÇtodos','_',Ancho Div 2));
      GotoXY(1,3);
      Write( Center ('****** con ******',Ancho Div 2));
      If opc.curva Then
        Write( '[curva]' );
      If opc.derivada Then
        Write( '[derivada]');
      If (opc.curva Or opc.derivada) Then
        If opc.precision = Prec_Normal Then
          Write(': Prec.Normal')
        Else
          Write(': Prec.Total');
      GotoXY(1,4);
      If v.clase_val = Acido_con_base Then
           Write(Center(Nombre_ba(v),Ancho Div 2))
      Else Write(Center(Nombre_ac(v),Ancho Div 2));
      If opc.tangentes Then
        Write( '[tangentes]');
      If opc.indicadores Then
        Write( '[indicador]');
      If opc.dibujo Then
        Write( '[dibujo]' );


      Window(0,0,79,24);
    End;(*Mostrar_info_val*)



    Procedure Inic_info_val( Var val_win : WindowPtr;
                             Var v       : Tipo_valoracion );
    (*
     * Definir valoracion por defecto
     *)
      Const Val_attr     = 7;
            Val_bor_attr = 5;
            Val_cab_attr = 10;

      Var void      : Boolean;
    Begin
      If Not MakeWindow( val_win, Val_xi, Val_yi, Val_xs, Val_ys,
                         True, True, True,
                         Val_attr, Val_bor_attr, Val_cab_attr,
                         'Informaci¢n de la valoraci¢n' )  Then Begin
         WriteLn(' Eq : Error al inicializar ventanas...');
         Halt( 1 );
      End;
      void := SelectTiledWindow( val_win );
      Window( Val_xi+1, Val_yi+1, Val_xs-1, Val_ys-1 );
      ClrScr;
      Window(0,0,79,24);
      v.acido := 1;
      v.base  := 1;
      v.clase_val := Acido_con_base;
      v.vol   := 0
    End;(*Inic_info_val*)

    Function Indice_de_opcion( opc : MenuKey ) : Integer;
    (*
     * Devuelve el indice para acido/base asociado con la opcion opc.
     *)
    Begin
      Indice_de_opcion := opc MOD 100 (* Dos ultimos digitos *)
    End;



    Procedure Iniciar_ventanas( Var w,w1 : WindowPtr );
    Begin
      w := NIL;
      w1:= NIL;
      TpWindow.Explode := True;
      TpWindow.Shadow  := False;
      TpWindow.SoundFlagW := False
    End;


    Procedure Preguntar_Datos( Var ask_win : WindowPtr;
                               na, nb, vol : RealPt; Var abort:Boolean );

        Const Val_xi       = 5;
              Val_yi       = 5;
              Val_xs       = 75;
              Val_ys       = 13;
              Val_attr     = 7;
              Val_bor_attr = 5;
              Val_cab_attr = 10;
              Marco         = 2;
              Ancho         = Val_xs - Val_xi - Marco;

        Var void   : Boolean;
    Begin
      If ask_win = NIL Then
          If Not MakeWindow( ask_win, Val_xi, Val_yi, Val_xs, Val_ys,
                             True, True, True,
                             Val_attr, Val_bor_attr, Val_cab_attr,
                             'Normalidades y Volumen?' )  Then Begin
             WriteLn(' Eq : Error al inicializar ventanas...');
             Halt( 1 );
          End;
      void := SelectTiledWindow( ask_win );
      void := False;
      Window( Val_xi+1, Val_yi+1, Val_xs-1, Val_ys-1 );
      ClrScr;
      Window(0,0,79,24);
      ReadReal('Normalidad del acido [0.1-1]?',
               Val_yi+1,Val_xi+8,8,Val_cab_attr,Val_attr,
               4,0.1,1.0,void,na^);
      If Not void Then Begin
         ReadReal('Normalidad de la base[0.1-1]?',
                  Val_yi+3,Val_xi+8,8,Val_cab_attr,Val_attr,
                  4,0.1,1.0,void,nb^);
         If Not void Then
            ReadReal('Volumen a valorar[0-100]mL?',
                     Val_yi+5,Val_xi+8,8,Val_cab_attr,Val_attr,
                     4,0.00001,100.0,void,vol^);
      End;
      ask_win := EraseTopWindow;
      Window(0,0,79,24);
      abort := void;
    End; (*Preguntar_datos*)




Begin (* Eq ******************************************************************)



   (* Definir el fichero del que leemos...*)

   Definir_opcs( opc_entrada, opc_salida );

   {$I-}Assign( datos, opc_entrada.nombre_fdat );{$I+}
   If IOResult <> 0 Then Begin
     WriteLn('eq: No existe el fichero de datos.');
     Halt(1)
   End;

   Iniciar_ventanas( ask_win,ventana_input);
   Inicializar_graficos;

   Crear_val( valoracion );
   Leer_mundo_de_val( datos, valoracion );


   Init_cop( ventana_cop );
   InitMenu( menu_pr, valoracion.acds, valoracion.bass);
   Inic_info_val( ventana_val,valoracion); (* Definir valoracion *)
   Mostrar_info_val( ventana_val, valoracion, opc_salida );

   Repeat
      opcion := MenuChoice( menu_pr, tecla );
      Case opcion Of
        (*
         * ACIDOS y BASES
         *)
        200..299: (* Acido -> 02xx *)
          valoracion.acido := Indice_de_opcion( opcion );
        300..399: (* Base ->  03xx *)
          valoracion.base := Indice_de_opcion( opcion );
        (*
         * Tipos de VALORACION
         *)
        Op_tv_ab: (* Ac. con Bas.  *)
          valoracion.clase_val := Acido_con_base;
        Op_tv_ba: (* Ba. con Ac.   *)
          valoracion.clase_val := Base_con_acido;
        (*
         * OPCIONES
         *)
        Op_opc_prn:
           opc_salida.a_impresora := Not opc_salida.a_impresora;
        Op_opc_fich:
           Pedir_opc_fich( opc_salida, ventana_input );
        Op_opc_val_curva:
           opc_salida.curva  := Not opc_salida.curva;
        Op_opc_val_tang: Begin
           opc_salida.tangentes := Not opc_salida.tangentes;
           opc_salida.curva     := True
           End;
        Op_opc_val_der:
           opc_salida.derivada  := Not opc_salida.derivada;
        Op_opc_val_ind:
           opc_salida.indicadores:= Not opc_salida.indicadores;
        Op_opc_val_dib:
           opc_salida.dibujo    := Not opc_salida.dibujo;
        Op_opc_prec_norm:
           opc_salida.precision := Prec_Normal;
        Op_opc_prec_total:
           opc_salida.precision := Prec_Total;
        (*
         * VALORACION
         *)
        Op_val  : (* Go!           *)
          If opc_salida.curva Or opc_salida.derivada Or
                                 opc_salida.indicadores Then Begin
            Preguntar_datos( ask_win,
                             Norm_ac_pt( valoracion),
                             Norm_ba_pt( valoracion),
                             Vol_pt( valoracion ), abort );
            If Not abort Then Begin
               ventana_val := EraseTopWindow;
               ventana_cop := EraseTopWindow;
               EraseMenu( menu_pr, True );

               Valorar( valoracion, opc_salida );
               Mostrar_cop( ventana_cop )
            End
          End;

        Op_salir: Begin (* Bye!          *)
          EraseMenu( menu_pr, True );
          ventana_cop := EraseTopWindow;
          ventana_val := EraseTopWindow;
          ClrScr;
          WriteLn('Eq: ≠≠adi¢s, oh mundo!!')
        End
      End;
      If (opcion <> Op_salir) Then
         Mostrar_info_val( ventana_val, valoracion, opc_salida )
   Until (opcion = Op_salir) Or (MenuStatus <> MenuSuccess);

   If MenuStatus <> MenuSuccess Then Begin
          EraseMenu( menu_pr, True );
          ventana_cop := EraseTopWindow;
          ventana_val := EraseTopWindow;
          ClrScr;
          MirarMenuStatus(MenuStatus);
   End



End. (* Eq *)


