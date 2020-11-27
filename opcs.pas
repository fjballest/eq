(*
 * Fuente TPascal + ToolBox -- (c) 1991,1992  Esther Li#an y Nemo
 *
 * Modulo : [Opcs] Opciones.
 * Rev    : 1.0
 * O.S.   : DOS
 *
 *
 *)


Unit Opcs;

Interface (*Opcs*)

  Uses  TpString,
        Graph;   (* Unid. std. *)

  Const Path_Nulo = '';

        (* Interp. *)
        Lim_Inf_TPrec  = 90;     (* Limites inferior y superior por def. *)
        Lim_Sup_TPrec  = 110;    (* Para afinar la precision en P. normal*)
        Max_Ptos_Normal=  50;     (* Num. ptos a prec. normal *)
        Nombre_FDat_Def: String  = 'eq.dat';

        (* Colores *)
        Color_curva   : Word = Cyan;
        Color_der     : Word = Green;
        Color_eq      : Word = LightRed;
        Color_der_eq  : Word = LightMagenta;
        Color_titulo  : Word = Yellow;
        Color_info    : Word = LightGreen;
        Color_ind     : Word = Yellow;
        Color_crs_c   : Word = LightRed;
        Color_crs_d   : Word = LightMagenta;
        Sonido        : Boolean = True;


  Type  Tipo_precision = ( Prec_Normal, Prec_Total ); (* Prec. de interp.*)
        Tipo_path      = String;

        Tipo_opc_salida = Record
              precision  : Tipo_precision;
              salida_std : Boolean; (* Si esta a True se imprime la tabla en
                                      la salida std. sin mas.
                                      ANULA la opcion de "a_pantalla" *)
                                    (* !! Actualmente NO SE USA!! *)
              a_pantalla : Boolean; (* Si esta a True se dibuja la tabla. *)
                                    (* !! Actualmente NO SE USA!! *)
              a_impresora: Boolean; (* Si esta a True se quieren *)
                                    (* imprimir *)
              nombre_fich: Tipo_path; (* Si distinto de Path_nulo
                                           los resultados a ese fichero *)
              fichero_dos: Text;
              dibujo,
              curva, derivada, tangentes, indicadores : Boolean; (* curvas a
                                                                   dibujar *)
        End;

        Tipo_opc_entrada = Record
            nombre_fdat : Tipo_path
        End;

  Procedure Definir_opcs( Var oe : Tipo_opc_entrada;
                          Var os : Tipo_opc_salida );
  (*
   * Define opcs. segun la linea de cmds.
   *)

Implementation (*Opcs*)

  Uses TpCmdLin;


  Procedure Parametro( Var oe: Tipo_opc_entrada;
                       Var os: Tipo_opc_salida;
                       argc : Integer; Var err : Boolean );
  (* Fija oe y os segun el parametro argc-esimo. Devuelve err a True si
   *  hubo un error en el parametro. Para el programa dando ayuda si el
   *  parametro es /h o -h.
   *)
   Var p : String;
       c : Integer;
  Begin
      p := StUpCase(ParamStr(argc));

      If (p[1] = '-') Or (p[1]='/')
         And (Integer(p[0]) > 1) Then Begin (* Opcion *)
        c := 2;
        While Not err And (c <= Integer(p[0])) Do Begin
          Case p[c] Of
            'P': os.derivada := True;
            'C': os.curva    := True;
            'T': os.tangentes:= True;
            'I': os.indicadores:= True;
            'N': os.precision:= Prec_Normal;
            'M': os.precision:= Prec_Total;
            'W': os.a_impresora:= True;
            'D': os.dibujo := True;
            'A': Begin
                 os.derivada := True; os.curva := True;
                 os.tangentes:= True; os.indicadores := True;
                 os.dibujo   := True;
                 End;
            'V': Begin
                 WriteLn( ' eq v3.0 :');
                 WriteLn( '    (c) 1991,1992 Esther L. Veganzones & Francisco J. Ballesteros');
                 WriteLn( '                  Facultad de Ciencias   Facultad de informatica');
                 WriteLn( '                  Univ. de Granada       Univ. Politecnica Madrid');
                 WriteLn( '                                         email: nemo@sol.dia.fi.upm.es');
                 WriteLn( '                                         tlfno: +91-336-74-48 (CLIP)');
                 Halt(1)
                 End;
            'H': Begin
              Writeln(' uso : eq [-hv] ');
              WriteLn('       eq [-acdinptw] nombre_fichero_datos');
              WriteLn('          -p: Primera derivada');
              WriteLn('          -c: Curva de valoracion');
              WriteLn('          -t: Tangentes');
              WriteLn('          -i: Indicadores');
              WriteLn('          -n: Precision Normal');
              WriteLn('          -m: Precision Total (Maxima)');
              WriteLn('          -w: Copiar a impresora (Write)');
              WriteLn('          -d: Dibujar el montaje de laboratorio');
              WriteLn('          -a: Todas las salidas (All)');
              WriteLn('          -h: Ayuda (Help)');
              WriteLN('          -v: Version');
              Halt(1)
              End;
            Else err := True
          End;
          Inc(c)
        End
      End
      Else If oe.nombre_fdat <> Path_Nulo Then (* Mas de un nombre...*)
          err := True
      Else
          oe.nombre_fdat := p
  End;

  Procedure Definir_opcs( Var oe : Tipo_opc_entrada;
                          Var os : Tipo_opc_salida );
  (*
   * Define opcs. segun la linea de cmds.
   *)
     Var pcount : Word;
         error  : Boolean;
  Begin
     oe.nombre_fdat := Nombre_FDat_Def;
     With os Do Begin
       precision := Prec_Normal;
       salida_std:= False;
       a_pantalla:= True;
       a_impresora:=False;
       dibujo := False;
       nombre_fich:= Path_Nulo;
       curva := True;
       derivada := False; tangentes := False; indicadores := False
     End;

     If ParamCount > 0 Then Begin (* Hay opciones*)
        pcount := 1;
        error  := False;
        While Not error And (pcount <= ParamCount ) Do Begin
            Parametro( oe,os, pcount, error );
            Inc(pcount)
        End;

        If error Then Begin
         Writeln(' uso : eq [-h] ');
         WriteLn('       eq [-pctinaw] nombre_fichero_datos');
         Halt(1)
        End
     End;





  End; (*Definir_opcs*)

Begin

End. (*Opcs*)

