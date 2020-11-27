(*
 * Fuente TPascal + ToolBox -- (c) 1991,1992  Esther Li#an y Nemo
 *
 * Modulo : [GrIO] : Graph IO
 * Rev    : 1.0
 * O.S.   : DOS
 *
 *
 *)


Unit GrIO;

Interface (*GrIO*)


   Uses  Graph,     (* Unid. std. *)
         BgiDriv,   (* Tool-Box ... *)
         BgiFont,
         TpString,  (* Tool-Box   *)
         Opcs;


   Const Monocromo : Boolean = False; (* Fijar a true para blanco/negro *)

   Type
        Tipo_nombre_color = String[80];
        Tipo_color        = ShortInt;

        Tipo_linea        = String[80];


    Procedure Inicializar_graficos;
    (*
     * Inicializa (registra) los drivers y fonts graficos utilizados.
     *)

    Function Nombre2Color( n : Tipo_nombre_color ) : Tipo_color;
    (*
     * Devuelve el color asociado a un nombre.
     *)


   Procedure Imprimir_str_xyl( x,y,l : Integer; s : String );
   (*
    * Imprime s ( de longitud l ) en (x,y) borrando el fondo.
    *)

   Procedure Dar_info(  min_y : Integer; str: String );
   (*
    * Imprime str en (min_y,0) borrando el fondo hasta el final de la lin.
    *)


Implementation (*GrIO*)

    Procedure Inicializar_graficos;
    (*
     * Inicializa (registra) los drivers y fonts graficos utilizados.
     *)
    Begin
      If (RegisterBGIdriver(@CGADriverProc)<0) Or
         (RegisterBGIdriver(@EGAVGADriverProc)<0) Or
         (RegisterBGIFont(@TriplexFontProc)<0) Then Begin
         WriteLn('eq: Error de inicializacion de graficos.');
         halt(1)
      End
    End;(*Inicializar_graficos*)


    Function Nombre2Color( n : Tipo_nombre_color ) : Tipo_color;
    (*
     * Devuelve el color asociado a un nombre.
     *)
    Begin
       Nombre2Color := White;

       If Not Monocromo Then Begin
         n := StUpCase( n );
         If n = 'NEGRO' Then
           Nombre2Color := DarkGray
         Else if n = 'AZUL' Then
           Nombre2Color := Blue
         Else if n = 'VERDE' Then
           Nombre2Color := Green
         Else if n = 'CELESTE' Then
           Nombre2Color := Cyan
         Else if n = 'NARANJA' Then
           Nombre2Color := LightRed
         Else if n = 'ROJO' Then
           Nombre2Color := Red
         Else if n = 'VIOLETA' Then
           Nombre2Color := Magenta
         Else if n = 'MARRON' Then
           Nombre2Color := Brown
         Else if n = 'GRIS' Then
           Nombre2Color := LightGray
         Else if n = 'AMARILLO' Then
           Nombre2Color := Yellow
         Else
           Nombre2Color := White

       End
    End;



   Procedure Imprimir_str_xyl( x,y,l : Integer; s : String );
   (*
    * Imprime s ( de longitud l )en (x,y) borrando el fondo.
    *)

     Var view : ViewPortType;
   Begin
     GetViewSettings( view );
     SetViewPort( x, y, x + TextWidth('X') * l, y + TextHeight('X'), True);
     ClearViewPort;
     SetViewPort(view.x1,view.y1,view.x2,view.y2,view.clip);
     OutTextXY( x, y, s );

   End;


   Procedure Dar_info(  min_y : Integer; str: String );
   (*
    * Imprime str en (min_y,0) borrando el fondo hasta el final de la lin.
    *)
      Var view : ViewPortType;
          col  : Word;
   Begin
       col := GetColor;
       SetColor( Color_info );
       GetViewSettings( view );
       SetViewPort(0,min_y,GetMaxX,min_y+TextHeight(str),True);
       ClearViewPort;
       OutTextXY(0,0,str);
       SetViewPort( view.x1, view.y1, view.x2, view.y2, view.clip );
       SetColor( col )
   End;



Begin

End. (*GrIO *)

