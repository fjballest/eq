(*
 * Fuente TPascal + ToolBox -- (c) 1991,1992  Esther Li#an y Nemo
 *
 * Modulo : [GrfVal] Grafica de valoracion.
 * Rev    : 1.0
 * O.S.   : DOS
 *
 *
 *)


Unit GrfVal;

Interface (*GrfVal*)


  Uses Graph,    (* Unids. std... *)

       TpString, (* Tool-Box... *)

       Opcs,     (* Colores,...*)
       GrIO;     (* TipoColor,...*)


  Type Tipo_grf = Record
           px_por_ph    : Integer;    (* Num pix. por unidad pH [Y] *)
           vol_por_px   : Real;       (* Vol. en mL por pixel   [X] *)

           pix_actual   : PointType;  (* Pixel actual para la curva *)
           der_pix_actual : PointType;                   (* y deriv.*)

           org_pix      : PointType;  (* Pixel para (0,0)      *)
           min_pix      : PointType;  (* Lim. inf. 3er. cuadr. *)
           max_pix      : PointType   (* Lim. sup. 1er. cuadr. *)
  End;

        (* En Tipo_grf los nombres de coords. suponen coords. cartesianas
         * del 1er. cuadr. para la pantalla,
         * los valores no obstante responden a valores del 4o. cuadr. como
         * sucede en la unidad "Graph".
         *)


  Procedure Iniciar_Grafica( min_y, max_y : Integer;
                             Var min_x    : Integer;
                             volb         : Real;
                             Var g        : Tipo_grf  );
  (*
   * Dibuja la grafica inicial y define las componentes relacionadas
   * con ella de i y de g.
   * min_y max_y son los limites disponibles. min_x es el limite
   * impuesto para las x. Toma todo lo que necesita desde la drcha.
   * y aumente min_x si le sobra.
   *
   * En volb ha de recibir el volumen de la bureta.
   *
   * Define en g : Todo excepto los campos pixy_actual y der_pixy_actual
   *               que se han de definir segun los valores iniciales.
   *
   *)

  Procedure Iniciar_derivada( Var g : Tipo_grf );
  (*
   * Inicializa la parte del iterador que corresponde a la derivada
   * tras haber sido inicializada la parte correspondiente a la
   * grafica.
   *)

  Procedure Indicar_En_Grafica( hacerlo : Boolean;
                                ph : Real;
                                primero,seguir: Boolean;
                                Var g  : Tipo_grf );
  (*
   * Continua el dibujo de la grafica. Se entiende que el incremento
   * en las x corresponde a 1pix. o a 0pix. is seguir es False.
   * Primero se utiliza para no enlazar con el pixel anterior.
   * No hace nada si hacero es False.
   * Cuando no se sigue marca el pto. con un circulo
   *)

  Procedure Indicar_En_Derivada(hacerlo : Boolean;
                                ph : Real;
                                primero,seguir: Boolean;
                                Var g  : Tipo_grf );
  (*
   * Continua el dibujo de la derivada. Se entiende que el incremento
   * en las x corresponde a 1pix. o a 0pix. is seguir es False.
   * Primero se utiliza para no enlazar con el pixel anterior.
   * No hace nada si hacero es False.
   * Cuando no se sigue marca el pto. con un circulo
   *)

  Procedure Indicar_eq( hacerlo : Boolean;
                        Var g: Tipo_grf; vol,ph: Real );
  (*
   * Marca el pto. correspondiente a (vol,ph) indicando que es el
   * pto de eq. y los valores que le corresponden. ( si hacerlo )
   *)

  Procedure Indicar_deq( hacerlo: Boolean;
                         Var  g: Tipo_grf;
                         vol,ph: Real             );
  (*
   * Marca el pto. correspondiente a (vol,ph) indicando que es el
   * pto max. de la der. y los valores que le corresponden. ( si hacerlo)
   *)


  Procedure Marcar_pH( Var g: Tipo_grf; ph : Real; color: Tipo_color );
  (*
   * marca con una linea de color pH= ph en g.
   *)

  Procedure Marcar_vol( Var g: Tipo_grf; vol : Real; color: Tipo_color );
  (*
   * marca con una linea de color vol= vol en g.
   *)

  Procedure Linea( Var g: Tipo_grf; v1,ph1,v2,ph2 : Real;
                   color: Tipo_color; ls : Integer );
  (*
   * marca con una linea de color
   *)


  Function Numero_ptos_g( Var g: Tipo_grf ): Integer;
  (*
   * Devuelve el No. de ptos. disponibles para el volumen en g.
   *)

Implementation (*GrfVal*)

  Procedure Iniciar_Grafica( min_y, max_y : Integer;
                             Var min_x    : Integer;
                             volb         : Real;
                             Var g        : Tipo_grf  );
  (*
   * Dibuja la grafica inicial y define las componentes relacionadas
   * con ella de i y de g.
   * min_y max_y son los limites disponibles. min_x es el limite
   * impuesto para las x. Toma todo lo que necesita desde la drcha.
   * y aumente min_x si le sobra.
   *
   * En volb ha de recibir el volumen de la bureta.
   *
   * Define en g : Todo excepto los campos pixy_actual y der_pixy_actual
   *               que se han de definir segun los valores iniciales.
   *
   *)

     Var cadena: String;
         pix   : Integer;
         long  : Integer;
         pixpml: Integer;
         count : Integer;
         upor_etqx : Real;   (* Unids. en el eje por ancho/alto de etiq.*)
         upor_etqy : Real;
         volval    : Real;
  Begin
      (* Definir limites y escalas.... *)
      g.min_pix.y := max_y;
      g.org_pix.y := min_y + Round((g.min_pix.y-min_y)*3/4);
      g.pix_actual.y := g.org_pix.y;
      g.max_pix.y := min_y;
      g.max_pix.x := GetMaxX;
      If GetMaxX DIV 3 > min_x Then
         min_x := GetMaxX Div 3;
      g.min_pix.x := min_x;
      g.pix_actual.x := min_x +TextWidth('XXX');
      g.org_pix.x := g.pix_actual.x;

      g.px_por_ph   := (g.org_pix.y - g.max_pix.y ) Div 14;
      g.vol_por_px  := volb /  (g.max_pix.x-g.pix_actual.x);

      (* Etiquetar.... *)
      cadena := 'vol(mL)';
      OutTextXY( GetMaxX-3-TextWidth(cadena),
                 g.org_pix.y -  TextHeight(cadena) - 2,cadena );
      cadena := '0';
      OutTextXY( min_x, g.org_pix.y- TextHeight('X') Div 2, cadena );
      cadena := 'pH';
      OutTextXY( g.pix_actual.x + TextWidth('X'),
                 min_y + TextHeight('X') DIV 2,cadena);

      (* Dibujar ejes.... *)
      Line( min_x +TextWidth('X'),g.org_pix.y,
            GetMaxX,g.org_pix.y );
      Line( min_x +TextWidth('XXX'),g.min_pix.y+TextHeight('X') Div 2,
            min_x +TextWidth('XXX'),min_y);

      (* Dividir ejes.... *)
      upor_etqy := Int(TextHeight('XXX')/g.px_por_ph)+1;
      If upor_etqy < 1 Then upor_etqy := 1;

      long := TextWidth('X') DIV 2;
      For pix := 1 To 14 Do Begin
         Line( g.pix_actual.x+long, g.org_pix.y - pix*g.px_por_ph,
               g.pix_actual.x-long, g.org_pix.y - pix*g.px_por_ph);
         Line( g.pix_actual.x,
               g.org_pix.y - pix*g.px_por_ph + g.px_por_ph Div 2,
               g.pix_actual.x-long,
               g.org_pix.y - pix*g.px_por_ph + g.px_por_ph Div 2);
         If (pix+1) Mod Round(upor_etqy) = 0 Then
            OutTextXY( g.pix_actual.x-long-TextWidth('XX'),
                       g.org_pix.y - pix*g.px_por_ph -
                         TextHeight('X') Div 2,
                       Form('##',pix))
      End;
      count := Trunc( (g.min_pix.y - g.org_pix.y) / g.px_por_ph);
      For pix := 1 To count Do Begin
         Line( g.pix_actual.x+long, g.org_pix.y + pix*g.px_por_ph,
               g.pix_actual.x-long, g.org_pix.y + pix*g.px_por_ph);
         Line( g.pix_actual.x,
               g.org_pix.y + pix*g.px_por_ph - g.px_por_ph Div 2,
               g.pix_actual.x-long,
               g.org_pix.y + pix*g.px_por_ph - g.px_por_ph Div 2);
         If (pix+1) Mod Round(upor_etqy) = 0 Then
            OutTextXY( g.pix_actual.x-long-TextWidth('XX'),
                       g.org_pix.y +pix*g.px_por_ph -
                         TextHeight('X') Div 2,
                       Form('##',-pix))
      End;

      pixpml := Round((g.max_pix.x - g.pix_actual.x)/volb*10);
      upor_etqx := Int(TextWidth('XXXX') /pixpml)+1;
      If upor_etqx < 1 Then upor_etqx := 1;
      pix := 0;
      count := 0;
      volval := 0;
      Repeat
         If Count Mod 5 = 0 Then
           Line( g.pix_actual.x+pix,g.org_pix.y,
                 g.pix_actual.x+pix,g.org_pix.y+long * 3 Div 2)
         Else
           Line( g.pix_actual.x+pix,g.org_pix.y,
                 g.pix_actual.x+pix,g.org_pix.y+long);
         If ((Count + 2) Mod Round(upor_etqx) = 0 ) And
            ( Count > 0 ) Then Begin
           OutTextXY( g.pix_actual.x+pix-TextWidth(Form('###',volval)),
                      g.org_pix.y + long + 1,
                     Form('###',volval ) )
         End;
         pix := pix + pixpml;
         volval := volval + 10;
         Inc(Count)
      Until pix/pixpml >= volb;


  End; (*Iniciar_Grafica*)

  Procedure Iniciar_derivada( Var g : Tipo_grf );
  (*
   * Inicializa la parte del iterador que corresponde a la derivada
   * tras haber sido inicializada la parte correspondiente a la
   * grafica.
   *)
  Begin
     g.der_pix_actual := g.pix_actual
  End;


  Procedure Indicar_En_Grafica( hacerlo : Boolean;
                                ph : Real;
                                primero,seguir: Boolean;
                                Var g  : Tipo_grf );
  (*
   * Continua el dibujo de la grafica. Se entiende que el incremento
   * en las x corresponde a 1pix. o a 0pix. is seguir es False.
   * Primero se utiliza para no enlazar con el pixel anterior.
   * No hace nada si hacero es False.
   * Cuando no se sigue marca el pto. con un circulo
   *)

    Var c : Word;
  Begin
      If hacerlo Then Begin
         c := GetColor;
         SetColor( Color_curva );
         If primero Then
            g.pix_actual.y := g.org_pix.y-Round(g.px_por_ph*ph);
         MoveTo(g.pix_actual.x,g.pix_actual.y);
         If seguir Then
           Inc(g.pix_actual.x);
         g.pix_actual.y := g.org_pix.y-Round(g.px_por_ph*ph);
         LineTo(g.pix_actual.x,g.pix_actual.y);
         If Not seguir Then Begin(* eq *)
           SetColor(Color_Eq);
           PieSlice( g.pix_actual.x,g.pix_actual.y,0,360,
                     TextWidth('X') Div 2)
           End;
         SetColor(c)
      End
  End;

  Procedure Indicar_En_Derivada(hacerlo : Boolean;
                                ph : Real;
                                primero,seguir: Boolean;
                                Var g  : Tipo_grf );
  (*
   * Continua el dibujo de la derivada. Se entiende que el incremento
   * en las x corresponde a 1pix. o a 0pix. is seguir es False.
   * Primero se utiliza para no enlazar con el pixel anterior.
   * No hace nada si hacero es False.
   * Cuando no se sigue marca el pto. con un circulo
   *)

    Var c    : Word;
  Begin
      If hacerlo Then Begin
         c := GetColor;
         SetColor( Color_der );
         If primero Then Begin
           g.der_pix_actual.y := g.org_pix.y-Round(g.px_por_ph*ph);
           If g.der_pix_actual.y > g.min_pix.y Then
              g.der_pix_actual.y := g.min_pix.y;
           If g.der_pix_actual.y < g.max_pix.y Then
              g.der_pix_actual.y := g.max_pix.y;
         End;
         MoveTo(g.der_pix_actual.x,g.der_pix_actual.y);
         If seguir Then
           Inc(g.der_pix_actual.x);
         If g.org_pix.y - g.px_por_ph * ph > g.min_pix.y Then
            g.der_pix_actual.y := g.min_pix.y
         Else If g.org_pix.y - g.px_por_ph * ph < g.max_pix.y Then
            g.der_pix_actual.y := g.max_pix.y
         Else
            g.der_pix_actual.y := g.org_pix.y-Round(g.px_por_ph*ph);
         LineTo(g.der_pix_actual.x,g.der_pix_actual.y);
(*         If Not seguir Then (Equilibrio)
           PieSlice( g.der_pix_actual.x,g.der_pix_actual.y,0,360,
                     TextWidth('X') Div 2); *)
         SetColor(c)
      End
  End;

  Procedure Indicar_eq( hacerlo : Boolean;
                        Var g: Tipo_grf; vol,ph: Real );
  (*
   * Marca el pto. correspondiente a (vol,ph) indicando que es el
   * pto de eq. y los valores que le corresponden. ( si hacerlo )
   *)

    Var c: Word;
        x,y : Integer;
  Begin
      If hacerlo Then Begin
        c := GetColor;
        SetColor(Color_eq);
        x := g.org_pix.x+Round(vol/g.vol_por_px)+TextWidth('X');
        y := g.org_pix.y - Round(g.px_por_ph * ph) -
                  TextHeight('X') Div 2;
        OutTextXY(x,y,'<--pto.eq. pH');
        OutTextXY(x,y+TextHeight('H')+1,
                  ' ('+Form('####.##',vol)
                      + ','+Form('##.##',ph)+')');
        SetColor(c)
      End;
  End;


  Procedure Indicar_deq( hacerlo: Boolean;
                         Var  g: Tipo_grf;
                         vol,ph: Real             );
  (*
   * Marca el pto. correspondiente a (vol,ph) indicando que es el
   * pto max. de la der. y los valores que le corresponden. ( si hacerlo)
   *)

  Var c:Word;
      x,y:Integer;
  Begin
      If hacerlo Then Begin
        c := GetColor;
        SetColor( Color_der_eq );
        x :=g.org_pix.x+Round(vol/g.vol_por_px)
             -TextWidth('pto.max.dpH -->');
        If g.org_pix.y - g.px_por_ph * ph - TextHeight('X') Div 2 >
           g.min_pix.y Then
          y:= g.min_pix.y - 3 * TextHeight('X')
        Else If g.org_pix.y - g.px_por_ph * ph - TextHeight('X') Div 2  <
                g.max_pix.y Then
          y := g.max_pix.y
        Else
          y := g.org_pix.y - Round(g.px_por_ph * ph) - TextHeight('X') Div 2;
        OutTextXY(x,y,'pto.max.dpH -->');
        OutTextXY(x,y+TextHeight('H')+1,
                  ' ('+Form('####.##',vol)+','
                      +Form('##.##',ph)+')');
        SetColor(c)
      End;
  End;


  Procedure Marcar_pH( Var g: Tipo_grf; ph : Real; color: Tipo_color );
  (*
   * marca con una linea de color pH= ph en g.
   *)
   Var c : Tipo_color;
       s : LineSettingsType;
  Begin
    c := GetColor;
    GetLineSettings( s );
    SetColor(color);
    With s Do
      SetLineStyle( DashedLn, pattern, thickness );
    MoveTo( g.org_pix.x, g.org_pix.y - Round( g.px_por_ph * ph) );
    LineRel( g.max_pix.x - g.org_pix.x, 0 );
    With s Do
      SetLineStyle( lineStyle, pattern, thickness );
    SetColor(c)
  End;

  Procedure Marcar_vol( Var g: Tipo_grf; vol : Real; color: Tipo_color );
  (*
   * marca con una linea de color vol= vol en g.
   *)
   Var c : Tipo_color;
       s : LineSettingsType;
  Begin
    c := GetColor;
    GetLineSettings( s );
    SetColor(color);
    With s Do
      SetLineStyle( DottedLn, pattern, thickness );
    MoveTo( g.org_pix.x+ Round( vol / g.vol_por_px), g.min_pix.y );
    LineRel( 0, g.max_pix.y - g.min_pix.y  );
    With s Do
      SetLineStyle( lineStyle, pattern, thickness );
    SetColor(c)
  End;

  Procedure Linea( Var g: Tipo_grf; v1,ph1,v2,ph2 : Real;
                   color: Tipo_color; ls : Integer );
  (*
   * marca con una linea de color
   *)

   Var c : Tipo_color;
       s : LineSettingsType;
  Begin
    c := GetColor;
    GetLineSettings( s );
    SetColor(color);
    With s Do
      SetLineStyle( ls, pattern, thickness );
    MoveTo( g.org_pix.x + Round( v1 / g.vol_por_px),
            g.org_pix.y - Round( g.px_por_ph * ph1) );
    LineTo( g.org_pix.x + Round( v2 / g.vol_por_px),
            g.org_pix.y - Round( g.px_por_ph * ph2) );
    With s Do
      SetLineStyle( lineStyle, pattern, thickness );
    SetColor(c)
  End;

  Function Numero_ptos_g( Var g: Tipo_grf ): Integer;
  (*
   * Devuelve el No. de ptos. disponibles para el volumen en g.
   *)
  Begin
     Numero_ptos_g := g.max_pix.x - g.org_pix.x
  End;

Begin

End. (*GrfVal*)

