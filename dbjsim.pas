Unit DbjSim; {Dibujo  de la valoracion}


Interface


   Procedure Situar_Bureta( x1, y1, x2, y2 : Integer; vol:Real );
   Procedure Dibujar_Bureta( vol: Real; col: Word );
   Procedure Vaciar_Bureta( vol : Real);
   Procedure Situar_Matraz( x1, y1, x2, y2 : Integer;
                            vol:Real;
                            c1,c2 : Word );
   Procedure Dibujar_Matraz;
   Procedure Llenar_Matraz( vol : Real; estaAcido : Boolean);
   Procedure Goteo( c : Word; dly : LongInt );

Implementation


   Uses Graph,Crt;

   Type Tipo_Matraz = Record
           x1,y1,x2,y2: Integer;
           x_inf_izq,   x_inf_der    : Integer;
           y_inf_cuello,y_sup_cuello : Integer;
           x_izq_cuello,x_der_cuello : Integer;
           y_base,y_cuello           : Integer;
           volumen                   : Real;
           ult_vol                   : Integer;
           inc_vol                   : Real;
           c1,c2                     : Word
        End;
        Tipo_Bureta = Record
           x1,y1,x2,y2: Integer;
           x_inf_izq, x_inf_der      : Integer;
           x_izq_cuello, x_der_cuello: Integer;
           y_inf,y_sup               : Integer;
           y_vol_inf, y_vol_sup      : Integer;
           volumen                   : Real;
           inc_vol                   : Real
        End;

   Var matraz: Tipo_Matraz;
       bureta: Tipo_Bureta;


   Procedure Goteo( c : Word; dly : LongInt );
      Var viej : Word;
          y    : Integer;
   Begin
      viej := GetColor;
      y := bureta.y_inf;
      Repeat 
         SetColor( c );
         Line( (bureta.x2 - bureta.x1 ) Div 2 + bureta.x1, y,
               (bureta.x2 - bureta.x1 ) Div 2 + bureta.x1+1, y+1 );
         Line( (bureta.x2 - bureta.x1 ) Div 2 + bureta.x1-1, y,
               (bureta.x2 - bureta.x1 ) Div 2 + bureta.x1, y+1 );
         SetColor( Black );
         If dly > 0 Then
           Delay(5);
         Line( (bureta.x2 - bureta.x1 ) Div 2 + bureta.x1, y,
               (bureta.x2 - bureta.x1 ) Div 2 + bureta.x1+1, y+1 );
         Line( (bureta.x2 - bureta.x1 ) Div 2 + bureta.x1-1, y,
               (bureta.x2 - bureta.x1 ) Div 2 + bureta.x1, y+1 );
         If dly > 0 Then
           y := y + 1
         Else
           y := y + 3
      Until y > matraz.ult_vol;
      SetColor( viej )
   End;

   Procedure Situar_Bureta( x1, y1, x2, y2 : Integer; vol:Real );
       Var margen,margen_vert : Integer;
   Begin
       bureta.x1 := x1;
       bureta.x2 := x2;
       bureta.y1 := y1;
       bureta.y2 := y2;
       With bureta Do Begin
          volumen := vol;
          margen := ( x2 - x1 ) * 75 Div 100;
          { Debido a que el margen es del 75% resulta que la bureta esta
            "vista desde ATRAS" ( drcha->izqda y viceversa :- Sorry! }
          margen_vert := ( y2 - y1 ) * 10 Div 100;
          x_inf_izq := x1 + margen;
          x_inf_der := x2 - margen;
          y_inf     := y2 - margen_vert;
          y_sup     := y1 + margen_vert;
          y_vol_inf := y_inf - ( y_inf - y_sup ) * 15 Div 100;
          y_vol_sup := y_sup + ( y_inf - y_sup ) * 5 Div 100;
          x_izq_cuello:= x_inf_izq + ( x_inf_der - x_inf_izq ) Div 3;
          x_der_cuello:= x_inf_der - ( x_inf_der - x_inf_izq ) Div 3;
          inc_vol := volumen / (y_vol_inf-y_vol_sup)
       End
   End;

   Procedure Dibujar_Bureta( vol : Real; col:Word );
      Const Angulo = 5;
      Function Vol_a_altura( v : Real ) : Integer;
      Begin
           Vol_a_altura := Round( v / bureta.inc_vol )
      End;
      Var puntos : Array[1..8] Of PointType;
          sett   : FillSettingsType;
          colviejo: Word;
   Begin
       With bureta Do Begin
          Line( x_izq_cuello, y_inf, x_izq_cuello, y_vol_inf + Angulo);
          Line( x_izq_cuello, y_vol_inf + Angulo, x_inf_izq, y_vol_inf);
          Line( x_inf_izq, y_vol_inf, x_inf_izq, y_vol_sup );
          Line( x_der_cuello, y_inf, x_der_cuello, y_vol_inf + Angulo);
          Line( x_der_cuello, y_vol_inf + Angulo, x_inf_der, y_vol_inf);
          Line( x_inf_der, y_vol_inf, x_inf_der, y_vol_sup );
          Line( x_inf_der, y_vol_sup, x_der_cuello, y_vol_sup - Angulo );
          Line( x_inf_izq, y_vol_sup, x_izq_cuello, y_vol_sup - Angulo );
          Line( x_der_cuello, y_vol_sup - Angulo,
                x_der_cuello, y_sup - Angulo );
          Line( x_izq_cuello, y_vol_sup - Angulo,
                x_izq_cuello, y_sup - Angulo );
          MoveTo( x_izq_cuello, y_sup - Angulo );
          LineRel( Angulo, -Angulo);
          MoveTo( x_der_cuello, y_sup - Angulo );
          LineRel( -Angulo, -Angulo);
          Rectangle( x_der_cuello-Angulo, ( y_vol_inf + Angulo + y_inf ) Div 2,
                     x2,           ( y_vol_inf + y_inf ) Div 2 + Angulo );
          Rectangle( x2 - 2 * Angulo,   y_vol_inf + 3 * Angulo Div 2 ,
                     x2 - Angulo,  y_inf );

          puntos[1].x := x_izq_cuello;
          puntos[1].y := y_inf;
          puntos[2].x := x_izq_cuello;
          puntos[2].y := y_vol_inf + Angulo;
          puntos[3].x := x_inf_izq-1;
          puntos[3].y := y_vol_inf;
          puntos[4].x := x_inf_izq-1;
          puntos[4].y := y_vol_inf - Vol_a_altura( vol );
          puntos[5].x := x_inf_der+1;
          puntos[5].y := y_vol_inf - Vol_a_altura( vol );
          puntos[6].x := x_inf_der+1;
          puntos[6].y := y_vol_inf;
          puntos[7].x := x_der_cuello;
          puntos[7].y := y_vol_inf+Angulo;
          puntos[8].x := x_der_cuello;
          puntos[8].y := y_inf;
          GetFillSettings( sett );
          colviejo := GetColor;
          SetColor( col );
          SetFillStyle( CloseDotFill, col );
          FillPoly( 8, puntos );
          SetColor( colviejo );
          SetFillStyle( sett.pattern, sett.color )

       End;
   End;

   Procedure Vaciar_Bureta( vol : Real);
      Function Vol_a_altura( v : Real ) : Integer;
      Begin
           Vol_a_altura := Round( v / bureta.inc_vol )
      End;
       Var sett: FillSettingsType;
           viej: Word;
           puntos: Array[1..4] Of PointType;
   Begin
       viej:= GetColor;
       GetFillSettings( sett );
       SetColor( Black );
       SetFillStyle( SolidFill, Black );
       puntos[1].x := bureta.x_inf_izq-1;
       puntos[1].y := bureta.y_vol_sup;
       puntos[2].x := bureta.x_inf_izq-1;
       puntos[2].y := bureta.y_vol_inf - Vol_a_altura( bureta.volumen - vol );
       puntos[3].x := bureta.x_inf_der+1;
       puntos[3].y := bureta.y_vol_inf - Vol_a_altura( bureta.volumen - vol );
       puntos[4].x := bureta.x_inf_der+1;
       puntos[4].y := bureta.y_vol_sup;
       FillPoly( 4, puntos );
       SetFillStyle( sett.pattern, sett.color );
       SetColor( viej );
   End;


   Procedure Situar_Matraz( x1, y1, x2, y2 : Integer;
                            vol : Real;
                            c1,c2 : Word );
      Var margen_inf, margen_sup : Integer;
   Begin
     matraz.x1 := x1;
     matraz.x2 := x2;
     matraz.y1 := y1;
     matraz.y2 := y2;
     matraz.c1 := c1;
     matraz.c2 := c2;
     margen_inf:= (X2 - x1) * 10 Div 100;
     margen_sup:= (x2 - x1) * 30 Div 100;
     With matraz Do Begin
       x_inf_izq := x1 + margen_inf;
       x_inf_der := x2 - margen_inf;
       y_inf_cuello := y2 - (y2 - y1) * 70 Div 100;
       x_izq_cuello := x1 + margen_sup;
       x_der_cuello := x2 - margen_sup;
       y_base       := y2 - margen_inf;
       y_cuello     := y1 + margen_inf;
       volumen      := vol;
       inc_vol      := volumen / ( y_base - y_inf_cuello )
     End
   End;


   Procedure Dibujar_Matraz;
      Var sett : LineSettingsType;
   Begin
       GetLineSettings( sett );
       With matraz Do Begin
         SetLineStyle( SolidLn, 0, ThickWidth);
         Line( x_inf_izq, y_base, x_inf_der, y_base );
         Line( x_inf_izq, y_base, x_izq_cuello, y_inf_cuello);
         Line( x_izq_cuello,y_inf_cuello,
               x_izq_cuello, y_cuello+( y_inf_cuello - y_cuello) Div 2 );
         Line( x_izq_cuello, y_cuello+( y_inf_cuello - y_cuello) Div 2 ,
               x_inf_izq, y_cuello);
         Line( x_der_cuello, y_cuello, x_der_cuello, y_inf_cuello);
         Line( x_der_cuello, y_inf_cuello, x_inf_der, y_base );
         SetLineStyle( sett.LineStyle,sett.Pattern,sett.Thickness );
         ult_vol := matraz.y_base - 2;
       End
   End;

   Procedure Llenar_Matraz( vol : Real; estaAcido : Boolean );
      Function Vol_a_altura( v : Real ) : Integer;
      Begin
           Vol_a_altura := Round( v / matraz.inc_vol )
      End;

      Var puntos : Array[1..4] Of PointType;
          info   : FillSettingsType;
          colviejo,col: Word;
   Begin
      If estaAcido Then col := matraz.c1
                   Else col := matraz.c2;
      puntos[1].x := matraz.x_inf_izq;
      puntos[1].y := matraz.y_base;
      puntos[2].x := matraz.x_inf_der;
      puntos[2].y := matraz.y_base;
      puntos[3].y := matraz.y_base - Vol_a_altura( vol );
      puntos[4].y := puntos[3].y;
      puntos[3].x := Round(Vol_a_altura(vol) * ( matraz.x_inf_der - matraz.x_der_cuello )
                     / ( matraz.y_base - matraz.y_inf_cuello ));
      puntos[4].x := matraz.x_inf_izq + puntos[3].x;
      puntos[3].x := matraz.x_inf_der - puntos[3].x;
      matraz.ult_vol := puntos[3].y;
      GetFillSettings( info );
      colviejo:= GetColor;
      SetFillStyle( CloseDotFill, col );
      SetColor( col );
      FillPoly( 4, puntos );
      SetFillStyle( info.pattern, info.color );
      SetColor( colviejo)
   End;




Begin

End.