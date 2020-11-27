(*
 * Fuente TPascal + ToolBox -- (c) 1991,1992  Esther Li#an y Nemo
 *
 * Modulo : [React] Reactivos: Acidos, bases e indicadores.
 * Rev    : 1.0
 * O.S.   : DOS
 *
 *
 *)


Unit React;

Interface (*AciBas*)

   Uses TpString, (* Tool-Box *)
        GrIO;     (* Colores  *)

   Type
        Tipo_nombre = String[80];
        Tipo_fuerza = ( Fuerte, Debil );

        Tipo_acido = Record
                       nombre : Tipo_nombre;
                       formula: Tipo_nombre;
                       fuerza : Tipo_fuerza;
                       n      : Real;           (* Normalidad *)
                       cte    : Real;           (* Cte. 1 para ac. fuerte *)
                     End;

        Tipo_base  = Tipo_acido; (* Los mismos datos *)

        Tipo_indicador = Record
                            min_vph, max_vph : Real;
                            color1, color2   : Tipo_nombre_color;
                            nombre           : Tipo_nombre
                         End;

   Const Ac_Nulo : Tipo_acido = ( nombre:'';
                                  formula: '';
                                  fuerza: Debil;
                                  n: 0;
                                  cte :0 );
         Ba_Nula : Tipo_base  = ( nombre:'';
                                  formula: '';
                                  fuerza: Debil;
                                  n: 0;
                                  cte :0 );

         Ind_Nulo: Tipo_indicador = ( min_vph:0;
                                      max_vph:0;
                                      color1: '';
                                      color2: '';
                                      nombre: '');

   Procedure Leer_ac( Var f: Text; Var ac: Tipo_acido );
   (*
    * Lee ac. de f.
    *)

   Procedure Leer_ba( Var f: Text; Var ba: Tipo_base );
   (*
    * Lee ba. de f.
    *)

   Procedure Leer_ind( Var f: Text; Var i: Tipo_indicador );
   (*
    * Lee i de f.
    *)

   Function  Nombre_fuerza( f : Tipo_fuerza ) : String;
   (*
    * Devuelve la cadena correspondiente a f.
    *)

Implementation (*React*)


    Procedure Read_name( Var f : Text; Var s: Tipo_Nombre );
    (*
     * Lee una cadena de caracteres distintos de blanco. Para tras ella.
     *)
        Var c : Char;
        Begin
           s[0] := char(0);
           c := #13;
           While (c In [#13,#32,#9,#10]) And (Not Eof(f) ) Do
              Read(f,c);
           While ( Not (c In [#13,#32,#9,#10])) And (Not Eof(f) ) Do Begin
              s[0]:=Char(Byte(s[0])+1);
              s[Byte(s[0])] := c;
              Read(f,c);
           End;
    End;


    Function  Nombre_fuerza( f : Tipo_fuerza ) : String;
    (*
     * Devuelve la cadena correspondiente a f.
     *)
    Begin
      If f = Fuerte Then
        Nombre_fuerza := '[Fuerte]'
      Else Nombre_fuerza := '[Debil]'
    End;

   Procedure Leer_ac( Var f: Text; Var ac: Tipo_acido );
   (*
    * Lee ac. de f.
    *)
     Var aux_n : Tipo_Nombre;
   Begin
      Read_name( f, ac.formula );
      Read_name( f, aux_n );
      Read( f,ac.cte);
      ReadLn( f, ac.nombre   );
      ac.nombre := Trim( ac.nombre );
      If aux_n = 'fuerte' Then
         ac.fuerza := Fuerte
      Else ac.fuerza := Debil;
      ac.n := 0
   End;(*Leer_ac*)

   Procedure Leer_ba( Var f: Text; Var ba: Tipo_base );
   (*
    * Lee ba. de f.
    *)
    Var aux_n : Tipo_Nombre;
   Begin
      Read_name( f, ba.formula );
      Read_name( f, aux_n );
      Read( f,ba.cte);
      ReadLn( f, ba.nombre   );
      ba.nombre := Trim( ba.nombre );
      If aux_n = 'fuerte' Then
         ba.fuerza := Fuerte
      Else ba.fuerza := Debil;
      ba.n := 0.0
   End;(*Leer_ba*)

   Procedure Leer_ind( Var f: Text; Var i: Tipo_indicador );
   (*
    * Lee i de f.
    *)
   Begin
      Read( f, i.min_vph, i.max_vph );
      Read_name( f, i.color1 );
      Read_name( f, i.color2 );
      ReadLn( f, i.nombre )
   End;(*Leer_ind*)

Begin
End.  (*React*)