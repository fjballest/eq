Unit PhFun; (* Funciones de Ph *)

Interface

       Uses Fun_Mat; (* Funciones matematicas *)


       (*
        * Las funciones que siguen calculan el PH para cada tipo
        * de caso...
        * Se nombran Ph_xxxx_<cuando>
        *   xxxx es afbf bfaf adbf bdaf ( segun acido fuerte base fuerte, etc. )
        *   <cuando> es antes, durante, eq, despues
        *)

       (*
        * El orden de los parametros es siempre el mismo:
        *    <Vol. acido> <Norm.acido> <Cte. Acido> <Vol. Ac. ecc.>
        *      <Vol.base> <Norm.base> <Cte.Base> <VOl. base ecc.>
        *
        * para todas las funciones.
        *)

       Function Ph_afbf_antes( Na : Real ): Real;

       Function Ph_afbf_durante( Va,Na,Vb,Nb : Real ): Real;

       Function Ph_afbf_eq: Real;

       Function Ph_afbf_despues( Va,Vb,Nb,Vbecc : Real ): Real;

       Function Ph_bfaf_antes( N : Real ): Real;

       Function Ph_bfaf_durante( Va,Na,Vb,Nb : Real ): Real;

       Function Ph_bfaf_eq: Real;

       Function Ph_bfaf_despues( Va,Na,Vaecc,Vb : Real ): Real;

       Function Ph_adbf_antes( Na,Ka : Real ): Real;

       Function Ph_adbf_durante( Va,Na,Ka,Vb,Nb : Real ): Real;

       Function Ph_adbf_eq( Va,Na,Ka,Vb : Real ): Real;

       Function Ph_adbf_despues( Va,Vb,Nb,Vbecc : Real ): Real;

       Function Ph_bdaf_antes( Nb,Kb : Real ): Real;

       Function Ph_bdaf_durante( Va,Na,Vb,Nb,Kb : Real ): Real;

       Function Ph_bdaf_eq( Va,Vb,Nb,Kb : Real ): Real;

       Function Ph_bdaf_despues( Va,Na,Vaecc,Vb : Real ): Real;


Implementation


       (* ACIDO FUERTE - BASE FUERTE *)

       Function Ph_afbf_antes( Na : Real ): Real;
       (*
        * N -> normalidad.
        *)
       Begin
         Ph_afbf_antes := -1 * Log( Na )
       End;

       Function Ph_afbf_durante( Va,Na,Vb,Nb : Real ): Real;
       (*
        * N -> normalidad. V-> volumen.
        *)
       Begin
         Ph_afbf_durante :=
           Log( Va + Vb )- Log(Va*Na-Vb*Nb)
       End;

       Function Ph_afbf_eq: Real;
       (*
        * N -> normalidad.
        *)
       Begin
         Ph_afbf_eq := 7
       End;

       Function Ph_afbf_despues( Va,Vb,Nb,Vbecc : Real ): Real;
       (*
        * N -> normalidad.
        *)
       Begin
         Ph_afbf_despues :=
           14  - Log(Va+Vb+Vbecc) + Log(Vbecc) + Log(Nb)
       End;

       (* BASE FUERTE - ACIDO FUERTE *)

       Function Ph_bfaf_antes( N : Real ): Real;
       (*
        * N -> normalidad.
        *)
       Begin
         Ph_bfaf_antes := 14 + Log( N )
       End;

       Function Ph_bfaf_durante( Va,Na,Vb,Nb : Real ): Real;
       (*
        * N -> normalidad. V-> volumen.
        *)
       Begin
         Ph_bfaf_durante :=
           14 + Log( Vb*Nb - Va*Na ) - Log(Va+Vb)
       End;

       Function Ph_bfaf_eq: Real;
       (*
        * N -> normalidad.
        *)
       Begin
         Ph_bfaf_eq := 7
       End;

       Function Ph_bfaf_despues( Va,Na,Vaecc,Vb : Real ): Real;
       (*
        * N -> normalidad.
        *)
       Begin
         Ph_bfaf_despues :=
           Log( Va + Vb + Vaecc ) - Log( Vaecc ) - Log( Na )
       End;

       (* ACIDO DEBIL - BASE FUERTE *)

       Function Ph_adbf_antes( Na,Ka : Real ): Real;
       (*
        * N -> normalidad.
        *)
       Begin
         Ph_adbf_antes :=   (Log( Ka)+Log(Na) ) / ( -2 )
       End;

       Function Ph_adbf_durante( Va,Na,Ka,Vb,Nb : Real ): Real;
       (*
        * N -> normalidad.
        *)
       Begin
         Ph_adbf_durante :=
           Log(Vb)+Log(Nb)- Log( Ka ) - Log(Va*Na-Vb*Nb)
       End;

       Function Ph_adbf_eq( Va,Na,Ka,Vb : Real ): Real;
       (*
        * N -> normalidad.
        *)
       Begin
         Ph_adbf_eq :=
            7 + (Log(Na)+Log(Va))/2 - (Log(Ka)+Log(Va+Vb))/2
       End;

       Function Ph_adbf_despues( Va,Vb,Nb,Vbecc : Real ): Real;
       (*
        * N -> normalidad.
        *)
       Begin
         Ph_adbf_despues :=
           14 + Log(Vbecc) + Log(Nb) - Log(Va+Vb+Vbecc)
       End;

       (* BASE DEBIL - ACIDO FUERTE *)

       Function Ph_bdaf_antes( Nb,Kb : Real ): Real;
       (*
        * N -> normalidad.
        *)
       Begin
         Ph_bdaf_antes := 14 + (Log(Kb)+Log(Nb))/2
       End;

       Function Ph_bdaf_durante( Va,Na,Vb,Nb,Kb : Real ): Real;
       (*
        * N -> normalidad.
        *)
       Begin
         Ph_bdaf_durante :=
           14 + Log(Kb) + Log(Vb*Nb - Na*Va ) - Log(Va) - Log(Na)
       End;

       Function Ph_bdaf_eq( Va,Vb,Nb,Kb : Real ): Real;
       (*
        * N -> normalidad.
        *)
       Begin
         Ph_bdaf_eq :=
           7 + (Log(Kb)+Log(Va+Vb) - Log(Vb)- Log(Nb))/2
       End;

       Function Ph_bdaf_despues( Va,Na,Vaecc,Vb : Real ): Real;
       (*
        * N -> normalidad.
        *)
       Begin
         Ph_bdaf_despues :=
           Log(Va+Vb+Vaecc) - Log(Vaecc)  - Log(Na)
       End;

End.