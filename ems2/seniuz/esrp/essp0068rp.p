/* Programa: ESSP0068RP.P (Chamado pelo programa ESSP0068.W)
** Objetivo: Imprimir a listagem do cadastro de Testes de PKUP 
**           (tabela: teste-pkup)
** Sistema.: Espec¡ficos
** M¢dulo..: Espec¡ficos
** Autor...: Fábio Coelho Lanza - Janeiro/2003
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0068RP 2.04.00.000}
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

define temp-table tt-param no-undo
       field destino            as integer
       field arquivo            as char format "x(35)"
       field usuario            as char format "x(12)"
       field data-exec          as date
       field hora-exec          as integer
       field classifica         as integer
       field desc-classifica    as char format "x(40)"
       FIELD fi-ini-cod-maq     LIKE teste-pkup.cod-maq
       FIELD fi-fin-cod-maq     LIKE teste-pkup.cod-maq
       FIELD fi-ini-data-teste  LIKE teste-pkup.data-teste
       FIELD fi-fin-data-teste  LIKE teste-pkup.data-teste.

define temp-table tt-raw-digita
       field raw-digita as raw.

/* recebimento de parƒmetros */
def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

/* defini‡Æo de frames do relat¢rio */

FORM 
   "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
   tt-param.desc-classifica    LABEL "Classific."    SKIP
   tt-param.fi-ini-cod-maq     label "Maquina..."
   "A"  AT 30
   tt-param.fi-fin-cod-maq     NO-LABELS             SKIP
   tt-param.fi-ini-data-teste  label "Data......"
   "A"  AT 30                  
   tt-param.fi-fin-data-teste  no-labels             SKIP
   skip(1)
   with FRAME f-param side-labels no-box width 132 STREAM-IO.

FORM   
   teste-pkup.cod-maq
   teste-pkup.data-teste
   teste-pkup.num-seq
   teste-pkup.pre-esqrd
   teste-pkup.pre-centro
   teste-pkup.pre-direita
   teste-pkup.res-esqrd
   teste-pkup.res-centro
   teste-pkup.res-direita
   teste-pkup.observ 
   with frame f-teste-pkup-fam WIDTH 132 down stream-io.

FORM
   teste-pkup.data-teste
   teste-pkup.cod-maq
   teste-pkup.num-seq
   teste-pkup.pre-esqrd
   teste-pkup.pre-centro
   teste-pkup.pre-direita
   teste-pkup.res-esqrd
   teste-pkup.res-centro
   teste-pkup.res-direita
   teste-pkup.observ 
   with frame f-teste-pkup-dat WIDTH 132 down stream-io.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i &STREAM="stream str-rp"}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i &STREAM="str-rp"}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESPECÖFICOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Cadastro_de_Testes_de_PKUP * r}
assign c-titulo-relat = trim(return-value).

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

display STREAM str-rp
        tt-param.desc-classifica
        tt-param.fi-ini-cod-maq              
        tt-param.fi-fin-cod-maq
        tt-param.fi-ini-data-teste                
        tt-param.fi-fin-data-teste                
        with frame f-param.           

if tt-param.classific = 1 then do:
   FOR each teste-pkup no-lock
      where teste-pkup.cod-maq     >= tt-param.fi-ini-cod-maq
        AND teste-pkup.cod-maq     <= tt-param.fi-fin-cod-maq
        AND teste-pkup.data-teste  >= tt-param.fi-ini-data-teste
        AND teste-pkup.data-teste  <= tt-param.fi-fin-data-teste
      on stop undo,leave:
      run pi-acompanhar in h-acomp (input string(teste-pkup.cod-maq)).

	  display stream str-rp
              teste-pkup.cod-maq
              teste-pkup.data-teste
              teste-pkup.num-seq
              teste-pkup.pre-esqrd
              teste-pkup.pre-centro
              teste-pkup.pre-direita
              teste-pkup.res-esqrd
              teste-pkup.res-centro
              teste-pkup.res-direita
              teste-pkup.observ 
              with frame f-teste-pkup-fam.
	 down stream str-rp with frame f-teste-pkup-fam.
   END.
end.
else do:
   FOR each teste-pkup no-lock
      where teste-pkup.cod-maq     >= tt-param.fi-ini-cod-maq
        AND teste-pkup.cod-maq     <= tt-param.fi-fin-cod-maq
        AND teste-pkup.data-teste  >= tt-param.fi-ini-data-teste
        AND teste-pkup.data-teste  <= tt-param.fi-fin-data-teste
      BY teste-pkup.data-teste
      BY teste-pkup.cod-maq
      BY teste-pkup.num-seq
      on stop undo,leave:
         run pi-acompanhar in h-acomp (input string(teste-pkup.data-teste)).
         display stream str-rp
                 teste-pkup.data-teste
                 teste-pkup.cod-maq
                 teste-pkup.num-seq
                 teste-pkup.pre-esqrd
                 teste-pkup.pre-centro
                 teste-pkup.pre-direita
                 teste-pkup.res-esqrd
                 teste-pkup.res-centro
                 teste-pkup.res-direita
                 teste-pkup.observ 
                 with frame f-teste-pkup-dat.
         down stream str-rp with frame f-teste-pkup-dat.
   END.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.
