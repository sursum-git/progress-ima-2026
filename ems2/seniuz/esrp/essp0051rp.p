/* Programa: ESSP0051RP.P (Chamado pelo programa ESSP0051.W)
** Objetivo: Imprimir a listagem do cadastro de Testes de NIP 
**           (tabela: teste-nip)
** Sistema.: Espec¡ficos
** M¢dulo..: Espec¡ficos
** Autor...: Gilvando Souza Araujo - Novembro/2003
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0051RP 2.04.00.000}
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

define temp-table tt-param no-undo
       field destino            as integer
       field arquivo            as char format "x(35)"
       field usuario            as char format "x(12)"
       field data-exec          as date
       field hora-exec          as integer
       field classifica         as integer
       field desc-classifica    as char format "x(40)"
       FIELD fi-ini-cod-maq     LIKE teste-nip.cod-maq
       FIELD fi-fin-cod-maq     LIKE teste-nip.cod-maq
       FIELD fi-ini-num-foulard LIKE teste-nip.num-foulard
       FIELD fi-fin-num-foulard LIKE teste-nip.num-foulard
       FIELD fi-ini-data-teste  LIKE teste-nip.data-teste
       FIELD fi-fin-data-teste  LIKE teste-nip.data-teste.

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
   tt-param.fi-ini-num-foulard LABEL "Foulard..."
   "A"  AT 30
   tt-param.fi-fin-num-foulard NO-LABELS             SKIP
   tt-param.fi-ini-data-teste  label "Data......"
   "A"  AT 30                  
   tt-param.fi-fin-data-teste  no-labels             SKIP
   skip(1)
   with FRAME f-param side-labels no-box width 132 STREAM-IO.

FORM   
   teste-nip.cod-maq
   teste-nip.num-foulard
   teste-nip.data-teste
   teste-nip.num-seq
   teste-nip.pre-esqrd
   teste-nip.pre-centro
   teste-nip.pre-direita
   teste-nip.res-esqrd
   teste-nip.res-centro
   teste-nip.res-direita
   teste-nip.observ 
   with frame f-teste-nip-fam WIDTH 132 down stream-io.

FORM
   teste-nip.data-teste
   teste-nip.cod-maq
   teste-nip.num-foulard
   teste-nip.num-seq
   teste-nip.pre-esqrd
   teste-nip.pre-centro
   teste-nip.pre-direita
   teste-nip.res-esqrd
   teste-nip.res-centro
   teste-nip.res-direita
   teste-nip.observ 
   with frame f-teste-nip-dat WIDTH 132 down stream-io.

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
{utp/ut-liter.i Cadastro_de_Testes_de_NIP * r}
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
        tt-param.fi-ini-num-foulard
        tt-param.fi-fin-num-foulard
        tt-param.fi-ini-data-teste                
        tt-param.fi-fin-data-teste                
        with frame f-param.           

if tt-param.classific = 1 then do:
   FOR each teste-nip no-lock
      where teste-nip.cod-maq     >= tt-param.fi-ini-cod-maq
        AND teste-nip.cod-maq     <= tt-param.fi-fin-cod-maq
        AND teste-nip.num-foulard >= tt-param.fi-ini-num-foulard
        AND teste-nip.num-foulard <= tt-param.fi-fin-num-foulard
        AND teste-nip.data-teste  >= tt-param.fi-ini-data-teste
        AND teste-nip.data-teste  <= tt-param.fi-fin-data-teste
      BY teste-nip.cod-maq
      BY teste-nip.num-foulard
      BY teste-nip.data-teste
      BY teste-nip.num-seq
      on stop undo,leave:
      run pi-acompanhar in h-acomp (input string(teste-nip.cod-maq)).

	  display stream str-rp
              teste-nip.cod-maq
              teste-nip.num-foulard
              teste-nip.data-teste
              teste-nip.num-seq
              teste-nip.pre-esqrd
              teste-nip.pre-centro
              teste-nip.pre-direita
              teste-nip.res-esqrd
              teste-nip.res-centro
              teste-nip.res-direita
              teste-nip.observ 
              with frame f-teste-nip-fam.
	 down stream str-rp with frame f-teste-nip-fam.
   END.
end.
else do:
   FOR each teste-nip no-lock
      where teste-nip.cod-maq     >= tt-param.fi-ini-cod-maq
        AND teste-nip.cod-maq     <= tt-param.fi-fin-cod-maq
        AND teste-nip.num-foulard >= tt-param.fi-ini-num-foulard
        AND teste-nip.num-foulard <= tt-param.fi-fin-num-foulard
        AND teste-nip.data-teste  >= tt-param.fi-ini-data-teste
        AND teste-nip.data-teste  <= tt-param.fi-fin-data-teste
      BY teste-nip.data-teste
      BY teste-nip.num-seq
      BY teste-nip.cod-maq
      BY teste-nip.num-foulard
      on stop undo,leave:
         run pi-acompanhar in h-acomp (input string(teste-nip.cod-maq)).
         display stream str-rp
                 teste-nip.data-teste
                 teste-nip.cod-maq
                 teste-nip.num-foulard
                 teste-nip.num-seq
                 teste-nip.pre-esqrd
                 teste-nip.pre-centro
                 teste-nip.pre-direita
                 teste-nip.res-esqrd
                 teste-nip.res-centro
                 teste-nip.res-direita
                 teste-nip.observ 
                 with frame f-teste-nip-dat.
         down stream str-rp with frame f-teste-nip-dat.
   END.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.
