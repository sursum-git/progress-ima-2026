/* Programa: ESSP0055RP.P (Chamado pelo programa ESSP0055.W)
** Objetivo: Imprimir a listagem do cadastro de Testes de Resistˆncia
**           (tabela: teste-resist)
** Sistema.: Espec¡ficos
** M¢dulo..: Espec¡ficos
** Autor...: Gilvando Souza Araujo - Novembro/2003
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0055RP 2.04.00.000}
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

define temp-table tt-param no-undo
       field destino            as integer
       field arquivo            as char format "x(35)"
       field usuario            as char format "x(12)"
       field data-exec          as date
       field hora-exec          as integer
       field classifica         as integer
       field desc-classifica    as char format "x(40)"
       FIELD fi-ini-fm-codigo   LIKE teste-resist.fm-codigo
       FIELD fi-fin-fm-codigo   LIKE teste-resist.fm-codigo
       FIELD fi-ini-cor         LIKE teste-resist.cor
       FIELD fi-fin-cor         LIKE teste-resist.cor
       FIELD fi-ini-data-teste  LIKE teste-resist.data-teste
       FIELD fi-fin-data-teste  LIKE teste-resist.data-teste
       FIELD fi-ini-cod-maq     LIKE teste-resist.cod-maq
       FIELD fi-fin-cod-maq     LIKE teste-resist.cod-maq.

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
   tt-param.fi-ini-fm-codigo   label "Familia..."
   "A"  AT 30
   tt-param.fi-fin-fm-codigo   NO-LABELS             SKIP
   tt-param.fi-ini-cor         LABEL "Cor......."
   "A"  AT 30
   tt-param.fi-fin-cor         NO-LABELS             SKIP
   tt-param.fi-ini-data-teste  label "Data......"
   "A"  AT 30                  
   tt-param.fi-fin-data-teste  no-labels             SKIP
   tt-param.fi-ini-cod-maq     label "Maquina..."
   "A"  AT 30                  
   tt-param.fi-fin-cod-maq     no-labels             SKIP
   skip(1)
   with FRAME f-param side-labels no-box width 132 STREAM-IO.

FORM   
   teste-resist.fm-codigo
   teste-resist.cor
   teste-resist.data-teste
   teste-resist.cod-maq
   teste-resist.num-seq
   teste-resist.nome-proc
   teste-resist.num-ob 
   teste-resist.res-esqrd
   teste-resist.res-centro
   teste-resist.res-direita
   teste-resist.observ 
   with frame f-teste-resist-fam WIDTH 132 down stream-io.

FORM
   teste-resist.data-teste
   teste-resist.fm-codigo
   teste-resist.cor
   teste-resist.cod-maq
   teste-resist.num-seq
   teste-resist.nome-proc
   teste-resist.num-ob
   teste-resist.res-esqrd
   teste-resist.res-centro
   teste-resist.res-direita
   teste-resist.observ 
   with frame f-teste-resist-dat WIDTH 132 down stream-io.

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
{utp/ut-liter.i Cadastro_de_Testes_de_Resistˆncia * r}
assign c-titulo-relat = trim(return-value).

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

display STREAM str-rp
        tt-param.desc-classifica
        tt-param.fi-ini-fm-codigo
        tt-param.fi-fin-fm-codigo
        tt-param.fi-ini-cor
        tt-param.fi-fin-cor
        tt-param.fi-ini-data-teste                
        tt-param.fi-fin-data-teste                
        tt-param.fi-ini-cod-maq              
        tt-param.fi-fin-cod-maq
        with frame f-param.           

if tt-param.classific = 1 then do:
   FOR each teste-resist no-lock
      where teste-resist.fm-codigo   >= tt-param.fi-ini-fm-codigo
        AND teste-resist.fm-codigo   <= tt-param.fi-fin-fm-codigo
        AND teste-resist.cor         >= tt-param.fi-ini-cor
        AND teste-resist.cor         <= tt-param.fi-fin-cor
        AND teste-resist.data-teste  >= tt-param.fi-ini-data-teste
        AND teste-resist.data-teste  <= tt-param.fi-fin-data-teste
        AND teste-resist.cod-maq     >= tt-param.fi-ini-cod-maq
        AND teste-resist.cod-maq     <= tt-param.fi-fin-cod-maq
      on stop undo,leave:
      run pi-acompanhar in h-acomp (input string(teste-resist.cod-maq)).

	  display stream str-rp
              teste-resist.fm-codigo
              teste-resist.cor
              teste-resist.data-teste
              teste-resist.cod-maq
              teste-resist.num-seq
              teste-resist.nome-proc
              teste-resist.num-ob
              teste-resist.res-esqrd
              teste-resist.res-centro
              teste-resist.res-direita
              teste-resist.observ 
              with frame f-teste-resist-fam.
	 down stream str-rp with frame f-teste-resist-fam.
   END.
end.
else do:
   FOR each teste-resist no-lock
      where teste-resist.fm-codigo   >= tt-param.fi-ini-fm-codigo
        AND teste-resist.fm-codigo   <= tt-param.fi-fin-fm-codigo
        AND teste-resist.cor         >= tt-param.fi-ini-cor
        AND teste-resist.cor         <= tt-param.fi-fin-cor
        AND teste-resist.data-teste  >= tt-param.fi-ini-data-teste
        AND teste-resist.data-teste  <= tt-param.fi-fin-data-teste
        AND teste-resist.cod-maq     >= tt-param.fi-ini-cod-maq
        AND teste-resist.cod-maq     <= tt-param.fi-fin-cod-maq
      BY teste-resist.data-teste
      BY teste-resist.fm-codigo
      BY teste-resist.cor
      BY teste-resist.cod-maq
      BY teste-resist.num-seq
      on stop undo,leave:
         run pi-acompanhar in h-acomp (input string(teste-resist.cod-maq)).
         display stream str-rp
                 teste-resist.data-teste
                 teste-resist.fm-codigo
                 teste-resist.cor
                 teste-resist.cod-maq
                 teste-resist.num-seq
                 teste-resist.nome-proc
                 teste-resist.num-ob
                 teste-resist.res-esqrd
                 teste-resist.res-centro
                 teste-resist.res-direita
                 teste-resist.observ 
                 with frame f-teste-resist-dat.
         down stream str-rp with frame f-teste-resist-dat.
   END.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.
