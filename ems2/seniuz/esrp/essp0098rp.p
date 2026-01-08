/* Programa: ESSP0098RP.P (Chamado pelo programa ESSP0098.W)
** Objetivo: Imprimir a listagem do cadastro de Composi‡äes de Itens 
** Sistema.: Espec¡ficos
** M¢dulo..: Espec¡ficos
** Autor...: Gilvando Souza Araujo - Abril/2005
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0098RP 2.04.00.000}
/* defini‡Æo das temp-tables para recebimento de parƒmetros */

define temp-table tt-param no-undo
       field destino            as integer
       field arquivo            as char format "x(35)"
       field usuario            as char format "x(12)"
       field data-exec          as date
       field hora-exec          as integer
       field classifica         as integer
       field desc-classifica    as char format "x(40)"
       FIELD fi-ini-cod-composi LIKE composi.cod-composi
       FIELD fi-fin-cod-composi LIKE composi.cod-composi.

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
   "*-- Parƒmetros/Sele‡Æo --*" SKIP
   tt-param.desc-classifica    LABEL "Classific."    SKIP
   tt-param.fi-ini-cod-composi label "C¢digo...."
   "A"  AT 17
   tt-param.fi-fin-cod-composi NO-LABELS             SKIP
   skip(1)
   with FRAME f-param side-labels no-box width 132 STREAM-IO.

FORM   
   composi.cod-composi
   composi.descricao
   composi.descricao2
   composi.preimpresso FORMAT "Sim/NÆo"
   with frame f-detalhe WIDTH 132 down stream-io.

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
{utp/ut-liter.i Composi‡äes_de_Itens * r}
assign c-titulo-relat = trim(return-value).

view stream str-rp frame f-cabec.
view stream str-rp frame f-rodape.
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

display STREAM str-rp
        tt-param.desc-classifica
        tt-param.fi-ini-cod-composi              
        tt-param.fi-fin-cod-composi
        with frame f-param.           

FOR each composi no-lock
   where composi.cod-composi >= tt-param.fi-ini-cod-composi
     AND composi.cod-composi <= tt-param.fi-fin-cod-composi
   BY IF tt-param.classifica = 1 THEN composi.cod-composi
                                 ELSE composi.descricao:

   run pi-acompanhar in h-acomp (input string(composi.cod-composi)).

   display stream str-rp
           composi.cod-composi
           composi.descricao
           composi.descricao2
           composi.preimpresso
           with frame f-detalhe.
  down stream str-rp with frame f-detalhe.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i &STREAM="stream str-rp"}
run pi-finalizar in h-acomp.
return "OK":U.
