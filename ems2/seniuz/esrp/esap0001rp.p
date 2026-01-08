/* Programa: ESAP001.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Contas a Pagar  
** Objetivo: Gerar o relatorio T¡tulos a Pagar por Grupo de Fornecedor
** Autor...: Sandro Wiest/Gilvando de Souza Araujo - Fevereiro/97
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL / SANTA ELISABETH
**
** Conversao para EMS 2.04:
**   Programa: ESAP001.P  =>  ESAP0013RP.P
**   Autor...: ProDB - Toninho
**   Data....: 12/02/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESAP0001RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino          as integer
       field arquivo          as char format "x(35)"
       field usuario          as char format "x(12)"
       field data-exec        as date
       field hora-exec        as integer
       FIELD ep-codigo        LIKE tit-ap.ep-codigo
       FIELD dt-corte         LIKE mov-ap.dt-trans
       FIELD gr-forn-ini      LIKE emitente.cod-gr-forn
       FIELD gr-forn-fim      LIKE emitente.cod-gr-forn
       FIELD cod-estabel-ini  LIKE tit-ap.cod-estabel
       FIELD cod-estabel-fim  LIKE tit-ap.cod-estabel
       FIELD cod-esp-ini      LIKE tit-ap.cod-esp
       FIELD cod-esp-fim      LIKE tit-ap.cod-esp 
       FIELD impr-param       AS LOGICAL.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var de-saldo as dec format "->>>,>>>,>>9.99" label "Saldo".
def var de-tot-dia as dec.
def var de-tot-grupo as dec.
def var de-tot-geral as dec.

form
    tt-param.ep-codigo       LABEL "Empresa"            AT 12
    "-" 
    c-empresa                NO-LABELS
    tt-param.dt-corte        LABEL "Data de Corte"      AT 6
    tt-param.gr-forn-ini     LABEL "Grupo Fornecedor"   AT 3
    "a"                                                 AT 33
    tt-param.gr-forn-fim     NO-LABELS 
    tt-param.cod-estabel-ini LABEL "Estabelecimento de" AT 1
    "a"                                                 AT 33
    tt-param.cod-estabel-fim NO-LABELS
    tt-param.cod-esp-ini     LABEL "Esp‚cie de"         AT 09
    "a"                                                 AT 33
    tt-param.cod-esp-fim     NO-LABELS 
    with no-box side-labels width 132 stream-io frame f-parlis.

form
    emitente.cod-gr-forn
    tit-ap.dt-vencimen
    tit-ap.cod-forn
    tit-ap.nome-abrev
    tit-ap.cod-est
    tit-ap.cod-esp
    tit-ap.serie
    tit-ap.nr-docto
    tit-ap.parcela
    tit-ap.portador
    tit-ap.referencia
    de-saldo
    with no-box 55 down width 132 STREAM-IO frame f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i FINANCEIRO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i T¡tulos_a_Pagar_por_Grupo_de_Fornecedor * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each emitente where
         emitente.cod-gr-forn >= tt-param.gr-forn-ini and
         emitente.cod-gr-forn <= tt-param.gr-forn-fim NO-LOCK,
    each tit-ap where
         tit-ap.ep-codigo   =  tt-param.ep-codigo       AND
         tit-ap.cod-estabel >= tt-param.cod-estabel-ini AND
         tit-ap.cod-estabel <= tt-param.cod-estabel-fim AND
         tit-ap.cod-esp     >= tt-param.cod-esp-ini     AND
         tit-ap.cod-esp     <= tt-param.cod-esp-fim     AND
         tit-ap.cod-fornec   = emitente.cod-emit no-lock
         break by emitente.cod-gr-forn
               by tit-ap.dt-vencimen:

    run pi-acompanhar in h-acomp (input emitente.cod-emit). 

    assign de-saldo = tit-ap.valor-saldo.

    for each mov-ap where mov-ap.ep-codigo  = tit-ap.ep-codigo
                      and mov-ap.cod-fornec = tit-ap.cod-fornec
                      and mov-ap.cod-estab  = tit-ap.cod-estab
                      and mov-ap.cod-esp    = tit-ap.cod-esp
                      and mov-ap.serie      = tit-ap.serie
                      and mov-ap.nr-docto   = tit-ap.nr-docto
                      and mov-ap.parcela    = tit-ap.parcela
                      and mov-ap.dt-trans   > tt-param.dt-corte no-lock.

         if mov-ap.transacao = 1 then
            assign de-saldo = de-saldo - mov-ap.valor-mov.
         else
         if  mov-ap.transacao = 2 then
             assign de-saldo = de-saldo  + mov-ap.valor-mov.
         else
         if  mov-ap.transacao = 5
         and mov-ap.lancamento = 1 then
             assign de-saldo = de-saldo - mov-ap.valor-mov.
         else
         if  mov-ap.transacao = 5
         and mov-ap.lancamento = 2 then
             assign de-saldo = de-saldo + mov-ap.valor-mov.
         if  mov-ap.transacao = 6
         and mov-ap.lancamento = 2 then
             assign de-saldo = de-saldo - mov-ap.valor-mov.
         if  mov-ap.transacao = 6
         and mov-ap.lancamento = 1 then
             assign de-saldo = de-saldo + mov-ap.valor-mov.
    end.

    if de-saldo > 0 then do:
       if tit-ap.tipo = 2 then
          assign de-saldo = de-saldo * -1.

       display emitente.cod-gr-forn
               tit-ap.dt-vencimen
               tit-ap.cod-forn
               tit-ap.nome-abrev
               tit-ap.cod-estab
               tit-ap.serie
               tit-ap.cod-esp
               tit-ap.nr-docto
               tit-ap.parcela
               tit-ap.portador
               tit-ap.referencia
               de-saldo
               with frame f-detalhe.

       down with frame f-detalhe.
       assign de-tot-dia   = de-tot-dia + de-saldo
              de-tot-grupo = de-tot-grupo + de-saldo
              de-tot-geral = de-tot-geral + de-saldo.
    end.
    
    if last-of(tit-ap.dt-vencimen) AND
       de-tot-dia <> 0 then do:
       display "Total Dia:" @ tit-ap.referencia
                de-tot-dia   @ de-saldo
                with frame f-detalhe.
       down with frame f-detalhe.
       assign de-tot-dia = 0.
    end.
    
    if last-of(emitente.cod-gr-forn) AND
       de-tot-grupo <> 0 then do:
       display "Total Grupo:" @ tit-ap.referencia
               de-tot-grupo @ de-saldo
               with frame f-detalhe.
       down with frame f-detalhe.
       assign de-tot-grupo = 0.
    end.
end.

display "Total Geral:" @ tit-ap.referencia
        de-tot-geral   @ de-saldo
        with frame f-detalhe.

down with frame f-detalhe.

assign de-tot-geral = 0.

IF tt-param.impr-param THEN DO:
   PAGE.

   find first empresa
        where empresa.ep-codigo = tt-param.ep-codigo no-lock no-error. 

   assign c-empresa = (if avail empresa then empresa.razao-social else "").

   PUT "*****----- PAR¶METROS -----*****"
       SKIP(1).

   display tt-param.ep-codigo
           c-empresa
           tt-param.dt-corte
           tt-param.gr-forn-ini
           tt-param.gr-forn-fim
           tt-param.cod-estabel-ini
           tt-param.cod-estabel-fim
           tt-param.cod-esp-ini    
           tt-param.cod-esp-fim    
           with frame f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.



