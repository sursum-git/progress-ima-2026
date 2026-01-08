/* Programa: ESFT010.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Emitir o relat¢iro Faturamento por Natureza de Opera‡Æo/Vencimento
** Autor...: Gilvando de Souza Araujo - Julho/95
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESFT010.P  =>  ESFT0007RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 20/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0007RP 2.04.00.000}

define temp-table tt-param  no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD cod-estabel       like nota-fiscal.cod-estabel
       FIELD serie             LIKE nota-fiscal.serie
       FIELD ini-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD fin-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var de-val-venc00  as dec format ">>>>>>,>>9.99".
def var de-val-venc30  as dec format ">>>>>>,>>9.99".
def var de-val-venc60  as dec format ">>>>>>,>>9.99".
def var de-val-venc90  as dec format ">>>>>>,>>9.99".
def var de-val-venc99  as dec format ">>>>>>,>>9.99".
def var de-val-total   as dec format ">>>,>>>,>>9.99".
def var de-tot-venc00  as dec format ">>>>>>,>>9.99".
def var de-tot-venc30  as dec format ">>>>>>,>>9.99".
def var de-tot-venc60  as dec format ">>>>>>,>>9.99".
def var de-tot-venc90  as dec format ">>>>>>,>>9.99".
def var de-tot-venc99  as dec format ">>>>>>,>>9.99".
def var de-tot-total   as dec format ">>>,>>>,>>9.99".

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelec....." SKIP
    tt-param.serie            LABEL "S‚rie da NF..." SKIP
    tt-param.ini-dt-emissao   label "Data Emissao.."
    "A"  AT 30
    tt-param.fin-dt-emissao   NO-LABELS SKIP
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    natur-oper.nat-operacao   label "Nat Op"
    natur-oper.denominacao    label "Denominacao"
    de-val-venc00             label "a Vista"
    de-val-venc30             label "01 a 30 Dias"
    de-val-venc60             label "31 a 60 Dias"
    de-val-venc90             label "61 a 90 Dias"
    de-val-venc99             label "Mais 90 Dias"
    de-val-total              label "Total"
    with no-box 55 down width 132 STREAM-IO frame f-detalhe no-label.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i FATURAMENTO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Faturamento_por_Natureza_de_Opera‡Æo/Vencimento * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel  =  tt-param.cod-estabel
                       AND nota-fiscal.serie        =  tt-param.serie
                       AND nota-fiscal.dt-emis-nota >= tt-param.ini-dt-emissao
                       AND nota-fiscal.dt-emis-nota <= tt-param.fin-dt-emissao
                       AND nota-fiscal.dt-cancela   = ?
                       AND nota-fiscal.emite-dup    = YES
                     NO-LOCK,
    EACH fat-duplic WHERE fat-duplic.cod-estabel = nota-fiscal.cod-estabel
                      AND fat-duplic.serie       = nota-fiscal.serie      
                      AND fat-duplic.nr-fatura   = nota-fiscal.nr-nota-fis
                    NO-LOCK
    BREAK BY fat-duplic.nat-operacao:

    run pi-acompanhar in h-acomp (input nota-fiscal.nr-nota-fis).

    if fat-duplic.dt-venciment = fat-duplic.dt-emissao then
       assign de-val-venc00 = de-val-venc00 + fat-duplic.vl-parcela.
    else
    if fat-duplic.dt-venciment - fat-duplic.dt-emissao <= 30 then
       assign de-val-venc30 = de-val-venc30 + fat-duplic.vl-parcela.
    else
    if fat-duplic.dt-venciment - fat-duplic.dt-emissao <= 60 then
       assign de-val-venc60 = de-val-venc60 + fat-duplic.vl-parcela.
    else
    if fat-duplic.dt-venciment - fat-duplic.dt-emissao <= 90 then
       assign de-val-venc90 = de-val-venc90 + fat-duplic.vl-parcela.
    else
       assign de-val-venc99 = de-val-venc99 + fat-duplic.vl-parcela.

    if  last-of(fat-duplic.nat-operacao) then do:
        find natur-oper
             where fat-duplic.nat-operacao = natur-oper.nat-operacao
             no-lock.
        assign de-val-total = de-val-venc00 +
                              de-val-venc30 +
                              de-val-venc60 +
                              de-val-venc90 +
                              de-val-venc99.
        display natur-oper.nat-operacao
                natur-oper.denominacao
                de-val-venc00
                de-val-venc30
                de-val-venc60
                de-val-venc90
                de-val-venc99
                de-val-total
                with frame f-detalhe.
        down with frame f-detalhe.
        assign de-tot-venc00 = de-tot-venc00 + de-val-venc00
               de-tot-venc30 = de-tot-venc30 + de-val-venc30
               de-tot-venc60 = de-tot-venc60 + de-val-venc60
               de-tot-venc90 = de-tot-venc90 + de-val-venc90
               de-tot-venc99 = de-tot-venc99 + de-val-venc99
               de-tot-total  = de-tot-total  + de-val-total.
        assign de-val-venc00 = 0
               de-val-venc30 = 0
               de-val-venc60 = 0
               de-val-venc90 = 0
               de-val-venc99 = 0
               de-val-total  = 0.
    end.
end.

display "Total Geral" @ natur-oper.nat-operacao
        de-tot-venc00 @ de-val-venc00
        de-tot-venc30 @ de-val-venc30
        de-tot-venc60 @ de-val-venc60
        de-tot-venc90 @ de-val-venc90
        de-tot-venc99 @ de-val-venc99
        de-tot-total  @ de-val-total
        with frame f-detalhe.
down with frame f-detalhe.
assign de-tot-venc00 = 0
       de-tot-venc30 = 0
       de-tot-venc60 = 0
       de-tot-venc90 = 0
       de-tot-venc99 = 0
       de-tot-total  = 0.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.cod-estabel
           tt-param.serie
           tt-param.ini-dt-emissao
           tt-param.fin-dt-emissao
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

