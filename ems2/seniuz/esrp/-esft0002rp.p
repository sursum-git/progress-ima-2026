/* Programa: ESFT003.P
** Sistema.: Magnus da Datasul
** Modulo..: Faturamento
** Objetivo: Relatorio Resumo do Faturamento Por Natureza de Opera‡Æo
** Autor...: 
** Data....: 
** Obs.....: Programa especifico de SANTA ELISABETH/RENASCENCA
**
** Conversao para EMS 2.04:
**   Programa: ESFT0003.P  =>  ESFT0002RP.P
**   Autor...: F bio Coelho Lanza
**   Data....: 22/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0002RP 2.04.00.000}

define temp-table tt-param  no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD cod-estabel       like nota-fiscal.cod-estabel 
       FIELD ini-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD fin-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD ini-it-codigo     LIKE item.it-codigo
       FIELD fin-it-codigo     LIKE item.it-codigo
       FIELD ini-ge-codigo     LIKE ITEM.ge-codigo
       FIELD fin-ge-codigo     LIKE ITEM.ge-codigo
       FIELD emite-dup         AS LOG FORMAT "Sim/NÆo"
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

define TEMP-TABLE w-aux
       field un         like item.un
       field quantidade as dec format ">>>,>>>,>>9.99" label "Quantidade"
       INDEX ch-aux un.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var de-mercadoria     as dec    format ">>,>>>,>>9.99" label "Total Merc".
def var de-nota           as dec    format ">>,>>>,>>9.99" label "Valor da Nota".
def var de-icm            as dec    format ">>,>>>,>>9.99" label "Valor do ICM".
def var de-ipi            as dec    format ">>,>>>,>>9.99" label "Valor do IPI".
def var de-quantidade     as dec    format ">>,>>>,>>9.99" label "Quantidade".
def var de-mercadoria-ger as dec    format ">>>,>>>,>>9.99".
def var de-nota-ger       as dec    format ">>>,>>>,>>9.99".
def var de-icm-ger        as dec    format ">>>,>>>,>>9.99".
def var de-ipi-ger        as dec    format ">>>,>>>,>>9.99".

form 
    "*-------------- Parƒmetros/Sele‡Æo ---------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelec....."  SKIP
    tt-param.ini-dt-emissao   LABEL "Data Emissao.."
    "A"  AT 34
    tt-param.fin-dt-emissao   NO-LABELS SKIP
    tt-param.ini-it-codigo    LABEL "Item.........."
    "A"  AT 34
    tt-param.fin-it-codigo    NO-LABELS SKIP
    tt-param.ini-ge-codigo    LABEL "Grupo Estoque."
    "A"  AT 34
    tt-param.fin-ge-codigo    NO-LABELS SKIP
    tt-param.emite-dup        LABEL "Emite Duplic.." 
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    nota-fiscal.nat-operacao
    natur-oper.denominacao
    de-mercadoria
    de-nota
    de-icm
    de-ipi
    w-aux.quantidade
    w-aux.un
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

{utp/ut-liter.i FATURAMENTO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Resumo_do_Faturamento_Por_Natureza_de_Opera‡Æo * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each w-aux.
    delete w-aux.
end.

/* Inicia Processamento */ 
for each nota-fiscal where nota-fiscal.cod-estabel   = tt-param.cod-estabel
                       and nota-fiscal.dt-emis-nota >= tt-param.ini-dt-emissao
                       and nota-fiscal.dt-emis-nota <= tt-param.fin-dt-emissao
                       and nota-fiscal.dt-cancela   =  ? 
                       AND (nota-fiscal.emite-dup   = tt-param.emite-dup OR
                                                      tt-param.emite-dup = NO)
                       AND NOT (nota-fiscal.nat-operacao BEGINS "521" OR
                                nota-fiscal.nat-operacao BEGINS "522" OR
                                nota-fiscal.nat-operacao BEGINS "599FAB" OR
                                nota-fiscal.nat-operacao BEGINS "599F")
                     NO-LOCK
                     break by nota-fiscal.nat-operacao:
       
    run pi-acompanhar in h-acomp (input "Nota Fiscal: " + nota-fiscal.nr-nota-fis).

    for each it-nota-fisc of nota-fiscal no-lock,
        EACH ITEM OF it-nota-fisc
        WHERE ITEM.it-codigo >= tt-param.ini-it-codigo
          AND ITEM.it-codigo <= tt-param.fin-it-codigo
          AND ITEM.ge-codigo >= tt-param.ini-ge-codigo
          AND ITEM.ge-codigo <= tt-param.fin-ge-codigo
        NO-LOCK:

        assign de-icm     = de-icm     + it-nota-fis.vl-icms-it
               de-ipi     = de-ipi     + it-nota-fis.vl-ipi-it
               de-icm-ger = de-icm-ger + it-nota-fis.vl-icms-it
               de-ipi-ger = de-ipi-ger + it-nota-fis.vl-ipi-it.
        find first w-aux where w-aux.un = it-nota-fis.un-fatur[1]
                         no-error.
        if  not avail w-aux then do:
            create w-aux.
            assign w-aux.un = it-nota-fis.un-fatur[1]
                   w-aux.quantidade = 0.
        end.
        assign w-aux.quantidade  = w-aux.quantidade + it-nota-fis.qt-faturada[1]
               de-nota           = de-nota           + it-nota-fisc.vl-tot-item
               de-mercadoria     = de-mercadoria     + it-nota-fisc.vl-merc-liq
               de-nota-ger       = de-nota-ger       + it-nota-fisc.vl-tot-item
               de-mercadoria-ger = de-mercadoria-ger + it-nota-fisc.vl-merc-liq.
    END.

    if  last-of(nota-fiscal.nat-operacao) then do:
        find natur-oper where natur-oper.nat-operacao = nota-fiscal.nat-operacao
                        no-lock no-error.
        display nota-fiscal.nat-operacao
                natur-oper.denominacao when available natur-oper
                de-mercadoria
                de-nota
                de-icm
                de-ipi
                with frame f-detalhe.
        assign de-mercadoria = 0
               de-nota       = 0
               de-icm        = 0
               de-ipi        = 0.
        for each w-aux:
            display w-aux.un
                    w-aux.quantidade
                    with frame f-detalhe.
            down with frame f-detalhe.
            delete w-aux.
        end.
    end.
end.
display  "Total Geral:"    @ natur-oper.denominacao
         de-mercadoria-ger @ de-mercadoria
         de-nota-ger       @ de-nota
         de-icm-ger        @ de-icm
         de-ipi-ger        @ de-ipi
         with frame f-detalhe.
down with frame f-detalhe.
assign de-mercadoria-ger = 0
       de-nota-ger       = 0
       de-icm-ger        = 0
       de-ipi-ger        = 0.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.cod-estabel
           tt-param.ini-dt-emissao
           tt-param.fin-dt-emissao
           tt-param.ini-it-codigo 
           tt-param.fin-it-codigo 
           tt-param.ini-ge-codigo 
           tt-param.fin-ge-codigo 
           tt-param.emite-dup
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

