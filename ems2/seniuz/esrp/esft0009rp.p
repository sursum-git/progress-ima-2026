/* Programa: ESFT021.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Gerar o relatorio 100 Maiores Clientes do Faturamento/C.Receber
** Autor...: Gilvando de Souza Araujo - Setembro/96
**
** Conversao para EMS 2.04:
**   Programa: ESFT021.P  =>  ESFT0009RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 21/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0009RP 2.04.00.000}

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
       FIELD ini-cod-emitente  LIKE nota-fiscal.cod-emitente
       FIELD fin-cod-emitente  LIKE nota-fiscal.cod-emitente
       FIELD ini-cod-rep       LIKE nota-fiscal.cod-rep
       FIELD fin-cod-rep       LIKE nota-fiscal.cod-rep
       FIELD ini-dt-acumulo    LIKE nota-fiscal.dt-emis-nota
       FIELD inc-topazio       AS LOG FORMAT "Sim/NÆo"
       FIELD inc-prata         AS LOG FORMAT "Sim/NÆo"
       FIELD qtd-clientes      AS INT
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def TEMP-TABLE w-work
    field nome-abrev    like nota-fiscal.nome-ab-cli
    field quantidade    as dec format ">>>,>>>,>>9.99"
    field valor         as dec format ">>>,>>>,>>9.99"
    field quant-acum    as dec format ">>>,>>>,>>9.99"
    field valor-acum    as dec format ">>>,>>>,>>9.99"
    field valor-dup-abe AS DEC FORMAT ">>>,>>>,>>9.99"
    INDEX ch-work nome-abrev.

def TEMP-TABLE w-aux2
    field it-codigo like item.it-codigo
    INDEX ch-aux2 it-codigo.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var l-passou-aux2 as log.
def var l-prim-vez as log.
def var de-quantidade as dec.
def var i-cont as int.
def var i-contador as int.
def var de-valor as dec.
def var de-quant-acum as dec.
def var de-valor-acum as dec.
def var de-dupl-acum as dec.
def var de-saldo-cli as dec.
def var de-saldo as dec.
def var de-perc-1 as dec format ">>9.99".
def var de-perc-2 like de-perc-1.
def var de-perc-3 like de-perc-1.
def var de-perc-4 like de-perc-1.
def var de-perc-5 like de-perc-1.

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelec....." SKIP
    tt-param.ini-dt-emissao   label "Data Emissao.."
    "A"  AT 30
    tt-param.fin-dt-emissao   NO-LABELS SKIP
    tt-param.ini-cod-emitente label "Cliente......."
    "A"  AT 30
    tt-param.fin-cod-emitente no-labels SKIP
    tt-param.ini-cod-rep      label "Representante."
    "A"  AT 30
    tt-param.fin-cod-rep      no-labels SKIP
    tt-param.ini-dt-acumulo   LABEL "Acumul.Desde.." SKIP
    tt-param.inc-topazio      LABEL "Parcela Top..." SKIP
    tt-param.inc-prata        LABEL "Parcela Prata." SKIP
    tt-param.qtd-clientes     LABEL "Qtd.Clientes.."
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    emitente.cod-emit       LABEL "Cliente" format "999999"
    emitente.nome-emit      LABEL "Razao Social"  
    emitente.cgc            LABEL "CNPJ"    format "99.999.999/9999-99"
    emitente.cidade         LABEL "Cidade"
    emitente.estado         LABEL "UF"
    w-work.quantidade       label "QTD.FATURADA"   at 10
    de-perc-1               label "%QT.FAT"
    w-work.valor            label "VLR.FATURADO"
    de-perc-2               label "%VL.FAT"
    w-work.quant-acum       label "QT.FAT.ACUM"
    de-perc-3               label "%QT.ACM"
    w-work.valor-acum       label "VL.FAT.ACUM"
    de-perc-4               label "%VL.ACM"
    w-work.valor-dup-abe    label "DUPL.EM ABERTO"
    de-perc-5               label "%DP.ABE"
    with no-labels no-box 55 down width 132 STREAM-IO frame f-detalhe.

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
{utp/ut-liter.i Relatorio_Os_Maiores_Clientes_do_Faturamento * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW FRAME f-cabecalho.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH emitente where emitente.cod-emit >= tt-param.ini-cod-emitente
                    and emitente.cod-emit <= tt-param.fin-cod-emitente
                    and emitente.cod-rep  >= tt-param.ini-cod-rep
                    and emitente.cod-rep  <= tt-param.fin-cod-rep
                        no-lock:
    for each nota-fiscal where nota-fiscal.cod-estabel  =  tt-param.cod-estabel
                           AND nota-fiscal.cod-emit     =  emitente.cod-emit
                           and nota-fiscal.dt-emis-nota >= tt-param.ini-dt-acumulo
                           and nota-fiscal.dt-emis-nota <= tt-param.fin-dt-emissao
                           and nota-fiscal.dt-cancela   =  ?
                           and nota-fiscal.emite-duplic =  yes
                         no-lock:
        for each it-nota-fisc of nota-fiscal no-lock.
            find w-work
                 where w-work.nome-abrev = emitente.nome-matriz
                       no-error.
            if not avail w-work then do:
               assign i-contador = i-contador + 1.
               run pi-acompanhar in h-acomp (input string(i-contador)).
               create w-work.
               assign w-work.nome-abrev    = emitente.nome-matriz
                      w-work.quantidade    = 0
                      w-work.valor         = 0
                      w-work.quant-acum    = 0
                      w-work.valor-acum    = 0
                      w-work.valor-dup-abe = 0.
            end.
            if it-nota-fisc.un-fatur[1] <> "m" then do:
               find item where item.it-codigo = it-nota-fisc.it-codigo
                         no-lock no-error.
               find item-ext
                    where item-ext.it-codigo = item.it-codigo
                          no-lock no-error.
               if not avail item-ext then do:
                  find first w-aux2
                       where w-aux2.it-codigo = it-nota-fisc.it-codigo
                             no-error.
                  if not avail w-aux2 then do:
                     assign l-passou-aux2 = yes.
                     create w-aux2.
                     assign w-aux2.it-codigo = it-nota-fisc.it-codigo.
                  end.
               end.
               else
                  assign de-quantidade = it-nota-fisc.qt-faturada[1]
                                         * item-ext.fator-conv.
               next.
            end.
            else
               assign de-quantidade = it-nota-fisc.qt-faturada[1].

            if it-nota-fisc.dt-emis-nota >= tt-param.ini-dt-emissao then
               assign w-work.quantidade = w-work.quantidade +
                                          de-quantidade
                      w-work.valor      = w-work.valor +
                                          it-nota-fisc.vl-tot-item.
            if it-nota-fisc.dt-emis-nota >= tt-param.ini-dt-acumulo then
               assign w-work.quant-acum = w-work.quant-acum +
                                          de-quantidade
                      w-work.valor-acum = w-work.valor-acum +
                                          it-nota-fisc.vl-tot-item.
        end.
    end.
END.
ASSIGN i-cont        = 0        de-quantidade = 0
       de-valor      = 0        de-quant-acum = 0
       de-valor-acum = 0        de-dupl-acum  = 0
       i-contador = i-contador + 1.

for each w-work:
    assign i-cont     = i-cont + 1
           i-contador = i-contador - 1.
    run pi-acompanhar in h-acomp (input string(i-contador)).
    if i-cont > tt-param.qtd-clientes then do:
       delete w-work.
       next.
    end.

    find emitente where emitente.nome-abrev = w-work.nome-abrev
                  use-index nome
                  no-lock no-error.
    assign de-saldo-cli = 0.
    for each titulo where titulo.cod-emitente = emitente.cod-emitente
                          use-index emitente
                          no-lock:
        assign de-saldo-cli = de-saldo-cli + titulo.vl-saldo.
    end.
    if de-saldo-cli <> 0 then
       assign w-work.valor-dup-abe = de-saldo-cli.

    assign de-quantidade = de-quantidade + w-work.quantidade
           de-valor      = de-valor      + w-work.valor
           de-quant-acum = de-quant-acum + w-work.quant-acum
           de-valor-acum = de-valor-acum + w-work.valor-acum
           de-dupl-acum  = de-dupl-acum  + w-work.valor-dup-abe.
end.

for each w-work:
    find emitente where emitente.nome-abrev = w-work.nome-abrev
                        use-index nome
                        no-lock.
    assign de-perc-1 = (w-work.quantidade * 100) / de-quantidade
           de-perc-2 = (w-work.valor * 100) / de-valor
           de-perc-3 = (w-work.quant-acum * 100) / de-quant-acum
           de-perc-4 = (w-work.valor-acum * 100) / de-valor-acum
           de-perc-5 = (w-work.valor-dup-abe * 100) / de-dupl-acum.

    display emitente.cod-emit
            emitente.nome-emit
            emitente.cgc
            emitente.cidade
            emitente.estado
            w-work.quantidade
            de-perc-1
            w-work.valor
            de-perc-2
            w-work.quant-acum
            de-perc-3
            w-work.valor-acum
            de-perc-4
            w-work.valor-dup-abe
            de-perc-5
            with frame f-detalhe.
    down with frame f-detalhe.
    put skip(1).
end.

display "Totais"       @ emitente.cod-emit
        de-quantidade  @ w-work.quantidade
        de-valor       @ w-work.valor
        de-quant-acum  @ w-work.quant-acum
        de-valor-acum  @ w-work.valor-acum
        de-dupl-acum   @ w-work.valor-dup-abe
        with frame f-detalhe.
down with frame f-detalhe.

if l-passou-aux2 = yes then do:
   assign l-passou-aux2 = no.
   page.
   put "Itens sem fator de conversao:" SKIP.
   for each w-aux2.
       PUT w-aux2.it-codigo SKIP.
       delete w-aux2.
   end.
end.

IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.cod-estabel
           tt-param.ini-dt-emissao
           tt-param.fin-dt-emissao
           tt-param.ini-cod-emitente
           tt-param.fin-cod-emitente
           tt-param.ini-cod-rep
           tt-param.fin-cod-rep
           tt-param.ini-dt-acumulo
           tt-param.inc-topazio
           tt-param.inc-prata
           tt-param.qtd-clientes
           WITH FRAME f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

