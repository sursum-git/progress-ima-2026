/* Programa: ESFT004.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Gerar o relatorio Os Maiores Clientes do Faturamento.
** Autor...: Sandro Wiest/Gilvando de Souza Araujo - Junho/95
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESFT004.P  =>  ESFT0008RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 21/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0008RP 2.04.00.000}

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
       FIELD ini-ge-codigo     LIKE ITEM.ge-codigo
       FIELD fin-ge-codigo     LIKE ITEM.ge-codigo
       FIELD dt-acumulo        LIKE nota-fiscal.dt-emis-nota
       FIELD inc-topazio       AS LOG FORMAT "Sim/NÆo"
       FIELD inc-prata         AS LOG FORMAT "Sim/NÆo"
       FIELD qtd-clientes      AS INT
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def TEMP-TABLE w-work
    field nome-abrev  like nota-fiscal.nome-ab-cli
    field quantidade  as dec format ">>>,>>>,>>9.99"
    field valor       as dec format ">>>,>>>,>>9.99"
    field quant-acum  as dec format ">>>,>>>,>>9.99"
    field valor-acum  as dec format ">>>,>>>,>>9.99"
    field nome-rep    like repres.nome-abrev
    FIELD dt-ult-nota LIKE nota-fiscal.dt-emis-nota
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

def var l-falta-fator as log.
def var de-quantidade as dec.
def var i-cont        as int.
def var i-contador    as int.
def var de-valor      as dec.
def var de-quant-acum as dec.
def var de-valor-acum as dec.
def var de-perc-1     as dec format ">>9.99".
def var de-perc-2     like de-perc-1.
def var de-perc-3     like de-perc-1.
def var de-perc-4     like de-perc-1.

form 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelec....." SKIP
    tt-param.ini-dt-emissao   label "Data Emissao.."
    "A"  AT 30
    tt-param.fin-dt-emissao   NO-LABELS SKIP
    tt-param.ini-cod-emitente label "Cliente......."
    "A"  AT 30
    tt-param.fin-cod-emitente no-labels SKIP
    tt-param.ini-ge-codigo    label "Grupo Estoque."
    "A"  AT 30
    tt-param.fin-ge-codigo    no-labels SKIP
    tt-param.ini-cod-rep      label "Representante."
    "A"  AT 30
    tt-param.fin-cod-rep      no-labels SKIP
    tt-param.dt-acumulo       LABEL "Acumul.Desde.." SKIP
    tt-param.inc-topazio      LABEL "Parcela Top..." SKIP
    tt-param.inc-prata        LABEL "Parcela Prata." SKIP
    tt-param.qtd-clientes     LABEL "Qtd.Clientes.."
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    emitente.cod-emit        label "Cliente" FORMAT ">>>>>>9"
    w-work.nome-abrev        label "Nome"
    w-work.nome-rep          label "Repres"
    w-work.quantidade        label "Qtdade"
    de-perc-1                label "Perc"
    w-work.valor             label "Valor"
    de-perc-2                label "Perc"
    w-work.quant-acum        label "Qtdade Acum"
    de-perc-3                label "Perc"
    w-work.valor-acum        label "Valor Acum"
    de-perc-4                label "Perc"
    w-work.dt-ult-nota       label "Ult.Nota"
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

VIEW FRAME f-cabec.
VIEW FRAME f-cabecalho.
VIEW FRAME f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each nota-fiscal 
    where nota-fiscal.cod-estabel  =  tt-param.cod-estabel
      and nota-fiscal.cod-emit     >= tt-param.ini-cod-emitente
      and nota-fiscal.cod-emit     <= tt-param.fin-cod-emitente
      and nota-fiscal.dt-emis-nota >= tt-param.dt-acumulo
      and nota-fiscal.dt-emis-nota <= tt-param.fin-dt-emissao
      and nota-fiscal.esp-docto    =  22 /*"nfs"*/
      and nota-fiscal.dt-cancela   =  ?
      and nota-fiscal.emite-duplic =  yes
    no-lock,
    each repres where repres.nome-abrev = nota-fiscal.no-ab-reppri
              and repres.cod-rep >= tt-param.ini-cod-rep
              and repres.cod-rep <= tt-param.fin-cod-rep
            no-lock:  

    run pi-acompanhar in h-acomp (input "Nota: " + nota-fiscal.nr-nota-fis +
                                        " " + STRING(nota-fiscal.dt-emis-nota)).

    find emitente 
         where emitente.cod-emitente = nota-fiscal.cod-emitente
         no-lock.
    find ped-venda 
         where ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
           and ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
         no-lock no-error.
    for each it-nota-fisc of nota-fiscal no-lock,
        EACH ITEM OF it-nota-fisc 
           WHERE ITEM.ge-codigo >= tt-param.ini-ge-codigo
             AND ITEM.ge-codigo <= tt-param.fin-ge-codigo:
        find first w-work
             where w-work.nome-abrev = emitente.nome-matriz
               and w-work.nome-rep   = nota-fiscal.no-ab-reppri
                       no-error.
        if not avail w-work then do:
           assign i-contador = i-contador + 1.

           run pi-acompanhar in h-acomp (input string(i-contador)).
            
           create w-work.
           assign w-work.nome-abrev  = emitente.nome-matriz
                  w-work.quantidade  = 0
                  w-work.valor       = 0
                  w-work.quant-acum  = 0
                  w-work.valor-acum  = 0
                  w-work.nome-rep    = nota-fiscal.no-ab-reppri
                  w-work.dt-ult-nota = nota-fiscal.dt-emis-nota.
        end.
        IF w-work.dt-ult-nota < nota-fiscal.dt-emis-nota THEN
           ASSIGN w-work.dt-ult-nota = nota-fiscal.dt-emis-nota.

        if it-nota-fisc.un-fatur[1] <> "m" then do:
           find item-ext
                where item-ext.it-codigo = item.it-codigo
                 no-lock no-error.
           if not avail item-ext then do:
              find first w-aux2
                   where w-aux2.it-codigo = it-nota-fisc.it-codigo
                   no-error.
              if not avail w-aux2 then do:
                 assign l-falta-fator = yes.
                 create w-aux2.
                 assign w-aux2.it-codigo = it-nota-fisc.it-codigo.
              end.
           end.
           else
              assign de-quantidade = it-nota-fisc.qt-faturada[1]
                                     * item-ext.fator-conv.
        end.
        else
           assign de-quantidade = it-nota-fisc.qt-faturada[1].
        
        if it-nota-fisc.dt-emis-nota >= tt-param.ini-dt-emissao then do:
           assign w-work.quantidade = w-work.quantidade +
                                      de-quantidade
                  w-work.valor      = w-work.valor +
                                      it-nota-fisc.vl-tot-item.
           IF AVAIL ped-venda THEN DO:
              if  tt-param.inc-topazio 
              and (ped-venda.tp-pedido = "t" or
                   ped-venda.tp-pedido = "w") then
                  assign w-work.valor = w-work.valor +
                                        it-nota-fisc.vl-tot-item. 
              if  tt-param.inc-prata 
              and ped-venda.tp-pedido = "p"  then
                  assign w-work.valor = w-work.valor +
                                        (it-nota-fisc.vl-tot-item / 0.70 * 0.30).
           END.
        end.                          
        if it-nota-fisc.dt-emis-nota >= tt-param.dt-acumulo then do:
           assign w-work.quant-acum = w-work.quant-acum +
                                      de-quantidade
                  w-work.valor-acum = w-work.valor-acum +
                                      it-nota-fisc.vl-tot-item.
           IF AVAIL ped-venda THEN DO:
              if  tt-param.inc-topazio 
              and (ped-venda.tp-pedido = "t" or 
                   ped-venda.tp-pedido = "w") then
                  assign w-work.valor-acum = w-work.valor-acum +
                                             it-nota-fisc.vl-tot-item.
              if  tt-param.inc-prata 
              and ped-venda.tp-pedido = "p"  then
                  assign w-work.valor-acum = w-work.valor-acum +
                                        (it-nota-fisc.vl-tot-item / 0.70 * 0.30).
           END.
        END.
    END.
END.

assign i-cont        = 0
       de-quantidade = 0
       de-valor      = 0
       de-quant-acum = 0
       de-valor-acum = 0
       i-contador    = i-contador + 1.

for each w-work by w-work.valor-acum descend:
    assign i-cont     = i-cont + 1
           i-contador = i-contador - 1.

    run pi-acompanhar in h-acomp (input string(i-contador)).

    IF i-cont > tt-param.qtd-clientes THEN
       DELETE w-work.
    ELSE
       ASSIGN de-quantidade = de-quantidade + w-work.quantidade
              de-valor      = de-valor      + w-work.valor
              de-quant-acum = de-quant-acum + w-work.quant-acum
              de-valor-acum = de-valor-acum + w-work.valor-acum.
end.

for each w-work by w-work.valor-acum descend:
    find first emitente where emitente.nome-abrev = w-work.nome-abrev
                        no-lock.
    assign de-perc-1 = (w-work.quantidade * 100) / de-quantidade
           de-perc-2 = (w-work.valor * 100) / de-valor
           de-perc-3 = (w-work.quant-acum * 100) / de-quant-acum
           de-perc-4 = (w-work.valor-acum * 100) / de-valor-acum.

    display emitente.cod-emit
            w-work.nome-abrev
            w-work.nome-rep
            w-work.quantidade
            de-perc-1
            w-work.valor
            de-perc-2
            w-work.quant-acum
            de-perc-3
            w-work.valor-acum
            de-perc-4
            w-work.dt-ult-nota
            with frame f-detalhe.
    down with frame f-detalhe.
end.

display "Totais"      @ w-work.nome-abrev
        de-quantidade @ w-work.quantidade
        de-valor      @ w-work.valor
        de-quant-acum @ w-work.quant-acum
        de-valor-acum @ w-work.valor-acum
        with frame f-detalhe.
down with frame f-detalhe.

if l-falta-fator = yes then do:
   assign l-falta-fator = no.
   page.
   put "Itens sem fator de conversao:" SKIP.
   for each w-aux2.
       put w-aux2.it-codigo SKIP.
       delete w-aux2.
   end.
end.

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.cod-estabel
           tt-param.ini-dt-emissao
           tt-param.fin-dt-emissao
           tt-param.ini-cod-emitente
           tt-param.fin-cod-emitente
           tt-param.ini-cod-rep
           tt-param.fin-cod-rep
           tt-param.ini-ge-codigo
           tt-param.fin-ge-codigo
           tt-param.dt-acumulo
           tt-param.inc-topazio
           tt-param.inc-prata
           tt-param.qtd-clientes
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

