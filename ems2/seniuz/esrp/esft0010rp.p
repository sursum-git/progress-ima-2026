/* Programa: ESFT005.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Gerar o relatorio Ranking do Faturamento por Representante.
** Autor...: Sandro Wiest/Gilvando de Souza Araujo - Junho/95
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESFT0005.P  =>  ESFT0010RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 21/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0010RP 2.04.00.000}

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
       FIELD ini-it-codigo     LIKE item.it-codigo     
       FIELD fin-it-codigo     LIKE item.it-codigo     
       FIELD ini-ge-codigo     LIKE ITEM.ge-codigo     
       FIELD fin-ge-codigo     LIKE ITEM.ge-codigo     
       FIELD ini-dt-acumulo    LIKE nota-fiscal.dt-emis-nota
       FIELD gerar-excel       AS LOG FORMAT "Sim/NÆo"
       FIELD arq-excel         AS CHAR FORMAT "x(45)"
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

DEF TEMP-TABLE w-work
    field nome-abrev    like nota-fiscal.nome-ab-cli
    field quantidade    as dec format ">>>,>>>,>>9.99"
    field valor         as dec format ">>>,>>>,>>9.99"
    field quant-acum    as dec format ">>>,>>>,>>9.99"
    field valor-acum    as dec format ">>>,>>>,>>9.99"
    field valor-dup-abe AS DEC FORMAT ">>>,>>>,>>9.99"
    INDEX ch-valor IS PRIMARY valor-acum DESCENDING
    INDEX ch-repres IS UNIQUE nome-abrev.

DEF TEMP-TABLE tt-work-un
    FIELD unidade    LIKE ITEM.un
    FIELD quantidade AS DEC EXTENT 2 FORMAT ">>>,>>>,>>9.99"
    INDEX ch-work-un unidade.

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
def var de-valor as dec.
def var de-val as dec.
def var de-quant as dec.
def var de-quant-acum as dec.
def var de-valor-acum as dec.
def var de-perc-1 as dec format ">>9.99".
def var de-perc-2 like de-perc-1.
def var de-perc-3 like de-perc-1.
def var de-perc-4 like de-perc-1.
DEF VAR i-lin-excel AS INTEGER INITIAL 1.

form 
    "*-------------- Parƒmetros/Sele‡Æo ---------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelec....." SKIP
    tt-param.ini-dt-emissao   LABEL "Data Emissao.."
    "A"  AT 34
    tt-param.fin-dt-emissao   NO-LABELS SKIP
    tt-param.ini-cod-emitente LABEL "Cliente......."
    "A"  AT 34
    tt-param.fin-cod-emitente NO-LABELS SKIP
    tt-param.ini-cod-rep      LABEL "Representante."
    "A"  AT 34
    tt-param.fin-cod-rep      NO-LABELS SKIP
    tt-param.ini-it-codigo    LABEL "Item.........."
    "A"  AT 34
    tt-param.fin-it-codigo    NO-LABELS SKIP
    tt-param.ini-ge-codigo    LABEL "Grupo Estoque."
    "A"  AT 34
    tt-param.fin-ge-codigo    NO-LABELS SKIP
    tt-param.ini-dt-acumulo   LABEL "Acumul.Desde.." SKIP
    tt-param.gerar-excel      LABEL "Gerar Excel..." SKIP
    tt-param.arq-excel        LABEL "Arquivo Excel."
    skip(2)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    repres.cod-rep     label "Repres"
    w-work.nome-abrev  label "Nome"
    w-work.quantidade  label "Qtdade"      format ">>>>,>>>,>>9.99"
    de-perc-1          label "Perc"
    w-work.valor       label "Valor"       format ">>>>,>>>,>>9.99"
    de-perc-2          label "Perc"
    w-work.quant-acum  label "Qtdade Acum" format ">>>>,>>>,>>9.99"
    de-perc-3          label "Perc"
    w-work.valor-acum  label "Valor Acum"  format ">>>>,>>>,>>9.99"
    de-perc-4          label "Perc"
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
{utp/ut-liter.i Ranking_do_Faturamento_por_Representante * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW FRAME f-cabecalho.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

IF tt-param.gerar-excel THEN DO:
   {utp/utapi003.i}
   OS-DELETE VALUE(tt-param.arq-excel).
   CREATE tt-configuracao.
   ASSIGN tt-configuracao.versao-integracao   = 2
          tt-configuracao.arquivo-num         = 1
          tt-configuracao.arquivo             = tt-param.arq-excel
          tt-configuracao.total-planilha      = 1
          tt-configuracao.exibir-construcao   = yes 
          tt-configuracao.abrir-excel-termino = NO.

   CREATE tt-planilha.
   ASSIGN tt-planilha.arquivo-num       = 1
          tt-planilha.planilha-num      = 1
          tt-planilha.planilha-nome     = "Ranking"
          tt-planilha.linhas-grade      = NO
          tt-planilha.largura-coluna    = 12
          tt-planilha.formatar-planilha = YES.

   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = 1
          tt-dados.celula-valor  = empresa.razao-social
          tt-dados.celula-fonte-tamanho = 24
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor     = 3.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = 3
          tt-dados.celula-valor  = "RANKING DO FATURAMENTO"
          tt-dados.celula-fonte-tamanho = 14
          tt-dados.celula-fonte-negrito = YES.
   ASSIGN i-lin-excel = 5.
   
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 1
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Repres"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 2
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Nome"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 3
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha   = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor   = "Quantidade"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 4
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Perc"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 5
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Valor"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 6
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Perc"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 7
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Qtdade Acum"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 8
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Perc"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 9
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor = "Valor Acum"
          tt-dados.celula-fonte-negrito  = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 10
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor = "Perc"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   ASSIGN i-lin-excel = i-lin-excel + 1.
END.

FOR EACH repres WHERE 
         repres.cod-rep >= tt-param.ini-cod-rep AND
         repres.cod-rep <= tt-param.fin-cod-rep NO-LOCK:

    FOR each nota-fiscal where nota-fiscal.cod-estabel  =  tt-param.cod-estabel
                           and nota-fiscal.cod-rep      =  repres.cod-rep
                           and nota-fiscal.dt-emis-nota >= tt-param.ini-dt-acumulo
                           and nota-fiscal.dt-emis-nota <= tt-param.fin-dt-emissao
                           and nota-fiscal.dt-cancela   =  ?
                           and nota-fiscal.emite-duplic = yes
                         no-lock.

        for each it-nota-fisc of nota-fiscal 
            WHERE it-nota-fisc.it-codigo >= tt-param.ini-it-codigo
              AND it-nota-fisc.it-codigo <= tt-param.fin-it-codigo
            NO-LOCK,

            EACH ITEM OF it-nota-fisc
            WHERE ITEM.ge-codigo >= tt-param.ini-ge-codigo
              AND ITEM.ge-codigo <= tt-param.fin-ge-codigo
            NO-LOCK:

            find w-work where
                 w-work.nome-abrev = repres.nome-abrev no-error.

            if  not avail w-work then do.
                run pi-acompanhar in h-acomp (input "Repres.: " + string(repres.cod-rep)).
                create w-work.
                assign w-work.nome-abrev = repres.nome-abrev
                       w-work.quantidade = 0
                       w-work.valor      = 0
                       w-work.quant-acum = 0
                       w-work.valor-acum = 0.
            end.

            /*--- Acumula valores por unidade de medida ---*/
            FIND tt-work-un WHERE tt-work-un.unidade = it-nota-fisc.un-fatur[1] NO-LOCK NO-ERROR.
            IF NOT AVAIL tt-work-un THEN DO:
               CREATE tt-work-un.
               ASSIGN tt-work-un.unidade = it-nota-fisc.un-fatur[1].
            END.
            IF it-nota-fisc.dt-emis-nota >= tt-param.ini-dt-emissao THEN
               ASSIGN tt-work-un.quantidade[1] = tt-work-un.quantidade[1] + it-nota-fisc.qt-faturada[1].
            ASSIGN tt-work-un.quantidade[2] = tt-work-un.quantidade[2] + it-nota-fisc.qt-faturada[1].

            if it-nota-fisc.un-fatur[1] <> "m" then do:
               find item-ext where
                    item-ext.it-codigo = item.it-codigo no-lock no-error.
               if not avail item-ext OR item-ext.fator-conv = 0 then do:
                  assign de-quantidade = it-nota-fisc.qt-faturada[1].
                  find first w-aux2
                       where w-aux2.it-codigo = it-nota-fisc.it-codigo
                        no-error.
                  if not avail w-aux2 then do.
                      assign l-passou-aux2 = yes.
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

            if  it-nota-fisc.dt-emis-nota >= tt-param.ini-dt-emissao then
                assign w-work.quantidade = w-work.quantidade + de-quantidade
                       w-work.valor      = w-work.valor + it-nota-fisc.vl-tot-item
                       de-quant          = de-quant + de-quantidade
                       de-val            = de-val + it-nota-fisc.vl-tot-item.
            assign w-work.quant-acum = w-work.quant-acum + de-quantidade
                   w-work.valor-acum = w-work.valor-acum + it-nota-fisc.vl-tot-item
                   de-quant-acum     = de-quant-acum + de-quantidade
                   de-valor-acum     = de-valor-acum + it-nota-fisc.vl-tot-item.
        end.
    END.
END.

for each w-work:
    run pi-acompanhar in h-acomp (input "Repres.: " + w-work.nome-abrev).

    find repres where repres.nome-abrev = w-work.nome-abrev
                no-lock.
    assign de-perc-1 = (w-work.quantidade * 100) / de-quant
           de-perc-2 = (w-work.valor * 100) / de-val
           de-perc-3 = (w-work.quant-acum * 100) / de-quant-acum
           de-perc-4 = (w-work.valor-acum * 100) / de-valor-acum.
    DISPLAY repres.cod-rep
            w-work.nome-abrev
            w-work.quantidade
            de-perc-1
            w-work.valor
            de-perc-2
            w-work.quant-acum
            de-perc-3
            w-work.valor-acum
            de-perc-4
            with frame f-detalhe.
    down with frame f-detalhe.

    IF tt-param.gerar-excel THEN DO:
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 1
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-valor  = string(repres.cod-rep).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 2
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-valor  = w-work.nome-abrev.
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 1
              tt-dados.celula-coluna  = 3
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor   = string(w-work.quantidade).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 1
              tt-dados.celula-coluna  = 4
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-formato = "##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor   = string(de-perc-1).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 5
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(w-work.valor).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 6
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(de-perc-2).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 7
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(w-work.quant-acum).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 8
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(de-perc-3).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 9
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(w-work.valor-acum).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 10
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(de-perc-4).
       ASSIGN i-lin-excel = i-lin-excel + 1.
    END.
end.

DISPLAY "Totais"      @ w-work.nome-abrev
        de-quant      @ w-work.quantidade
        de-val        @ w-work.valor
        de-quant-acum @ w-work.quant-acum
        de-valor-acum @ w-work.valor-acum
        with frame f-detalhe.
DOWN 2 WITH FRAME f-detalhe.
FOR EACH tt-work-un:
    DISPLAY "   Em " + tt-work-un.unidade @ w-work.nome-abrev
            tt-work-un.quantidade[1]      @ w-work.quantidade
            tt-work-un.quantidade[2]      @ w-work.quant-acum
            WITH FRAME f-detalhe.
    DOWN WITH FRAME f-detalhe.
END.

IF tt-param.gerar-excel THEN DO:
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 2
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Totais".
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 3
          tt-dados.celula-linha   = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 1
          tt-dados.celula-valor   = string(de-quant).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 5
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-val).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 7
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###,###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-quant-acum).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 9
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-valor-acum).
   ASSIGN i-lin-excel = i-lin-excel + 1.

   CREATE tt-grafico.
   ASSIGN tt-grafico.arquivo-num    = 1
          tt-grafico.planilha-num   = 1
          tt-grafico.grafico-nome   = "Vendas"
          tt-grafico.grafico-titulo = "Ranking de Vendas por Representantes"
          tt-grafico.grafico-tipo   = 7
          tt-grafico.intervalo-linha-ini = 5
          tt-grafico.intervalo-linha-fin = i-lin-excel - 2
          tt-grafico.intervalo-coluna-ini = 1
          tt-grafico.intervalo-coluna-fin = 10.
END.

RUN utp/utapi003.p (INPUT-OUTPUT TABLE tt-configuracao,
                    INPUT-OUTPUT TABLE tt-planilha,
                    INPUT-OUTPUT TABLE tt-dados,
                    INPUT-OUTPUT TABLE tt-grafico,
                    INPUT-OUTPUT TABLE tt-erros).
/*
if return-value = "nok" then do: 
for each tt-erros: 
    DISPLAY tt-erros with 1 col width 500. 
end.
end.                  
*/


assign de-quant      = 0
       de-val        = 0
       de-quant-acum = 0
       de-valor-acum = 0.
down with frame f-detalhe.

if l-passou-aux2 = yes then do:
   assign l-passou-aux2 = no.
   page.
   put "Itens sem fator de conversao:" SKIP.
   for each w-aux2:
       PUT w-aux2.it-codigo SKIP.
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
           tt-param.ini-it-codigo 
           tt-param.fin-it-codigo 
           tt-param.ini-ge-codigo 
           tt-param.fin-ge-codigo 
           tt-param.ini-dt-acumulo
           tt-param.gerar-excel
           tt-param.arq-excel  
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

