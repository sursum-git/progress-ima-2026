/* Programa: ESPD008
** Sistema.: Magnus da Datasul
** Modulo..: Pedidos
** Objetivo: Relatorio Resumo do Faturamento
** Autor...: Sandro Wiest/Gilvando Souza Araujo
** Data....: 30/07/95
** Obs.....: Programa especifico de SANTA ELISABETH/RENASCENCA
**
** Conversao para EMS 2.04:
**   Programa: ESPD008.P  =>  ESFT0001RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 18/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0001RP 2.04.00.000}

define temp-table tt-param  no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD cod-estabel       like nota-fiscal.cod-estabel 
       FIELD ini-repres        LIKE nota-fiscal.no-ab-reppri 
       FIELD fin-repres        LIKE nota-fiscal.no-ab-reppri
       FIELD ini-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD fin-dt-emissao    LIKE nota-fiscal.dt-emis-nota
       FIELD ini-dt-precoant   LIKE nota-fiscal.dt-emis-nota
       FIELD fin-dt-precoant   LIKE nota-fiscal.dt-emis-nota
       FIELD ini-it-codigo     LIKE item.it-codigo
       FIELD fin-it-codigo     LIKE item.it-codigo
       FIELD ini-ge-codigo     LIKE ITEM.ge-codigo
       FIELD fin-ge-codigo     LIKE ITEM.ge-codigo
       FIELD all-types         AS LOG FORMAT "Sim/NÆo"
       FIELD tp-pedido1        AS CHAR FORMAT "x(2)"
       FIELD tp-pedido2        AS CHAR FORMAT "x(2)"
       FIELD tp-pedido3        AS CHAR FORMAT "x(2)"
       FIELD tp-pedido4        AS CHAR FORMAT "x(2)"
       FIELD tp-pedido5        AS CHAR FORMAT "x(2)"
       FIELD tp-normal         AS LOG FORMAT "Sim/NÆo"
       FIELD emite-dup         AS LOG FORMAT "Sim/NÆo"
       FIELD gerar-excel       AS LOG FORMAT "Sim/NÆo"
       FIELD qualidade         AS INTEGER
       FIELD desc-qualidade    AS CHAR FORMAT "x(10)"
       FIELD arq-excel         AS CHAR FORMAT "x(45)"
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

DEF TEMP-TABLE w-work
    field it-codigo       AS CHAR FORMAT "x(7)"
    field un              like item.un
    field quantidade      as dec format ">>>,>>>,>>9.99"
    field valor           as dec format ">>>,>>>,>>9.99"
    field quantidade-ant  as dec
    field qt-soma         as dec
    field qt-ant-soma     as dec
    FIELD quantidade-conv AS DEC
    field prazo           as int
    field valor-ant       as dec
    field brasil          as log
    FIELD indigo          AS LOG
    INDEX ch-work it-codigo
                  un
                  brasil.

DEF TEMP-TABLE w-work2
    field un             like item.un
    field quantidade     as dec
    field quantidade-ant as dec
    field valor-ant      as dec
    field brasil         as log
    INDEX ch-work2 un
                   brasil.

DEF TEMP-TABLE tt-work  /* Itens sem fator de conversao */
    field it-codigo like item.it-codigo
    INDEX ch-work it-codigo.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var de-valor-ant as dec.
def var de-valor-aux as dec.
def var de-quantidade-ant as dec.
def var de-tot-quantidade as dec.
DEF VAR de-qtd-tot-conv AS DEC.
def var de-tot-valor as dec.
def var de-tot-qtd-un as dec.
def var de-tot-qtd-ger as dec.
def var de-tot-valor-un as dec.
def var de-tot-valor-ger as dec.
def var de-tot-valor-ant as dec.
def var de-tot-quantidade-ant as dec.
def var l-brasil as log.
def var de-vl-total as dec.
def var de-quantidade as dec.
def var de-qt-conv as dec.
def var i-cont as int.
DEF VAR i-contador AS INT.
def var i-prazo as int format "->>9".
def var i-tot-prazo as DEC.
def var i-tot-prazo-un as DEC.
def var i-tot-prazo-ger as DEC.
def var de-perc-qtd as dec format "->>9.99".
def var de-preco-medio as dec format "->,>>9.99".
def var de-preco-medio-ant like de-preco-medio.
def var de-perc-medio  as dec format "->>9.99".
def var de-perc-valor  as dec format "->>9.99".
DEF VAR l-falta-fator  AS LOG.
DEF VAR i-lin-excel AS INTEGER INITIAL 1.

FORM 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelec....." SKIP
    tt-param.ini-repres       label "Representante."
    "A"  AT 34
    tt-param.fin-repres       NO-LABELS SKIP
    tt-param.ini-dt-emissao   label "Data Emissao.."
    "A"  AT 34
    tt-param.fin-dt-emissao   NO-LABELS SKIP
    tt-param.ini-dt-precoant  label "Data Pre‡o Ant"
    "A"  AT 34
    tt-param.fin-dt-precoant  no-labels SKIP
    tt-param.ini-it-codigo    label "Item.........."
    "A"  AT 34
    tt-param.fin-it-codigo    no-labels SKIP
    tt-param.ini-ge-codigo    label "Grupo Estoque."
    "A"  AT 34
    tt-param.fin-ge-codigo    no-labels SKIP
    tt-param.all-types        LABEL "Todos Tipos..."
    tt-param.tp-pedido1       NO-LABELS AT 22
    tt-param.tp-pedido2       NO-LABELS AT 25
    tt-param.tp-pedido3       NO-LABELS AT 28
    tt-param.tp-pedido4       NO-LABELS AT 31
    tt-param.tp-pedido5       NO-LABELS AT 34 
    tt-param.tp-normal        LABEL "Tipo Normal..." AT 1 SKIP
    tt-param.emite-dup        LABEL "Emite Duplic.." AT 1 SKIP
    tt-param.gerar-excel      LABEL "Gerar p/Excel." SKIP
    tt-param.arq-excel        LABEL "Arquivo Excel." SKIP
    tt-param.desc-qualidade   LABEL "Qualidade....." AT 1
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form  header
    "Item                                         Prazo                " at  1
    " % Qtd        Preco Var %                  % Valor                " at 67
    "Produto   Descricao                          Medio    Quantidade  " at  1
    "S/Unid Un     Medio S/M ant    Valor Total S/Total                " at 67
    "------------------------------------------------------------------" at  1
    "------------------------------------------------------------------" at 67
    with no-box page-top no-labels STREAM-IO frame f-cabecalho width 132 down.

form
    w-work.it-codigo
    ITEM.desc-item FORMAT "x(36)"
    i-prazo
    w-work.quantidade
    de-perc-qtd
    w-work.un
    de-preco-medio
    de-perc-medio
    w-work.valor
    de-perc-valor
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
{utp/ut-liter.i Listagem_Resumo_do_Faturamento * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW FRAME f-cabecalho.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

for each w-work.
    delete w-work.
end.

for each w-work2.
    delete w-work2.
end.

IF tt-param.gerar-excel THEN DO:
   {utp/utapi003.i}
   OS-DELETE VALUE(tt-param.arq-excel).
   CREATE tt-configuracao.
   ASSIGN tt-configuracao.versao-integracao   = 2
          tt-configuracao.arquivo-num         = 1
          tt-configuracao.arquivo             = tt-param.arq-excel
          tt-configuracao.total-planilha      = 1
          tt-configuracao.exibir-construcao   = NO 
          tt-configuracao.abrir-excel-termino = NO.

   CREATE tt-planilha.
   ASSIGN tt-planilha.arquivo-num       = 1
          tt-planilha.planilha-num      = 1
          tt-planilha.planilha-nome     = "Resumo"
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
          tt-dados.celula-valor  = "RESUMO DO FATURAMENTO"
          tt-dados.celula-fonte-tamanho = 14
          tt-dados.celula-fonte-negrito = YES.
   ASSIGN i-lin-excel = 5.
   
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 1
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Produto"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 2
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Descri‡Æo"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 3
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha   = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 1
          tt-dados.celula-valor   = "Prazo"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 4
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha   = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor   = "Quantidade"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 5
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "% s/Un"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 6
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 1
          tt-dados.celula-valor  = "Un"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 7
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Pre‡o Med"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 8
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "% Var"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 9
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor = "Valor Total"
          tt-dados.celula-fonte-negrito  = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 10
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor = "% s/Vlr.Total"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 11
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha   = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor   = "Quantid(M)"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   ASSIGN i-lin-excel = i-lin-excel + 2.
END.

for each item WHERE 
         item.it-codigo >= tt-param.ini-it-codigo AND
         item.it-codigo <= tt-param.fin-it-codigo AND
         item.ge-codigo >= tt-param.ini-ge-codigo AND
         item.ge-codigo <= tt-param.fin-ge-codigo NO-LOCK.

   assign i-prazo = 0.
       
   run pi-acompanhar in h-acomp (input item.it-codigo).
       
   for each it-nota-fisc
       where it-nota-fisc.cod-estabel  =  tt-param.cod-estabel
         and it-nota-fisc.it-codigo    =  item.it-codigo
         and it-nota-fisc.dt-emis-nota >= tt-param.ini-dt-emissao
         and it-nota-fisc.dt-emis-nota <= tt-param.fin-dt-emissao
         and it-nota-fisc.dt-cancela   =  ? 
       NO-LOCK,

       EACH nota-fiscal 
       where nota-fiscal.cod-estabel  =  it-nota-fisc.cod-estabel
         and nota-fiscal.nr-nota-fis  =  it-nota-fisc.nr-nota-fis
         and nota-fiscal.serie        =  it-nota-fisc.serie
         AND nota-fiscal.no-ab-reppri >= tt-param.ini-repres
         AND nota-fiscal.no-ab-reppri <= tt-param.fin-repres
         AND ((nota-fiscal.emite-duplic = YES AND tt-param.emite-dup = YES) OR
                                                  tt-param.emite-dup = NO)
         AND NOT (nota-fiscal.nat-operacao BEGINS "521" OR
                  nota-fiscal.nat-operacao BEGINS "522" OR
                  nota-fiscal.nat-operacao BEGINS "599FAB" OR
                  nota-fiscal.nat-operacao BEGINS "599F")
       NO-LOCK:

       find ped-venda 
          where ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
            and ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
          no-lock no-error.

       IF tt-param.all-types = NO THEN DO:
          IF ped-venda.tp-pedido <> tt-param.tp-pedido1 AND
             ped-venda.tp-pedido <> tt-param.tp-pedido2 AND
             ped-venda.tp-pedido <> tt-param.tp-pedido3 AND
             ped-venda.tp-pedido <> tt-param.tp-pedido4 AND
             ped-venda.tp-pedido <> tt-param.tp-pedido5 AND
             ped-venda.tp-pedido <> "" THEN NEXT.
          IF ped-venda.tp-pedido = "" AND tt-param.tp-normal = NO THEN NEXT.
       END.
       
       FIND ped-item WHERE ped-item.nome-abrev   = it-nota-fisc.nome-ab-cli
                       AND ped-item.nr-pedcli    = it-nota-fisc.nr-pedcli
                       AND ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped
                     NO-LOCK NO-ERROR.
       IF AVAIL ped-item THEN DO:
          FIND ped-item-ext OF ped-item NO-LOCK NO-ERROR.
          IF AVAIL ped-item-ext THEN
             IF (SUBSTR(ped-item-ext.lote,2,1) = "p" AND tt-param.qualidade = 2) OR
                (SUBSTR(ped-item-ext.lote,2,1) = "d" AND tt-param.qualidade = 1) THEN NEXT.
       END.

       FIND natur-oper 
            WHERE natur-oper.nat-operacao = it-nota-fisc.nat-operacao 
            NO-LOCK NO-ERROR.
       IF NOT AVAIL natur-oper THEN
          NEXT.
       IF natur-oper.emite-duplic = NO AND tt-param.emite-dup = YES THEN
          NEXT.
       /*find first emitente where emitente.nome-abrev = it-nota-fisc.nome-ab-cli*/
       find first emitente where emitente.cod-emitente = nota-fiscal.cod-emitente
                           no-lock.
       assign l-brasil = if  emitente.pais =  "brasil" then
                             yes
                         else no.
       find first w-work
            where w-work.it-codigo = item.it-codigo
              and w-work.un        = item.un
              and w-work.brasil    = l-brasil
            no-error.
       if not avail w-work then do:
          FIND item-ext WHERE item-ext.it-codigo = ITEM.it-codigo
                        NO-LOCK NO-ERROR.
          create w-work.
          assign w-work.quantidade      = 0
                 w-work.un              = item.un
                 w-work.valor           = 0
                 w-work.valor-ant       = 0
                 w-work.qt-soma         = 0
                 w-work.qt-ant-soma     = 0
                 w-work.quantidade-conv = 0
                 w-work.it-codigo       = item.it-codigo
                 w-work.brasil          = l-brasil
                 w-work.indigo          = IF (AVAIL item-ext AND
                                             item-ext.indigo = YES) THEN YES
                                          ELSE NO.
       end.
       if  it-nota-fisc.dt-emis-nota <= tt-param.ini-dt-precoant
       and it-nota-fisc.dt-emis-nota >= tt-param.fin-dt-precoant then do:
           assign w-work.valor-ant      = w-work.valor-ant + it-nota-fisc.vl-tot-item
                  w-work.quantidade-ant = w-work.quantidade-ant +
                                          it-nota-fisc.qt-faturada[1].

           /*------ Conversao de Kg para M ------- */
           IF it-nota-fisc.un-fatur[1] <> "m" THEN DO:
              FIND item-ext WHERE ITEM-ext.it-codigo = it-nota-fisc.it-codigo
                            NO-LOCK NO-ERROR.
              IF AVAIL item-ext AND item-ext.fator-conv <> 0 THEN
                 ASSIGN de-qt-conv = it-nota-fisc.qt-faturada[1] * item-ext.fator-conv.
              ELSE DO:
                 ASSIGN l-falta-fator = YES
                        de-qt-conv    = it-nota-fisc.qt-faturada[1].
                 FIND FIRST tt-work WHERE tt-work.it-codigo = it-nota-fisc.it-codigo
                                    NO-LOCK NO-ERROR.
                 IF NOT AVAIL tt-work THEN DO:
                    CREATE tt-work.
                    ASSIGN tt-work.it-codigo = it-nota-fisc.it-codigo.
                 END.
              END.   
           END.
           ELSE
              ASSIGN de-qt-conv = it-nota-fisc.qt-faturada[1].

           ASSIGN w-work.qt-ant-soma = w-work.qt-ant-soma + de-qt-conv.

           ASSIGN de-qtd-tot-conv = de-qtd-tot-conv + de-qt-conv.
       END.
       if it-nota-fisc.dt-emis-nota < tt-param.ini-dt-precoant then
           next.
       assign de-quantidade = it-nota-fisc.qt-faturada[1].
       assign i-prazo    = 0
              i-contador = 0.
       for each fat-duplic 
           where fat-duplic.cod-estab = it-nota-fisc.cod-estab
             and fat-duplic.serie     = it-nota-fisc.serie
             and fat-duplic.nr-fatura = it-nota-fisc.nr-nota-fis
           no-lock.
           assign i-contador = i-contador + 1.
       end.
       ASSIGN de-valor-aux = it-nota-fisc.vl-tot-item / i-contador.
       for each fat-duplic 
           where fat-duplic.cod-estab = it-nota-fisc.cod-estab
             and fat-duplic.serie     = it-nota-fisc.serie
             and fat-duplic.nr-fatura = it-nota-fisc.nr-nota-fis
           no-lock.
           assign i-prazo = i-prazo + ((fat-duplic.dt-vencimen -
                                        fat-duplic.dt-emissao) *
                                        de-valor-aux).
       end.
       assign w-work.prazo      = w-work.prazo + i-prazo
              w-work.quantidade = w-work.quantidade + de-quantidade
              w-work.valor      = w-work.valor + it-nota-fisc.vl-tot-item
              w-work.brasil = (if  emitente.pais = "brasil" THEN yes
                                                            else no).
       /*------ Conversao de Kg para M ------- */
       IF it-nota-fisc.un-fatur[1] <> "m" THEN DO:
          FIND item-ext WHERE ITEM-ext.it-codigo = it-nota-fisc.it-codigo
                        NO-LOCK NO-ERROR.
          IF AVAIL item-ext AND item-ext.fator-conv <> 0 then
             ASSIGN de-qt-conv = it-nota-fisc.qt-faturada[1] * item-ext.fator-conv.
          ELSE DO:
             ASSIGN l-falta-fator = YES
                    de-qt-conv    = it-nota-fisc.qt-faturada[1].
             FIND FIRST tt-work WHERE
                        tt-work.it-codigo = it-nota-fisc.it-codigo
                        NO-LOCK NO-ERROR.
             IF NOT AVAIL tt-work THEN DO:
                CREATE tt-work.
                ASSIGN tt-work.it-codigo = it-nota-fisc.it-codigo.
             END.
          END.   
       END.
       ELSE
          ASSIGN de-qt-conv = it-nota-fisc.qt-faturada[1].

       ASSIGN w-work.qt-soma = w-work.qt-soma + de-qt-conv
              w-work.quantidade-conv = w-work.quantidade-conv + de-qt-conv.

       ASSIGN de-qtd-tot-conv = de-qtd-tot-conv + de-qt-conv.
   end.
end.

for each w-work.
   find first w-work2 where w-work2.un     = w-work.un
                        and w-work2.brasil = w-work.brasil no-error.
   if not avail w-work2 then do:
      create w-work2.
      assign w-work2.un             = w-work.un
             w-work2.quantidade     = 0
             w-work2.quantidade-ant = 0
             w-work2.valor-ant      = 0
             w-work2.brasil         = w-work.brasil.
   end.
   assign w-work2.quantidade     = w-work2.quantidade + w-work.quantidade
          w-work2.quantidade-ant = w-work2.quantidade-ant + w-work.qt-ant-soma
          w-work2.valor-ant      = w-work2.valor-ant + w-work.valor-ant
          w-work2.brasil         = w-work.brasil.
end.

put " " skip(1)
   "V e n d a s  M e r c a d o  I n t e r n o" at 45 skip(1).

IF tt-param.gerar-excel THEN DO:
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Mercado Interno"
          tt-dados.celula-fonte-tamanho = 14.
   ASSIGN i-lin-excel = i-lin-excel + 2.
END.

{esinc/esft0001.i "yes" "no"}

put " " skip(2)
    "V e n d a s  M e r c a d o  E x t e r n o" at 45 skip(1).

IF tt-param.gerar-excel THEN DO:
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Mercado Externo"
          tt-dados.celula-fonte-tamanho = 14.
   ASSIGN i-lin-excel = i-lin-excel + 2.
END.

{esinc/esft0001.i "no" "no"}

put " " skip(2).

assign de-preco-medio = de-tot-valor / de-tot-quantidade.
if de-preco-medio = ? then
   assign de-preco-medio = 0.
assign de-preco-medio-ant = de-tot-valor-ant / de-tot-quantidade-ant.
if de-preco-medio-ant = ? then
   assign de-preco-medio-ant = 0.
assign de-perc-medio = ((de-preco-medio / de-preco-medio-ant) - 1) * 100.
if de-perc-medio = ? then
   assign de-perc-medio = 0.
assign i-prazo = i-tot-prazo / de-tot-valor.

display "Total Geral"     @ ITEM.desc-item
        i-prazo
        de-qtd-tot-conv   @ w-work.quantidade
        de-preco-medio
        de-perc-medio
        de-tot-valor      @ w-work.valor
        with frame f-detalhe.
down with frame f-detalhe.

IF tt-param.gerar-excel THEN DO:
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 2
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Total Geral".
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 3
          tt-dados.celula-linha   = i-lin-excel
          tt-dados.celula-formato = "##0"
          tt-dados.celula-alinhamento-horizontal = 1
          tt-dados.celula-valor   = string(i-prazo).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 4
          tt-dados.celula-linha   = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor   = string(de-qtd-tot-conv).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 7
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "##.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-preco-medio).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 8
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-perc-medio).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 9
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-tot-valor).
   ASSIGN i-lin-excel = i-lin-excel + 2.
END.

assign de-tot-quantidade     = 0
       i-tot-prazo           = 0
       de-tot-valor          = 0
       de-tot-valor-ant      = 0
       de-tot-quantidade-ant = 0.

put " " skip(2)
    "V e n d a s   d e   I n d i g o" at 50 skip(1).

IF tt-param.gerar-excel THEN DO:
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Vendas de Indigo"
          tt-dados.celula-fonte-tamanho = 14.
   ASSIGN i-lin-excel = i-lin-excel + 2.
   /*
   CREATE tt-grafico.
   ASSIGN tt-grafico.arquivo-num    = 1
          tt-grafico.planilha-num   = 1
          tt-grafico.grafico-nome   = "Vendas"
          tt-grafico.grafico-titulo = "Vendas por Produtos"
          tt-grafico.grafico-tipo   = 4
          tt-grafico.intervalo-linha-ini = 1
          tt-grafico.intervalo-linha-fin = i-lin-excel
          tt-grafico.intervalo-coluna-ini = 1
          tt-grafico.intervalo-coluna-fin = 10.
   */        
END.

{esinc/esft0001.i "no" "yes"}

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

if l-falta-fator then do:
   page.
   put "Atencao ! - Ha itens sem fator de conversao e sem peso l¡quido:"
       skip(1).
   for each tt-work:
       FIND ITEM WHERE ITEM.it-codigo = tt-work.it-codigo
                 NO-LOCK NO-ERROR.
       IF AVAIL ITEM THEN
          PUT tt-work.it-codigo
              ITEM.desc-item
              SKIP.
       ELSE
          PUT tt-work.it-codigo
              SKIP.
   end.
   for each tt-work.
       delete tt-work.
   end.
end.  

IF tt-param.imp-param THEN DO:
   PAGE.
   display tt-param.cod-estabel
           tt-param.ini-repres
           tt-param.fin-repres
           tt-param.ini-dt-emissao
           tt-param.fin-dt-emissao
           tt-param.ini-dt-precoant
           tt-param.fin-dt-precoant
           tt-param.ini-it-codigo
           tt-param.fin-it-codigo
           tt-param.ini-ge-codigo
           tt-param.fin-ge-codigo
           tt-param.all-types
           tt-param.tp-pedido1
           tt-param.tp-pedido2
           tt-param.tp-pedido3
           tt-param.tp-pedido4
           tt-param.tp-pedido5
           tt-param.tp-normal
           tt-param.emite-dup
           tt-param.gerar-excel
           tt-param.arq-excel  
           tt-param.desc-qualidade
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

