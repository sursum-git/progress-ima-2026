/* Programa: ESFT044.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Consulta a Precos praticos por Item
** Autor...: Gilvando de Souza Araujo - Maio/2003
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
** Conversao para EMS 2.04:
**   Programa: ESFT044.P  =>  ESPD0018RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 25/03/2005
*/

/* include de controle de vers∆o */
{include/i-prgvrs.i ESPD0018RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino        as integer
       field arquivo        as char format "x(35)"
       field usuario        as char format "x(12)"
       field data-exec      as date
       field hora-exec      as integer
       field classifica     as integer
       FIELD grupo-ini      LIKE ITEM.ge-codigo
       FIELD grupo-fin      LIKE ITEM.ge-codigo
       FIELD item-ini       LIKE ITEM.it-codigo
       FIELD item-fin       LIKE ITEM.it-codigo
       FIELD refer-ini      LIKE ped-item.cod-refer
       FIELD refer-fin      LIKE ped-item.cod-refer
       FIELD desenho-ini    AS CHAR FORMAT "x(4)"
       FIELD desenho-fin    AS CHAR FORMAT "x(4)"
       FIELD cliente-ini    LIKE ped-venda.nome-abrev
       FIELD cliente-fin    LIKE ped-venda.nome-abrev
       FIELD repres-ini     LIKE ped-venda.no-ab-reppri 
       FIELD repres-fin     LIKE ped-venda.no-ab-reppri
       FIELD dt-entr-ini    LIKE ped-venda.dt-entrega   
       FIELD dt-entr-fin    LIKE ped-venda.dt-entrega   
       FIELD dt-impl-ini    LIKE ped-venda.dt-implant
       FIELD dt-impl-fin    LIKE ped-venda.dt-implant
       FIELD tipo-mercado   AS INTEGER
       FIELD desc-tipo-merc AS CHAR FORMAT "x(10)"
       FIELD cond-pagto     AS INTEGER
       FIELD desc-cond-pag  AS CHAR FORMAT "x(10)"
       FIELD all-types      AS LOGICAL FORMAT "Sim/N∆o"
       FIELD tp-pedido      AS CHAR FORMAT "x"
       FIELD tipo-rel       AS INTEGER
       FIELD desc-tipo-rel  AS CHAR FORMAT "x(10)"
       FIELD qualidade      AS INTEGER
       FIELD desc-qualidade AS CHAR FORMAT "x(10)"
       FIELD gerar-excel    AS LOG FORMAT "Sim/N∆o"
       FIELD arq-excel      AS CHAR FORMAT "x(45)"
       FIELD impr-param     AS LOGICAL.

define temp-table tt-digita no-undo
       field it-codigo  LIKE ped-item.it-codigo
       index id it-codigo.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

/* definiá∆o de vari†veis  */
def var h-acomp as handle no-undo.

def var de-acm-preco   as dec.
def var de-med-preco   as dec.
def var de-mai-preco   as dec.
def var de-men-preco   as dec.
def var i-cont         as int.
DEF VAR c-lista-item   AS CHAR.
DEF VAR c-desc-ref     LIKE referencia.descricao.
DEF VAR c-desc-cpag    LIKE cond-pag.descricao.
DEF VAR i-lin-excel AS INTEGER INITIAL 1.

FOR EACH tt-raw-digita.
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
    IF c-lista-item <> "" THEN
       ASSIGN c-lista-item = c-lista-item + ",".
    ASSIGN c-lista-item = c-lista-item + tt-digita.it-codigo.
END.

form
    tt-param.grupo-ini      LABEL "Grupo Estoque.."       
    "a"  AT 34                        
    tt-param.grupo-fin      NO-LABELS SKIP                 
    tt-param.item-ini       LABEL "Item..........."       
    "a"  AT 34                        
    tt-param.item-fin       NO-LABELS SKIP                 
    tt-param.refer-ini      LABEL "Referància....."       
    "a"  AT 34                             
    tt-param.refer-fin      NO-LABELS SKIP
    tt-param.desenho-ini    LABEL "Desenho........"
    "a"  AT 34
    tt-param.desenho-fin    NO-LABELS SKIP
    tt-param.cliente-ini    LABEL "Cliente........"       
    "a"  AT 34                        
    tt-param.cliente-fin    NO-LABELS SKIP                 
    tt-param.repres-ini     LABEL "Representante.." 
    "a"  AT 34  
    tt-param.repres-fin     NO-LABELS SKIP                          
    tt-param.dt-entr-ini    LABEL "Data Entrega..."           
    "a"  AT 34                                                
    tt-param.dt-entr-fin    NO-LABELS SKIP                    
    tt-param.dt-impl-ini    LABEL "Data Implant..."           
    "a"  AT 34                          
    tt-param.dt-impl-fin    NO-LABELS SKIP
    tt-param.desc-tipo-merc LABEL "Tipo Mercado..." SKIP
    tt-param.desc-cond-pag  LABEL "Cond.Pagamento." SKIP
    tt-param.all-types      LABEL "Todos os Tipos." SKIP
    tt-param.tp-pedido      LABEL "Tipos de Pedido" SKIP
    tt-param.desc-qualidade LABEL "Qualidade......" SKIP
    tt-param.desc-tipo-rel  LABEL "Tipo Relatorio." SKIP
    tt-param.gerar-excel    LABEL "Gerar Excel...." SKIP
    tt-param.arq-excel      LABEL "Arquivo Excel.." SKIP
    with no-box side-labels width 132 STREAM-IO frame f-parlis.

FORM
    "Item:"
    ITEM.it-codigo
    ITEM.desc-item
    WITH NO-LABEL NO-BOX 2 DOWN WIDTH 132 STREAM-IO FRAME f-item.

FORM HEADER
    "Referencia Desc Cliente      Pedido Repres       Implant       Preco   Quantid CPag Descricao                      TP" SKIP
    "---------- ---- ------------ ------ ------------ ---------- -------- --------- ---- ------------------------------ --"
    with page-top no-box width 132 STREAM-IO frame f-cab-detalhe.

form
    ped-item.cod-refer       FORMAT "XX XXXX X" AT   1
    referencia.descricao     FORMAT "x(3)"      AT  12
    ped-venda.nome-abrev                        AT  17
    ped-venda.nr-pedcli      format "x(6)"      AT  30
    ped-venda.no-ab-reppri                      AT  37
    ped-venda.dt-implant                        AT  50
    ped-item.vl-preori       format ">,>>9.99"  AT  61
    ped-item.qt-pedida       format ">>,>>9.99" AT  70
    ped-venda.cod-cond-pag                      AT  81
    cond-pagto.descricao                        AT  85
    ped-venda.tp-pedido                         AT 116
    with NO-LABEL no-box 55 down width 132 STREAM-IO frame f-detalhe.

/* include padr∆o para output de relat¢rios */
{include/i-rpout.i}

/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i PEDIDOS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Pedidos_de_Vendas_com_Preáos_por_Item * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW FRAME f-cab-detalhe.
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
          tt-planilha.planilha-nome     = "Itens"
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
          tt-dados.celula-valor  = "PEDIDOS DE VENDAS COM PREÄO POR ITEM"
          tt-dados.celula-fonte-tamanho = 14
          tt-dados.celula-fonte-negrito = YES.
   ASSIGN i-lin-excel = 5.
   
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 1
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Item"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 2
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Descriá∆o"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 3
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha   = i-lin-excel
          tt-dados.celula-valor   = "Referància"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 4
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Desc"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 5
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Cliente"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 6
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Pedido"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 7
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Representante"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 8
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Dt.Implant"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 9
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor = "Preáo"
          tt-dados.celula-fonte-negrito  = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 10
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor = "Quantidade"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 11
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha = i-lin-excel
          tt-dados.celula-valor = "CPag"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 12
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha = i-lin-excel
          tt-dados.celula-valor = "Descriá∆o"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 13
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha = i-lin-excel
          tt-dados.celula-valor = "TP"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   ASSIGN i-lin-excel = i-lin-excel + 1.
END.

for each ped-venda where ped-venda.dt-implant   >= tt-param.dt-impl-ini
                     and ped-venda.dt-implant   <= tt-param.dt-impl-fin
                     AND ped-venda.dt-entrega   >= tt-param.dt-entr-ini
                     AND ped-venda.dt-entrega   <= tt-param.dt-entr-fin
                     and ped-venda.nome-abrev   >= tt-param.cliente-ini
                     AND ped-venda.nome-abrev   <= tt-param.cliente-fin
                     and ped-venda.no-ab-reppri >= tt-param.repres-ini
                     AND ped-venda.no-ab-reppri <= tt-param.repres-fin
                     AND (ped-venda.tp-pedido    =  tt-param.tp-pedido OR
                                                    tt-param.all-types = YES)
                     AND (((ped-venda.cod-cond-pag = 0 OR ped-venda.cod-cond-pag > 3) AND 
                                                          tt-param.cond-pagto = 2) OR
                          ((ped-venda.cod-cond-pag > 0 AND ped-venda.cod-cond-pag < 4) AND 
                                                          tt-param.cond-pagto = 1) OR
                                                          tt-param.cond-pagto = 3)
                   no-lock,
    EACH ped-item OF ped-venda 
         WHERE ped-item.it-codigo             >= tt-param.item-ini
           AND ped-item.it-codigo             <= tt-param.item-fin
           AND ped-item.cod-refer             >= tt-param.refer-ini
           AND ped-item.cod-refer             <= tt-param.refer-fin
           AND SUBSTR(ped-item.cod-refer,3,4) >= tt-param.desenho-ini
           AND SUBSTR(ped-item.cod-refer,3,4) <= tt-param.desenho-fin
           AND (LOOKUP(ped-item.it-codigo,c-lista-item) <> 0 OR c-lista-item = "")
           AND ped-item.cod-sit-item <> 6 
         NO-LOCK,
    EACH ped-item-ext OF ped-item
         WHERE (substr(ped-item-ext.lote,2,1) = "p" AND tt-param.qualidade = 1) OR
               (substr(ped-item-ext.lote,2,1) = "d" AND tt-param.qualidade = 2) OR
                                                       (tt-param.qualidade = 3)
         NO-LOCK,
    EACH ITEM OF ped-item 
         WHERE ITEM.ge-codigo >= tt-param.grupo-ini
           AND ITEM.ge-codigo <= tt-param.grupo-fin
         NO-LOCK,
    EACH emitente WHERE emitente.cod-emitente = ped-venda.cod-emitente
                    AND ((emitente.natureza < 3 AND tt-param.tipo-mercado = 1) OR
                         (emitente.natureza > 2 AND tt-param.tipo-mercado = 2) OR
                                                    tt-param.tipo-mercado = 3)
                  NO-LOCK
    BREAK BY ped-item.it-codigo
          BY ped-item.cod-refer
          BY ped-item.vl-preori:

    run pi-acompanhar in h-acomp (input "Pedido: " + ped-venda.nr-pedcli).

    IF FIRST-OF(ped-item.it-codigo) THEN DO:
       DISPLAY ITEM.it-codigo
               ITEM.desc-item
               WITH FRAME f-item.
       DOWN 2 WITH FRAME f-item.
    END.

    IF tt-param.tipo-rel = 2 AND LAST-OF(ped-item.vl-preori) THEN DO:
       FIND referencia WHERE referencia.cod-refer = ped-item.cod-refer
                       NO-LOCK NO-ERROR.
       IF AVAIL referencia THEN
          ASSIGN c-desc-ref = referencia.descricao.
       ELSE
          ASSIGN c-desc-ref = "".
       FIND cond-pagto WHERE cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag
                       NO-LOCK NO-ERROR.
       IF AVAIL cond-pagto THEN
          ASSIGN c-desc-cpag = cond-pagto.descricao.
       ELSE
          ASSIGN c-desc-cpag = "".

       display ped-item.cod-refer
               referencia.descricao WHEN AVAIL referencia
               ped-venda.nome-abrev
               ped-venda.nr-pedcli
               ped-venda.no-ab-reppri
               ped-venda.dt-implant
               ped-item.vl-preori
               ped-item.qt-pedida
               ped-venda.cod-cond-pag
               cond-pagto.descricao WHEN AVAIL cond-pagto
               ped-venda.tp-pedido
               with frame f-detalhe.
       down with frame f-detalhe.

       IF tt-param.gerar-excel THEN DO:
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 1
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = item.it-codigo.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 2
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = item.descricao-1.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num    = 1
                 tt-dados.planilha-num   = 1
                 tt-dados.celula-coluna  = 3
                 tt-dados.celula-linha   = i-lin-excel
                 tt-dados.celula-valor   = ped-item.cod-refer.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num    = 1
                 tt-dados.planilha-num   = 1
                 tt-dados.celula-coluna  = 4
                 tt-dados.celula-linha   = i-lin-excel
                 tt-dados.celula-valor   = c-desc-ref.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 5
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = ped-venda.nome-abrev.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 6
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = ped-venda.nr-pedcli.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 7
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = ped-venda.no-ab-reppri.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 8
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = string(ped-venda.dt-implant).
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 9
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-formato = "###.##0,00"
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor  = string(ped-item.vl-preori).
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 10
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-formato = "###.##0,00"
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor  = string(ped-item.qt-pedida).
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 11
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = string(ped-venda.cod-cond-pag).
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 12
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = string(c-desc-cpag).
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 13
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = ped-venda.tp-pedido.
          ASSIGN i-lin-excel = i-lin-excel + 1.
       END.
    END.

    IF tt-param.tipo-rel = 2 AND LAST-OF(ped-item.cod-refer) THEN DO:
       DOWN 1 WITH FRAME f-detalhe.

       IF tt-param.gerar-excel THEN
          ASSIGN i-lin-excel = i-lin-excel + 1.
    END.

    if ped-item.vl-preori < de-men-preco or de-men-preco = 0 then
       assign de-men-preco = ped-item.vl-preori.
    if ped-item.vl-preori > de-mai-preco or de-mai-preco = 0 then
       assign de-mai-preco = ped-item.vl-preori.   
    assign de-acm-preco = de-acm-preco + ped-item.vl-preori
           i-cont       = i-cont + 1.
    if i-cont > 0 then
       assign de-med-preco = de-acm-preco / i-cont.
    else
       assign de-med-preco = 0.
    
    IF LAST-OF(ped-item.it-codigo) THEN DO:
       DISPLAY "Precos:"    @ no-ab-reppri
               "Menor"      @ dt-implant
               de-men-preco @ ped-item.vl-preori
               WITH FRAME f-detalhe.
       DOWN WITH FRAME f-detalhe.
       DISPLAY "Maior"      @ dt-implant
               de-mai-preco @ ped-item.vl-preori
               WITH FRAME f-detalhe.    
       DOWN WITH FRAME f-detalhe.
       DISPLAY "MÇdio"      @ dt-implant
               de-med-preco @ ped-item.vl-preori
               WITH FRAME f-detalhe.

       IF tt-param.gerar-excel THEN DO:
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 1
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = item.it-codigo.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 2
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = item.descricao-1.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 7
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = "Preáos:".
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 8
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = "Menor".
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 9
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-formato = "###.##0,00"
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor  = string(de-men-preco).
          ASSIGN i-lin-excel = i-lin-excel + 1.
          
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 8
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = "Maior".
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 9
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-formato = "###.##0,00"
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor  = string(de-mai-preco).
          ASSIGN i-lin-excel = i-lin-excel + 1.

          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 8
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-valor  = "Medio".
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 9
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-formato = "###.##0,00"
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor  = string(de-med-preco).
          ASSIGN i-lin-excel = i-lin-excel + 1.
       END.

       ASSIGN i-cont       = 0
              de-mai-preco = 0
              de-men-preco = 0
              de-acm-preco = 0.
    END.
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

IF tt-param.impr-param THEN DO:
   PAGE.
   PUT "****--------------- PAR∂METROS ----------------****".

   DISPLAY tt-param.grupo-ini       
           tt-param.grupo-fin       
           tt-param.item-ini       
           tt-param.item-fin       
           tt-param.refer-ini       
           tt-param.refer-fin       
           tt-param.desenho-ini    
           tt-param.desenho-fin    
           tt-param.cliente-ini     
           tt-param.cliente-fin     
           tt-param.repres-ini     
           tt-param.repres-fin     
           tt-param.dt-entr-ini    
           tt-param.dt-entr-fin    
           tt-param.dt-impl-ini    
           tt-param.dt-impl-fin    
           tt-param.desc-tipo-merc 
           tt-param.desc-cond-pag  
           tt-param.all-types      
           tt-param.tp-pedido
           tt-param.desc-qualidade
           tt-param.desc-tipo-rel
           tt-param.gerar-excel
           tt-param.arq-excel  
           WITH FRAME f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.


