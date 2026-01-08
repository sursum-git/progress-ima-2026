/* Programa: ESFT025.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Listar o Relatorio Ranking do Faturamento por Representante/Estado.
** Autor...: Fabio Coelho Lanza - Abril/1998
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL
**
** Conversao para EMS 2.04:
**   Programa: ESFT025.P  =>  ESFT0019RP.P
**   Autor...: Gilvando Souza Araujo
**   Data....: 31/01/2005
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0019RP 2.04.00.000}

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
       FIELD all-types         AS LOG FORMAT "Sim/NÆo"
       FIELD tp-pedido1        AS CHAR FORMAT "x(2)"
       FIELD tp-pedido2        AS CHAR FORMAT "x(2)"
       FIELD tp-pedido3        AS CHAR FORMAT "x(2)"
       FIELD tp-pedido4        AS CHAR FORMAT "x(2)"
       FIELD tp-pedido5        AS CHAR FORMAT "x(2)"
       FIELD tp-normal         AS LOG FORMAT "Sim/NÆo"
       FIELD imp-val-adic      AS LOG
       FIELD gerar-excel       AS LOG FORMAT "Sim/NÆo"
       FIELD arq-excel         AS CHAR FORMAT "x(45)"
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

def temp-table tt-work
    field cod-rep    like nota-fiscal.cod-rep
    field uf         like repres.estado
    field qtd        as dec format ">>>,>>>,>>9.99"
    field valor      as dec format ">>>,>>>,>>9.99"
    field vlr-adic   as dec format ">>>,>>>,>>9.99".

def temp-table tt-work1
    field cod-rep    like nota-fiscal.cod-rep
    field qtd        as dec format ">>>,>>>,>>9.99"
    field valor      as dec format ">>>,>>>,>>9.99"
    field vlr-adic   as dec format ">>>,>>>,>>9.99".

def temp-table tt-work2    
    field uf         like repres.estado
    field qtd        as dec format ">>>,>>>,>>9.99"
    field valor      as dec format ">>>,>>>,>>9.99"
    field vlr-adic   as dec format ">>>,>>>,>>9.99".

def temp-table tt-work3
    field regiao     as char format "x(12)"
    field qtd        as dec format ">>>,>>>,>>9.99"
    field valor      as dec format ">>>,>>>,>>9.99"
    field vlr-adic   as dec format ">>>,>>>,>>9.99".
    
create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

def var de-perc1    as dec format ">>9.99".
def var de-perc2    as dec format ">>9.99". 
def var de-qtde     as dec format ">>>,>>>,>>9.99".
def var de-valor    as dec format ">>>,>>>,>>>,>>9.99".
def var de-vlr-adic as dec format ">>>,>>>,>>>,>>9.99".
def var de-vlr-tot  as dec format ">>>,>>>,>>>,>>9.99".
def var de-tot-qtd  as dec format ">>>,>>>,>>9.99".
def var de-tot-vlr  as dec format ">>>,>>>,>>>,>>9.99".
def var de-tot-adic as dec format ">>>,>>>,>>>,>>9.99".
def var de-tot-ger  as dec format ">>>,>>>,>>>,>>9.99".
def var c-regiao    as char format "x(12)".

DEF VAR i-lin-excel AS INTEGER INITIAL 1.

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
    tt-param.all-types        LABEL "Todos Tipos..."
    tt-param.tp-pedido1       NO-LABELS AT 22
    tt-param.tp-pedido2       NO-LABELS AT 25
    tt-param.tp-pedido3       NO-LABELS AT 28
    tt-param.tp-pedido4       NO-LABELS AT 31
    tt-param.tp-pedido5       NO-LABELS AT 34 SKIP
    tt-param.tp-normal        LABEL "Tipo Normal..." SKIP
    tt-param.gerar-excel      LABEL "Gerar Excel..." SKIP
    tt-param.arq-excel        LABEL "Arquivo Excel." SKIP(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    tt-work.cod-rep   label "Cod"
    repres.nome       label "Nome do Representante"
    tt-work.uf        label "UF"
    tt-work.qtd       label "Quantidade"
    de-perc1          label "Perc"
    tt-work.valor     label "Valor"
    de-perc2          label "Perc"
    tt-work.vlr-adic  label "Vlr.Adicional"
    de-vlr-tot        label "Valor Total"
    with no-labels no-box 55 down width 133 STREAM-IO frame f-detalhe.

form
    tt-work1.cod-rep  label "Cod"
    repres.nome       label "Nome do Representante"
    tt-work1.qtd      label "Quantidade"
    de-perc1          label "Perc"
    tt-work1.valor    label "Valor"
    de-perc2          label "Perc"
    tt-work1.vlr-adic label "Vlr.Adicional"
    de-vlr-tot        label "Valor Total"
    with no-labels no-box 55 down width 133 STREAM-IO frame f-detalhe1.

form
    tt-work2.uf       label "Estado"
    tt-work2.qtd      label "Quantidade"
    de-perc1          label "Perc"
    tt-work2.valor    label "Valor"
    de-perc2          label "Perc"
    tt-work2.vlr-adic label "Vlr.Adicional"
    de-vlr-tot        label "Valor Total"
    with no-labels no-box 55 down width 133 STREAM-IO frame f-detalhe2.

form
    tt-work3.regiao   label "Regiao"
    tt-work3.qtd      label "Quantidade"
    de-perc1          label "Perc"
    tt-work3.valor    label "Valor"
    de-perc2          label "Perc"
    tt-work3.vlr-adic label "Vlr.Adicional"
    de-vlr-tot        label "Valor Total"
    with no-labels no-box 55 down width 133 STREAM-IO frame f-detalhe3.

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
{utp/ut-liter.i Ranking_do_Faturamento_por_Representante/Estado * r}
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
          tt-configuracao.total-planilha      = 4
          tt-configuracao.exibir-construcao   = NO 
          tt-configuracao.abrir-excel-termino = NO.

   CREATE tt-planilha.
   ASSIGN tt-planilha.arquivo-num       = 1
          tt-planilha.planilha-num      = 1
          tt-planilha.planilha-nome     = "Rank-Rep-Estado"
          tt-planilha.linhas-grade      = NO
          tt-planilha.largura-coluna    = 12
          tt-planilha.formatar-planilha = YES.
   CREATE tt-planilha.
   ASSIGN tt-planilha.arquivo-num       = 1
          tt-planilha.planilha-num      = 2
          tt-planilha.planilha-nome     = "Rank-Repres"
          tt-planilha.linhas-grade      = NO
          tt-planilha.largura-coluna    = 12
          tt-planilha.formatar-planilha = YES.
   CREATE tt-planilha.
   ASSIGN tt-planilha.arquivo-num       = 1
          tt-planilha.planilha-num      = 3
          tt-planilha.planilha-nome     = "Rank-Estado"
          tt-planilha.linhas-grade      = NO
          tt-planilha.largura-coluna    = 12
          tt-planilha.formatar-planilha = YES.
   CREATE tt-planilha.
   ASSIGN tt-planilha.arquivo-num       = 1
          tt-planilha.planilha-num      = 4
          tt-planilha.planilha-nome     = "Rank-RegiÆo"
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
          tt-dados.celula-valor  = "RANKING DO FATURAMENTO POR REPRESENTANTE/ESTADO"
          tt-dados.celula-fonte-tamanho = 14
          tt-dados.celula-fonte-negrito = YES.
   
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 1
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-valor  = "Cod"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 2
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-valor  = "Nome do Representante"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 3
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha   = 5
          tt-dados.celula-valor   = "UF"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 4
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Quantidade"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 5
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Perc"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 6
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Valor"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 7
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Perc"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 8
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Vlr.Adicional"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 9
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor = "Valor Total"
          tt-dados.celula-fonte-negrito  = YES
          tt-dados.celula-fonte-cor = 2.

   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 2
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = 1
          tt-dados.celula-valor  = empresa.razao-social
          tt-dados.celula-fonte-tamanho = 24
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor     = 3.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 2
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = 3
          tt-dados.celula-valor  = "RANKING DO FATURAMENTO POR REPRESENTANTE"
          tt-dados.celula-fonte-tamanho = 14
          tt-dados.celula-fonte-negrito = YES.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 2
          tt-dados.celula-coluna = 1
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-valor  = "Cod"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 2
          tt-dados.celula-coluna = 2
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-valor  = "Nome do Representante"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 2
          tt-dados.celula-coluna = 3
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Quantidade"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 2
          tt-dados.celula-coluna = 4
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Perc"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 2
          tt-dados.celula-coluna = 5
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Valor"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 2
          tt-dados.celula-coluna = 6
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Perc"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 2
          tt-dados.celula-coluna = 7
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Vlr.Adicional"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 2
          tt-dados.celula-coluna  = 8
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha   = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor = "Valor Total"
          tt-dados.celula-fonte-negrito  = YES
          tt-dados.celula-fonte-cor = 2.
   
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 3
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = 1
          tt-dados.celula-valor  = empresa.razao-social
          tt-dados.celula-fonte-tamanho = 24
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor     = 3.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 3
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = 3
          tt-dados.celula-valor  = "RANKING DO FATURAMENTO POR ESTADO"
          tt-dados.celula-fonte-tamanho = 14
          tt-dados.celula-fonte-negrito = YES.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 3
          tt-dados.celula-coluna = 1
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-valor  = "Estado"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 3
          tt-dados.celula-coluna = 2
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Quantidade"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 3
          tt-dados.celula-coluna = 3
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Perc"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 3
          tt-dados.celula-coluna = 4
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Valor"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 3
          tt-dados.celula-coluna = 5
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Perc"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 3
          tt-dados.celula-coluna = 6
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Vlr.Adicional"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 3
          tt-dados.celula-coluna = 7
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Valor Total"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 4
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = 1
          tt-dados.celula-valor  = empresa.razao-social
          tt-dados.celula-fonte-tamanho = 24
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor     = 3.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 4
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = 3
          tt-dados.celula-valor  = "RANKING DO FATURAMENTO POR REGIÇO"
          tt-dados.celula-fonte-tamanho = 14
          tt-dados.celula-fonte-negrito = YES.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 4
          tt-dados.celula-coluna = 1
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-valor  = "RegiÆo"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 4
          tt-dados.celula-coluna = 2
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-valor  = "Quantidade"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 4
          tt-dados.celula-coluna = 3
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Perc"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 4
          tt-dados.celula-coluna = 4
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Valor"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 4
          tt-dados.celula-coluna = 5
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Perc"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 4
          tt-dados.celula-coluna = 6
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Vlr.Adicional"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 4
          tt-dados.celula-coluna = 7
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = 5
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Valor Total"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
END.

FOR each nota-fiscal where nota-fiscal.cod-estabel  =  tt-param.cod-estabel
                       and nota-fiscal.dt-emis-nota >= tt-param.ini-dt-emissao
                       and nota-fiscal.dt-emis-nota <= tt-param.fin-dt-emissao
                       AND nota-fiscal.cod-rep      >= tt-param.ini-cod-rep
                       AND nota-fiscal.cod-rep      <= tt-param.fin-cod-rep
                       and nota-fiscal.dt-cancela   =  ?
                       and nota-fiscal.emite-duplic = yes
                     NO-LOCK:

    run pi-acompanhar in h-acomp (input "Nota Fiscal: " + nota-fiscal.nr-nota-fis).

    find ped-venda where ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                     and ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
                   no-lock no-error.
                   
    if not avail ped-venda then
       next.
    else
       IF tt-param.all-types = NO THEN DO:
          IF ped-venda.tp-pedido <> tt-param.tp-pedido1 AND
             ped-venda.tp-pedido <> tt-param.tp-pedido2 AND
             ped-venda.tp-pedido <> tt-param.tp-pedido3 AND
             ped-venda.tp-pedido <> tt-param.tp-pedido4 AND
             ped-venda.tp-pedido <> tt-param.tp-pedido5 AND
             ped-venda.tp-pedido <> "" THEN NEXT.
          IF ped-venda.tp-pedido = "" AND tt-param.tp-normal = NO THEN NEXT.
       END.
       
    find emitente where emitente.cod-emitente = nota-fiscal.cod-emitente
                  no-lock no-error.
                
    for each it-nota-fisc of nota-fiscal
        no-lock:
        if it-nota-fisc.un-fatur[1] <> "m" then do:
           find item where item.it-codigo = it-nota-fisc.it-codigo
                     no-lock no-error.
           find item-ext
                where item-ext.it-codigo = item.it-codigo
                no-lock no-error.
        if avail item-ext then
           assign de-qtde = it-nota-fisc.qt-faturada[1]
                            * item-ext.fator-conv.
        end.
        else
           assign de-qtde = it-nota-fisc.qt-faturada[1].

        assign de-valor = it-nota-fisc.vl-tot-item.
    
        find emitente 
             where emitente.cod-emitente = nota-fiscal.cod-emitente
                   no-lock no-error.

        find first tt-work
             where tt-work.cod-rep = nota-fiscal.cod-rep
               and tt-work.uf      = emitente.estado
             no-lock no-error.
        if not avail tt-work then do:
           create tt-work.
           assign tt-work.cod-rep  = nota-fiscal.cod-rep
                  tt-work.uf       = emitente.estado
                  tt-work.qtd      = 0
                  tt-work.valor    = 0
                  tt-work.vlr-adic = 0.
        end.

        ASSIGN de-vlr-adic = 0.
        IF tt-param.imp-val-adic AND (ped-venda.tp-pedido = "t" or ped-venda.tp-pedido = "w") THEN
           ASSIGN de-vlr-adic = de-valor.
        IF tt-param.imp-val-adic AND ped-venda.tp-pedido = "p" THEN
           ASSIGN de-vlr-adic = de-valor / 70 * 30.

        assign tt-work.qtd      = tt-work.qtd      + de-qtde
               tt-work.valor    = tt-work.valor    + de-valor
               tt-work.vlr-adic = tt-work.vlr-adic + de-vlr-adic
               de-tot-qtd       = de-tot-qtd       + de-qtde
               de-tot-vlr       = de-tot-vlr       + de-valor
               de-tot-adic      = de-tot-adic      + de-vlr-adic
               de-tot-ger       = de-tot-ger       + (de-valor + de-vlr-adic)
               de-qtde          = 0
               de-valor         = 0.
    end.                   
end.  

ASSIGN i-lin-excel = 6.

for each tt-work by tt-work.qtd descend:
    find repres where repres.cod-rep = tt-work.cod-rep
                no-lock no-error.
                
    assign de-perc1   = (tt-work.qtd   * 100) / de-tot-qtd  
           de-perc2   = (tt-work.valor * 100) / de-tot-vlr
           de-vlr-tot = tt-work.valor + tt-work.vlr-adic.
    
    display tt-work.cod-rep
            repres.nome
            tt-work.uf
            tt-work.qtd
            de-perc1
            tt-work.valor
            de-perc2
            tt-work.vlr-adic when tt-param.imp-val-adic
            de-vlr-tot       when tt-param.imp-val-adic
            with frame f-detalhe.
    down with frame f-detalhe.        

    IF tt-param.gerar-excel THEN DO:
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 1
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-valor  = string(tt-work.cod-rep).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 1
              tt-dados.celula-coluna  = 2
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-valor   = repres.nome.
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 3
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-valor  = tt-work.uf.
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 1
              tt-dados.celula-coluna  = 4
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor   = string(tt-work.qtd).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 1
              tt-dados.celula-coluna  = 5
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-formato = "##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor   = string(de-perc1).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 6
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(tt-work.valor).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 7
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(de-perc2).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 8
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(tt-work.vlr-adic).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 1
              tt-dados.celula-coluna = 9
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(de-vlr-tot).
       ASSIGN i-lin-excel = i-lin-excel + 1.
    END.

    find first tt-work1
         where tt-work1.cod-rep = tt-work.cod-rep no-lock no-error.
    if not avail tt-work1 then do:
       create tt-work1.
       assign tt-work1.cod-rep  = tt-work.cod-rep
              tt-work1.qtd      = 0
              tt-work1.valor    = 0
              tt-work1.vlr-adic = 0.
    end.
    assign tt-work1.qtd      = tt-work1.qtd      + tt-work.qtd
           tt-work1.valor    = tt-work1.valor    + tt-work.valor
           tt-work1.vlr-adic = tt-work1.vlr-adic + tt-work.vlr-adic.

    find first tt-work2
         where tt-work2.uf = tt-work.uf no-lock no-error.
    if not avail tt-work2 then do:
       create tt-work2.
       assign tt-work2.uf       = tt-work.uf
              tt-work2.qtd      = 0
              tt-work2.valor    = 0
              tt-work2.vlr-adic = 0.
    end.
    assign tt-work2.qtd      = tt-work2.qtd      + tt-work.qtd
           tt-work2.valor    = tt-work2.valor    + tt-work.valor
           tt-work2.vlr-adic = tt-work2.vlr-adic + tt-work.vlr-adic.
    
    if lookup(tt-work.uf,"MG,RJ,ES,RJ,SP") <> 0 then
       assign c-regiao = "Sudeste".
    else
    if lookup(tt-work.uf,"PR,SC,RS") <> 0 then
       assign c-regiao = "Sul". 
    else
    if lookup(tt-work.uf,"MT,MS,DF,GO") <> 0 then
       assign c-regiao = "Centro-Oeste".
    else
    if lookup(tt-work.uf,"AC,AM,RO,RR,PA,AP,TO") <> 0 then
       assign c-regiao = "Norte".
    else
    if lookup(tt-work.uf,"BA,PI,MA,CE,RN,PB,PE,AL,SE") <> 0 then
       assign c-regiao = "Nordeste".
    else
       assign c-regiao = "Exportacao".
       
    find first tt-work3
         where tt-work3.regiao = c-regiao no-lock no-error.
    if not avail tt-work3 then do:
       create tt-work3.
       assign tt-work3.regiao = c-regiao
              tt-work3.qtd      = 0
              tt-work3.valor    = 0
              tt-work3.vlr-adic = 0.
    end.
    assign tt-work3.qtd      = tt-work3.qtd      + tt-work.qtd
           tt-work3.valor    = tt-work3.valor    + tt-work.valor
           tt-work3.vlr-adic = tt-work3.vlr-adic + tt-work.vlr-adic.
end.

assign de-tot-ger = de-tot-vlr + de-tot-adic.

display "Total"     @ repres.nome
        de-tot-qtd  @ tt-work.qtd
        de-tot-vlr  @ tt-work.valor
        de-tot-adic when tt-param.imp-val-adic @ tt-work.vlr-adic
        de-tot-ger  when tt-param.imp-val-adic @ de-vlr-tot
        with frame  f-detalhe.
down with frame f-detalhe.

IF tt-param.gerar-excel THEN DO:
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 2
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Total".
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 4
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-tot-qtd).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 6
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-tot-vlr).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 8
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-tot-adic).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 9
          tt-dados.celula-linha = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor = string(de-tot-ger).
   ASSIGN i-lin-excel = i-lin-excel + 2.
END.

page.

ASSIGN i-lin-excel = 6.

for each tt-work1 no-lock by tt-work1.qtd descend:
    assign de-perc1 = (tt-work1.qtd   * 100) / de-tot-qtd  
           de-perc2 = (tt-work1.valor * 100) / de-tot-vlr.
    
    find repres where repres.cod-rep = tt-work1.cod-rep
                no-lock no-error.
    ASSIGN de-vlr-tot = tt-work1.valor + tt-work1.vlr-adic.
    display tt-work1.cod-rep
            repres.nome
            tt-work1.qtd
            de-perc1
            tt-work1.valor
            de-perc2
            tt-work1.vlr-adic when tt-param.imp-val-adic
            de-vlr-tot        when tt-param.imp-val-adic
            with frame f-detalhe1.
    down with frame f-detalhe1.

    IF tt-param.gerar-excel THEN DO:
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 2
              tt-dados.celula-coluna = 1
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-valor  = string(tt-work1.cod-rep).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 2
              tt-dados.celula-coluna  = 2
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-valor   = repres.nome.
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 2
              tt-dados.celula-coluna  = 3
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor   = string(tt-work1.qtd).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 2
              tt-dados.celula-coluna  = 4
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-formato = "##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor   = string(de-perc1).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 2
              tt-dados.celula-coluna = 5
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(tt-work1.valor).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 2
              tt-dados.celula-coluna = 6
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(de-perc2).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 2
              tt-dados.celula-coluna = 7
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(tt-work1.vlr-adic).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 2
              tt-dados.celula-coluna = 8
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(de-vlr-tot).
       ASSIGN i-lin-excel = i-lin-excel + 1.
    END.
END.

display "Total"     @ tt-work1.cod-rep
        de-tot-qtd  @ tt-work1.qtd
        de-tot-vlr  @ tt-work1.valor
        de-tot-adic when tt-param.imp-val-adic @ tt-work1.vlr-adic
        de-tot-ger  when tt-param.imp-val-adic @ de-vlr-tot
        with frame f-detalhe1.
down with frame f-detalhe1.

IF tt-param.gerar-excel THEN DO:
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 2
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Total".
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 2
          tt-dados.celula-coluna = 3
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-tot-qtd).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 2
          tt-dados.celula-coluna = 5
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-tot-vlr).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 2
          tt-dados.celula-coluna = 7
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-tot-adic).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 2
          tt-dados.celula-coluna  = 8
          tt-dados.celula-linha = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor = string(de-tot-ger).
   ASSIGN i-lin-excel = i-lin-excel + 2.
END.

page.
       
ASSIGN i-lin-excel = 6.

for each tt-work2 no-lock by tt-work2.qtd descend:
    assign de-perc1 = (tt-work2.qtd   * 100) / de-tot-qtd  
           de-perc2 = (tt-work2.valor * 100) / de-tot-vlr.

    ASSIGN de-vlr-tot = tt-work2.valor + tt-work2.vlr-adic.
    
    display tt-work2.uf
            tt-work2.qtd
            de-perc1
            tt-work2.valor
            de-perc2
            tt-work2.vlr-adic when tt-param.imp-val-adic
            de-vlr-tot        when tt-param.imp-val-adic
            with frame f-detalhe2.
    down with frame f-detalhe2.
    
    IF tt-param.gerar-excel THEN DO:
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 3
              tt-dados.celula-coluna = 1
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-valor  = tt-work2.uf.
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 3
              tt-dados.celula-coluna  = 2
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor   = string(tt-work2.qtd).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 3
              tt-dados.celula-coluna  = 3
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-formato = "##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor   = string(de-perc1).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 3
              tt-dados.celula-coluna = 4
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(tt-work2.valor).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 3
              tt-dados.celula-coluna = 5
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(de-perc2).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 3
              tt-dados.celula-coluna = 6
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(tt-work2.vlr-adic).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 3
              tt-dados.celula-coluna = 7
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(de-vlr-tot).
       ASSIGN i-lin-excel = i-lin-excel + 1.
    END.
end.

display "Total"     @ tt-work2.uf
        de-tot-qtd  @ tt-work2.qtd
        de-tot-vlr  @ tt-work2.valor
        de-tot-adic when tt-param.imp-val-adic @ tt-work2.vlr-adic
        de-tot-ger  when tt-param.imp-val-adic @ de-vlr-tot
        with frame f-detalhe2.
down with frame f-detalhe2.

IF tt-param.gerar-excel THEN DO:
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 3
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Total".
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 3
          tt-dados.celula-coluna = 2
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-tot-qtd).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 3
          tt-dados.celula-coluna = 4
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-tot-vlr).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 3
          tt-dados.celula-coluna = 6
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-tot-adic).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 3
          tt-dados.celula-coluna  = 7
          tt-dados.celula-linha = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor = string(de-tot-ger).
   ASSIGN i-lin-excel = i-lin-excel + 2.
END.

page.

ASSIGN i-lin-excel = 6.

for each tt-work3 no-lock by tt-work3.qtd descend:
    assign de-perc1 = (tt-work3.qtd   * 100) / de-tot-qtd  
           de-perc2 = (tt-work3.valor * 100) / de-tot-vlr.
    
    ASSIGN de-vlr-tot = tt-work3.valor + tt-work3.vlr-adic.

    display tt-work3.regiao
            tt-work3.qtd
            de-perc1
            tt-work3.valor
            de-perc2
            tt-work3.vlr-adic when tt-param.imp-val-adic
            de-vlr-tot        when tt-param.imp-val-adic
            with frame f-detalhe3.
    down with frame f-detalhe3.
    
    IF tt-param.gerar-excel THEN DO:
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 4
              tt-dados.celula-coluna = 1
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-valor  = tt-work3.regiao.
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 4
              tt-dados.celula-coluna  = 2
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor   = string(tt-work3.qtd).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 4
              tt-dados.celula-coluna  = 3
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-formato = "##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor   = string(de-perc1).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 4
              tt-dados.celula-coluna = 4
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(tt-work3.valor).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 4
              tt-dados.celula-coluna = 5
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(de-perc2).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 4
              tt-dados.celula-coluna = 6
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(tt-work3.vlr-adic).
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num   = 1
              tt-dados.planilha-num  = 4
              tt-dados.celula-coluna = 7
              tt-dados.celula-linha  = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor  = string(de-vlr-tot).
       ASSIGN i-lin-excel = i-lin-excel + 1.
    END.
end.

display "Total"     @ tt-work3.regiao
        de-tot-qtd  @ tt-work3.qtd
        de-tot-vlr  @ tt-work3.valor
        de-tot-adic when tt-param.imp-val-adic @ tt-work3.vlr-adic
        de-tot-ger  when tt-param.imp-val-adic @ de-vlr-tot
        with frame f-detalhe3.
down with frame f-detalhe3.

IF tt-param.gerar-excel THEN DO:
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 4
          tt-dados.celula-coluna = 1
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-valor  = "Total".
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 4
          tt-dados.celula-coluna = 2
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-tot-qtd).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 4
          tt-dados.celula-coluna = 4
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-tot-vlr).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 4
          tt-dados.celula-coluna = 6
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-tot-adic).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 4
          tt-dados.celula-coluna  = 7
          tt-dados.celula-linha = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor = string(de-tot-ger).
   ASSIGN i-lin-excel = i-lin-excel + 2.
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

for each tt-work:
    delete tt-work.
end.
for each tt-work1:
    delete tt-work1.
end.
for each tt-work2:
    delete tt-work2.
end.
for each tt-work3:
    delete tt-work3.
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
           tt-param.all-types
           tt-param.tp-pedido1
           tt-param.tp-pedido2
           tt-param.tp-pedido3
           tt-param.tp-pedido4
           tt-param.tp-pedido5
           tt-param.tp-normal
           tt-param.gerar-excel
           tt-param.arq-excel  
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

