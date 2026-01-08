/* Programa: ESFT0020RP.P
** Sistema.: EMS da Datasul
** M¢dulo..: Faturamento
** Objetivo: EmissÆo do relat¢rio de Resumo de Faturamento em Metros/Reais.
** Autor...: Gilvando Souza Araujo
** Data....: 01/12/2006
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0020RP 2.04.00.000}

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
       FIELD all-types         AS LOG FORMAT "Sim/NÆo"
       FIELD tp-pedido1        AS CHAR FORMAT "x(2)"
       FIELD tp-pedido2        AS CHAR FORMAT "x(2)"
       FIELD tp-pedido3        AS CHAR FORMAT "x(2)"
       FIELD tp-pedido4        AS CHAR FORMAT "x(2)"
       FIELD tp-pedido5        AS CHAR FORMAT "x(2)"
       FIELD tp-normal         AS LOG FORMAT "Sim/NÆo"
       FIELD gerar-excel       AS LOG FORMAT "Sim/NÆo"
       FIELD arq-excel         AS CHAR FORMAT "x(45)"
       FIELD imp-param         AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

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

DEF VAR de-qt-conv  AS DEC.
DEF VAR de-tot-conv AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-tot-norm AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-tot-vlr  AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-ger-conv AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-ger-norm AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR de-ger-vlr  AS DEC FORMAT ">>>,>>>,>>9.99".
DEF VAR l-falta-fator  AS LOG.
DEF VAR i-lin-excel AS INTEGER INITIAL 1.

FORM 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel      LABEL "Estabelec....." SKIP
    tt-param.ini-dt-emissao   label "Data Emissao.." 
    "A"                                              AT 34
    tt-param.fin-dt-emissao   NO-LABELS              
    tt-param.ini-it-codigo    label "Item.........." 
    "A"                                              AT 34
    tt-param.fin-it-codigo    no-labels SKIP
    tt-param.ini-ge-codigo    label "Grupo Estoque."
    "A"                                              AT 34
    tt-param.fin-ge-codigo    no-labels SKIP
    tt-param.all-types        LABEL "Todos Tipos..."
    tt-param.tp-pedido1       NO-LABELS              AT 22
    tt-param.tp-pedido2       NO-LABELS              AT 25
    tt-param.tp-pedido3       NO-LABELS              AT 28
    tt-param.tp-pedido4       NO-LABELS              AT 31
    tt-param.tp-pedido5       NO-LABELS              AT 34 
    tt-param.tp-normal        LABEL "Tipo Normal..." AT  1
    tt-param.gerar-excel      LABEL "Gerar p/Excel." AT  1
    tt-param.arq-excel        LABEL "Arquivo Excel." AT  1
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

form
    it-nota-fisc.it-codigo   FORMAT "x(7)"   LABEL "Item"        
    ITEM.desc-item           FORMAT "x(36)"  LABEL "Descri‡Æo"
    it-nota-fisc.un-fatur[1]                 LABEL "Un"
    de-tot-norm                              LABEL "Qtd.Unidade"
    de-tot-conv                              LABEL "Qtd.Metros"
    de-tot-vlr                               LABEL "Valor"
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
{utp/ut-liter.i Resumo_do_Faturamento_em_Metros/R$ * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
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
          tt-dados.celula-valor  = "RESUMO DO FATURAMENTO EM M/R$" +
                                   " - Per¡odo: " + STRING(tt-param.ini-dt-emissao) +
                                   " a " + STRING(tt-param.fin-dt-emissao)
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
          tt-dados.celula-valor   = "Un"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 4
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha   = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor   = "Qtd.Unidade"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 5
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Qtd.Metros"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 6
          tt-dados.celula-cor-interior = 10
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = "Valor"
          tt-dados.celula-fonte-negrito = YES
          tt-dados.celula-fonte-cor = 2.
   ASSIGN i-lin-excel = i-lin-excel + 1.
END.

for each it-nota-fisc
    where it-nota-fisc.cod-estabel  =  tt-param.cod-estabel
      and it-nota-fisc.it-codigo    >= tt-param.ini-it-codigo
      AND it-nota-fisc.it-codigo    <= tt-param.fin-it-codigo
      and it-nota-fisc.dt-emis-nota >= tt-param.ini-dt-emissao
      and it-nota-fisc.dt-emis-nota <= tt-param.fin-dt-emissao
      and it-nota-fisc.dt-cancela   =  ? 
    NO-LOCK,

    EACH ITEM WHERE ITEM.it-codigo =  it-nota-fisc.it-codigo 
                AND item.ge-codigo >= tt-param.ini-ge-codigo
                AND item.ge-codigo <= tt-param.fin-ge-codigo
              NO-LOCK,
    /*
    EACH item-ext WHERE ITEM-ext.it-codigo = it-nota-fisc.it-codigo
                    AND item-ext.indigo    = YES
                  NO-LOCK,
    */

    EACH nota-fiscal 
    where nota-fiscal.cod-estabel  = it-nota-fisc.cod-estabel
      and nota-fiscal.nr-nota-fis  = it-nota-fisc.nr-nota-fis
      and nota-fiscal.serie        = it-nota-fisc.serie
      and nota-fiscal.emite-duplic = yes
    NO-LOCK,

    EACH emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente
                   /* AND emitente.natureza     < 3*/
                  NO-LOCK
    BREAK BY it-nota-fisc.it-codigo:

    run pi-acompanhar in h-acomp (input "Nota: " + it-nota-fisc.nr-nota-fis).

    /*------ Conversao de Kg para M ------- */
    if it-nota-fisc.un-fatur[1] <> "m" then do:
       FIND item-ext WHERE ITEM-ext.it-codigo = it-nota-fisc.it-codigo
                     NO-LOCK NO-ERROR.
       if AVAIL item-ext then
          assign de-qt-conv = it-nota-fisc.qt-faturada[1] * 
                              item-ext.fator-conv.
       else do:
          assign l-falta-fator = yes
                 de-qt-conv    = it-nota-fisc.qt-faturada[1].
          find first tt-work where
                     tt-work.it-codigo = it-nota-fisc.it-codigo
                     no-lock no-error.
          if not avail tt-work then do:
             create tt-work.
             assign tt-work.it-codigo = it-nota-fisc.it-codigo.
          end.
       end.   
    end.
    ELSE
       ASSIGN de-qt-conv = it-nota-fisc.qt-faturada[1].

    ASSIGN de-tot-norm = de-tot-norm + it-nota-fisc.qt-faturada[1]
           de-ger-norm = de-ger-norm + it-nota-fisc.qt-faturada[1]
           de-tot-conv = de-tot-conv + de-qt-conv
           de-ger-conv = de-ger-conv + de-qt-conv
           de-tot-vlr  = de-tot-vlr  + it-nota-fisc.vl-tot-item
           de-ger-vlr  = de-ger-vlr  + it-nota-fisc.vl-tot-item.

    IF LAST-OF(it-nota-fisc.it-codigo) THEN DO:
       DISPLAY it-nota-fisc.it-codigo
               ITEM.desc-item
               it-nota-fisc.un-fatur[1]
               de-tot-norm
               de-tot-conv
               de-tot-vlr
               WITH FRAME f-detalhe.
       DOWN WITH FRAME f-detalhe.

       IF tt-param.gerar-excel THEN DO:
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num    = 1
                 tt-dados.planilha-num   = 1
                 tt-dados.celula-coluna  = 1
                 tt-dados.celula-linha   = i-lin-excel
                 tt-dados.celula-valor   = it-nota-fisc.it-codigo.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num    = 1
                 tt-dados.planilha-num   = 1
                 tt-dados.celula-coluna  = 2
                 tt-dados.celula-linha   = i-lin-excel
                 tt-dados.celula-valor   = item.desc-item.
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num    = 1
                 tt-dados.planilha-num   = 1
                 tt-dados.celula-coluna  = 3
                 tt-dados.celula-linha   = i-lin-excel
                 tt-dados.celula-alinhamento-horizontal = 1
                 tt-dados.celula-valor   = it-nota-fisc.un-fatur[1].
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num    = 1
                 tt-dados.planilha-num   = 1
                 tt-dados.celula-coluna  = 4
                 tt-dados.celula-linha   = i-lin-excel
                 tt-dados.celula-formato = "###.###.##0,00"
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor   = string(de-tot-norm).
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num    = 1
                 tt-dados.planilha-num   = 1
                 tt-dados.celula-coluna  = 5
                 tt-dados.celula-linha   = i-lin-excel
                 tt-dados.celula-formato = "###.###.##0,00"
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor  = string(de-tot-conv).
          CREATE tt-dados.
          ASSIGN tt-dados.arquivo-num   = 1
                 tt-dados.planilha-num  = 1
                 tt-dados.celula-coluna = 6
                 tt-dados.celula-linha  = i-lin-excel
                 tt-dados.celula-formato = "###.###.##0,00"
                 tt-dados.celula-alinhamento-horizontal = 5
                 tt-dados.celula-valor  = string(de-tot-vlr).
          ASSIGN i-lin-excel = i-lin-excel + 1.
       END.

       ASSIGN de-tot-conv = 0
              de-tot-norm = 0
              de-tot-vlr  = 0.
    END.
END.

DISPLAY "Total"      @ it-nota-fisc.it-codigo
        de-ger-norm  @ de-tot-norm
        de-ger-conv  @ de-tot-conv
        de-ger-vlr   @ de-tot-vlr
        WITH FRAME f-detalhe.

IF tt-param.gerar-excel THEN DO:
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 1
          tt-dados.celula-linha   = i-lin-excel
          tt-dados.celula-valor   = "Total".
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 4
          tt-dados.celula-linha   = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor   = string(de-ger-norm).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num    = 1
          tt-dados.planilha-num   = 1
          tt-dados.celula-coluna  = 5
          tt-dados.celula-linha   = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-ger-conv).
   CREATE tt-dados.
   ASSIGN tt-dados.arquivo-num   = 1
          tt-dados.planilha-num  = 1
          tt-dados.celula-coluna = 6
          tt-dados.celula-linha  = i-lin-excel
          tt-dados.celula-formato = "###.###.##0,00"
          tt-dados.celula-alinhamento-horizontal = 5
          tt-dados.celula-valor  = string(de-ger-vlr).
   ASSIGN i-lin-excel = i-lin-excel + 1.

   RUN utp/utapi003.p (INPUT-OUTPUT TABLE tt-configuracao,
                       INPUT-OUTPUT TABLE tt-planilha,
                       INPUT-OUTPUT TABLE tt-dados,
                       INPUT-OUTPUT TABLE tt-grafico,
                       INPUT-OUTPUT TABLE tt-erros).
END.

if l-falta-fator then do:
   page.
   put "Atencao ! - Ha itens sem fator de conversao:"
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
           tt-param.ini-dt-emissao
           tt-param.fin-dt-emissao
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
           tt-param.gerar-excel
           tt-param.arq-excel  
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

