/* Programa: ESFT0033
** Sistema.: EMS 2.04 da Datasul
** Modulo..: Faturamento - e-Vendas da Tear Tˆxtil
** Objetivo: Relatorio Rank de Vendas por Produtos
** Autor...: Gilvando Souza Araujo
** Data....: 08/08/2005
** Obs.....: Programa especifico da TEAR TÒXTIL INDéSTRIA E COMRCIO LTDA.
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0033RP 2.04.00.000}

define temp-table tt-param  no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD ini-periodo       AS CHAR FORMAT "99/9999"
       FIELD fin-periodo       AS CHAR FORMAT "99/9999"
       FIELD ini-it-codigo     LIKE item.it-codigo
       FIELD fin-it-codigo     LIKE item.it-codigo
       FIELD ini-nome-abrev    LIKE nota-fiscal.nome-abrev
       FIELD fin-nome-abrev    LIKE nota-fiscal.nome-abrev
       FIELD ini-no-ab-reppri  LIKE nota-fiscal.no-ab-reppri
       FIELD fin-no-ab-reppri  LIKE nota-fiscal.no-ab-reppri
       FIELD tipo-inform       AS INT.

define temp-table tt-raw-digita
       field raw-digita as raw.

DEF TEMP-TABLE tt-work
    field it-codigo  LIKE it-nota-fisc.it-codigo
    field valor      AS DEC FORMAT ">>>,>>>,>>9.99" EXTENT 14
    field quantidade AS DEC FORMAT ">>>,>>>,>>9.99" EXTENT 14
    INDEX ch-work it-codigo.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i} 

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i} 
{include/pi-edit.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

DEF VAR dt-emis-ini LIKE nota-fiscal.dt-emis-nota.
DEF VAR dt-emis-fin LIKE nota-fiscal.dt-emis-nota.
DEF VAR dt-emis-aux LIKE nota-fiscal.dt-emis-nota.
DEF VAR c-repres    LIKE repres.nome.
DEF VAR c-cab-mes   AS CHAR FORMAT "x(7)" EXTENT 12.
DEF VAR i-mes       AS INT.
DEF VAR i-ano       AS INT.
DEF VAR i-cont      AS INT.
DEF VAR i-cont-mes  AS INT.
DEF VAR de-qt-conv  AS DEC.
DEF VAR de-tot-qtd  AS DEC FORMAT ">>>,>>>,>>9.99" EXTENT 14.
DEF VAR de-tot-vlr  AS DEC FORMAT ">>>,>>>,>>9.99" EXTENT 14.
DEF VAR i-lin-excel AS INTEGER INITIAL 1.
DEF VAR i-col       AS INT.

/*include padrÆo para output de relat¢rios */
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
{utp/ut-liter.i Rank_do_Faturamento_por_Produtos * r}
assign c-titulo-relat = trim(return-value).

IF tt-param.destino <> 2 THEN DO.
   VIEW FRAME f-cabec.
   VIEW FRAME f-rodape.
END.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

ASSIGN dt-emis-ini = DATE(INT(SUBSTR(tt-param.ini-periodo,1,2)),1,INT(SUBSTR(tt-param.ini-periodo,3,4)))
       dt-emis-aux = DATE(INT(SUBSTR(tt-param.fin-periodo,1,2)),28,INT(SUBSTR(tt-param.fin-periodo,3,4))) + 4
       dt-emis-fin = DATE(MONTH(dt-emis-aux),1,YEAR(dt-emis-aux)) - 1.

ASSIGN i-mes = month(dt-emis-ini)
       i-ano = year(dt-emis-ini).

IF SUBSTR(tt-param.ini-periodo,3,4) <> SUBSTR(tt-param.fin-periodo,3,4) THEN
   ASSIGN i-cont-mes = (13 - INT(SUBSTR(tt-param.ini-periodo,1,2))) + INT(SUBSTR(tt-param.fin-periodo,1,2)).
ELSE
   ASSIGN i-cont-mes = INT(SUBSTR(tt-param.fin-periodo,1,2)) - INT(SUBSTR(tt-param.ini-periodo,1,2)) + 1.

DO i-cont = 1 TO i-cont-mes:
   ASSIGN c-cab-mes[i-cont] = STRING(i-mes,"99") + "/" + STRING(i-ano,"9999").
   ASSIGN i-ano = IF i-mes < 12 THEN i-ano ELSE i-ano + 1.
   ASSIGN i-mes = IF i-mes < 12 THEN i-mes + 1
                                ELSE i-mes + 1 - 12.
END.

IF tt-param.usuario BEGINS "REP" THEN DO:
   FIND repres WHERE repres.nome-abrev = tt-param.ini-no-ab-reppri NO-LOCK NO-ERROR.
   IF AVAIL repres THEN 
      ASSIGN c-repres  = repres.nome.
   ELSE
      ASSIGN c-repres  = "".
END.
ELSE
   ASSIGN c-repres = tt-param.ini-no-ab-reppri + " a " + tt-param.fin-no-ab-reppri.

{utp/utapi003.i}

FOR EACH nota-fiscal WHERE
         nota-fiscal.no-ab-reppri >= tt-param.ini-no-ab-reppri AND
         nota-fiscal.no-ab-reppri <= tt-param.fin-no-ab-reppri AND
         nota-fiscal.dt-emis-nota >= dt-emis-ini AND
         nota-fiscal.dt-emis-nota <= dt-emis-fin AND 
         nota-fiscal.cod-estabel = '2' AND
         nota-fiscal.serie = '1' AND
         nota-fiscal.emite-duplic =  YES AND
         nota-fiscal.dt-cancela = ? NO-LOCK USE-INDEX ch-clinota,
    EACH it-nota-fisc OF nota-fiscal WHERE
         it-nota-fisc.it-codigo    >= tt-param.ini-it-codigo AND
         it-nota-fisc.it-codigo    <= tt-param.fin-it-codigo NO-LOCK.

    RUN pi-acompanhar IN h-acomp (INPUT "Nota Fiscal: " + it-nota-fisc.nr-nota-fis).

    /*------ Conversao de Kg para M ------- */
    if it-nota-fisc.un-fatur[1] <> "m" then do:
       FIND item-ext WHERE ITEM-ext.it-codigo = it-nota-fisc.it-codigo
                     NO-LOCK NO-ERROR.
       if AVAIL item-ext then
          assign de-qt-conv = it-nota-fisc.qt-faturada[1] * 
                              item-ext.fator-conv.
    end.
    ELSE
       ASSIGN de-qt-conv = it-nota-fisc.qt-faturada[1].

    DO i-cont = 1 TO 12:
       IF c-cab-mes[i-cont] = STRING(MONTH(it-nota-fisc.dt-emis-nota),"99") + "/" + 
                              STRING(YEAR(it-nota-fisc.dt-emis-nota),"9999") THEN DO:
          ASSIGN i-mes = i-cont.
          LEAVE.
       END.
    END.

    FIND tt-work WHERE tt-work.it-codigo = it-nota-fisc.it-codigo NO-ERROR.
    IF NOT AVAIL tt-work THEN DO:
       CREATE tt-work.
       ASSIGN tt-work.it-codigo = it-nota-fisc.it-codigo.
    END.

    ASSIGN tt-work.valor[i-mes]      = tt-work.valor[i-mes] + it-nota-fisc.vl-tot-item
           tt-work.quantidade[i-mes] = tt-work.quantidade[i-mes] + de-qt-conv
           tt-work.valor[13]         = tt-work.valor[13] + it-nota-fisc.vl-tot-item
           tt-work.quantidade[13]    = tt-work.quantidade[13] + de-qt-conv
           tt-work.valor[14]         = IF tt-work.valor[13] > 0 THEN tt-work.valor[13] / 12 ELSE 0
           tt-work.quantidade[14]    = IF tt-work.quantidade[13] > 0 THEN tt-work.quantidade[13] / 12 ELSE 0.
end.

IF tt-param.destino <> 2 THEN DO.
   ASSIGN i-col = 24.
   DO i-cont = 1 TO i-cont-mes.
      IF i-cont-mes > 2 THEN NEXT.
      IF i-cont <> 1 THEN
         ASSIGN i-col = i-col + 28.
      PUT c-cab-mes[i-cont] AT i-col.
   END.
   PUT "TOTAL" AT 81
       "MEDIA" AT 109
       SKIP.

   PUT "Item"        AT 1
       "Quantidade"  AT 17
       "Valor"       AT 36 
       "Quantidade"  AT 45
       "Valor"       AT 64 
       "Quantidade"  AT 73
       "Valor"       AT 92
       "Quantidade"  AT 101
       "Valor"       AT 120 
       SKIP
       FILL("-",12)  AT 1 FORMAT "x(12)"
       FILL("-",27)  AT 14 FORMAT "x(27)"
       FILL("-",27)  AT 42 FORMAT "x(27)"
       FILL("-",27)  AT 70 FORMAT "x(27)"
       FILL("-",27)  AT 98 FORMAT "x(27)"
       SKIP.

   FOR EACH tt-work NO-LOCK,
       FIRST item WHERE
             item.it-codigo = tt-work.it-codigo NO-LOCK
       BREAK BY IF tt-param.classifica = 1
                THEN string(tt-work.valor[14],">>>,>>9.99")
                ELSE IF tt-param.classifica = 2
                     THEN string(tt-work.quantidade[14],">>>,>>9.99")
                     ELSE item.it-codigo.

       PUT item.it-codigo FORMAT "x(12)".

       DO i-cont = 1 TO i-cont-mes.
          IF i-cont-mes > 2 THEN NEXT.
          PUT tt-work.quantidade[i-cont]
              tt-work.valor[i-cont].
       END.
       PUT tt-work.quantidade[13]
           tt-work.valor[13]
           tt-work.quantidade[14]
           tt-work.valor[14]
           SKIP.
   END.
END.
ELSE DO.
    OS-DELETE VALUE(tt-param.arquivo).
    CREATE tt-configuracao.
    ASSIGN tt-configuracao.versao-integracao   = 2
           tt-configuracao.arquivo-num         = 1
           tt-configuracao.arquivo             = tt-param.arquivo
           tt-configuracao.total-planilha      = 1
           tt-configuracao.exibir-construcao   = YES 
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
           tt-dados.celula-valor  = "RANKING DE VENDAS POR PRODUTOS - Per¡odo: " + 
            string(dt-emis-ini) + " a " + string(dt-emis-fin) + " - Em " +
            IF tt-param.tipo-inform = 1 THEN "Quantidades - Metros" ELSE "Valores - R$"
           tt-dados.celula-fonte-tamanho = 14
           tt-dados.celula-fonte-negrito = YES.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num   = 1
           tt-dados.planilha-num  = 1
           tt-dados.celula-coluna = 1
           tt-dados.celula-linha  = 5
           tt-dados.celula-valor  = "Representante: " + c-repres
           tt-dados.celula-fonte-tamanho = 14
           tt-dados.celula-fonte-negrito = YES.
    ASSIGN i-lin-excel = 7.
    
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
           tt-dados.celula-valor  = "Descri‡Æo"
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num    = 1
           tt-dados.planilha-num   = 1
           tt-dados.celula-coluna  = 3
           tt-dados.celula-cor-interior = 10
           tt-dados.celula-linha   = i-lin-excel
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor   = c-cab-mes[1]
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num    = 1
           tt-dados.planilha-num   = 1
           tt-dados.celula-coluna  = 4
           tt-dados.celula-cor-interior = 10
           tt-dados.celula-linha   = i-lin-excel
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor   = c-cab-mes[2]
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num   = 1
           tt-dados.planilha-num  = 1
           tt-dados.celula-coluna = 5
           tt-dados.celula-cor-interior = 10
           tt-dados.celula-linha  = i-lin-excel
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor  = c-cab-mes[3]
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num   = 1
           tt-dados.planilha-num  = 1
           tt-dados.celula-coluna = 6
           tt-dados.celula-cor-interior = 10
           tt-dados.celula-linha  = i-lin-excel
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor  = c-cab-mes[4]
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num   = 1
           tt-dados.planilha-num  = 1
           tt-dados.celula-coluna = 7
           tt-dados.celula-cor-interior = 10
           tt-dados.celula-linha  = i-lin-excel
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor  = c-cab-mes[5]
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num   = 1
           tt-dados.planilha-num  = 1
           tt-dados.celula-coluna = 8
           tt-dados.celula-cor-interior = 10
           tt-dados.celula-linha  = i-lin-excel
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor  = c-cab-mes[6]
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num    = 1
           tt-dados.planilha-num   = 1
           tt-dados.celula-coluna  = 9
           tt-dados.celula-cor-interior = 10
           tt-dados.celula-linha = i-lin-excel
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor = c-cab-mes[7]
           tt-dados.celula-fonte-negrito  = YES
           tt-dados.celula-fonte-cor = 2.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num   = 1
           tt-dados.planilha-num  = 1
           tt-dados.celula-coluna = 10
           tt-dados.celula-cor-interior = 10
           tt-dados.celula-linha = i-lin-excel
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor = c-cab-mes[8]
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num   = 1
           tt-dados.planilha-num  = 1
           tt-dados.celula-coluna = 11
           tt-dados.celula-cor-interior = 10
           tt-dados.celula-linha = i-lin-excel
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor = c-cab-mes[9]
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num   = 1
           tt-dados.planilha-num  = 1
           tt-dados.celula-coluna = 12
           tt-dados.celula-cor-interior = 10
           tt-dados.celula-linha = i-lin-excel
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor = c-cab-mes[10]
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num   = 1
           tt-dados.planilha-num  = 1
           tt-dados.celula-coluna = 13
           tt-dados.celula-cor-interior = 10
           tt-dados.celula-linha = i-lin-excel
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor = c-cab-mes[11]
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num   = 1
           tt-dados.planilha-num  = 1
           tt-dados.celula-coluna = 14
           tt-dados.celula-cor-interior = 10
           tt-dados.celula-linha = i-lin-excel
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor = c-cab-mes[12]
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num   = 1
           tt-dados.planilha-num  = 1
           tt-dados.celula-coluna = 15
           tt-dados.celula-cor-interior = 10
           tt-dados.celula-linha = i-lin-excel
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor = "Total"
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num   = 1
           tt-dados.planilha-num  = 1
           tt-dados.celula-coluna = 16
           tt-dados.celula-cor-interior = 10
           tt-dados.celula-linha = i-lin-excel
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor = "M‚dia"
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    ASSIGN i-lin-excel = i-lin-excel + 1.
    
    FOR EACH tt-work BY IF tt-param.tipo-inform = 1 THEN tt-work.quantidade[14]
                                                    ELSE tt-work.valor[14]:
        ACCUMULATE tt-work.it-codigo(COUNT).
        DO i-cont = 1 TO 13:
           ASSIGN de-tot-qtd[i-cont] = de-tot-qtd[i-cont] + tt-work.quantidade[i-cont]
                  de-tot-vlr[i-cont] = de-tot-vlr[i-cont] + tt-work.valor[i-cont].
        END.
        ASSIGN de-tot-qtd[14] = IF de-tot-qtd[13] > 0 THEN de-tot-qtd[13] / 12 ELSE 0
               de-tot-vlr[14] = IF de-tot-vlr[13] > 0 THEN de-tot-vlr[13] / 12 ELSE 0.
    
        FIND ITEM WHERE ITEM.it-codigo = tt-work.it-codigo NO-LOCK NO-ERROR.
        
        CREATE tt-dados.
        ASSIGN tt-dados.arquivo-num   = 1
               tt-dados.planilha-num  = 1
               tt-dados.celula-coluna = 1
               tt-dados.celula-linha  = i-lin-excel
               tt-dados.celula-valor  = tt-work.it-codigo.
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
               tt-dados.celula-formato = "###.###.##0,00"
               tt-dados.celula-alinhamento-horizontal = 5
               tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(tt-work.quantidade[1])
                                                                     ELSE STRING(tt-work.valor[1]).
        CREATE tt-dados.
        ASSIGN tt-dados.arquivo-num    = 1
               tt-dados.planilha-num   = 1
               tt-dados.celula-coluna  = 4
               tt-dados.celula-linha   = i-lin-excel
               tt-dados.celula-formato = "###.###.##0,00"
               tt-dados.celula-alinhamento-horizontal = 5
               tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(tt-work.quantidade[2])
                                                                     ELSE STRING(tt-work.valor[2]).
         CREATE tt-dados.
         ASSIGN tt-dados.arquivo-num    = 1
                tt-dados.planilha-num   = 1
                tt-dados.celula-coluna  = 5
                tt-dados.celula-linha   = i-lin-excel
                tt-dados.celula-formato = "###.###.##0,00"
                tt-dados.celula-alinhamento-horizontal = 5
                tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(tt-work.quantidade[3])
                                                                      ELSE STRING(tt-work.valor[3]).
        CREATE tt-dados.                                                                               
        ASSIGN tt-dados.arquivo-num    = 1                                                             
               tt-dados.planilha-num   = 1                                                             
               tt-dados.celula-coluna  = 6                                                             
               tt-dados.celula-linha   = i-lin-excel                                                   
               tt-dados.celula-formato = "###.###.##0,00"                                              
               tt-dados.celula-alinhamento-horizontal = 5                                              
               tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(tt-work.quantidade[4])
                                                                     ELSE STRING(tt-work.valor[4]).    
        CREATE tt-dados.                                                                               
        ASSIGN tt-dados.arquivo-num    = 1                                                             
               tt-dados.planilha-num   = 1                                                             
               tt-dados.celula-coluna  = 7                                                             
               tt-dados.celula-linha   = i-lin-excel                                                   
               tt-dados.celula-formato = "###.###.##0,00"                                              
               tt-dados.celula-alinhamento-horizontal = 5                                              
               tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(tt-work.quantidade[5])
                                                                     ELSE STRING(tt-work.valor[5]).    
        CREATE tt-dados.                                                                               
        ASSIGN tt-dados.arquivo-num    = 1                                                             
               tt-dados.planilha-num   = 1                                                             
               tt-dados.celula-coluna  = 8                                                             
               tt-dados.celula-linha   = i-lin-excel                                                   
               tt-dados.celula-formato = "###.###.##0,00"                                              
               tt-dados.celula-alinhamento-horizontal = 5                                              
               tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(tt-work.quantidade[6])
                                                                     ELSE STRING(tt-work.valor[6]).    
        CREATE tt-dados.                                                                               
        ASSIGN tt-dados.arquivo-num    = 1                                                             
               tt-dados.planilha-num   = 1                                                             
               tt-dados.celula-coluna  = 9                                                             
               tt-dados.celula-linha   = i-lin-excel                                                   
               tt-dados.celula-formato = "###.###.##0,00"                                              
               tt-dados.celula-alinhamento-horizontal = 5                                              
               tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(tt-work.quantidade[7])
                                                                     ELSE STRING(tt-work.valor[7]).    
        
        CREATE tt-dados.                                                                               
        ASSIGN tt-dados.arquivo-num    = 1                                                             
               tt-dados.planilha-num   = 1                                                             
               tt-dados.celula-coluna  = 10                                                             
               tt-dados.celula-linha   = i-lin-excel                                                   
               tt-dados.celula-formato = "###.###.##0,00"                                              
               tt-dados.celula-alinhamento-horizontal = 5                                              
               tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(tt-work.quantidade[8])
                                                                     ELSE STRING(tt-work.valor[8]).    
        CREATE tt-dados.                                                                               
        ASSIGN tt-dados.arquivo-num    = 1                                                             
               tt-dados.planilha-num   = 1                                                             
               tt-dados.celula-coluna  = 11                                                            
               tt-dados.celula-linha   = i-lin-excel                                                   
               tt-dados.celula-formato = "###.###.##0,00"                                              
               tt-dados.celula-alinhamento-horizontal = 5                                              
               tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(tt-work.quantidade[9])
                                                                     ELSE STRING(tt-work.valor[9]).    
        CREATE tt-dados.                                                                                
        ASSIGN tt-dados.arquivo-num    = 1                                                              
               tt-dados.planilha-num   = 1                                                              
               tt-dados.celula-coluna  = 12                                                             
               tt-dados.celula-linha   = i-lin-excel                                                    
               tt-dados.celula-formato = "###.###.##0,00"                                               
               tt-dados.celula-alinhamento-horizontal = 5                                               
               tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(tt-work.quantidade[10]) 
                                                                     ELSE STRING(tt-work.valor[10]).     
        CREATE tt-dados.                                                                                
        ASSIGN tt-dados.arquivo-num    = 1                                                              
               tt-dados.planilha-num   = 1                                                              
               tt-dados.celula-coluna  = 13                                                             
               tt-dados.celula-linha   = i-lin-excel                                                    
               tt-dados.celula-formato = "###.###.##0,00"                                               
               tt-dados.celula-alinhamento-horizontal = 5                                               
               tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(tt-work.quantidade[11])
                                                                     ELSE STRING(tt-work.valor[11]).    
        CREATE tt-dados.                                                                                
        ASSIGN tt-dados.arquivo-num    = 1                                                              
               tt-dados.planilha-num   = 1                                                              
               tt-dados.celula-coluna  = 14                                                             
               tt-dados.celula-linha   = i-lin-excel                                                    
               tt-dados.celula-formato = "###.###.##0,00"                                               
               tt-dados.celula-alinhamento-horizontal = 5                                               
               tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(tt-work.quantidade[12])
                                                                     ELSE STRING(tt-work.valor[12]).    
        CREATE tt-dados.                                                                                 
        ASSIGN tt-dados.arquivo-num    = 1                                                               
               tt-dados.planilha-num   = 1                                                               
               tt-dados.celula-coluna  = 15                                                              
               tt-dados.celula-linha   = i-lin-excel                                                     
               tt-dados.celula-formato = "###.###.##0,00"                                                
               tt-dados.celula-alinhamento-horizontal = 5                                                
               tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(tt-work.quantidade[13]) 
                                                                     ELSE STRING(tt-work.valor[13]).     
        
        CREATE tt-dados.                                                                                
        ASSIGN tt-dados.arquivo-num    = 1                                                              
               tt-dados.planilha-num   = 1                                                              
               tt-dados.celula-coluna  = 16                                                             
               tt-dados.celula-linha   = i-lin-excel                                                    
               tt-dados.celula-formato = "###.###.##0,00"                                               
               tt-dados.celula-alinhamento-horizontal = 5                                               
               tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(tt-work.quantidade[14])
                                                                     ELSE STRING(tt-work.valor[14]).    
        ASSIGN i-lin-excel = i-lin-excel + 1.
    END.
    
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num   = 1
           tt-dados.planilha-num  = 1
           tt-dados.celula-coluna = 2
           tt-dados.celula-cor-interior = 5
           tt-dados.celula-linha  = i-lin-excel
           tt-dados.celula-valor  = "Total"
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num    = 1
           tt-dados.planilha-num   = 1
           tt-dados.celula-coluna  = 3
           tt-dados.celula-cor-interior = 5
           tt-dados.celula-linha   = i-lin-excel
           tt-dados.celula-formato = "###.###.##0,00"
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(de-tot-qtd[1])
                                                                 ELSE STRING(de-tot-vlr[1])
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num    = 1
           tt-dados.planilha-num   = 1
           tt-dados.celula-coluna  = 4
           tt-dados.celula-cor-interior = 5
           tt-dados.celula-linha   = i-lin-excel
           tt-dados.celula-formato = "###.###.##0,00"
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(de-tot-qtd[2])
                                                                 ELSE STRING(de-tot-vlr[2])
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
     CREATE tt-dados.
     ASSIGN tt-dados.arquivo-num    = 1
            tt-dados.planilha-num   = 1
            tt-dados.celula-coluna  = 5
            tt-dados.celula-cor-interior = 5
            tt-dados.celula-linha   = i-lin-excel
            tt-dados.celula-formato = "###.###.##0,00"
            tt-dados.celula-alinhamento-horizontal = 5
            tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(de-tot-qtd[3])
                                                                  ELSE STRING(de-tot-vlr[3])
            tt-dados.celula-fonte-negrito = YES
            tt-dados.celula-fonte-cor = 2.
    CREATE tt-dados.                                                                               
    ASSIGN tt-dados.arquivo-num    = 1                                                             
           tt-dados.planilha-num   = 1                                                             
           tt-dados.celula-coluna  = 6
           tt-dados.celula-cor-interior = 5
           tt-dados.celula-linha   = i-lin-excel                                                   
           tt-dados.celula-formato = "###.###.##0,00"                                              
           tt-dados.celula-alinhamento-horizontal = 5                                              
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(de-tot-qtd[4])
                                                                 ELSE STRING(de-tot-vlr[4])
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.    
    CREATE tt-dados.                                                                               
    ASSIGN tt-dados.arquivo-num    = 1                                                             
           tt-dados.planilha-num   = 1                                                             
           tt-dados.celula-coluna  = 7
           tt-dados.celula-cor-interior = 5
           tt-dados.celula-linha   = i-lin-excel                                                   
           tt-dados.celula-formato = "###.###.##0,00"                                              
           tt-dados.celula-alinhamento-horizontal = 5                                              
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(de-tot-qtd[5])
                                                                 ELSE STRING(de-tot-vlr[5])
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.    
    CREATE tt-dados.                                                                               
    ASSIGN tt-dados.arquivo-num    = 1                                                             
           tt-dados.planilha-num   = 1                                                             
           tt-dados.celula-coluna  = 8
           tt-dados.celula-cor-interior = 5
           tt-dados.celula-linha   = i-lin-excel                                                   
           tt-dados.celula-formato = "###.###.##0,00"                                              
           tt-dados.celula-alinhamento-horizontal = 5                                              
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(de-tot-qtd[6])
                                                                 ELSE STRING(de-tot-vlr[6])
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.    
    CREATE tt-dados.                                                                               
    ASSIGN tt-dados.arquivo-num    = 1                                                             
           tt-dados.planilha-num   = 1                                                             
           tt-dados.celula-coluna  = 9
           tt-dados.celula-cor-interior = 5
           tt-dados.celula-linha   = i-lin-excel                                                   
           tt-dados.celula-formato = "###.###.##0,00"                                              
           tt-dados.celula-alinhamento-horizontal = 5                                              
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(de-tot-qtd[7])
                                                                 ELSE STRING(de-tot-vlr[7])
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.    
    
    CREATE tt-dados.                                                                               
    ASSIGN tt-dados.arquivo-num    = 1                                                             
           tt-dados.planilha-num   = 1                                                             
           tt-dados.celula-coluna  = 10
           tt-dados.celula-cor-interior = 5
           tt-dados.celula-linha   = i-lin-excel                                                   
           tt-dados.celula-formato = "###.###.##0,00"                                              
           tt-dados.celula-alinhamento-horizontal = 5                                              
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(de-tot-qtd[8])
                                                                 ELSE STRING(de-tot-vlr[8])
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.    
    CREATE tt-dados.                                                                               
    ASSIGN tt-dados.arquivo-num    = 1                                                             
           tt-dados.planilha-num   = 1                                                             
           tt-dados.celula-coluna  = 11
           tt-dados.celula-cor-interior = 5
           tt-dados.celula-linha   = i-lin-excel                                                   
           tt-dados.celula-formato = "###.###.##0,00"                                              
           tt-dados.celula-alinhamento-horizontal = 5                                              
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(de-tot-qtd[9])
                                                                 ELSE STRING(de-tot-vlr[9])
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.    
    CREATE tt-dados.                                                                                
    ASSIGN tt-dados.arquivo-num    = 1                                                              
           tt-dados.planilha-num   = 1                                                              
           tt-dados.celula-coluna  = 12
           tt-dados.celula-cor-interior = 5
           tt-dados.celula-linha   = i-lin-excel                                                    
           tt-dados.celula-formato = "###.###.##0,00"                                               
           tt-dados.celula-alinhamento-horizontal = 5                                               
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(de-tot-qtd[10]) 
                                                                 ELSE STRING(de-tot-vlr[10])
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.     
    CREATE tt-dados.                                                                                
    ASSIGN tt-dados.arquivo-num    = 1                                                              
           tt-dados.planilha-num   = 1                                                              
           tt-dados.celula-coluna  = 13
           tt-dados.celula-cor-interior = 5
           tt-dados.celula-linha   = i-lin-excel                                                    
           tt-dados.celula-formato = "###.###.##0,00"                                               
           tt-dados.celula-alinhamento-horizontal = 5                                               
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(de-tot-qtd[11])
                                                                 ELSE STRING(de-tot-vlr[11])
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.    
    CREATE tt-dados.                                                                                
    ASSIGN tt-dados.arquivo-num    = 1                                                              
           tt-dados.planilha-num   = 1                                                              
           tt-dados.celula-coluna  = 14
           tt-dados.celula-cor-interior = 5
           tt-dados.celula-linha   = i-lin-excel                                                    
           tt-dados.celula-formato = "###.###.##0,00"                                               
           tt-dados.celula-alinhamento-horizontal = 5                                               
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(de-tot-qtd[12])
                                                                 ELSE STRING(de-tot-vlr[12])
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.    
    CREATE tt-dados.                                                                                 
    ASSIGN tt-dados.arquivo-num    = 1                                                               
           tt-dados.planilha-num   = 1                                                               
           tt-dados.celula-coluna  = 15
           tt-dados.celula-cor-interior = 5
           tt-dados.celula-linha   = i-lin-excel                                                     
           tt-dados.celula-formato = "###.###.##0,00"                                                
           tt-dados.celula-alinhamento-horizontal = 5                                                
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(de-tot-qtd[13]) 
                                                                 ELSE STRING(de-tot-vlr[13])
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.     
    
    CREATE tt-dados.                                                                                
    ASSIGN tt-dados.arquivo-num    = 1                                                              
           tt-dados.planilha-num   = 1                                                              
           tt-dados.celula-coluna  = 16
           tt-dados.celula-cor-interior = 5
           tt-dados.celula-linha   = i-lin-excel                                                    
           tt-dados.celula-formato = "###.###.##0,00"                                               
           tt-dados.celula-alinhamento-horizontal = 5                                               
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(de-tot-qtd[14])
                                                                 ELSE STRING(de-tot-vlr[14])
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.    
    ASSIGN i-lin-excel = i-lin-excel + 1.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num   = 1
           tt-dados.planilha-num  = 1
           tt-dados.celula-coluna = 2
           tt-dados.celula-cor-interior = 20
           tt-dados.celula-linha  = i-lin-excel
           tt-dados.celula-valor  = "M‚dia"
           tt-dados.celula-fonte-negrito = YES.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num    = 1
           tt-dados.planilha-num   = 1
           tt-dados.celula-coluna  = 3
           tt-dados.celula-cor-interior = 20
           tt-dados.celula-linha   = i-lin-excel
           tt-dados.celula-formato = "###.###.##0,00"
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 
                                        THEN STRING(IF de-tot-qtd[1] > 0 THEN de-tot-qtd[1] / ACCUM COUNT tt-work.it-codigo ELSE 0)
                                        ELSE STRING(IF de-tot-vlr[1] > 0 THEN de-tot-vlr[1] / ACCUM COUNT tt-work.it-codigo ELSE 0)
           tt-dados.celula-fonte-negrito = YES.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num    = 1
           tt-dados.planilha-num   = 1
           tt-dados.celula-coluna  = 4
           tt-dados.celula-cor-interior = 20
           tt-dados.celula-linha   = i-lin-excel
           tt-dados.celula-formato = "###.###.##0,00"
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 
                                        THEN STRING(IF de-tot-qtd[2] > 0 THEN de-tot-qtd[2] / ACCUM COUNT tt-work.it-codigo ELSE 0) 
                                        ELSE STRING(IF de-tot-vlr[2] > 0 THEN de-tot-vlr[2] / ACCUM COUNT tt-work.it-codigo ELSE 0)
           tt-dados.celula-fonte-negrito = YES.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num    = 1
           tt-dados.planilha-num   = 1
           tt-dados.celula-coluna  = 5
           tt-dados.celula-cor-interior = 20
           tt-dados.celula-linha   = i-lin-excel
           tt-dados.celula-formato = "###.###.##0,00"
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 
                                        THEN STRING(IF de-tot-qtd[3] > 0 THEN de-tot-qtd[3] / ACCUM COUNT tt-work.it-codigo ELSE 0) 
                                        ELSE STRING(IF de-tot-vlr[3] > 0 THEN de-tot-vlr[3] / ACCUM COUNT tt-work.it-codigo ELSE 0)
           tt-dados.celula-fonte-negrito = YES.
    CREATE tt-dados.                                                                               
    ASSIGN tt-dados.arquivo-num    = 1                                                             
           tt-dados.planilha-num   = 1                                                             
           tt-dados.celula-coluna  = 6
           tt-dados.celula-cor-interior = 20
           tt-dados.celula-linha   = i-lin-excel                                                   
           tt-dados.celula-formato = "###.###.##0,00"                                              
           tt-dados.celula-alinhamento-horizontal = 5                                              
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 
                                        THEN STRING(IF de-tot-qtd[4] > 0 THEN de-tot-qtd[4] / ACCUM COUNT tt-work.it-codigo ELSE 0) 
                                        ELSE STRING(IF de-tot-vlr[4] > 0 THEN de-tot-vlr[4] / ACCUM COUNT tt-work.it-codigo ELSE 0)
           tt-dados.celula-fonte-negrito = YES.   
    CREATE tt-dados.                                                                               
    ASSIGN tt-dados.arquivo-num    = 1                                                             
           tt-dados.planilha-num   = 1                                                             
           tt-dados.celula-coluna  = 7
           tt-dados.celula-cor-interior = 20
           tt-dados.celula-linha   = i-lin-excel                                                   
           tt-dados.celula-formato = "###.###.##0,00"                                              
           tt-dados.celula-alinhamento-horizontal = 5                                              
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 
                                        THEN STRING(IF de-tot-qtd[5] > 0 THEN de-tot-qtd[5] / ACCUM COUNT tt-work.it-codigo ELSE 0) 
                                        ELSE STRING(IF de-tot-vlr[5] > 0 THEN de-tot-vlr[5] / ACCUM COUNT tt-work.it-codigo ELSE 0)
           tt-dados.celula-fonte-negrito = YES.  
    CREATE tt-dados.                                                                               
    ASSIGN tt-dados.arquivo-num    = 1                                                             
           tt-dados.planilha-num   = 1                                                             
           tt-dados.celula-coluna  = 8
           tt-dados.celula-cor-interior = 20
           tt-dados.celula-linha   = i-lin-excel                                                   
           tt-dados.celula-formato = "###.###.##0,00"                                              
           tt-dados.celula-alinhamento-horizontal = 5                                              
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 
                                        THEN STRING(IF de-tot-qtd[6] > 0 THEN de-tot-qtd[6] / ACCUM COUNT tt-work.it-codigo ELSE 0) 
                                        ELSE STRING(IF de-tot-vlr[6] > 0 THEN de-tot-vlr[6] / ACCUM COUNT tt-work.it-codigo ELSE 0)
           tt-dados.celula-fonte-negrito = YES.   
    CREATE tt-dados.                                                                               
    ASSIGN tt-dados.arquivo-num    = 1                                                             
           tt-dados.planilha-num   = 1                                                             
           tt-dados.celula-coluna  = 9
           tt-dados.celula-cor-interior = 20
           tt-dados.celula-linha   = i-lin-excel                                                   
           tt-dados.celula-formato = "###.###.##0,00"                                              
           tt-dados.celula-alinhamento-horizontal = 5                                              
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 
                                        THEN STRING(IF de-tot-qtd[7] > 0 THEN de-tot-qtd[7] / ACCUM COUNT tt-work.it-codigo ELSE 0) 
                                        ELSE STRING(IF de-tot-vlr[7] > 0 THEN de-tot-vlr[7] / ACCUM COUNT tt-work.it-codigo ELSE 0)
           tt-dados.celula-fonte-negrito = YES.   
    
    CREATE tt-dados.                                                                               
    ASSIGN tt-dados.arquivo-num    = 1                                                             
           tt-dados.planilha-num   = 1                                                             
           tt-dados.celula-coluna  = 10
           tt-dados.celula-cor-interior = 20
           tt-dados.celula-linha   = i-lin-excel                                                   
           tt-dados.celula-formato = "###.###.##0,00"                                              
           tt-dados.celula-alinhamento-horizontal = 5                                              
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 
                                        THEN STRING(IF de-tot-qtd[8] > 0 THEN de-tot-qtd[8] / ACCUM COUNT tt-work.it-codigo ELSE 0) 
                                        ELSE STRING(IF de-tot-vlr[8] > 0 THEN de-tot-vlr[8] / ACCUM COUNT tt-work.it-codigo ELSE 0)
           tt-dados.celula-fonte-negrito = YES.   
    CREATE tt-dados.                                                                               
    ASSIGN tt-dados.arquivo-num    = 1                                                             
           tt-dados.planilha-num   = 1                                                             
           tt-dados.celula-coluna  = 11
           tt-dados.celula-cor-interior = 20
           tt-dados.celula-linha   = i-lin-excel                                                   
           tt-dados.celula-formato = "###.###.##0,00"                                              
           tt-dados.celula-alinhamento-horizontal = 5                                              
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 
                                        THEN STRING(IF de-tot-qtd[9] > 0 THEN de-tot-qtd[9] / ACCUM COUNT tt-work.it-codigo ELSE 0) 
                                        ELSE STRING(IF de-tot-vlr[9] > 0 THEN de-tot-vlr[9] / ACCUM COUNT tt-work.it-codigo ELSE 0)
           tt-dados.celula-fonte-negrito = YES.  
    CREATE tt-dados.                                                                                
    ASSIGN tt-dados.arquivo-num    = 1                                                              
           tt-dados.planilha-num   = 1                                                              
           tt-dados.celula-coluna  = 12
           tt-dados.celula-cor-interior = 20
           tt-dados.celula-linha   = i-lin-excel                                                    
           tt-dados.celula-formato = "###.###.##0,00"                                               
           tt-dados.celula-alinhamento-horizontal = 5                                               
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 
                                        THEN STRING(IF de-tot-qtd[10] > 0 THEN de-tot-qtd[10] / ACCUM COUNT tt-work.it-codigo ELSE 0) 
                                        ELSE STRING(IF de-tot-vlr[10] > 0 THEN de-tot-vlr[10] / ACCUM COUNT tt-work.it-codigo ELSE 0)
           tt-dados.celula-fonte-negrito = YES.   
    CREATE tt-dados.                                                                                
    ASSIGN tt-dados.arquivo-num    = 1                                                              
           tt-dados.planilha-num   = 1                                                              
           tt-dados.celula-coluna  = 13
           tt-dados.celula-cor-interior = 20
           tt-dados.celula-linha   = i-lin-excel                                                    
           tt-dados.celula-formato = "###.###.##0,00"                                               
           tt-dados.celula-alinhamento-horizontal = 5                                               
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 
                                        THEN STRING(IF de-tot-qtd[11] > 0 THEN de-tot-qtd[11] / ACCUM COUNT tt-work.it-codigo ELSE 0) 
                                        ELSE STRING(IF de-tot-vlr[11] > 0 THEN de-tot-vlr[11] / ACCUM COUNT tt-work.it-codigo ELSE 0)
           tt-dados.celula-fonte-negrito = YES.  
    CREATE tt-dados.                                                                                
    ASSIGN tt-dados.arquivo-num    = 1                                                              
           tt-dados.planilha-num   = 1                                                              
           tt-dados.celula-coluna  = 14
           tt-dados.celula-cor-interior = 20
           tt-dados.celula-linha   = i-lin-excel                                                    
           tt-dados.celula-formato = "###.###.##0,00"                                               
           tt-dados.celula-alinhamento-horizontal = 5                                               
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 
                                        THEN STRING(IF de-tot-qtd[12] > 0 THEN de-tot-qtd[12] / ACCUM COUNT tt-work.it-codigo ELSE 0) 
                                        ELSE STRING(IF de-tot-vlr[12] > 0 THEN de-tot-vlr[12] / ACCUM COUNT tt-work.it-codigo ELSE 0)
           tt-dados.celula-fonte-negrito = YES.
    CREATE tt-dados.                                                                                 
    ASSIGN tt-dados.arquivo-num    = 1                                                               
           tt-dados.planilha-num   = 1                                                               
           tt-dados.celula-coluna  = 15
           tt-dados.celula-cor-interior = 20
           tt-dados.celula-linha   = i-lin-excel                                                     
           tt-dados.celula-formato = "###.###.##0,00"                                                
           tt-dados.celula-alinhamento-horizontal = 5                                                
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 
                                        THEN STRING(IF de-tot-qtd[13] > 0 THEN de-tot-qtd[13] / ACCUM COUNT tt-work.it-codigo ELSE 0) 
                                        ELSE STRING(IF de-tot-vlr[13] > 0 THEN de-tot-vlr[13] / ACCUM COUNT tt-work.it-codigo ELSE 0)
           tt-dados.celula-fonte-negrito = YES.    
    CREATE tt-dados.                                                                                
    ASSIGN tt-dados.arquivo-num    = 1                                                              
           tt-dados.planilha-num   = 1                                                              
           tt-dados.celula-coluna  = 16
           tt-dados.celula-cor-interior = 20
           tt-dados.celula-linha   = i-lin-excel                                                    
           tt-dados.celula-formato = "###.###.##0,00"                                               
           tt-dados.celula-alinhamento-horizontal = 5                                               
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 
                                        THEN STRING(IF de-tot-qtd[14] > 0 THEN de-tot-qtd[14] / ACCUM COUNT tt-work.it-codigo ELSE 0) 
                                        ELSE STRING(IF de-tot-vlr[14] > 0 THEN de-tot-vlr[14] / ACCUM COUNT tt-work.it-codigo ELSE 0)
           tt-dados.celula-fonte-negrito = YES.  
    ASSIGN i-lin-excel = i-lin-excel + 1.
    
    RUN utp/utapi003.p (INPUT-OUTPUT TABLE tt-configuracao,
                        INPUT-OUTPUT TABLE tt-planilha,
                        INPUT-OUTPUT TABLE tt-dados,
                        INPUT-OUTPUT TABLE tt-grafico,
                        INPUT-OUTPUT TABLE tt-erros).
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i} 
run pi-finalizar in h-acomp.
return "OK":U.

