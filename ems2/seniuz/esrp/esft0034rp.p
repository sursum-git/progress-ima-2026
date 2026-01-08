/* Programa: ESFT0034
** Sistema.: EMS 2.04 da Datasul
** Modulo..: Faturamento - e-Vendas da Tear Tˆxtil
** Objetivo: Relatorio Rank de Vendas por Clientes
** Autor...: Gilvando Souza Araujo
** Data....: 10/08/2005
** Obs.....: Programa especifico da TEAR TÒXTIL INDéSTRIA E COMRCIO LTDA.
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0034RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param  NO-UNDO
       FIELD destino           AS INTEGER
       FIELD arquivo           AS CHAR FORMAT "x(35)"
       FIELD usuario           AS CHAR FORMAT "x(12)"
       FIELD data-exec         AS DATE
       FIELD hora-exec         AS INTEGER
       FIELD classifica        AS INTEGER
       FIELD desc-classifica   AS CHAR FORMAT "x(40)"
       FIELD ini-periodo       AS CHAR FORMAT "99/9999"
       FIELD fin-periodo       AS CHAR FORMAT "99/9999"
       FIELD ini-it-codigo     LIKE item.it-codigo
       FIELD fin-it-codigo     LIKE item.it-codigo
       FIELD ini-nome-abrev    LIKE nota-fiscal.nome-abrev
       FIELD fin-nome-abrev    LIKE nota-fiscal.nome-abrev
       FIELD ini-no-ab-reppri  LIKE nota-fiscal.no-ab-reppri
       FIELD fin-no-ab-reppri  LIKE nota-fiscal.no-ab-reppri
       FIELD tipo-inform       AS   INT.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita AS RAW.

DEF TEMP-TABLE tt-work
    FIELD cod-emitente  LIKE nota-fiscal.cod-emitente
    FIELD valor         AS DEC FORMAT ">>>,>>>,>>9.99" EXTENT 14
    FIELD quantidade    AS DEC FORMAT ">>>,>>>,>>9.99" EXTENT 14
    INDEX ch-work cod-emitente.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i} 

/* Includes para ImpressÆo de Observa‡oes */
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
DEF VAR i-col-excel AS INTEGER INITIAL 1.
DEF VAR i-col       AS INTEGER.

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
{utp/ut-liter.i Rank_do_Faturamento_por_Clientes * r}
assign c-titulo-relat = trim(return-value).

IF tt-param.destino <> 2 THEN DO.
   VIEW FRAME f-cabec.
   VIEW FRAME f-rodape.
END.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

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
      ASSIGN c-repres = repres.nome.
   ELSE
      ASSIGN c-repres = "".
END.
ELSE
   ASSIGN c-repres = tt-param.ini-no-ab-reppri + " a " + tt-param.fin-no-ab-reppri.

{utp/utapi003.i}

FOR EACH nota-fiscal WHERE
         nota-fiscal.nome-ab-cli >= tt-param.ini-nome-abrev AND
         nota-fiscal.nome-ab-cli <= tt-param.fin-nome-abrev AND
         nota-fiscal.dt-emis-nota >= dt-emis-ini AND
         nota-fiscal.dt-emis-nota <= dt-emis-fin AND 
         nota-fiscal.cod-estabel = '2' AND
         nota-fiscal.serie = '1' AND
         nota-fiscal.no-ab-reppri >= tt-param.ini-no-ab-reppri AND
         nota-fiscal.no-ab-reppri <= tt-param.fin-no-ab-reppri AND
         nota-fiscal.emite-duplic =  YES AND
         nota-fiscal.dt-cancela = ? NO-LOCK USE-INDEX ch-clinota,
    EACH it-nota-fisc OF nota-fiscal WHERE
         it-nota-fisc.it-codigo    >= tt-param.ini-it-codigo AND
         it-nota-fisc.it-codigo    <= tt-param.fin-it-codigo NO-LOCK.

    RUN pi-acompanhar IN h-acomp (INPUT "Nota Fiscal: " + it-nota-fisc.nr-nota-fis).

    /*------ Conversao de Kg para M ------- */
    IF it-nota-fisc.un-fatur[1] <> "m" THEN DO:
       FIND item-ext WHERE
            item-ext.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
       IF AVAIL item-ext then
          ASSIGN de-qt-conv = it-nota-fisc.qt-faturada[1] * 
                              item-ext.fator-conv.
    END.
    ELSE
       ASSIGN de-qt-conv = it-nota-fisc.qt-faturada[1].

    DO i-cont = 1 TO i-cont-mes:
       IF c-cab-mes[i-cont] = STRING(MONTH(it-nota-fisc.dt-emis-nota),"99") + "/" + 
                              STRING(YEAR(it-nota-fisc.dt-emis-nota),"9999") THEN DO:
          ASSIGN i-mes = i-cont.
          LEAVE.
       END.
    END.

    FIND tt-work WHERE
         tt-work.cod-emitente = nota-fiscal.cod-emitente NO-ERROR.
    IF NOT AVAIL tt-work THEN DO:
       CREATE tt-work.
       ASSIGN tt-work.cod-emitente = nota-fiscal.cod-emitente.
    END.
    
    ASSIGN tt-work.valor[i-mes]      = tt-work.valor[i-mes] + it-nota-fisc.vl-tot-item
           tt-work.quantidade[i-mes] = tt-work.quantidade[i-mes] + de-qt-conv
           tt-work.valor[13]         = tt-work.valor[13] + it-nota-fisc.vl-tot-item
           tt-work.quantidade[13]    = tt-work.quantidade[13] + de-qt-conv
           tt-work.valor[14]         = IF tt-work.valor[13] > 0 THEN tt-work.valor[13] / i-cont-mes ELSE 0
           tt-work.quantidade[14]    = IF tt-work.quantidade[13] > 0 THEN tt-work.quantidade[13] / i-cont-mes ELSE 0.
END.

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

   PUT "Cliente"     AT 1
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
       FIRST emitente WHERE
             emitente.cod-emitente = tt-work.cod-emitente NO-LOCK
       BREAK BY IF tt-param.classifica = 1
                THEN string(tt-work.valor[14],">>>,>>9.99")
                ELSE IF tt-param.classifica = 2
                     THEN string(tt-work.quantidade[14],">>>,>>9.99")
                     ELSE emitente.nome-abrev.

       PUT emitente.nome-abrev.

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
          tt-configuracao.exibir-construcao   = NO 
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
          tt-dados.celula-valor  = "RANKING DE VENDAS POR CLIENTES - Per¡odo: " + 
          STRING(dt-emis-ini) + " a " + STRING(dt-emis-fin) + " - Em Quantidades - Metros e Valores - R$"
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
          tt-dados.celula-valor  = "Cliente"
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

    ASSIGN i-col-excel = 3.
    DO i-cont = 1 TO i-cont-mes:
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 1
              tt-dados.celula-coluna  = i-col-excel
              tt-dados.celula-cor-interior = 10
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor   = c-cab-mes[i-cont]
              tt-dados.celula-fonte-negrito = YES
              tt-dados.celula-fonte-cor = 2.
       ASSIGN i-col-excel = i-col-excel + 1.
    END.

    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num   = 1
           tt-dados.planilha-num  = 1
           tt-dados.celula-coluna = i-col-excel
           tt-dados.celula-cor-interior = 10
           tt-dados.celula-linha = i-lin-excel
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor = "Total"
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    ASSIGN i-col-excel = i-col-excel + 1.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num   = 1
           tt-dados.planilha-num  = 1
           tt-dados.celula-coluna = i-col-excel
           tt-dados.celula-cor-interior = 10
           tt-dados.celula-linha = i-lin-excel
           tt-dados.celula-alinhamento-horizontal = 5
           tt-dados.celula-valor = "M‚dia"
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    ASSIGN i-lin-excel = i-lin-excel + 1.

    FOR EACH tt-work BY  tt-work.quantidade[14] DESCENDING:
        ACCUMULATE tt-work.cod-emitente(COUNT).
        DO i-cont = 1 TO 13:
           ASSIGN de-tot-qtd[i-cont] = de-tot-qtd[i-cont] + tt-work.quantidade[i-cont]
                  de-tot-vlr[i-cont] = de-tot-vlr[i-cont] + tt-work.valor[i-cont].
        END.
        ASSIGN de-tot-qtd[14] = IF de-tot-qtd[13] > 0 THEN de-tot-qtd[13] / i-cont-mes ELSE 0
               de-tot-vlr[14] = IF de-tot-vlr[13] > 0 THEN de-tot-vlr[13] / i-cont-mes ELSE 0.
    
        FIND emitente WHERE emitente.cod-emitente = tt-work.cod-emitente NO-LOCK NO-ERROR.
        
        CREATE tt-dados.
        ASSIGN tt-dados.arquivo-num   = 1
               tt-dados.planilha-num  = 1
               tt-dados.celula-coluna = 1
               tt-dados.celula-linha  = i-lin-excel
               tt-dados.celula-valor  = string(tt-work.cod-emitente).
        CREATE tt-dados.
        ASSIGN tt-dados.arquivo-num   = 1
               tt-dados.planilha-num  = 1
               tt-dados.celula-coluna = 2
               tt-dados.celula-linha  = i-lin-excel
               tt-dados.celula-valor  = emitente.nome-emit.
    
        /* Dados dos meses */
        ASSIGN i-col-excel = 3.
        DO i-cont = 1 TO i-cont-mes:
           CREATE tt-dados.
           ASSIGN tt-dados.arquivo-num    = 1
                  tt-dados.planilha-num   = 1
                  tt-dados.celula-coluna  = i-col-excel
                  tt-dados.celula-linha   = i-lin-excel
                  tt-dados.celula-formato = "###.###.##0,00"
                  tt-dados.celula-alinhamento-horizontal = 5
                  tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(tt-work.quantidade[i-cont])
                                                                        ELSE STRING(tt-work.valor[i-cont]).
           ASSIGN i-col-excel = i-col-excel + 1.
        END.

        /* total */
        CREATE tt-dados.                                                                                 
        ASSIGN tt-dados.arquivo-num    = 1                                                               
               tt-dados.planilha-num   = 1                                                               
               tt-dados.celula-coluna  = i-col-excel                                                              
               tt-dados.celula-linha   = i-lin-excel                                                     
               tt-dados.celula-formato = "###.###.##0,00"                                                
               tt-dados.celula-alinhamento-horizontal = 5                                                
               tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(tt-work.quantidade[13]) 
                                                                     ELSE STRING(tt-work.valor[13]).     
        ASSIGN i-col-excel = i-col-excel + 1.

        /* M‚dia */
        CREATE tt-dados.                                                                                
        ASSIGN tt-dados.arquivo-num    = 1                                                              
               tt-dados.planilha-num   = 1                                                              
               tt-dados.celula-coluna  = i-col-excel                                                             
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
    ASSIGN i-col-excel = 3.
    DO i-cont = 1 TO i-cont-mes:
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 1
              tt-dados.celula-coluna  = i-col-excel
              tt-dados.celula-cor-interior = 5
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(de-tot-qtd[i-cont])
                                                                    ELSE STRING(de-tot-vlr[i-cont])
              tt-dados.celula-fonte-negrito = YES
              tt-dados.celula-fonte-cor = 2.
       ASSIGN i-col-excel = i-col-excel + 1.
    END.
    CREATE tt-dados.                                                                                 
    ASSIGN tt-dados.arquivo-num    = 1                                                               
           tt-dados.planilha-num   = 1                                                               
           tt-dados.celula-coluna  = i-col-excel
           tt-dados.celula-cor-interior = 5
           tt-dados.celula-linha   = i-lin-excel                                                     
           tt-dados.celula-formato = "###.###.##0,00"                                                
           tt-dados.celula-alinhamento-horizontal = 5                                                
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 THEN STRING(de-tot-qtd[13]) 
                                                                 ELSE STRING(de-tot-vlr[13])
           tt-dados.celula-fonte-negrito = YES
           tt-dados.celula-fonte-cor = 2.
    
    ASSIGN i-col-excel = i-col-excel + 1.
    CREATE tt-dados.                                                                                
    ASSIGN tt-dados.arquivo-num    = 1                                                              
           tt-dados.planilha-num   = 1                                                              
           tt-dados.celula-coluna  = i-col-excel
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
    ASSIGN i-col-excel = 3.
    DO i-cont = 1 TO i-cont-mes:
       CREATE tt-dados.
       ASSIGN tt-dados.arquivo-num    = 1
              tt-dados.planilha-num   = 1
              tt-dados.celula-coluna  = i-col-excel
              tt-dados.celula-cor-interior = 20
              tt-dados.celula-linha   = i-lin-excel
              tt-dados.celula-formato = "###.###.##0,00"
              tt-dados.celula-alinhamento-horizontal = 5
              tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 
                                        THEN STRING(IF de-tot-qtd[i-cont] > 0 THEN de-tot-qtd[i-cont] / ACCUM COUNT tt-work.cod-emitente ELSE 0)
                                        ELSE STRING(IF de-tot-vlr[i-cont] > 0 THEN de-tot-vlr[i-cont] / ACCUM COUNT tt-work.cod-emitente ELSE 0)
              tt-dados.celula-fonte-negrito = YES.
       ASSIGN i-col-excel = i-col-excel + 1.
    END.
    CREATE tt-dados.
    ASSIGN tt-dados.arquivo-num    = 1                                                               
           tt-dados.planilha-num   = 1                                                               
           tt-dados.celula-coluna  = i-col-excel
           tt-dados.celula-cor-interior = 20
           tt-dados.celula-linha   = i-lin-excel                                                     
           tt-dados.celula-formato = "###.###.##0,00"                                                
           tt-dados.celula-alinhamento-horizontal = 5                                                
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 
                                        THEN STRING(IF de-tot-qtd[13] > 0 THEN de-tot-qtd[13] / ACCUM COUNT tt-work.cod-emitente ELSE 0) 
                                        ELSE STRING(IF de-tot-vlr[13] > 0 THEN de-tot-vlr[13] / ACCUM COUNT tt-work.cod-emitente ELSE 0)
           tt-dados.celula-fonte-negrito = YES.
    ASSIGN i-col-excel = i-col-excel + 1.
    CREATE tt-dados.                                                                                
    ASSIGN tt-dados.arquivo-num    = 1                                                              
           tt-dados.planilha-num   = 1                                                              
           tt-dados.celula-coluna  = i-col-excel
           tt-dados.celula-cor-interior = 20
           tt-dados.celula-linha   = i-lin-excel                                                    
           tt-dados.celula-formato = "###.###.##0,00"                                               
           tt-dados.celula-alinhamento-horizontal = 5                                               
           tt-dados.celula-valor   = IF tt-param.tipo-inform = 1 
                                        THEN STRING(IF de-tot-qtd[14] > 0 THEN de-tot-qtd[14] / ACCUM COUNT tt-work.cod-emitente ELSE 0) 
                                        ELSE STRING(IF de-tot-vlr[14] > 0 THEN de-tot-vlr[14] / ACCUM COUNT tt-work.cod-emitente ELSE 0)
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
RUN pi-finalizar in h-acomp.
RETURN "OK":U.

