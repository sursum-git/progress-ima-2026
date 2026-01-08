DEFINE TEMP-TABLE tt
    FIELD mes               AS INT
    FIELD conta             AS CHAR FORMAT 'X(8)'
    FIELD saldo_inicial     AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD debito            AS DECIMAL        FORMAT "->>>,>>>,>>9.99"
    FIELD credito           AS DECIMAL       FORMAT "->>>,>>>,>>9.99"
    FIELD saldo_final       AS DECIMAL   FORMAT "->>>,>>>,>>9.99"
    FIELD forcado AS LOGICAL INIT NO.


DEFINE VARIABLE dAcumSI         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dAcumDB         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dAcumCR         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dAcumSF         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cSinal          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dNovoValor      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE mesCorrente     AS INTEGER     NO-UNDO.

DEFINE TEMP-TABLE ttSaldoInicial
    FIELD conta         LIKE tt.conta
    FIELD saldo_inicial LIKE tt.saldo_inicial.

DEFINE TEMP-TABLE ttContaMes
    FIELD conta LIKE tt.conta
    FIELD mes   LIKE tt.mes.

DEFINE TEMP-TABLE ttConta
    FIELD conta LIKE tt.conta.

DEFINE TEMP-TABLE ttResultado
        FIELD conta AS CHAR FORMAT 'x(8)'
        FIELD mes   AS INT 
        FIELD saldo AS DECIMAL FORMAT "->>>,>>>,>>9.99"
        FIELD forcado AS LOGICAL INIT NO.
DEFINE VARIABLE i AS INTEGER     NO-UNDO.

DEFINE TEMP-TABLE ttAnalitico
       FIELD mes                AS INT
       FIELD debito             AS DECIMAL        FORMAT "->>>,>>>,>>9.99"         
       FIELD credito            AS DECIMAL        FORMAT "->>>,>>>,>>9.99"         
       FIELD saldo_inicial      AS DECIMAL        FORMAT "->>>,>>>,>>9.99"         
       FIELD saldo_final        AS DECIMAL        FORMAT "->>>,>>>,>>9.99"         
       FIELD conta              AS CHAR FORMAT 'x(8)'.


DEFINE TEMP-TABLE ttArquivo
    FIELD linha AS CHAR FORMAT "x(500)".

DEFINE TEMP-TABLE ttArquivo02 LIKE ttArquivo
    FIELD mes   AS INT.

DEFINE BUFFER bf FOR tt.
DEFINE BUFFER bfResultado FOR ttResultado.
DEFINE VARIABLE cLinha          AS CHARACTER   NO-UNDO FORMAT 'x(80)'.
DEFINE VARIABLE cLinha02        AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
/*DEFINE VARIABLE cCaminho        AS CHARACTER   NO-UNDO FORMAT 'x(50)' INIT 'p:\tadeu\ecf\med\v01\' LABEL "Caminho".*/
DEFINE VARIABLE cCaminho        AS CHARACTER   NO-UNDO FORMAT 'x(50)' INIT 'c:\temp\ecf\' LABEL "Caminho".
DEFINE VARIABLE cArquivoDados   AS CHARACTER   NO-UNDO FORMAT 'x(50)' LABEL "Arquivo Dados Balancete" INIT "dados.txt".
DEFINE VARIABLE cArquivoECF     AS CHARACTER   NO-UNDO FORMAT 'x(50)' LABEL "Arquivo ECF" INIT "ecf.txt".
UPDATE cCaminho
       cArquivoDados
       cArquivoECF 
       WITH 1 COL WIDTH 550.



INPUT FROM value( cCaminho + cArquivoDados).
REPEAT:
   
   CREATE ttAnalitico.
   IMPORT DELIMITER ";" ttAnalitico.
   
END.
INPUT CLOSE.
OUTPUT TO  value(cCaminho + 'LOG.txt').

/*FOR EACH ttAnalitico
    WHERE ttAnalitico.mes = 1:

END.*/

FOR EACH ttAnalitico
    WHERE ttAnalitico.conta <> '' 
    /*AND ttAnalitico.conta = '31101001'*/  BREAK BY ttAnalitico.conta BY ttAnalitico.mes:

    FIND FIRST ttConta
        WHERE ttConta.conta = ttAnalitico.conta
        NO-ERROR.
    IF NOT AVAIL ttConta THEN DO:
       CREATE ttConta.
       ASSIGN ttConta.conta = ttAnalitico.conta.
              
    END.

    FIND FIRST ttContaMes
        WHERE ttContaMes.conta = ttAnalitico.conta
        AND   ttContames.mes   = ttAnalitico.mes   NO-ERROR.
    IF NOT AVAIL ttContaMes THEN DO:
       CREATE ttContaMes.
       ASSIGN ttContaMes.conta = ttAnalitico.conta
              ttContames.mes   = ttAnalitico.mes.
    END.
    FIND FIRST tt
        WHERE tt.conta = ttAnalitico.conta
        AND   tt.mes   = ttAnalitico.mes NO-ERROR.

    FIND FIRST ttresultado
        WHERE ttResultado.conta = ttAnalitico.conta
        AND   ttResultado.mes   = ttAnalitico.mes NO-ERROR.
    
    IF FIRST-OF(ttAnalitico.conta) THEN DO:
       ASSIGN /*dAcumSI = 0  */
              dAcumDB = 0
              dAcumCR = 0 .
    END.
    
    CASE substr(ttAnalitico.conta,1,1):

        WHEN '1' OR WHEN '2' THEN DO:
           EXPORT DELIMITER "|" ttAnalitico.
           PUT "CONTA:" ttAnalitico.conta SKIP
               "MES:"   ttAnalitico.mes   SKIP 
               "SI ACUM:" dAcumSI SKIP
               /*"DB ACUM:" dAcumDB SKIP
               "CR ACUM:" dAcumCR SKIP*/  . 
           
           IF ttAnalitico.mes = 13 THEN NEXT.
          /* ASSIGN     dAcumSI = dAcumSI + ttAnalitico.saldo_inicial
                      dAcumDB = dAcumDB + ttAnalitico.debito
                      dAcumCR = dAcumCR + ttAnalitico.credito.*/

           IF NOT AVAIL tt THEN DO:
              CREATE tt.
              ASSIGN tt.conta = ttAnalitico.conta
                     tt.mes   = ttAnalitico.mes.
           END.
           FIND FIRST ttSaldoInicial
                   WHERE ttSaldoInicial.conta = tt.conta NO-ERROR.
           IF tt.saldo_inicial = 0 THEN DO:  
               IF AVAIL ttSaldoInicial THEN
                  ASSIGN tt.saldo_inicial = ttSaldoInicial.saldo_inicial.
               ELSE
                  PUT "conta sem tt saldo inicial:" tt.conta.
           END.
            /*ASSIGN tt.saldo_inicial =  dAcumSI.*/
           ASSIGN   tt.saldo_inicial =    tt.saldo_inicial + ttAnalitico.saldo_inicial   /*dAcumSI + tt.saldo_inicial*/
                    tt.debito        =    tt.debito  +  ttAnalitico.debito  /*tt.debito        + dAcumDB*/
                    tt.credito       =   tt.credito  +  ttAnalitico.credito /*tt.credito       + dAcumCR*/
                    /*tt.saldo_final   = tt.saldo_final   + tt.saldo_inicial + tt.debito + tt.credito*/  .
           PUT "saldo inicial:" tt.saldo_inicial SKIP
               "debito:" tt.debito SKIP
               "credito:" tt.credito SKIP .
        END.
          


        WHEN  '3' OR WHEN '4' THEN DO:

           IF ttAnalitico.mes = 12 THEN NEXT.
           /*ASSIGN     dAcumSI = dAcumSI + ttAnalitico.saldo_inicial
                      dAcumDB = dAcumDB + ttAnalitico.debito
                      dAcumCR = dAcumCR + ttAnalitico.credito.*/
/*            MESSAGE "Mes:" ttAnalitico.mes SKIP                  */
/*                    "valor DB do mˆs:"  ttAnalitico.debito  SKIP */
/*                    "valor CR do mˆs:"  ttAnalitico.credito SKIP */
/*                    "valor DB acum:"    dAcumDB  SKIP            */
/*                    "valor CR acum:"    dAcumCR  SKIP            */
/*                VIEW-AS ALERT-BOX INFO BUTTONS OK.               */
           IF NOT AVAIL ttresultado THEN DO:
              CREATE ttResultado.
              ASSIGN ttResultado.conta = ttAnalitico.conta.
           END.
           
           ASSIGN ttResultado.mes   = IF ttAnalitico.mes = 13 THEN 12 ELSE ttAnalitico.mes
                  ttResultado.saldo = ttResultado.saldo + ttAnalitico.debito + ttAnalitico.credito.
        END.
    END CASE.
    /*MESSAGE "Mes:" ttAnalitico.mes SKIP  
                     "conta:" ttAnalitico.conta SKIP
                   "valor DB do mes:"  ttAnalitico.debito  SKIP
                   "valor CR do mes:"  ttAnalitico.credito SKIP
                   "valor DB acum:"    dAcumDB SKIP
                   "valor CR acum:"   dAcumCR SKIP
                   "saldo inicial:"   tt.saldo_inicial SKIP
               VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
     IF LAST-OF(ttAnalitico.mes) THEN DO:
        CASE substr(ttAnalitico.conta,1,1):
             WHEN  '1' OR WHEN '2' THEN DO:
                IF ttAnalitico.mes = 1  THEN DO:
                    CREATE ttSaldoInicial.
                    ASSIGN ttSaldoInicial.conta = ttAnalitico.conta
                           ttSaldoInicial.saldo_inicial  = tt.saldo_inicial.
                END.
               /* ASSIGN dAcumdb =  dAcumdb + tt.debito
                dAcumcr =  dAcumcr + tt.credito.*/
             END.
        END CASE.
     END.

     IF LAST-OF(ttAnalitico.mes)AND LAST-OF(ttAnalitico.conta) THEN  DO:
         
        /*FIND FIRST bf
                    WHERE  bf.conta = ttAnalitico.conta
                     AND   bf.mes   = ttAnalitico.mes - 1 NO-ERROR.
                IF AVAIL bf THEN
                   ASSIGN tt.debito = tt.debito + bf.debito
                          tt.credito = tt.credito + bf.credito.*/


        CASE substr(ttAnalitico.conta,1,1):
             /*WHEN  '1' OR WHEN '2' THEN DO:
                 FIND FIRST ttSaldoInicial 
                     WHERE ttSaldoInicial.conta = ttAnalitico.conta NO-ERROR.
                    
                 IF ttAnalitico.mes < 12 THEN DO:
                    REPEAT i = ttAnalitico.mes + 1  TO 12:
                        CREATE tt.
                        ASSIGN tt.conta          = ttAnalitico.conta
                               tt.mes            = i
                               tt.saldo_inicial  = IF AVAIL ttSaldoInicial THEN ttsaldoInicial.saldo_inicial ELSE 0
                               tt.debito         = ttAnalitico.debito
                               tt.credito        = ttAnalitico.credito
                               tt.saldo_final    =  0
                               tt.forcado        = YES.
                     END.
                 END.

             END.*/ 
             /*WHEN  '3' OR WHEN '4' THEN DO:
                 IF ttAnalitico.mes < 13 THEN DO:
                    REPEAT i = ttAnalitico.mes + 1  TO 12:
                        CREATE ttResultado.
                        ASSIGN ttResultado.conta          = ttAnalitico.conta
                               ttResultado.mes            =  i
                               ttResultado.saldo          = ttResultado.saldo
                               ttResultado.forcado        = YES.
                     END.
                 END.

             END.*/

        END CASE.
     END.
END.
/*acumular debitos e creditos nos meses*/
FOR EACH tt 
    WHERE tt.mes <> 13 BREAK BY tt.conta BY tt.mes:
    IF FIRST-OF(tt.conta) THEN DO:
       ASSIGN dAcumdb = 0 
              dAcumCR = 0.
    END.
    ASSIGN dAcumdb = dAcumdb + tt.debito
           dAcumcr = dAcumcr + tt.credito.
    ASSIGN tt.debito = dAcumDb
           tt.credito = dAcumCR.
END.

FOR EACH ttResultado 
    /*WHERE ttResultado.mes <> 12*/ BREAK BY ttResultado.conta BY ttResultado.mes:
    IF FIRST-OF(ttResultado.conta) THEN DO:
       ASSIGN dAcumSF = 0.
    END.
    ASSIGN dAcumSF = dAcumSF + ttresultado.saldo
           ttresultado.saldo = dAcumSF.
END.

FOR EACH tt:
    ASSIGN tt.saldo_final = tt.saldo_inicial  + tt.debito + tt.credito.
END.

OUTPUT TO VALUE(cCaminho + "contames.txt").
FOR EACH ttcontaMes:
    EXPORT DELIMITER "|" ttContaMes.
END.
OUTPUT CLOSE.


OUTPUT TO VALUE(cCaminho + "saldoinicial.txt").
FOR EACH ttsaldoinicial:
    EXPORT DELIMITER "|" ttsaldoinicial.
END.
OUTPUT CLOSE.

REPEAT i = 1 TO 12:
    FOR EACH ttConta:
        FIND FIRST tt
            WHERE tt.conta = ttConta.conta NO-ERROR.
        IF AVAIL tt AND tt.mes > i THEN NEXT.
        FIND FIRST ttContaMes
            WHERE ttContaMes.mes = i
            AND   ttContaMes.conta = ttConta.conta NO-ERROR.

        IF NOT AVAIL ttContaMes THEN DO:
            /*MESSAGE i
                VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
           FIND FIRST tt
               WHERE tt.conta = ttConta.conta
               AND   tt.mes   = i - 1 NO-ERROR.
           IF AVAIL tt THEN DO:
              CREATE bf.
              BUFFER-COPY tt TO bf.
              ASSIGN bf.mes = i.
           END.
           /*ELSE DO:
              MESSAGE 'não achei tt.conta do mes anterior:' ttConta.conta
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.

           END.*/

           /*criar ttresultado tb*/
            FIND FIRST ttResultado
                WHERE ttResultado.conta = ttConta.conta
                AND   ttResultado.mes   = i - 1  NO-ERROR.
            IF AVAIL ttResultado THEN DO:
               CREATE bfResultado.
               BUFFER-COPY ttResultado TO bfResultado.
               ASSIGN ttResultado.mes = i.
            END.

        END.   
    END.    
END.


OUTPUT TO value(cCaminho + 'difsaldo.txt').
FOR EACH tt
    WHERE tt.mes = 12:
    IF tt.saldo_inicial = 0 AND tt.debito = 0 AND tt.credito = 0 AND tt.saldo_final = 0 THEN
       EXPORT DELIMITER "|" tt.

    CREATE bf.
    BUFFER-COPY tt TO bf.
    ASSIGN bf.mes = 0.

END.

FOR EACH ttResultado
    WHERE ttResultado.mes = 12:
    CREATE bfResultado.
    BUFFER-COPY  ttResultado TO bfResultado.
    ASSIGN bfREsultado.mes = 0.

END.

OUTPUT CLOSE.

OUTPUT TO value(cCaminho + 'espelho.txt').
FOR EACH tt:
    EXPORT DELIMITER "|" tt.
END.

OUTPUT CLOSE.

OUTPUT TO value(cCaminho + 'espelhoResultado.txt').
FOR EACH ttResultado:
    EXPORT DELIMITER "|" ttResultado.
END.

OUTPUT CLOSE.

INPUT FROM value(cCaminho + cArquivoECF).
REPEAT:
     IMPORT UNFORM cLinha02 .
     IF SUBSTR(cLinha02,2,4) <> "k155" AND SUBSTR(cLinha02,2,4) <> "k355" THEN DO:
        CREATE ttArquivo.
        ASSIGN ttArquivo.linha = cLinha02.
     END.
END.



INPUT CLOSE.


OUTPUT TO value( cCaminho + 'novo_Arquivo_prova.txt').
FOR EACH ttArquivo:
    PUT UNFORM ttArquivo.linha SKIP.
END.

OUTPUT CLOSE.



OUTPUT TO value(cCaminho + 'ecfnovo.txt').


FOR EACH ttArquivo:
    IF substr(ttArquivo.linha,2,4) <> 'k030' THEN
      PUT UNFORM ttArquivo.linha SKIP.
    ELSE DO:
      
       PUT UNFORM ttArquivo.linha SKIP.
       FOR EACH tt WHERE tt.conta <> "" 
             AND  substr(tt.conta,1,1) <> '3'
             AND  substr(tt.conta,1,1) <> '4' 
             AND  tt.mes = int(SUBSTR(ttArquivo.linha,26,2))  BY tt.conta:
             IF tt.credito < 0  THEN
                ASSIGN tt.credito = tt.credito * -1 .
            /* IF FIRST-OF(tt.mes) THEN DO:
                ASSIGN cLinha = "|K030|01012014|31122014|A" +  STRING(tt.mes,"99") + "|"  . 
                PUT UNFORMATTED   cLinha SKIP .
             END.*/
             ASSIGN cLinha = "|K155|" + tt.conta  + "|/001|" .
             IF tt.saldo_inicial < 0 THEN 
                ASSIGN cLinha = cLinha + string(tt.saldo_inicial * -1) .
             ELSE 
                ASSIGN cLinha = cLinha + string(tt.saldo_inicial).
             
             ASSIGN cLinha = cLinha + "|".
             IF tt.saldo_inicial > 0 THEN 
                ASSIGN cLinha = cLinha + "D" .
             ELSE
                ASSIGN cLinha = cLinha + "C".
          
             ASSIGN cLinha = cLinha +  "|" +  string(tt.debito)+ "|" + string(tt.credito) + "|" .
          
             IF tt.saldo_final < 0 THEN 
                ASSIGN cLinha = cLinha + string(tt.saldo_final * -1) .
             ELSE 
                ASSIGN cLinha = cLinha + string(tt.saldo_final) .
             
                ASSIGN cLinha = cLinha +  "|".
          
              IF tt.saldo_final > 0 THEN
                 ASSIGN cLinha = cLinha +  "D" .
              ELSE
                 ASSIGN cLinha = cLinha +  "C".
          
              ASSIGN clinha = cLinha + "|" .
                
              ASSIGN cLinha = cLinha  .
          
              PUT UNFORMATTED cLinha SKIP.

    
       END.
       
       FOR EACH ttResultado WHERE ttResultado.conta <> "" 
            AND  ttResultado.mes = int(SUBSTR(ttArquivo.linha,26,2))  BY ttResultado.conta:
            ASSIGN cLinha = "|k355|" + ttResultado.conta + "|/001|".
            IF ttResultado.saldo < 0 THEN
               ASSIGN cSinal     = "C"
                      dNovoValor =  ttResultado.saldo * -1.
            ELSE
              ASSIGN cSinal      = "D"
                      dNovoValor =  ttResultado.saldo.

           ASSIGN cLinha = cLinha +  string(dNovoValor) + "|" + cSinal + "|". 

           PUT UNFORM cLinha SKIP.
       END.




    END.
END.



OUTPUT CLOSE.
/*registro 155*/
EMPTY TEMP-TABLE ttArquivo02.
INPUT FROM VALUE(cCaminho + "ecfnovo.txt").

REPEAT:
  IMPORT UNFORM cLinha02 .
  IF substr(cLinha02,2,4) = 'k030' THEN DO:
     ASSIGN mesCorrente = int(SUBSTR(cLinha02,26,2)). 
  END.

  IF SUBSTR(cLinha02,2,4) = "k155" THEN DO :
     CREATE ttArquivo02.
     ASSIGN ttArquivo02.linha = cLinha02
            ttArquivo02.mes   = mesCorrente.

  END.
END.

INPUT CLOSE.

OUTPUT TO VALUE(cCaminho + "verifica155.txt").
  FOR EACH ttArquivo02:
      PUT UNFORM ttArquivo02.linha ttArquivo02.mes SKIP.
  END.

OUTPUT CLOSE.

/*registro 355*/
EMPTY TEMP-TABLE ttArquivo02.
INPUT FROM VALUE(cCaminho + "ecfnovo.txt").

REPEAT:
  IMPORT UNFORM cLinha02 .
  IF substr(cLinha02,2,4) = 'k030' THEN DO:
     ASSIGN mesCorrente = int(SUBSTR(cLinha02,26,2)). 
  END.

  IF SUBSTR(cLinha02,2,4) = "k355" THEN DO :
     CREATE ttArquivo02.
     ASSIGN ttArquivo02.linha = cLinha02
            ttArquivo02.mes   = mesCorrente.

  END.
END.

INPUT CLOSE.

OUTPUT TO VALUE(cCaminho + "verifica355.txt").
  FOR EACH ttArquivo02:
      PUT UNFORM ttArquivo02.linha ttArquivo02.mes SKIP.
  END.

OUTPUT CLOSE.





