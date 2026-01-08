DEFINE TEMP-TABLE tt
    FIELD mes               AS INT
    FIELD conta             AS CHAR FORMAT 'X(8)'
    FIELD saldo_inicial     AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD debito            AS DECIMAL        FORMAT "->>>,>>>,>>9.99"
    FIELD credito           AS DECIMAL       FORMAT "->>>,>>>,>>9.99"
    FIELD saldo_final       AS DECIMAL   FORMAT "->>>,>>>,>>9.99"
    FIELD forcado AS LOGICAL INIT NO.


DEFINE VARIABLE dAcumSI AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dAcumDB AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dAcumCR AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cSinal AS CHARACTER    NO-UNDO.
DEFINE VARIABLE dNovoValor AS DECIMAL     NO-UNDO.


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
DEFINE BUFFER bf FOR tt.
DEFINE VARIABLE cLinha          AS CHARACTER   NO-UNDO FORMAT 'x(80)'.
DEFINE VARIABLE cLinha02        AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE cCaminho        AS CHARACTER   NO-UNDO FORMAT 'x(50)' INIT 'p:\tadeu\ecf\med\v01\' LABEL "Caminho".
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

FOR EACH ttAnalitico
    WHERE ttAnalitico.conta <> '' 
    /*AND ttAnalitico.conta = '41100001'*/  BREAK BY ttAnalitico.conta BY ttAnalitico.mes:
    IF FIRST-OF(ttAnalitico.conta) THEN DO:
       ASSIGN dAcumSI = 0  
              dAcumDB = 0
              dAcumCR = 0 .
    END.
    CASE substr(ttAnalitico.conta,1,1):

        WHEN  '1' OR WHEN '2' THEN DO:
            
           IF ttAnalitico.mes = 13 THEN NEXT.
           ASSIGN   dAcumSI = dAcumSI + ttAnalitico.saldo_inicial
                    dAcumDB = dAcumDB + ttAnalitico.debito
                    dAcumCR = dAcumCR + ttAnalitico.credito.
           CREATE tt.
           ASSIGN tt.conta          = ttAnalitico.conta
                  tt.mes            = ttAnalitico.mes
                  tt.saldo_inicial  = dAcumSI
                  tt.debito         = dAcumDB
                  tt.credito        = dAcumCR
                  tt.saldo_final    = tt.saldo_inicial + tt.debito + tt.credito.
        END.


        WHEN  '3' OR WHEN '4' THEN DO:

           IF ttAnalitico.mes = 12 THEN NEXT.
           ASSIGN   dAcumSI = dAcumSI + ttAnalitico.saldo_inicial
                    dAcumDB = dAcumDB + ttAnalitico.debito
                    dAcumCR = dAcumCR + ttAnalitico.credito.
/*            MESSAGE "Mes:" ttAnalitico.mes SKIP                  */
/*                    "valor DB do mˆs:"  ttAnalitico.debito  SKIP */
/*                    "valor CR do mˆs:"  ttAnalitico.credito SKIP */
/*                    "valor DB acum:"    dAcumDB  SKIP            */
/*                    "valor CR acum:"    dAcumCR  SKIP            */
/*                VIEW-AS ALERT-BOX INFO BUTTONS OK.               */
           CREATE ttResultado.
           ASSIGN ttResultado.conta = ttAnalitico.conta
                  ttResultado.mes   = IF ttAnalitico.mes = 13 THEN 12 ELSE ttAnalitico.mes
                  ttResultado.saldo = dAcumSI + dAcumDB + dAcumCR.
        END.
    END CASE.

        IF LAST-OF(ttAnalitico.mes)AND LAST-OF(ttAnalitico.conta) THEN  DO:
           CASE substr(ttAnalitico.conta,1,1):
                WHEN  '1' OR WHEN '2' THEN DO:
                    IF ttAnalitico.mes < 12 THEN DO:
                       REPEAT i = ttAnalitico.mes + 1  TO 12:
                           CREATE tt.
                           ASSIGN tt.conta          = ttAnalitico.conta
                                  tt.mes            = i
                                  tt.saldo_inicial  = dAcumSI
                                  tt.debito         = 0
                                  tt.credito        = 0
                                  tt.saldo_final    = 0
                                  tt.forcado        = YES.
                        END.
                    END.

                END.
                WHEN  '3' OR WHEN '4' THEN DO:
                    IF ttAnalitico.mes < 13 THEN DO:
                       REPEAT i = ttAnalitico.mes + 1  TO 12:
                           CREATE ttResultado.
                           ASSIGN ttResultado.conta          = ttAnalitico.conta
                                  ttResultado.mes            =  i
                                  ttResultado.saldo          = dAcumSI + dAcumDB + dAcumCR
                                  ttResultado.forcado        = YES.
                        END.
                    END.

                END.

           END CASE.
        END.
END.
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


OUTPUT TO value(cCaminho + 'difsaldo.txt').
FOR EACH tt
    WHERE tt.mes = 12:
    IF tt.saldo_inicial = 0 AND tt.debito = 0 AND tt.credito = 0 AND tt.saldo_final = 0 THEN
       EXPORT DELIMITER "|" tt.

    CREATE bf.
    BUFFER-COPY tt TO bf.
    ASSIGN bf.mes = 0.

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
