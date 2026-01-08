DEFINE VARIABLE cArquivoDados    AS CHARACTER   NO-UNDO FORMAT 'x(30)' INIT 'c:\temp\ecf\med\ecfn.txt'.
DEFINE VARIABLE cArquivo         AS CHARACTER   NO-UNDO FORMAT 'x(30)' INIT 'c:\temp\ecf\med\depara.txt'.
DEFINE VARIABLE cNovaLinha       AS CHARACTER   NO-UNDO FORMAT 'x(100)'.
DEFINE TEMP-TABLE tt
    FIELD linha AS CHAR FORMAT 'x(500)'.
DEFINE TEMP-TABLE ttdePara
       FIELD contaAntiga AS CHAR FORMAT 'x(50)'
       FIELD contaNova   AS CHAR FORMAT 'x(50)' .

DEFINE TEMP-TABLE ttJ51
    FIELD nada AS CHAR
    FIELD cc   AS CHAR
    FIELD conta AS CHAR.

DEFINE VARIABLE cLinha02 AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
      
UPDATE cArquivodados cArquivo.

INPUT FROM VALUE(cArquivo).

REPEAT:
    CREATE ttdePara.
    IMPORT DELIMITER ";" ttDePara.
     


END.

/*FOR EACH ttDepara:
    DISP ttDePara WITH WIDTH 550.
END.*/
   
INPUT CLOSE.
INPUT FROM value(cArquivoDados).
REPEAT:
     IMPORT UNFORM cLinha02 .
     /*MESSAGE substr(cLinha02,2,4)
         VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
     IF substr(cLinha02,2,4) <> 'j051' THEN DO:
         
         CREATE tt.
         ASSIGN tt.linha = cLinha02.

     END.
     ELSE DO:
         CREATE ttJ51.
         ASSIGN ttJ51.conta = ENTRY(4,cLinha02,"|")
                ttJ51.cc    = ENTRY(3,cLinha02,"|").
         
        /* MESSAGE ttJ51.nada ttJ51.cc ttJ51.conta
             VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
          
         FIND FIRST ttDepara
         WHERE trim(ttdePara.contaAntiga) = trim(ttJ51.conta)
         NO-LOCK NO-ERROR.
         IF AVAIL ttDePara THEN DO:
            ASSIGN cNovaLinha = "|j051|" + ttJ51.cc + "|" + ttdePara.contaNova  + "|".
            CREATE tt.
            ASSIGN tt.linha =   cNovaLinha.
         END.
         ELSE DO:
           CREATE tt.
            ASSIGN tt.linha =   clinha02.
         END.
     END.
     /*
     IF SUBSTR(cLinha02,2,4) <> "k155" AND SUBSTR(cLinha02,2,4) <> "k355" THEN DO:
        CREATE ttArquivo.
        ASSIGN ttArquivo.linha = cLinha02.
     END.                                 */
END.
INPUT CLOSE.
OUTPUT TO c:\temp\previa.txt.
FOR EACH tt:
    PUT UNFORM tt.linha SKIP.
END.

OUTPUT CLOSE.
