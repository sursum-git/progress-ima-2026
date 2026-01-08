DEFINE VARIABLE cLinha AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE cCusto AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCont  AS INTEGER     NO-UNDO.
DEFINE VARIABLE numLinhaPaiCorrente AS INTEGER     NO-UNDO.
DEFINE TEMP-TABLE ttDados
    FIELD linha_inteira AS CHAR FORMAT 'x(200)'.
DEFINE BUFFER bf-ttDados FOR ttDados.
INPUT FROM c:\temp\resto_j_resultado.txt.
REPEAT:
    IMPORT UNFORMAT cLinha.
    IF NUM-ENTRIES(cLinha,"|") < 3 THEN NEXT.
 
    IF ENTRY(2,cLinha,"|") = 'j050' AND LENGTH(ENTRY(7,cLinha,"|")) = 7   THEN DO:
       CREATE ttDados.
       ASSIGN ttDados.linha_inteira  = cLinha.
    END.

    IF ENTRY(2,cLinha,"|") = 'j050' AND LENGTH(ENTRY(7,cLinha,"|")) > 7   THEN DO:
       ASSIGN cCusto = SUBSTR(ENTRY(7,cLinha,"|"),8,5).
    END.
    IF ENTRY(2,cLinha,"|") = 'j051'   THEN DO:
       CREATE ttDados.
       ASSIGN ttDados.linha_inteira = '|' + ENTRY(2,cLinha,"|") + "|" + cCusto + "|" +  ENTRY(4,cLinha,"|") + "|".
    END.
END.
OUTPUT TO c:\temp\ttDados_novo.txt.
FOR EACH ttDados.
   EXPORT ttDados.
END.
OUTPUT CLOSE.
