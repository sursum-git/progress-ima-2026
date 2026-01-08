DEFINE TEMP-TABLE tt
    FIELD itCodigo  AS CHAR
    FIELD codRefer  AS CHAR
    FIELD rolo      AS INT 
    .

INPUT FROM c:\temp\etq.csv.

REPEAT:

    CREATE tt.
    IMPORT DELIMITER ";" tt.

END.

INPUT CLOSE.
OUTPUT TO c:\temp\ob-etiquetas-modificadas.txt.
FOR EACH tt:
    FIND ob-etiqueta EXCLUSIVE-LOCK
        WHERE ob-etiqueta.nr-container  = 268923 
        AND   ob-etiqueta.it-codigo     = tt.ItCodigo
        AND   ob-etiqueta.cod-refer     = tt.codRefer
        AND   ob-etiqueta.num-rolo      = tt.rolo
        AND   ob-etiqueta.situacao      = 2
        AND   ob-etiqueta.cod-estabel   = '505' 
        NO-ERROR.
    IF AVAIL ob-etiqueta THEN DO:
       DISP ob-etiqueta.num-etiqueta 
            ob-etiqueta.quantidade 
            STRING(ROWID(ob-etiqueta)).
       ASSIGN ob-etiqueta.situacao = 3 .

    END.

END.
OUTPUT CLOSE.
