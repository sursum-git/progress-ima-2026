DEFINE INPUT  PARAMETER pHObj    AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER pTipo    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pArquivo AS CHARACTER   NO-UNDO.

CASE pTipo:
    WHEN 'tt' THEN DO:
        pHObj:WRITE-JSON(  'file',
                            pArquivo,
                            TRUE,'utf-8',FALSE,YES).
    END.
    WHEN 'dataset' THEN DO:
        pHObj:WRITE-JSON(  'file',
                           pArquivo,
                           FALSE,'utf-8',FALSE,YES).
    END.
                                                                  
END CASE. 



