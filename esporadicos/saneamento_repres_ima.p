OUTPUT TO c:\temp\emit.txt.
FOR EACH emitente
    WHERE cod-emitente = 1.
    
    FIND ext-emitente
        OF emitente NO-ERROR.
    IF AVAIL ext-emitente THEN DO:
        PUT  emitente.cod-emitente "|" ext-emitente.situacao SKIP.
        ASSIGN ext-emitente.situacao = 4.
    END.
        




END.
