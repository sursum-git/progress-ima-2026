OUTPUT TO c:\lixo\emitente.txt.
FOR EACH emitente WHERE
         emitente.identific = 1 NO-LOCK.
    IF emitente.e-mail = "" THEN NEXT.

    PUT emitente.nome-emit
        emitente.e-mail SKIP.

END.
OUTPUT CLOSE.
