DEF VAR i-cont    AS INT.
DEF VAR i-virgula AS INT.

FOR EACH emitente WHERE emitente.identific <> 2.
    ASSIGN i-virgula = 0.
    DO i-cont = 1 TO LENGTH(emitente.endereco):
       IF SUBSTR(emitente.endereco,i-cont,1) = "," THEN 
          ASSIGN i-virgula = i-virgula + 1.
    END.
    IF i-virgula > 1 THEN
        UPDATE emitente.endereco
               emitente.endereco-cob.
END.
