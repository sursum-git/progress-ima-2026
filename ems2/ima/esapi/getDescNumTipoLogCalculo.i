FUNCTION getDescNumTipo RETURNS CHAR(numTipo AS INT):

    CASE numTipo:
       WHEN 1 THEN
       DO:
           RETURN 'AVISO'.           
       END.
       WHEN 2 THEN
       DO:
          RETURN 'ERRO' .
       END.
       WHEN 3 THEN  DO:
        RETURN 'LOG'.           
       END.
    END CASE.


END FUNCTION.
