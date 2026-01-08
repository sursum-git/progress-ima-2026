DEF VAR i-etq AS INT.
INPUT FROM c:\temp\t5.txt.
REPEAT.
    SET i-etq.

    DISP i-etq.
    PAUSE 0.
            
      FIND FIRST ob-etiqueta WHERE
                 ob-etiqueta.num-etiqueta = 0 AND
                 ob-etiqueta.situacao = 5
                 USE-INDEX indice5 NO-ERROR.

      IF AVAIL ob-etiqueta THEN 
         ASSIGN ob-etiqueta.num-etiqueta = i-etq.
                
END.

INPUT CLOSE.
