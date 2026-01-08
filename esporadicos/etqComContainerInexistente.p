OUTPUT TO c:\temp\etq-container-nao-existe.csv .
FOR EACH ob-etiqueta NO-LOCK
    WHERE ob-etiqueta.nr-container <> 0       
    AND ob-etiqueta.situacao = 3
    AND ob-etiqueta.quantidade > 0.
    FIND pp-container NO-LOCK
        WHERE pp-container.nr-container = ob-etiqueta.nr-container
        NO-ERROR.
    IF NOT AVAIL pp-container THEN 
       EXPORT DELIMITER ";" ob-etiqueta.num-etiqueta ob-etiqueta.quantidade ob-etiqueta.nr-container  ob-etiqueta.cod-estabel.

END.
