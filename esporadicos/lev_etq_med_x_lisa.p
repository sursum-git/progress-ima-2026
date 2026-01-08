DEFINE VARIABLE cAchou AS CHARACTER   NO-UNDO.
OUTPUT TO c:\temp\lev_etq_lisa.csv.
PUT "etq;Achou?" SKIP.
FOR EACH ob-etiqueta
    WHERE ob-etiqueta.cod-estabel = '505':
    IF NOT CAN-FIND( FIRST etiqueta_lisa 
        WHERE etiqueta_lisa.num_etiqueta = ob-etiqueta.num-etiqueta
        AND   etiqueta_lisa.cod_estabel  = ob-etiqueta.cod-estabel
        ) THEN ASSIGN cAchou = 'nao' .
    ELSE
        ASSIGN cAChou = 'sim'.
    EXPORT DELIMITER ";" ob-etiqueta.num-etiqueta cAchou.    
        
    
END.
