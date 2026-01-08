FOR EACH ob-etiqueta WHERE
         ob-etiqueta.situacao = 3 AND
         ob-etiqueta.nr-lote BEGINS 'P'.
    ASSIGN ob-etiqueta.situacao = 5. 
END.
