/*OUTPUT TO "c:/lixo/sem-localiz.txt".*/
FOR EACH ob-etiqueta WHERE ob-etiqueta.situacao = 3 
                       AND ob-etiqueta.num-etiqueta <> 0 
                     NO-LOCK.
    IF ob-etiqueta.acondic BEGINS "peca" AND NOT ob-etiqueta.nr-lote BEGINS "p" OR
       ob-etiqueta.acondic BEGINS "rolo" AND NOT ob-etiqueta.nr-lote BEGINS "r" THEN
    DISP ob-etiqueta.it-codigo
         ob-etiqueta.cod-refer
         ob-etiqueta.num-etiqueta
         ob-etiqueta.localizacao
         ob-etiqueta.acondic
         ob-etiqueta.nr-lote WITH WIDTH 120.
END.
/*OUTPUT CLOSE.*/
