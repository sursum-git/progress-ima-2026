DEF BUFFER b-etiqueta FOR ob-etiqueta.
DEF VAR i-nr-seq AS INT INIT 321.

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.num-etiqueta = 175837.

    FIND b-etiqueta WHERE
         b-etiqueta.nr-ob =  ob-etiqueta.nr-ob AND
         b-etiqueta.nr-seq = ob-etiqueta.nr-seq AND
         b-etiqueta.num-etiqueta <> ob-etiqueta.num-etiqueta
         NO-ERROR.

    IF AVAIL b-etiqueta THEN DO.
       ASSIGN b-etiqueta.nr-seq = i-nr-seq.
       ASSIGN i-nr-seq = i-nr-seq + 1.
    END.
END.
