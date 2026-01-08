DEF VAR c-qualid     AS CHAR.

FIND ob-etiqueta WHERE
     ob-etiqueta.nr-ob =  7456 AND
     ob-etiqueta.nr-seq = 5.

        IF SUBSTR(ob-etiqueta.nr-lote,2,1) = "D" THEN 
           ASSIGN c-qualid = 'D'.

        IF SUBSTR(ob-etiqueta.nr-lote,2,1) = "P" THEN DO.
           FIND FIRST mov-est-acbd WHERE
                      mov-est-acbd.data-mov = ob-etiqueta.dt-emissao AND
                      mov-est-acbd.num-lote = ob-etiqueta.nr-ob AND
                      mov-est-acbd.nr-carro = ob-etiqueta.nr-carro AND
                      mov-est-acbd.acondic  = ob-etiqueta.acondic AND
                      mov-est-acbd.nr-sequencia = ob-etiqueta.nr-sequencia AND
                      mov-est-acbd.classif = "RG" NO-LOCK NO-ERROR. 
           IF AVAIL mov-est-acbd THEN
              ASSIGN c-qualid = "C".
           ELSE
              ASSIGN c-qualid = "B".
        END.


ASSIGN ob-etiqueta.cod-qualid = c-qualid.
