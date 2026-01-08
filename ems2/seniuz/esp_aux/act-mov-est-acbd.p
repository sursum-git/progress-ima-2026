FOR EACH mov-est-acbd WHERE
         mov-est-acbd.num-etiqueta = 0.

    FIND ob-etiqueta WHERE
         ob-etiqueta.nr-ob = mov-est-acbd.num-lote AND 
         ob-etiqueta.dt-emis = mov-est-acbd.data-mov AND
         ob-etiqueta.nr-carro = mov-est-acbd.nr-carro AND
         ob-etiqueta.nr-sequencia = mov-est-acbd.nr-sequencia
         NO-LOCK NO-ERROR.

    IF AVAIL ob-etiqueta THEN
       DISP ob-etiqueta.num-etiqueta
            ob-etiqueta.nr-carro
            ob-etiqueta.nr-ob
            ob-etiqueta.nr-seq.
                                 
END.
