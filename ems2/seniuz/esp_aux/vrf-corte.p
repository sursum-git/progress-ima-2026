FIND ob-etiqueta WHERE
     ob-etiqueta.num-etiqueta = 5922.


       FIND ITEM WHERE
            ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

       FIND corte-comerc WHERE
                corte-comerc.compr-min <= 15.48 AND
                corte-comerc.compr-max >= 15.48 AND
                corte-comerc.tp-embalag = 2 AND 
                corte-comerc.un = item.un  NO-LOCK.

       IF NOT AVAIL corte-comerc THEN DO.
          MESSAGE "Metragem do Saldo n∆o pertece a nenhum Corte Comercial Cadastrado..." 
                  VIEW-AS ALERT-BOX.
       END.
