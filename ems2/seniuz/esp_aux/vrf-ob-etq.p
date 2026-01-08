DEF VAR i-est AS INT.
DEF VAR i-fat AS INT.
DEF VAR i-erro AS INT.

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.cod-estabel = '5' AND 
         ob-etiqueta.situacao = 3 SHARE-LOCK.
         
    
    FIND bc-etiqueta WHERE
         bc-etiqueta.progressivo = ob-etiqueta.progressivo SHARE-LOCK NO-ERROR.

    IF NOT AVAIL bc-etiqueta THEN DO.
        /*
       DISP ob-etiqueta.num-etiqueta
            ob-etiqueta.localizacao.
            */
       ASSIGN i-fat = i-fat + 1.
       
       NEXT.
    END.
    

       /*
    IF bc-etiqueta.cod-estado <> 2 THEN DO.
       ASSIGN bc-etiqueta.cod-estado = 2.

       DISP bc-etiqueta.progressivo (COUNT). 
       PAUSE 0.
    END.
       */
END.

MESSAGE i-fat
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

