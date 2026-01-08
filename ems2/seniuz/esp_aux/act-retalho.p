DEF BUFFER b-item-ext FOR item-ext.

DEF VAR c-it-codigo AS CHAR.
DEF VAR i-ct AS INT.

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.situacao >= 3 AND
         ob-etiqueta.nr-reporte = 0 AND
         ob-etiqueta.quantidade > 0.

    IF ob-etiqueta.nr-lote <> 'sc' THEN NEXT.
    IF ob-etiqueta.tipo-ordem = 4 THEN NEXT.

    FIND item-ext WHERE
         item-ext.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

    IF ob-etiqueta.it-codigo BEGINS '599' THEN DO.
        ASSIGN i-ct = 0
               ob-etiqueta.ob-origem = ''.

        FOR EACH ordem-benefic WHERE
                 ordem-benefic.cod-estabel = ob-etiqueta.cod-estabel AND 
                 MONTH(ordem-benefic.dt-ob) = MONTH(ob-etiqueta.dt-emis) AND
                 ordem-benefic.situacao = 5 /* reportada */
                 NO-LOCK BY ordem-benefic.dt-ob DESCENDING. 

            IF ordem-benefic.tipo-ordem = 4 THEN NEXT.

            FIND b-item-ext WHERE
                 b-item-ext.it-codigo = ordem-benefic.it-codigo NO-LOCK NO-ERROR.

            IF b-item-ext.indigo <> item-ext.indigo THEN NEXT.

            ASSIGN i-ct = i-ct + 1.

            ASSIGN ob-etiqueta.ob-origem = IF i-ct = 1
                                           THEN string(ordem-benefic.nr-ob)
                                           ELSE ob-etiqueta.ob-origem + ";" + STRING(ordem-benefic.nr-ob).

            IF i-ct >= 5 THEN LEAVE.
        END.
    END.
    ELSE DO.
        ASSIGN c-it-codigo = substr(ob-etiqueta.it-codigo,1,5).
        
        ASSIGN i-ct = 0
               ob-etiqueta.ob-origem = ''.
    
        FOR EACH ordem-benefic WHERE
                 ordem-benefic.cod-estabel = ob-etiqueta.cod-estabel AND 
                 MONTH(ordem-benefic.dt-ob) = MONTH(ob-etiqueta.dt-emis) AND
                 SUBSTR(ordem-benefic.it-codigo,1,5) = c-it-codigo AND
                 ordem-benefic.situacao = 5 /* reportada */ NO-LOCK
                 BY ordem-benefic.dt-ob DESCENDING. 
    
            IF ordem-benefic.tipo-ordem = 4 THEN NEXT.

            ASSIGN i-ct = i-ct + 1.
    
            ASSIGN ob-etiqueta.ob-origem = IF i-ct = 1
                                           THEN string(ordem-benefic.nr-ob)
                                           ELSE ob-etiqueta.ob-origem + ";" + STRING(ordem-benefic.nr-ob).
    
            IF i-ct >= 5 THEN LEAVE.
        END.

        IF ob-etiqueta.ob-origem = '' THEN DO.
           ASSIGN i-ct = 0
                  ob-etiqueta.ob-origem = ''.

           FOR EACH ordem-benefic WHERE
                    ordem-benefic.cod-estabel = ob-etiqueta.cod-estabel AND 
                    MONTH(ordem-benefic.dt-ob) = MONTH(ob-etiqueta.dt-emis) AND
                    ordem-benefic.situacao = 5 /* reportada */ NO-LOCK
                    BY ordem-benefic.dt-ob DESCENDING. 
    
                IF ordem-benefic.tipo-ordem = 4 THEN NEXT.

                FIND b-item-ext WHERE
                     b-item-ext.it-codigo = ordem-benefic.it-codigo NO-LOCK NO-ERROR.

                IF b-item-ext.indigo <> item-ext.indigo THEN NEXT.

                ASSIGN i-ct = i-ct + 1.
    
                ASSIGN ob-etiqueta.ob-origem = IF i-ct = 1
                                               THEN string(ordem-benefic.nr-ob)
                                               ELSE ob-etiqueta.ob-origem + ";" + STRING(ordem-benefic.nr-ob).
    
                IF i-ct >= 5 THEN LEAVE.
            END.
        END.
    END.
END.

