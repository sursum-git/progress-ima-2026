DEF TEMP-TABLE tt-emitente LIKE db-aux.emitente.
DEF TEMP-TABLE tt-cont-emit LIKE db-aux.cont-emit.

DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-emitente.
DEF INPUT-OUTPUT PARAMETER TABLE FOR tt-cont-emit.

DEF INPUT PARAMETER p-cod-emit LIKE db-aux.emitente.cod-emitente.

FOR EACH db-aux.emitente WHERE
         db-aux.emitente.cod-emitente = p-cod-emit NO-LOCK.

    FIND tt-emitente WHERE
         tt-emitente.cgc = db-aux.emitente.cgc NO-LOCK NO-ERROR.

    IF NOT AVAIL tt-emitente THEN DO.
       CREATE tt-emitente.
       BUFFER-COPY emitente TO tt-emitente.
    END.

    FOR EACH db-aux.cont-emit OF db-aux.emitente NO-LOCK.
        FIND tt-cont-emit WHERE
             tt-cont-emit.cod-emitente = db-aux.cont-emit.cod-emitente AND 
             tt-cont-emit.sequencia = db-aux.cont-emit.sequencia
             NO-LOCK NO-ERROR.

        IF NOT AVAIL tt-cont-emit THEN DO.
           CREATE tt-cont-emit.
           BUFFER-COPY db-aux.cont-emit TO tt-cont-emit.
        END.
    END.
END.




