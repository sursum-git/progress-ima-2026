/* Temp-Table Definitions */
DEF TEMP-TABLE tt-emitente LIKE emitente.
DEF TEMP-TABLE tt-cont-emit LIKE cont-emit.
   
FIND emitente WHERE
    emitente.cod-emitente = 29596 NO-LOCK NO-ERROR.

IF AVAIL emitente THEN DO.
  CREATE tt-emitente.
  BUFFER-COPY emitente TO tt-emitente.

  FOR EACH cont-emit OF emitente NO-LOCK.
      CREATE tt-cont-emit.
      BUFFER-COPY cont-emit TO tt-cont-emit.
  END.
END.
                  
RUN esapi/importa-emitente.p(INPUT TABLE tt-emitente,
                             INPUT TABLE tt-cont-emit).
