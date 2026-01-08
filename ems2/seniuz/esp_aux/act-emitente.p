DEF VAR c-results AS CHAR.
/*RUN esapi/conecta-base.p (INPUT "MED-OFICIAL").*/

FOR FIRST ems2ima.emitente WHERE 
          ems2ima.emitente.cod-emit = 30 NO-LOCK.
    FIND ems2.emitente WHERE
         ems2.emitente.cod-emit = ems2ima.emitente.cod-emit.

    BUFFER-COMPARE ems2ima.emitente TO ems2.emitente SAVE RESULT IN c-results.
    IF c-results <> '' THEN DO.
        RUN esapi/integra-emitente.p (INPUT ems2ima.emitente.cod-emitente).
    END.
END.

