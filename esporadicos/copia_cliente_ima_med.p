
DISABLE TRIGGERS FOR LOAD OF ems2med.emitente.
DISABLE TRIGGERS FOR LOAD OF ems2ima2.emitente.

FIND FIRST ems2med.emitente 
    WHERE ems2med.emitente.cod-emitente = 29901 EXCLUSIVE-LOCK.
IF AVAIL ems2med.emitente THEN
    DELETE ems2med.emitente.

FIND FIRST ems2ima2.emitente 
    WHERE ems2ima2.emitente.cod-emitente = 29901 EXCLUSIVE-LOCK.
IF AVAIL ems2ima2.emitente THEN DO:
    CREATE ems2med.emitente.
    BUFFER-COPY ems2ima2.emitente TO ems2med.emitente.



END.
    

