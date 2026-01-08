DISABLE TRIGGERS FOR LOAD OF ems5.pessoa_jurid .
DISABLE TRIGGERS FOR LOAD OF ems5.cliente .
DISABLE TRIGGERS FOR LOAD OF ems5.clien_financ .
DEFINE INPUT  PARAMETER pCliente AS INTEGER     NO-UNDO.
FIND FIRST ems5.cliente
    WHERE ems5.cliente.cdn_cliente = pCliente
    NO-LOCK NO-ERROR.
IF AVAIL cliente THEN DO:
   MESSAGE 'cliente j  integrado'
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
   RETURN.
END.
FOR EACH db-aux.cliente
    WHERE cliente.cdn_cliente = pCliente.
    FIND db-aux.pessoa_jurid 
        WHERE  db-aux.cliente.num_pessoa = db-aux.pessoa_jurid.num_pessoa_jurid  NO-LOCK NO-ERROR.
    IF AVAIL db-aux.pessoa_jurid THEN DO:
       FIND   ems5.pessoa_jurid OF db-aux.pessoa_jurid
           NO-LOCK NO-ERROR.
       IF NOT AVAIL ems5.pessoa_jurid THEN DO:
          CREATE ems5.pessoa_jurid NO-ERROR.
          BUFFER-COPY db-aux.pessoa_jurid TO ems5.pessoa_jurid NO-ERROR.
       END.
       
    END.
    CREATE ems5.cliente NO-ERROR.
    BUFFER-COPY db-aux.cliente TO ems5.cliente NO-ERROR.
    FOR EACH  db-aux.clien_financ OF db-aux.cliente NO-LOCK .
       FIND ems5.clien_financ OF db-aux.clien_financ NO-LOCK NO-ERROR.
       IF NOT AVAIL ems5.clien_financ THEN DO:
          CREATE ems5.clien_financ.
          BUFFER-COPY db-aux.clien_financ TO ems5.clien_financ.
       END.
    END.   
END.
