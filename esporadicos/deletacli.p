DISABLE TRIGGERS FOR LOAD OF ems5.cliente.
DISABLE TRIGGERS FOR LOAD OF ems5.clien_financ.

DEF VAR p-cliente LIKE ems5.cliente.cdn_cliente.
UPDATE p-cliente.

FOR EACH ems5.cliente
    WHERE ems5.cliente.cdn_cliente = p-cliente.
    DELETE ems5.cliente.
    FOR EACH clien_financ OF ems5.cliente:
        DELETE clien_financ.
    END.
END.

FIND emitente WHERE
     emitente.cod-emit = p-cliente NO-LOCK NO-ERROR.
  RUN cdp/cd1608.p (INPUT emitente.cod-emitente,
                    INPUT emitente.cod-emitente,
                    INPUT emitente.identific,
                    INPUT YES,
                    INPUT 1,
                    INPUT 0,
                    INPUT "c:\temp\erro.txt",
                    INPUT "Arquivo":U,
                    INPUT "").
