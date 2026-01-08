DEFINE VARIABLE i AS INTEGER     NO-UNDO.
UPDATE i.
FOR EACH emitente
    WHERE emitente.cod-emitente = i NO-LOCK:
    DISP telefone.
    FOR EACH ems5.cliente
        WHERE cdn_cliente = cod-emitente NO-LOCK:
        FOR EACH pessoa_jurid 
            WHERE pessoa_jurid.num_pessoa_jurid = cliente.num_pessoa NO-LOCK:
            FOR EACH telef_pessoa WHERE 
                 telef_pessoa.num_pessoa = pessoa_jurid.num_pessoa_jurid NO-LOCK:
                DISP telef_pessoa WITH 1 COL WIDTH 550.
            END.

        END.

    END.
END.
