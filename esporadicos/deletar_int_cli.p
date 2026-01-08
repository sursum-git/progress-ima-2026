// netfilx, amazon, teste
DISABLE TRIGGERS  FOR LOAD OF emitente.
    
FOR EACH emitente
    WHERE emitente.nome-abrev = 'liasa'.
    DISP emitente.cod-emitente.
    FIND ext-emitente OF emitente NO-ERROR.
    IF AVAIL ext-emitente THEN
       DELETE ext-emitente.
    FOR EACH emitente_cnae
        WHERE emitente_cnae.cod_emitente = emitente.cod-emitente.
        DELETE emitente_cnae.
    END.
    FOR EACH cont-emit
        WHERE cont-emit.cod-emitente = emitente.cod-emitente.
        FOR EACH ext-cont-emit OF cont-emit :
            DELETE ext-cont-emit.
        END.
        DELETE cont-emit.
    END.
    FOR EACH his-emit OF emitente:
        DELETE his-emit.
    END.
    DELETE emitente.

    
END.
