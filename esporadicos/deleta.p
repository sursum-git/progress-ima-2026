DISABLE TRIGGERS FOR LOAD OF ems5.fornecedor.
DISABLE TRIGGERS FOR LOAD OF ems5.fornec_financ.

FOR EACH ems5.fornecedor
    WHERE cdn_fornec = 28959.
    DELETE fornecedor.
    FOR EACH fornec_financ OF fornecedor:
        DELETE fornec_financ.
    END.
END.

