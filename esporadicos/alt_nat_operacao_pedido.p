
FOR EACH ped-venda
    WHERE ped-venda.nr-pedido = 272510.
    FIND emitente 
        WHERE emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.
    UPDATE ped-venda.nat-operacao .
    DISP ped-venda.nr-pedido emitente.cod-emitente emitente.nome-abrev dt-implant emitente.estado .
    FOR EACH ped-item OF ped-venda.
        ASSIGN  ped-item.nat-operacao   = ped-venda.nat-operacao.
    END.
END.
