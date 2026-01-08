//DEFINE VARIABLE lFulano AS LOGICAL     NO-UNDO.
FOR EACH ped-venda NO-LOCK
    WHERE ped-venda.dt-implant >= 01.01.2018
    AND ped-venda.cod-sit-ped <> 6 .
    
    FIND FIRST ped-repre OF ped-venda 
        WHERE ped-repre.nome-ab-rep = 'fulano' NO-LOCK NO-ERROR.
    IF ped-venda.cod-prior <> 10 AND NOT AVAIL ped-repre
       THEN DO:
       DISP ped-venda.dt-implant ped-venda.nr-pedido int(ped-venda.cod-sit-ped).
    END.


END.
