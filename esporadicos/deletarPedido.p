FOR EACH ped-venda
    WHERE nr-pedido = 234792.
    //DISP ped-venda WITH 1 COL WIDTH 550.
    FOR EACH ped-item OF ped-venda.
        DELETE ped-item.
    END.
    
    FOR EACH ped-ent OF ped-venda.
        DELETE ped-ent.
    END.

    FOR EACH cond-ped OF ped-venda.
        DELETE cond-ped.
    END.
    FOR EACH ped-repre OF ped-venda.
        DELETE ped-repre.
    END.
    DELETE ped-venda.

END.
