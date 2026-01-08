OUTPUT TO c:\temp\tit_clientes.txt.
FOR EACH ems5.cliente :
    DISP cdn_cliente.
    FOR EACH tit_acr
        WHERE tit_Acr.num_pessoa <> cliente.num_pessoa
        AND   tit_acr.cdn_Cliente = cliente.cdn_cliente.
        DISP cliente.num_pessoa  tit_Acr.num_pessoa tit_acr.cod_tit_Acr tit_Acr.dat_emis_docto.
    END.
END.

OUTPUT CLOSE.
