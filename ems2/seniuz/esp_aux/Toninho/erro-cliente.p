OUTPUT TO p:\erro-cliente.txt.
FOR EACH ems5.cliente NO-LOCK.

    FIND FIRST ems5.fornecedor WHERE
               ems5.fornecedor.num_pessoa = ems5.cliente.num_pessoa AND
               ems5.fornecedor.nom_abrev <> ems5.cliente.nom_abrev NO-LOCK NO-ERROR.

    IF AVAIL ems5.fornecedor THEN
       DISP ems5.cliente.cdn_cliente
            ems5.cliente.nom_abrev
            ems5.cliente.num_pessoa
            ems5.fornecedor.cdn_fornecedor
            ems5.fornecedor.nom_abrev
            ems5.fornecedor.num_pessoa
            ems5.cliente.dat_impl
            WITH WIDTH 550.

END.


