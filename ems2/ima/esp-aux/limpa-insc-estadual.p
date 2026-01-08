FIND emitente 23642.
ASSIGN emitente.ins-estadual = ''.

FOR EACH ems5.cliente WHERE
         ems5.cliente.cdn_cliente = emitente.cod-emit NO-LOCK.

    FIND pessoa_jurid WHERE
         pessoa_jurid.num_pessoa_jurid = ems5.cliente.num_pessoa SHARE-LOCK NO-ERROR.
    
    ASSIGN pessoa_jurid.cod_id_estad_jurid = ''.
    
END.
