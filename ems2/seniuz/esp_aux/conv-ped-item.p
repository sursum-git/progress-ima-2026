DEF VAR i-tp-embal AS INT.

FOR EACH ped-venda WHERE
         ped-venda.cod-sit-ped = 1 NO-LOCK,
    EACH ped-item OF ped-venda NO-LOCK.
    
    FIND FIRST corte-comerc WHERE
               corte-comerc.compr-min <= ped-item.qt-pedida AND
               corte-comerc.compr-max >= ped-item.qt-pedida NO-LOCK NO-ERROR.

    IF NOT AVAIL corte-comerc THEN
       FIND FIRST corte-comerc WHERE
                  corte-comerc.codigo = 'C' NO-LOCK NO-ERROR.

    CREATE ped-item-ext.
    ASSIGN ped-item-ext.cod-estabel = ped-venda.cod-estabel   /*  daf  */
           ped-item-ext.nome-abrev = ped-item.nome-abrev
           ped-item-ext.nr-pedcli = ped-item.nr-pedcli 
           ped-item-ext.nr-sequencia = ped-item.nr-sequencia
           ped-item-ext.it-codigo = ped-item.it-codigo
           ped-item-ext.cod-refer = ped-item.cod-refer
           ped-item-ext.lote = IF ped-item.cod-refer = '888' 
                               THEN 'RD' ELSE 'RP'
           ped-item-ext.corte-comerc = corte-comerc.codigo
           ped-item-ext.acondic = corte-comerc.descricao.  
END.

