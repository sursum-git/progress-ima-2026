
FIND ped-venda WHERE
     ped-venda.nr-pedcli = '315359'.
       
ASSIGN ped-venda.cod-sit-ped = 1
       ped-venda.dsp-pre-fat = YES.


FOR EACH ped-item OF ped-venda SHARE-LOCK.
    ASSIGN ped-item.cod-sit-item = 1. 

    ASSIGN ped-item.qt-atendida = 0.
END.


FOR EACH ped-ent OF ped-venda.
    ASSIGN ped-ent.cod-sit-ent = 1.
    
    ASSIGN ped-ent.qt-atendida = 0.                              
END.

FOR EACH ped-item-res WHERE
         ped-item-res.nr-pedcli  = ped-venda.nr-pedcli AND
         ped-item-res.faturado    = YES SHARE-LOCK.

    ASSIGN ped-item-res.serie       = ""
           ped-item-res.nr-nota-fis = 0
           ped-item-res.faturado    = NO.

    FOR EACH ped-item-rom WHERE
             ped-item-rom.nome-abrev = ped-item-res.nome-abrev AND
             ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli AND
             ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
             BREAK BY ped-item-rom.nr-volume.

        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel AND
             ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
             SHARE-LOCK NO-ERROR.
        IF AVAIL ob-etiqueta THEN
           ASSIGN ob-etiqueta.situacao = 4.
    END.
END.


/*
FOR EACH ped-venda WHERE
         ped-venda.dt-implant >= 02.01.2019 AND
         ped-venda.dt-cancela = TODAY.
    IF ped-venda.user-canc <> 'emagno' THEN NEXT.

    DISP ped-venda.user-canc
         int(ped-venda.cod-sit-ped).


    ASSIGN ped-venda.dt-cancela = ?
           ped-venda.user-canc = ''
           ped-venda.desc-canc = ''.

    FOR EACH ped-item OF ped-venda.
        ASSIGN ped-item.dt-canseq = ?
               ped-item.desc-cancela = ''
               ped-item.user-canc = ''.
    END.

END.
*/
