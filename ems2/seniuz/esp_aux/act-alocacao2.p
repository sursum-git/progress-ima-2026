DEF VAR de-tot-aloc AS DEC.
DEF VAR de-tot-log AS DEC.

FOR EACH saldo-estoq WHERE 
         saldo-estoq.qt-alocada < 0 SHARE-LOCK.

    ASSIGN de-tot-aloc = 0.
    FOR EACH ped-item WHERE
             ped-item.cod-sit-item = 1 AND              
             ped-item.it-codigo = saldo-estoq.it-codigo AND
             ped-item.cod-refer = saldo-estoq.cod-refer NO-LOCK.
 
        FIND ped-venda OF ped-item NO-LOCK NO-ERROR.
        IF NOT ped-venda.completo THEN NEXT.

        ASSIGN de-tot-aloc = de-tot-aloc + ped-item.qt-pedida.
    END.

    DISP saldo-estoq.cod-estab
         saldo-estoq.it-codigo
         saldo-estoq.cod-refer
         saldo-estoq.qt-alocada   COLUMN-LABEL 'Qt Aloc' (COUNT)
         de-tot-log              COLUMN-LABEL 'Qt pedida' 
         WITH WIDTH 550.

    ASSIGN saldo-estoq.qt-alocada = de-tot-aloc.
END.
