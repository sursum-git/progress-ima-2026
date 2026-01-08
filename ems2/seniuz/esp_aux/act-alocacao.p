DEF VAR de-qt-log AS DEC.

FOR EACH saldo-estoq WHERE
         saldo-estoq.qt-aloc-ped > 0 SHARE-LOCK,
    FIRST ITEM OF saldo-estoq WHERE
          ITEM.ge-codigo >= 50 AND
          ITEM.ge-codigo <= 60 NO-LOCK
          BREAK BY saldo-estoq.it-codigo
                BY saldo-estoq.cod-refer.

    IF saldo-estoq.lote BEGINS 'ca' THEN NEXT.
    
    ASSIGN de-qt-log = 0.
    FOR EACH ped-item WHERE
             ped-item.cod-sit-item = 1 AND
             ped-item.it-codigo = saldo-estoq.it-codigo AND 
             ped-item.cod-refer = saldo-estoq.cod-refer NO-LOCK.

        ASSIGN de-qt-log = de-qt-log + ped-item.qt-log-aloc.
    END.
    
    IF saldo-estoq.qt-aloc-ped = de-qt-log THEN NEXT. 
    
    IF saldo-estoq.qtidade-atu >= de-qt-log THEN DO.  
       ASSIGN saldo-estoq.dec-2 = saldo-estoq.qt-aloc-ped
              saldo-estoq.qt-aloc-ped = de-qt-log.
    
       DISP saldo-estoq.cod-estab
            saldo-estoq.it-codigo
            saldo-estoq.cod-refer
            saldo-estoq.qt-aloc-ped COLUMN-LABEL 'Qt Aloc' (COUNT)
            de-qt-log               COLUMN-LABEL 'Qt pedida' 
            WITH WIDTH 550.
    END.
END.


