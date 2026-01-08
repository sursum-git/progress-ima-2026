FOR EACH saldo-estoq NO-LOCK
    WHERE saldo-estoq.qtidade-atu > 0,
    EACH ITEM OF saldo-estoq NO-LOCK
    WHERE ITEM.ge-codigo >= 50 AND ITEM.ge-codigo <= 60 .
    FIND referencia 
        WHERE referencia.cod-refer =  saldo-estoq.cod-refer NO-LOCK NO-ERROR.
    IF NOT AVAIL referencia AND saldo-estoq.cod-refer <> '' THEN
       DISP saldo-estoq.cod-refer.



    
END.
