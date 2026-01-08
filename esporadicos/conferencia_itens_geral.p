OUTPUT TO c:\temp\itens_base.txt.
FOR EACH ems2med.ITEM
    WHERE ITEM.deposito-pad = 'arm'
    NO-LOCK.
    FIND FIRST ima.ITEM
        WHERE ima.ITEM.it-codigo = ems2med.ITEM.it-codigo
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ima.ITEM THEN
        DISP ems2med.ITEM.it-codigo.
END.
