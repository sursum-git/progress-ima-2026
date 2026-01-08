DEF VAR l-movto AS LOG.
DEF VAR l-saldo AS LOG.
OUTPUT TO c:/temp/lixo1.txt CONVERT SOURCE "ibm850".
FOR EACH ITEM WHERE ITEM.ge-codigo = 13 OR ITEM.ge-codigo = 70 
                AND ITEM.data-implant <= 05/31/2009 NO-LOCK.
    ASSIGN l-movto = NO
           l-saldo = NO.
    FIND FIRST movto-estoq USE-INDEX item-data WHERE movto-estoq.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.
    IF AVAIL movto-estoq THEN
       ASSIGN l-movto = YES.
    FIND FIRST saldo-estoq USE-INDEX ITEM WHERE saldo-estoq.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.
    IF AVAIL saldo-estoq THEN
       ASSIGN l-saldo = YES.
        
        DISP ITEM.it-codigo
            ITEM.desc-item
            ITEM.ge-codigo
            l-saldo
            l-movto
            ITEM.data-implant WITH WIDTH 136.
        /*
        PUT UNFORMAT
            ITEM.it-codigo FORMAT "X(16)"
            ITEM.it-codigo FORMAT "X(16)"
            "UNUN00111111788888888" FORMAT "X(23)"
            SKIP.
        */
END.
OUTPUT CLOSE.
