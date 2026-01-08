/*
necess rio conectar o banco da ima com o nome logico ima
porta 10030
*/
DEFINE TEMP-TABLE tt
    FIELD itCodigo AS CHAR FORMAT 'x(20)'.
FOR EACH ems2med.nota-fiscal NO-LOCK
    WHERE  ems2med.nota-fiscal.dt-emis-nota >= 01.01.2018
    AND    ems2med.nota-fiscal.dt-emis-nota <= 01.31.2018:
    FOR EACH ems2med.it-nota-fisc OF ems2med.nota-fiscal NO-LOCK.
        FIND FIRST tt
            WHERE tt.itCodigo = it-nota-fisc.it-codigo
            NO-LOCK NO-ERROR.
        IF NOT AVAIL tt THEN DO:
           CREATE tt.
           ASSIGN tt.itCodigo = it-nota-fisc.it-codigo.
        END.
    END.    
END.
OUTPUT TO c:\temp\itens.txt.
FOR EACH tt:
    FIND FIRST ima.ITEM
        WHERE ima.ITEM.it-codigo = tt.itCodigo
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ima.ITEM THEN
       DISP tt.itCodigo.
END.





