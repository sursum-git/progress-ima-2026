DEFINE TEMP-TABLE ttSaldo
    FIELD itCodigo AS CHAR FORMAT 'x(12)'
    FIELD codRefer AS CHAR  
    FIELD nota     AS CHAR FORMAT 'X(10)'
    FIELD quantidade AS DECIMAL.
    
FOR EACH saldo-terc NO-LOCK
    WHERE saldo-terc.cod-estabel  = '505'.
    FIND ttSaldo 
        WHERE ttSaldo.itCodigo = saldo-terc.it-codigo
        AND   ttsaldo.codRefer = saldo-terc.cod-refer
        AND   ttsaldo.nota     = saldo-terc.nro-docto NO-ERROR
        .
    IF NOT AVAIL ttSaldo THEN DO:
       CREATE ttSaldo.
       ASSIGN ttSaldo.itCodigo = saldo-terc.it-codigo  
              ttsaldo.codRefer = saldo-terc.cod-refer  
              ttsaldo.nota     = saldo-terc.nro-docto  
              .
    END.
    ASSIGN ttSaldo.quantidade =  + saldo-terc.quantidade.

END.


OUTPUT TO c:\temp\saldo-terc.csv.

FOR EACH ttSaldo.
    EXPORT DELIMITER ";" ttSaldo.
END.

OUTPUT CLOSE.
