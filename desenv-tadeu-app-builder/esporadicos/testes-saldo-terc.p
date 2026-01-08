{esp/util.i}
FIND saldo-terc NO-LOCK
    WHERE saldo-terc.cod-emitente   = 38284
    AND   saldo-terc.nro-docto      = '0001821'
    AND   saldo-terc.serie          = '2'
    AND   saldo-terc.nat-operacao   = '59207i'
    AND   saldo-terc.it-codigo      = '135072'
    AND   saldo-terc.seq            = 40
    NO-ERROR.
IF AVAIL saldo-terc THEN DO:
   {esp/exportarRegRowidCsv.i saldo-terc ROWID(saldo-terc)}

END.
