FOR EACH ems2med.cheque WHERE
         ems2med.cheque.situacao = 1.
    DISP ems2med.cheque.cod-emit.

    ASSIGN ems2med.cheque.situacao = 6.
END.
   
   
FOR EACH ems2ima.cheque WHERE
         ems2ima.cheque.situacao = 1.
    DISP ems2ima.cheque.cod-emit.
END.

FOR EACH titulo WHERE
        titulo.vl-saldo <> 0 SHARE-LOCK.
    DISP titulo.vl-saldo
         titulo.dt-emissao.
    PAUSE 0.
    /*
    ASSIGN titulo.dec-2 = titulo.vl-saldo.
    ASSIGN titulo.vl-saldo = 0.
    */
END.

