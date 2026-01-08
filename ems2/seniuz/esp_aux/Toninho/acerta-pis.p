DISABLE TRIGGERS FOR LOAD OF emitente.
DISABLE TRIGGERS FOR DUMP OF emitente.

DISABLE TRIGGERS FOR LOAD OF ems5.fornecedor.
DISABLE TRIGGERS FOR DUMP OF ems5.fornecedor.


FOR EACH emitente WHERE 
         emitente.identific >= 2 SHARE-LOCK.
    
    ASSIGN emitente.idi-tributac-pis = 1
           emitente.idi-tributac-cofins = 1. 
    
    DISP emitente.cod-emit.
    PAUSE 0.
END.

/*
FOR EACH ems5.fornecedor SHARE-LOCK.
    ASSIGN ems5.fornecedor.LOG_cr_pis = YES
           ems5.fornecedor.LOG_cr_cofins = YES.
    DISP ems5.fornecedor.cdn_fornec.
    PAUSE 0.
END.

*/

