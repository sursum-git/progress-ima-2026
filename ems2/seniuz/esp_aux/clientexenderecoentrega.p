DEFINE VARIABLE cCodIBGECli  LIKE emitente.estado.
DEFINE VARIABLE cCodIBGEEntr LIKE emitente.estado.

DEFINE VARIABLE lEndereco AS LOGICAL INIT NO. 
DEFINE VARIABLE lCidade   AS LOGICAL INIT NO.
DEFINE VARIABLE lCEP      AS LOGICAL INIT NO.
DEFINE VARIABLE lUF       AS LOGICAL INIT NO.
DEFINE VARIABLE lIBGE     AS LOGICAL INIT NO.

OUTPUT TO c:\temp\emitentexlocalentrega.txt.
FOR EACH emitente
    WHERE emitente.identific = 1 NO-LOCK.
    FIND FIRST cidade OF emitente NO-LOCK NO-ERROR.
    ASSIGN cCodIBGECli = IF AVAIL cidade THEN string(cidade.int-2) ELSE 'cidade n∆o cadastrada'.
    FOR EACH loc-entr 
        WHERE loc-entr.nome-abrev =  emitente.nome-abrev NO-LOCK.
        IF(loc-entr.endereco <> emitente.endereco) THEN  lEndereco = YES ELSE NO.
        IF(loc-entr.cep <> emitente.cep)           THEN  lEndereco = YES ELSE NO.
        IF(loc-entr.estado <> emitente.estado)     THEN  lUF       = YES ELSE NO.
        IF(loc-entr.cidade <> emitente.cidade)     THEN  lCidade   = YES ELSE NO.
        FIND FIRST cidade OF loc-entr NO-LOCK NO-ERROR.
        ASSIGN cCodIBGEEntr = IF AVAIL cidade THEN string(cidade.INT-2) ELSE 'cidade n∆o cadastrada'.
        IF( cCodIBGECli <> cCodIBGEEntr )     THEN  lIBGE   = YES ELSE NO.  
        EXPORT DELIMITER "|" 
        emitente.cod-emitente 
        emitente.nome-abrev 
        emitente.nome-emit
        emitente.endereco
        cod-entrega
        lendereco
        loc-entr.endereco
        emitente.cep
        lCEP
        loc-entr.CEP
        emitente.estado
        lUF
        loc-entr.estado
        emitente.cidade
        lCidade
        loc-entr.cidade
        cCodIBGECli
        lIBGE
        cCodIBGEEntr .


            

        
        

    END.
END.
OUTPUT CLOSE.
