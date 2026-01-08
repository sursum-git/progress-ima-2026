DEF TEMP-TABLE tt-aux
    FIELD it-codigo AS CHAR
    FIELD cod-refer AS CHAR
    FIELD preco-item AS DECIMAL.

DEF VAR c-num-id AS CHAR.


INPUT FROM c:\temp\bordados.csv.
SET ^.
REPEAT.
    CREATE tt-aux.
    IMPORT DELIMITER ";" tt-aux.
END.
INPUT CLOSE.

FOR EACH tt-aux.
    IF tt-aux.it-codigo = '' THEN NEXT.

    DISP tt-aux.it-codigo
         tt-aux.cod-refer
         tt-aux.preco-item.


    IF tt-aux.cod-refer <> '' THEN DO.
       FIND liquida-ima WHERE
            liquida-ima.cod-estabel = '5' AND
            liquida-ima.it-codigo   = tt-aux.it-codigo  AND
            liquida-ima.cod-refer   = tt-aux.cod-refer  AND
            liquida-ima.dt-final    = ? 
            SHARE-LOCK NO-ERROR.

       IF AVAIL liquida-ima THEN
          ASSIGN liquida-ima.dt-final  = TODAY.


       // Cria Novo Registro com o novo percentual
       RUN esapi/calcula-id.p (OUTPUT c-num-id).
        
       CREATE liquida-ima.
       ASSIGN liquida-ima.cod-estabel   = '5'
              liquida-ima.it-codigo     = tt-aux.it-codigo
              liquida-ima.cod-refer     = tt-aux.cod-refer
              liquida-ima.preco-item    = tt-aux.preco-item
              liquida-ima.usuario       = 'super'
              liquida-ima.dt-inicio     = TODAY
              liquida-ima.num-id-liquida-ima = c-num-id.

    END.
    ELSE DO.
        FOR EACH saldo-estoq WHERE
                 saldo-estoq.cod-estabel = '5' AND
                 saldo-estoq.it-codigo = tt-aux.it-codigo AND 
                 saldo-estoq.qtidade-atu > 0 NO-LOCK.
    
            FIND liquida-ima WHERE
                 liquida-ima.cod-estabel = saldo-estoq.cod-estabel AND
                 liquida-ima.it-codigo   = saldo-estoq.it-codigo  AND
                 liquida-ima.cod-refer   = saldo-estoq.cod-refer  AND
                 liquida-ima.dt-final    = ? 
                 SHARE-LOCK NO-ERROR.
    
            IF AVAIL liquida-ima THEN
               ASSIGN liquida-ima.dt-final = TODAY.
    
            // Cria Novo Registro com o novo percentual
            RUN esapi/calcula-id.p (OUTPUT c-num-id).
            
            CREATE liquida-ima.
            ASSIGN liquida-ima.cod-estabel   = saldo-estoq.cod-estabel
                   liquida-ima.it-codigo     = saldo-estoq.it-codigo
                   liquida-ima.cod-refer     = saldo-estoq.cod-refer
                   liquida-ima.preco-item    = tt-aux.preco-item
                   liquida-ima.usuario       = 'super'
                   liquida-ima.dt-inicio     = TODAY
                   liquida-ima.num-id-liquida-ima = c-num-id.
        END.
    END.
END.

