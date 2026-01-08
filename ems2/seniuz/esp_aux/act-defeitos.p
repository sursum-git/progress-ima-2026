DEF VAR de-tot-def AS DEC.
FOR EACH mov-est-acbm WHERE
         mov-est-acbm.data-mov >= 09.01.2005 and
         mov-est-acbm.data-mov <= 09.30.2005 EXCLUSIVE-LOCK.

    ASSIGN de-tot-def = 0.
    FOR EACH mov-est-acbd OF mov-est-acbm NO-LOCK.
        ASSIGN de-tot-def = de-tot-def + mov-est-acbd.qtd-defeit.
    END.
       
    IF de-tot-def <> mov-est-acbm.qtd-tot-def THEN
       DISP mov-est-acbm.qtd-tot-def 
            de-tot-def.
END.
