FOR EACH antecip_pef_pend NO-LOCK
    WHERE antecip_pef_pend.cod_estab          = '501'
    AND   antecip_pef_pend.dat_vencto_tit_ap  = 06.22.2016:
    
    /*FIND FIRST emitente WHERE
        emitente.cod-emitente = antecip_pef_pend.cdn_fornecedor
        NO-LOCK NO-ERROR.*/
    
    FOR EACH  proces_pagto 
        WHERE PROCES_PAGTO.COD_ESTAB = ANTECIP_PEF_PEND.COD_ESTAB
        AND   PROCES_PAGTO.COD_REFER_ANTECIP_PEF = ANTECIP_PEF_PEND.COD_REFER.
        DISP proces_pagto WITH 1 COL WIDTH 550.
        /*MESSAGE 'deseja apagar?' UPDATE ljoice AS LOGICAL
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL .
        IF ljoice THEN
           DELETE proces_pagto.*/
        /*UPDATE proces_pagto EXCEPT WITH 1 COL WIDTH 50.*/
        /*MESSAGE 'deseja liberar?' UPDATE ljoice AS LOGICAL
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL .
        */
           UPDATE ind_sit_proces_pagto.
    END.
    

    /*
     caso o usuario n∆o selecione os j† aprovados ser∆o desconsiderados os registros
     j† liberados e ou confirmados
    */       

    
END.
