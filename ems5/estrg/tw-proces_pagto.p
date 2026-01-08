DEFINE PARAMETER BUFFER b-proces_pagto_new FOR proces_pagto.
DEFINE PARAMETER BUFFER b-proces_pagto_old FOR proces_pagto.
DEFINE BUFFER bf FOR proces_pagto.
DEFINE VARIABLE h AS HANDLE      NO-UNDO.
RUN value("t:\especificos\esporadicos\lOG.p") PERSISTENT SET h.
RUN arquivoSaida IN h('proces_pagto').


DEFINE VARIABLE deSoma AS DECIMAL     NO-UNDO.

DEFINE VARIABLE cProgramas AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
DEFINE VARIABLE level      AS INTEGER     NO-UNDO INIT 1.
DEFINE TEMP-TABLE tt       LIKE proces_pagto.

FIND FIRST b-proces_pagto_old NO-LOCK NO-ERROR.
FIND FIRST b-proces_pagto_new NO-LOCK NO-ERROR.
MESSAGE 'entrei na trigger'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

IF AVAIL b-proces_pagto_new THEN DO:
    IF b-proces_pagto_new.ind_sit_proces_pagto = 'confirmado' THEN DO:
       RUN incluirLog IN h('proces_pagto_new','situacao = confirmado') .
       ASSIGN deSoma = 0.
       FOR EACH bf NO-LOCK
           WHERE bf.cod_estab               = b-proces_pagto_new.cod_estab             
           AND   bf.cod_espec_docto         = b-proces_pagto_new.cod_espec_docto       
           AND   bf.cod_ser_docto           = b-proces_pagto_new.cod_ser_docto         
           AND   bf.cdn_fornecedor          = b-proces_pagto_new.cdn_fornecedor        
           AND   bf.cod_tit_ap              = b-proces_pagto_new.cod_tit_ap            
           AND   bf.cod_parcela             = b-proces_pagto_new.cod_parcela           
           AND   bf.cod_refer_antecip_pef   = b-proces_pagto_new.cod_refer_antecip_pef :
           ASSIGN deSoma = deSoma + bf.val_liberd_pagto . 
       END.
    END.
    FIND FIRST tit_ap OF b-proces_pagto_new 
        NO-LOCK NO-ERROR.
    IF AVAIL tit_ap THEN DO:
        RUN incluirLog IN h('proces_pagto_new-> tit_ap','a aprova‡Æo pertence a um titulo a pagar').
        IF tit_ap.val_origin_tit_ap > deSoma THEN DO:
           RUN incluirLog IN h('proces_pagto_new-> tit_ap -> tit_ap.val_origin_tit_ap > deSoma ','a soma dos valores liberados (' + 
                               string(deSoma)  
                               + ') para pagamento ‚ menor que o valor total do titulo (' + string(tit_ap.val_origin_tit_ap) + ')'  ).
           CREATE tt.
           BUFFER-COPY  b-proces_pagto_new TO tt.
           ASSIGN tt.val_liberd_pagto       = tit_ap.val_origin_tit_ap - deSoma
                  tt.val_liber_pagto_orig   = tt.val_liberd_pagto
                  tt.num_seq_pagto_tit_ap   = tt.num_seq_pagto_tit_ap + 1 
                  tt.ind_sit_proces_pagto   = "LIBERADO".
        END.
    END.
    ELSE DO:
        RUN incluirLog IN h('proces_pagto_new','nÆo pertence a um titulo').
        FIND FIRST antecip_pef_pend
            WHERE  antecip_pef_pend.cod_estab = b-proces_pagto_new.cod_estab
            AND antecip_pef_pend.cod_refer    = b-proces_pagto_new.cod_refer_antecip_pef
            NO-LOCK NO-ERROR.
        IF AVAIL antecip_pef_pend THEN DO:
           RUN incluirLog IN h('proces_pagto_new -> antecip_pef_pend','a aprova‡Æo pertence a um PEF').
           IF antecip_pef_pend.val_tit_ap > deSoma THEN DO:
              RUN incluirLog IN h('proces_pagto_new -> antecip_pef_pen -> antecip_pef_pend.val_tit_ap ','a soma dos valores liberados para pagamento ‚ menor que o valor total do PEF').
              CREATE tt.
              BUFFER-COPY  b-proces_pagto_new TO tt.
              ASSIGN tt.val_liberd_pagto       = antecip_pef_pend.val_tit_ap - deSoma
                  tt.val_liber_pagto_orig      = tt.val_liberd_pagto
                  tt.num_seq_pagto_tit_ap      = tt.num_seq_pagto_tit_ap + 1 
                  tt.ind_sit_proces_pagto      = "LIBERADO".
 
           END.
        END.
    END.
END.
FOR EACH tt:
    CREATE proces_pagto.
    BUFFER-COPY tt TO proces_pagto.
END.

RUN imprimirTxt IN h.
DELETE PROCEDURE h.





/*MESSAGE "anterior:" b-proces_pagto_old.val_liberd_pagto SKIP
        "novo:" b-proces_pagto_new.val_liberd_pagto
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

REPEAT WHILE PROGRAM-NAME(level) <> ?:  
    ASSIGN cProgramas = IF cProgramas = '' THEN PROGRAM-NAME(level) ELSE cProgramas + "," +  PROGRAM-NAME(level)
           level = level + 1.
END.*/



