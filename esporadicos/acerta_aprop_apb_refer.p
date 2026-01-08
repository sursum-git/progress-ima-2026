DEFINE VARIABLE d AS DECIMAL     NO-UNDO.
DEFINE VARIABLE c AS DECIMAL     NO-UNDO.
ASSIGN d = 0
       c = 0.
FOR EACH movto_tit_ap NO-LOCK
    WHERE cod_refer = 'ejpef2711'
    USE-INDEX mvtttp_refer:
    
    FOR EACH aprop_ctbl_ap OF movto_tit_ap EXCLUSIVE-LOCK
        /*WHERE cod_cta_ctbl = '11102341'*/ .
        /*FIND FIRST movto_cta_corren OF movto_tit_acr
            WHERE cod_cta_corren = '71537-6' NO-LOCK NO-ERROR.
        IF AVAIL movto_cta_corren THEN DO:                    
            IF ind_natur_lancto_ctbl = 'db' THEN
               ASSIGN d = d  + val_movto_tit_acr.
            ELSE
               ASSIGN c = c + val_movto_tit_acr.              */

         DISP aprop_ctbl_ap WITH 1 COL WIDTH 550.
         UPDATE aprop_ctbl_ap.
                                                 
    END.
END.       

