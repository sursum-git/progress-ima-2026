
DEFINE VARIABLE d AS DECIMAL     NO-UNDO.
DEFINE VARIABLE c AS DECIMAL     NO-UNDO.
ASSIGN d = 0
       c = 0.
OUTPUT TO c:\temp\corrigeEspecnacional909.txt.
FOR EACH movto_tit_ap NO-LOCK
    /*WHERE cod_espec_docto = 'dp'*/
     /*WHERE( cod_espec_docto = 'ci'
         OR cod_espec_docto = 'pi'
         OR cod_espec_docto = 'FT')
    AND cod_usuario = 'sfaria'     */
    WHERE cod_empresa = '500'
    /*AND ind_trans_ap_abrev = 'bxa'*/
    /*AND  log_bxa_contra_antecip = YES*/ : 
     FIND FIRST emitente
        WHERE emitente.cod-emitente  = movto_tit_ap.cdn_fornecedor
        AND emitente.cod-gr-forn = 1 NO-LOCK NO-ERROR.

    IF NOT AVAIL emitente THEN NEXT.
   /* DISP cod_tit_ap cod_espec_docto  dat_transacao ind_trans_ap_abrev ind_motiv_acerto_val.*/
    
    FIND FIRST movto_cta_corren OF movto_tit_ap NO-LOCK NO-ERROR.
    FIND FIRST cta_corren OF movto_cta_corren NO-LOCK NO-ERROR.

    FIND LAST cta_corren_cta_ctbl OF cta_corren NO-LOCK NO-ERROR.

    FIND FIRST tit_ap OF movto_tit_ap NO-LOCK NO-ERROR.

    ASSIGN c = 0.
    FOR EACH aprop_ctbl_ap OF movto_tit_ap
       WHERE cod_cta_ctbl = '21202001' EXCLUSIVE-LOCK.
       ASSIGN c = c + 1.
        /*DISP aprop_ctbl_ap.cod_cta_ctbl.              
        ASSIGN cod_cta_ctbl = '21201001'*/ 
       /*DISP cod_cta_ctbl ind_tip_aprop_ctbl ind_natur_lancto_ctbl.*/
        
        /*DISP aprop_ctbl_ap WITH 1 COL WIDTH 550.*/

        /*WHERE cod_cta_ctbl = '11102341'*/ .
        /*FIND FIRST movto_cta_corren OF movto_tit_acr
            WHERE cod_cta_corren = '71537-6' NO-LOCK NO-ERROR.*/
        
                                                 
    END.
    IF c = 2 THEN DO:
       DISP tit_ap.cod_tit_ap tit_ap.cod_espec_docto tit_ap.dat_transacao ind_trans_ap_abrev ind_motiv_acerto_val /*movto_cta_corren.cod_cta_corren*/ .
       FOR EACH aprop_ctbl_ap OF movto_tit_ap
       WHERE cod_cta_ctbl = '21202001'
           AND ind_natur_lancto_ctbl = 'cr' EXCLUSIVE-LOCK.
           DISP cod_cta_ctbl ind_tip_aprop_ctbl ind_natur_lancto_ctbl /*cta_corren_cta_ctbl.cod_cta_ctbl COLUMN-LABEL "nova conta"*/ .
           /*ASSIGN cod_cta_ctbl = '11501001'.*/
       /*ASSIGN c = c + 1.*/
        /*DISP aprop_ctbl_ap.cod_cta_ctbl.              
        ASSIGN cod_cta_ctbl = '21201001'*/ 
       /*DISP cod_cta_ctbl ind_tip_aprop_ctbl ind_natur_lancto_ctbl.*/
        
        /*DISP aprop_ctbl_ap WITH 1 COL WIDTH 550.*/

        /*WHERE cod_cta_ctbl = '11102341'*/ .
        /*FIND FIRST movto_cta_corren OF movto_tit_acr
            WHERE cod_cta_corren = '71537-6' NO-LOCK NO-ERROR.*/
       END.
                                                 
    END.


   

END.       
OUTPUT CLOSE.


