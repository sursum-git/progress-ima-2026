DEFINE BUFFER bf_movto_tit_ap   FOR movto_tit_ap.
DEFINE BUFFER bf_tit_ap         FOR tit_ap.

FOR EACH tit_ap
    WHERE tit_ap.cod_estab        = '501'
    AND   tit_ap.cdn_fornecedor   = 27946
    AND   tit_ap.cod_espec_docto  = 'CO'
    AND   tit_ap.cod_ser_docto    = ''
    AND   tit_ap.cod_tit_ap       = '0000195'
    AND   tit_ap.cod_parcela      = '01'.

    DISP tit_ap.num_id_tit_ap WITH 1 COL.
    FOR EACH relacto_tit_ap 
        WHERE relacto_tit_ap.cod_estab_tit_ap_pai = tit_ap.cod_estab
      and relacto_tit_ap.num_id_Movto_tit_ap_pai = tit_ap.num_id_movto_tit_ap.
        DISP relacto_tit_ap WITH 1 COL WIDTH 550.
    END.
    
    FOR EACH movto_tit_ap OF tit_ap.
        DISP movto_tit_ap WITH 1 COL WIDTH 550.

        FIND FIRST bf_movto_tit_ap 
            WHERE bf_movto_tit_ap.cod_estab_tit_ap_pai = movto_tit_ap.cod_estab
            AND   bf_movto_tit_ap.num_id_movto_tit_ap_pai = movto_tit_ap.num_id_movto_tit_ap
            NO-LOCK NO-ERROR.
        IF AVAIL bf_movto_tit_ap THEN DO:
           FIND FIRST bf_tit_ap OF bf_movto_tit_ap NO-LOCK NO-ERROR.
           DISP bf_tit_ap.cod_tit_ap WITH TITLE 'relac'.
        END.
           //DISP bf_movto_tit_ap WITH 1 COL WIDTH 550.


        /*DISP movto_tit_ap WITH 1 COL WIDTH 550.
         FOR EACH relacto_tit_ap 
            WHERE relacto_tit_ap.cod_estab =  movto_tit_ap.cod_estab
            AND   relacto_tit_ap.num_id_movto_tit_ap = movto_tit_ap.num_id_movto_tit_ap.
                DISP relacto_tit_ap WITH 1 COL WIDTH 550.

         END.

        //DISP movto_tit_ap WITH 1 COL WIDTH 550.
        FOR EACH antecip_pef_pend 
            WHERE antecip_pef_pend.cod_estab =  movto_tit_ap.cod_estab
            AND antecip_pef_pend.cod_Refer = movto_tit_ap.cod_refer.
            DISP antecip_pef_pend EXCEPT dsl_obs_tit_ap WITH 1 COL WIDTH 550.

        END.
       /*FOR EACH relacto_pend_tit_ap OF movto_tit_ap.
       END.*/
       FOR EACH his_movto_tit_ap_histor OF movto_tit_ap.
           DISP his_movto_tit_ap_histor WITH 1 COL WIDTH 550.
        END.*/


    END.
    
END.
