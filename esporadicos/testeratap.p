FOR EACH tit_ap
    WHERE tit_ap.cod_estab = '501'
    AND   tit_ap.cod_espec_doc  = 'ft'
    AND   tit_ap.cdn_fornecedor = 27079
    AND   tit_ap.cod_tit_ap = '293959'
    AND   tit_ap.cod_ser_doc = ''
    AND tit_ap.cod_parcela = '1'.
    DISP num_id_tit_ap . 

    /*FOR EACH val_tit_ap OF tit_ap.
        UPDATE val_tit_ap WITH 1 COL WIDTH 550. 
    END.*/
    /*FOR EACH movto_tit_ap OF tit_ap:
        DISP movto_tit_ap WITH 1 COL WIDTH 550.

        FOR EACH rat_movto_tit_ap OF movto_tit_ap:
            DISP rat_movto_tit_ap WITH 1 COL WIDTH 550.
            UPDATE rat_movto_tit_ap.val_aprop_ctbl
                    rat_movto_tit_ap.cod_tip_fluxo.
        END.
        FOR EACH val_movto_ap OF movto_tit_ap.
            DISP val_movto_ap WITH 1 COL WIDTH 550.
        END.
    END.*/
END.
