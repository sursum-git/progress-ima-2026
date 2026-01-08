FOR EACH TIT_AP
    WHERE tit_ap.cod_indic_econ = 'dolarc':
    DISP cod_tit_ap.
    ASSIGN tit_ap.cod_indic_econ = 'dolar'.
    FOR EACH movto_tit_ap OF tit_ap:
        UPDATE movto_tit_ap WITH 1 COL WIDTH 550.
        FOR EACH aprop_ctbl_ap OF movto_tit_ap:
        UPDATE aprop_ctbl_ap WITH 1 COL WIDTH 550.
        END.
    END.
    FOR EACH val_tit_ap OF tit_ap:
        UPDATE val_tit_ap WITH 1 COL WIDTH 550.
    END.
END.
