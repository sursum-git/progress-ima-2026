FOR EACH tit_ap
    WHERE cod_tit_ap = 'HR01006573'
    OR cod_tit_ap = 'HR01006574'
    :
    UPDATE tit_ap WITH 1 COL WIDTH 550.
    FOR EACH movto_tit_ap OF tit_ap.
        UPDATE movto_tit_ap WITH 1 COL WIDTH 550.
    END.
END.
