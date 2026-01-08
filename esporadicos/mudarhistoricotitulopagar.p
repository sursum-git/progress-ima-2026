FOR EACH tit_ap
    WHERE cdn_fornec = 5738
    AND   cod_tit_ap = '0066000'
    AND cod_parcela = '01'.
    FOR EACH movto_tit_ap OF tit_ap:
        FOR EACH histor_tit_movto_ap WHERE 
            histor_tit_movto_ap.num_id_tit_ap = tit_ap.num_id_tit_ap 
            AND histor_tit_movto_ap.num_id_movto_tit_ap = movto_tit_ap.num_id_movto_tit_ap 
            AND histor_tit_movto_ap.cod_estab = tit_ap.cod_estab  :

            UPDATE histor_tit_movto_ap WITH 1 COL WIDTH 550.
        END.
    END.
END.
