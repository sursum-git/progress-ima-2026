FOR EACH tit_ap
    WHERE tit_ap.cod_estab = '501'
    AND   tit_ap.cod_tit_ap = '0293137'
    AND   tit_ap.cod_espec_docto = 'ds'
    AND   tit_ap.cod_ser  = ''
    AND   tit_ap.cod_parcela = '01'
    AND   tit_ap.cdn_fornec = 11338 .
    //UPDATE  tit_ap WITH 1 COL WIDTH 550.
    FOR EACH movto_tit_ap OF tit_ap.
       // UPDATE movto_tit_ap WITH 1 COL WIDTH 550.
    END.
    FOR EACH val_tit_ap OF tit_ap.
        UPDATE val_tit_ap WITH 1 COL WIDTH 550 .
    END.

END.
