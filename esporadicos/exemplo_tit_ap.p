FOR EACH tit_ap
    WHERE tit_ap.cod_estab        = '501'
    AND   tit_ap.cdn_fornecedor   = 27946
    AND   tit_ap.cod_espec_docto  = 'AR'
    AND   tit_ap.cod_ser_docto    = ''
    AND   tit_ap.cod_tit_ap       = '27946/19'
    AND   tit_ap.cod_parcela      = '06'.
    DISP tit_ap WITH 1 COL WIDTH 550.
    FOR EACH movto_tit_ap OF tit_ap.
        DISP movto_tit_ap WITH 1 COL WIDTH 550.
    END.

END.

