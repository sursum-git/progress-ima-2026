FOR EACH tit_Ap
    WHERE cod_estab =  '501'
    AND   cod_espec_docto = 'ct'
    AND   cod_ser_docto = '8'
    /*AND   cod_tit_ap = '0322197'
    AND   cod_parcela = '01' */
    AND  cdn_fornecedor = 29452
    AND  dat_vencto_tit_ap = 11.25.2021 .

    DISP tit_ap WITH 1 COL WIDTH 550.
    FOR EACH val_tit_ap OF tit_ap.
        UPDATE val_tit_ap WITH 1 COL WIDTH 550.
    END.
    FOR EACH movto_tit_ap OF tit_ap :
        FOR EACH rat_movto_tit_ap OF movto_tit_ap .
            UPDATE rat_movto_tit_ap WITH 1 COL WIDTH 550.
        END.
    END.
END.
