FOR EACH aprop_ctbl_acr
WHERE cod_cta_ctbl = '19000020' AND dat_transacao >=04.14.2015  BY dat_transacao:
    DISP dat_transacao cod_estab val_aprop_ctbl ind_natur_lancto_ctbl log_ctbz_aprop_ctbl.
    
    FIND FIRST movto_tit_acr OF aprop_ctbl_acr.
    DISP movto_tit_acr WITH 1 COL WIDTH 550.
    FIND FIRST tit_acr OF movto_tit_acr NO-LOCK NO-ERROR.
    IF AVAIL tit_acr THEN
       DISP tit_acr.cod_tit_acr tit_acr.cod_ser_docto tit_Acr.cdn_cliente tit_acr.cod_parcela.
    UPDATE  cod_cta_ctbl.
END.
