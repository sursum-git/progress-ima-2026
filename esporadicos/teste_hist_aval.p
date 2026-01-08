FOR LAST peds_web
    WHERE login = 'carniel'
    AND date(peds_web.dt_hr_registro) = TODAY.
    DISP peds_web.ped_web_id   .
    UPDATE ind_sit_ped_web .
    FIND LAST hist_aval_ped_venda
        WHERE hist_aval_ped_venda.cod_estab = ''
        AND   hist_aval_ped_venda.nr_pedido = 0
        AND   hist_aval_ped_venda.ped_web_id = peds_web.ped_web_id
        SHARE-LOCK NO-ERROR.
    IF AVAIL hist_aval_ped_venda THEN
        DISP hist_aval_ped_venda EXCEPT DESC_motivo WITH 1 COL WIDTH 550 FRAME fr.
    //DELETE hist_aval_ped_venda.
END.

