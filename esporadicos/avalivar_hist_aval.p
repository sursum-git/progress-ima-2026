FOR EACH peds_web SHARE-LOCK
    WHERE peds_web.ped_web_id = 33029 .
    DISP nr_pedido_erp ind_sit_ped_web.
    UPDATE ind_sit_ped_web.
    FOR EACH hist_aval_ped_venda WHERE
        hist_aval_ped_venda.ped_web_id = peds_web.ped_web_id .
        DISP hist_aval_ped_venda EXCEPT desc_motivo WITH 1 COL WIDTH 550 NO-ERROR.
        UPDATE hist_aval_ped_venda_relac_id .
        MESSAGE "excluir" 
            VIEW-AS ALERT-BOX QUESTION  BUTTONS YES-NO-CANCEL UPDATE lExcluir AS LOGICAL .

        IF lExcluir THEN
           DELETE hist_aval_ped_venda .

    END.

END.
