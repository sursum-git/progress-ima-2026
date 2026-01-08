FOR EACH relacs_item_arquivo
WHERE
/*     relac_item_arquivo_id = 29736
AND */
    it_codigo = ''    .
    AND cod_refer = ''                    :
    DISP relacs_item_arquivo WITH 1 COL WIDTH 550.
    PAUSE 0.
    FOR EACH relacs_item_Ref
        WHERE relacs_item_ref.relac_item_arquivo_id = relacs_item_arquivo.relac_item_arquivo_id.
        DISP relacs_item_ref NO-ERROR. PAUSE 0.
        DELETE relacs_item_Ref.


    END.

    DELETE relacs_item_arquivo.




















END.
