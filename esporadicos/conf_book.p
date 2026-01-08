 DEFINE VARIABLE iQt AS INTEGER     NO-UNDO.
 FOR EACH relacs_item_ref
WHERE
     // it_codigo = '565496'.

    relac_item_arquivo_id = 40637:
    //DISP relacs_item_ref WITH 1 COL WIDTH 550.
    ASSIGN iQt = iQt + 1.
    //DELETE relacs_item_ref.
    DISPLAY relacs_item_ref  WITH WIDTH 550.
    UPDATE it_codigo.
   // DISP INT(cod_tipo_relac).

END.

DISP iQt.
