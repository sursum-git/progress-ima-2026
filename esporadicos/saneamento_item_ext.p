FOR EACH item-ext
WHERE item-ext.cod_tipo_item = 0
AND item-ext.cod_form_exib = 0 :
    FIND ITEM OF item-ext
        WHERE ge-codigo >= 50 AND ge-codigo <= 60  NO-LOCK NO-ERROR.
    IF NOT AVAIL ITEM THEN NEXT.
    IF substr(item-ext.it-codigo,3,1) = '5' THEN
       ASSIGN item-ext.cod_tipo_item = 2
              item-ext.cod_form_exib = 2.
    ELSE
      ASSIGN item-ext.cod_tipo_item = 1
              item-ext.cod_form_exib = 1.
DISP ITEM.it-codigo.

END.
