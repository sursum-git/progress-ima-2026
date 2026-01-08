
DEFINE VARIABLE refer AS CHARACTER   NO-UNDO.
REPEAT:
    UPDATE refer.
    FOR EACH lote_pagto
    WHERE cod_refer = refer:
        DISP lote_pagto WITH 1 COL WIDTH 550.
        FOR EACH ITEM_lote_pagto OF lote_pagto:
            DISP ITEM_lote_pagto WITH 1 COL WIDTH 550.
            MESSAGE 'deseja deletar?' UPDATE lresposta AS log
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL.
    
            IF lresposta THEN
               DELETE   ITEM_lote_pagto.
        END.
        IF lresposta THEN
           DELETE lote_pagto.
    END.
END.
