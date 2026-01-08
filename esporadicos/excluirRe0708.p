FOR EACH doc-orig-nfe
 WHERE doc-orig-nfe.ch-acesso-comp-nfe = '42250408248539000842550020001658971904636264' :
    DISP doc-orig-nfe.serie-docto
         doc-orig-nfe.nro-docto
        WITH 1 COL WIDTH 550.
    FOR EACH item-doc-orig-nfe OF doc-orig-nfe:
        DISP item-doc-orig-nfe.it-codigo WITH 1 COL WIDTH 550.
        DELETE item-doc-orig-nfe.
    END.
    DELETE doc-orig-nfe.    
        
END.
