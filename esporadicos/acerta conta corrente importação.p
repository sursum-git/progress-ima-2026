
DEFINE VARIABLE id AS INTEGER     NO-UNDO.

UPDATE id.
    
FOR EACH cta_corren_fornec_imp
    WHERE cta_corren_fornec_imp_id = id:
    DELETE cta_corren_fornec_imp.
END.
