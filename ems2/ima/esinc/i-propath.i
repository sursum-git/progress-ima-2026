DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

FIND fnd_usuar_univ WHERE
     fnd_usuar_univ.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.

CASE fnd_usuar_univ.cod_empresa.
    WHEN '1' THEN DO.  /* Estou na IMA */
        IF INDEX(PROPATH,"IMA") > INDEX(PROPATH,"MED") THEN DO.  /* IMA est  depois da MED */
           ASSIGN PROPATH=REPLACE(PROPATH,"MED,","AUX,").
           ASSIGN PROPATH=REPLACE(PROPATH,"IMA,","MED,").
           ASSIGN PROPATH=REPLACE(PROPATH,"AUX,","IMA,").
        END.
    END.
    WHEN '5' THEN DO.  /* Estou na MED */ 
        IF INDEX(PROPATH,"MED") > INDEX(PROPATH,"IMA") THEN DO.  /* MED est  depois da IMA */
           ASSIGN PROPATH=REPLACE(PROPATH,"IMA,","AUX,").
           ASSIGN PROPATH=REPLACE(PROPATH,"MED,","IMA,").
           ASSIGN PROPATH=REPLACE(PROPATH,"AUX,","MED,").
        END.
    END.
END.
