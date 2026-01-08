DEF VAR c-cod-estabel AS CHAR.
OUTPUT TO c:/temp/usuar-estab.csv CONVERT SOURCE "ibm850".
PUT "Codigo;Estab;Nome" SKIP.
FOR EACH usuar_mestre NO-LOCK.
    RUN esapi/busca-estab-usuar.p (INPUT usuar_mestre.cod_usuario,
                                   OUTPUT c-cod-estabel).
    PUT usuar_mestre.cod_usuario ";"
        c-cod-estabel ";"
        usuar_mestre.nom_usuario SKIP.
END.
OUTPUT CLOSE.
