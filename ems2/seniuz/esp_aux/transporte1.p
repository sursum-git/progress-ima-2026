OUTPUT TO c:/temp/transporte.csv CONVERT SOURCE "ibm850".
DEF VAR c-cgc AS CHAR.
FOR EACH transporte NO-LOCK.
    IF transporte.natureza = 1 THEN
       ASSIGN c-cgc = string(transporte.cgc,"999.999.999-99").
    ELSE
       ASSIGN c-cgc = STRING(transporte.cgc,"99.999.999/9999-99").
    PUT transporte.cod-transp ";"
        transporte.nome ";"
        transporte.nome-abrev ";"
        transporte.cidade ";"
        transporte.estado ";"
        c-cgc FORMAT "x(19)" ";"
        transporte.ins-estadual
        SKIP.
END.
OUTPUT CLOSE.


