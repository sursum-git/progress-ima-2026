OUTPUT TO "c:/temp/transporte.csv".

PUT "Codigo;" "Cgc;" "Nome;" "Nome-abrev;" "Telefone;" "Ramal;" "Telefax" SKIP.
FOR EACH transporte NO-LOCK BY transporte.cgc.
    put transporte.cod-transp ";"
        transporte.cgc ";"
        transporte.nome ";"
        transporte.nome-abrev ";"
        transporte.telefone ";"
        transporte.ramal ";"
        transporte.telefax
        SKIP.
END.
OUTPUT CLOSE.
