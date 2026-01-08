OUTPUT TO "c:/lixo/doc-fiscal.csv".
PUT "Nome-Abrev;" "CNPJ;" "Serie;" "Nr-Docto;" "Data-Docto;" "Valor-Cont;"
    "Valor-IPI" SKIP.
FOR EACH doc-fiscal WHERE doc-fiscal.dt-docto >= 10/01/2005
                      AND doc-fiscal.dt-docto <= 05/31/2006
                      AND doc-fiscal.vl-ipi   <> 0
                    NO-LOCK.
    put doc-fiscal.nome-ab-emi ";"
        doc-fiscal.cgc ";"
        doc-fiscal.serie ";"
        doc-fiscal.nr-doc-fis ";"
        doc-fiscal.dt-docto ";"
        doc-fiscal.vl-cont-doc ";"
        doc-fiscal.vl-ipi
        SKIP.
END.
OUTPUT CLOSE.
