OUTPUT TO c:/temp/nf-transp-2009.csv.

FOR EACH nota-fiscal WHERE nota-fiscal.dt-emis-nota >= 01/01/2009
                       AND nota-fiscal.dt-emis-nota <= 12/31/2009
                       AND nota-fiscal.dt-cancela    = ?
    BREAK BY nota-fiscal.nome-transp:

    ACCUMULATE nota-fiscal.vl-tot-nota (TOTAL BY nota-fiscal.nome-transp).

    IF LAST-OF(nota-fiscal.nome-transp) THEN DO:
       FIND transporte WHERE transporte.nome-abrev = nota-fiscal.nome-transp
                       NO-LOCK.
       PUT UNFORMAT 
           transporte.nome ";"
           transporte.cidade ";"
           transporte.ins-estadual ";"
           transporte.telefone ";"
           transporte.contato ";"
           (ACCUM TOTAL BY nota-fiscal.nome-transp nota-fiscal.vl-tot-nota)
           SKIP.
    END.
END.
OUTPUT CLOSE.
