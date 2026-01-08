//22302
DEFINE VARIABLE iRepres AS INTEGER     NO-UNDO.
DEFINE VARIABLE perc AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cNota AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSerie AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEstabel AS CHARACTER   NO-UNDO.


UPDATE cEstabel cSerie cNota iRepres perc.
FIND repres WHERE repres.cod-rep = iRepres
    NO-LOCK NO-ERROR.

FOR EACH nota-fiscal
    WHERE nota-fiscal.cod-estabel = cEstabel
    AND   nota-fiscal.serie = cSerie
    AND   nota-fiscal.nr-nota-fis = cNota .
    FOR EACH fat-repre 
        WHERE fat-repre.cod-estabel =  nota-fiscal.cod-estabel
        AND   fat-repre.serie       =  nota-fiscal.serie
        AND   fat-repre.nr-fatura    = nota-fiscal.nr-fatura:
        ASSIGN fat-repre.nome-ab-rep   = repres.nome-abrev
               fat-repre.perc-comis  = perc.

    END.
    ASSIGN nota-fiscal.cod-rep = repres.cod-rep
           nota-fiscal.no-ab-reppri = repres.nome-abrev.
    DISP nota-fiscal.no-ab-reppri.
END.
