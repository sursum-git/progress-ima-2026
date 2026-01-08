FOR EACH it-nota-fisc WHERE it-nota-fisc.cod-estabel = "2"
                        AND it-nota-fisc.serie       = "1"
                        AND it-nota-fisc.nr-nota-fis = "0158271".
    DISP it-nota-fisc.class-fiscal.
    UPDATE it-nota-fisc.class-fiscal.
END.
