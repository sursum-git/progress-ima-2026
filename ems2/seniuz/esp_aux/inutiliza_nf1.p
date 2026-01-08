FOR EACH nota-fiscal WHERE nota-fiscal.cod-estabel = "5"
                       AND nota-fiscal.serie       = "4"
                       AND nota-fiscal.nr-nota-fis = "0000030"
                     EXCLUSIVE-LOCK:
    DISP INT(nota-fiscal.ind-sit-nota).
    /*
    UPDATE nota-fiscal.ind-sit-nota VIEW-AS FILL-IN.
    */
END.
