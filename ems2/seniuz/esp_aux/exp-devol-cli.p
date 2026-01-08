OUTPUT TO "c:\temp\lixo.txt".
PUT "Est;" "Serie;" "NF;" "Emitente" SKIP.
FOR EACH devol-cli WHERE devol-cli.cod-estabel =  "2"
                     AND devol-cli.dt-devol    >= 06/01/2005
                     AND devol-cli.dt-devol    <= 06/30/2005
                   NO-LOCK:
    EXPORT DELIMITER ";"
           cod-estabel
           serie
           nr-nota-fis
           cod-emitente.
END.
OUTPUT CLOSE.
