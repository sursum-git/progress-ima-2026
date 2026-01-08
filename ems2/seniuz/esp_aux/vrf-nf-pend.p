FOR EACH nota-fiscal WHERE
         nota-fiscal.cod-estabel = '2' AND
         nota-fiscal.dt-cancela = ? AND
         nota-fiscal.dt-confirma = ? AND
         nota-fiscal.emite-dupl NO-LOCK.
    DISPLAY nota-fiscal.nr-nota-fis
            nota-fiscal.dt-emis.
END.
