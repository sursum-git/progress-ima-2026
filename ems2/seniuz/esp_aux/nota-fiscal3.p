OUTPUT TO c:/temp/notas_series.txt.
FOR EACH nota-fiscal NO-LOCK 
                     BREAK BY nota-fiscal.cod-estabel
                           BY nota-fiscal.serie:
    IF LAST-OF(nota-fiscal.serie) THEN
       DISP nota-fiscal.cod-estabel
            nota-fiscal.serie      
            nota-fiscal.nr-nota-fis.
END.
OUTPUT CLOSE.
