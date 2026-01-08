
OUTPUT TO "c:/temp/cli_503413_05.txt".

FOR EACH it-nota-fisc WHERE it-nota-fisc.cod-estabel = "2"
                        AND it-nota-fisc.it-codigo   = "503413"
                        AND it-nota-fisc.cod-refer BEGINS "05"
                      NO-LOCK,
    EACH nota-fiscal OF it-nota-fisc 
    WHERE nota-fiscal.dt-cancela = ?
    NO-LOCK,
    EACH emitente OF nota-fiscal WHERE emitente.pais = "brasil" NO-LOCK
    BREAK BY emitente.cod-emitente:
    IF LAST-OF(emitente.cod-emitente) THEN
       DISPLAY emitente.cod-emitente
               emitente.nome-emit
               nota-fiscal.nr-nota-fis
               nota-fiscal.dt-emis-nota
               WITH WIDTH 132.

END.
OUTPUT CLOSE.
