DEF TEMP-TABLE tt-nf    LIKE gw-nf.
DEF TEMP-TABLE tt-fibro LIKE gw-fibro.

DEF VAR c-comando AS CHAR.
DEF VAR i-arquivo AS INT.
DEF VAR c-dir-fibro AS CHAR.
DEF VAR c-arq-dbf AS CHAR.
DEF VAR c-arq-err AS CHAR.
DEF VAR c-arq-dump   AS CHAR.

ASSIGN c-dir-fibro = "m:\fibro"
       i-arquivo = 1763.

RUN pi-imp-nf.
RUN pi-imp-fibro.

FOR EACH gw-nf WHERE
         gw-nf.arquivo = i-arquivo AND
         gw-nf.dt-analise = TODAY NO-LOCK,
    EACH gw-fibro WHERE
         gw-fibro.arquivo = gw-nf.arquivo NO-LOCK
         BY gw-fibro.nfabr.
               
    DISP gw-fibro.sl1 * gw-nf.fc1
         gw-fibro.sl2 * gw-nf.fc2.
END.
 

PROCEDURE pi-imp-nf.

    ASSIGN c-arq-dbf = 'C' + STRING(i-arquivo,"9999999") + '.DBF'
           c-arq-err = SESSION:TEMP-DIRECTORY + REPLACE(c-arq-dbf,"DBF","ERR")
           c-arq-dump = SESSION:TEMP-DIRECTORY + REPLACE(c-arq-dbf,"DBF","D").

    ASSIGN c-comando = "K:\bin\dbf.exe 1 1 " + c-dir-fibro + "\" + c-arq-dbf + " " +
                       c-arq-err + " > " + c-arq-dump.

    OS-COMMAND SILENT VALUE(c-comando).

    EMPTY TEMP-TABLE tt-nf.

    INPUT FROM VALUE(c-arq-dump).
    REPEAT.
        CREATE tt-nf.
        IMPORT tt-nf.
        ASSIGN tt-nf.arquivo = i-arquivo
               tt-nf.dt-analise = TODAY.
    END.

    FOR EACH tt-nf.
        ASSIGN tt-nf.fc1     = tt-nf.fc1 / 1000
               tt-nf.minsl1  = tt-nf.minsl1 / 100
               tt-nf.iv1     = tt-nf.iv1 / 100
               tt-nf.fc2     = tt-nf.fc2 / 1000
               tt-nf.minsl2  = tt-nf.minsl2 / 100
               tt-nf.iv2     = tt-nf.iv2 / 100
               tt-nf.minunif = tt-nf.minunif / 100.
        IF tt-nf.nfiscal = 0 THEN
           DELETE tt-nf.
    END.

    FOR EACH gw-nf.
        DELETE gw-nf.
    END.

    FOR EACH tt-nf.
        CREATE gw-nf.
        BUFFER-COPY tt-nf TO gw-nf.
    END.

    OS-DELETE VALUE(c-arq-err).
    OS-DELETE VALUE(c-arq-dump).
END PROCEDURE.


PROCEDURE pi-imp-fibro.
    ASSIGN c-arq-dbf = 'F' + STRING(i-arquivo,"9999999") + '.DBF'
           c-arq-err = SESSION:TEMP-DIRECTORY + REPLACE(c-arq-dbf,"DBF","ERR")
           c-arq-dump = SESSION:TEMP-DIRECTORY + REPLACE(c-arq-dbf,"DBF","D").

    ASSIGN c-comando = "K:\bin\dbf.exe 1 1 " + c-dir-fibro + "\" + c-arq-dbf + " " +
                       c-arq-err + " > " + c-arq-dump.

    OS-COMMAND SILENT VALUE(c-comando).

    EMPTY TEMP-TABLE tt-fibro.

    INPUT FROM VALUE(c-arq-dump).
    REPEAT.
        CREATE tt-fibro.
        IMPORT tt-fibro.
        ASSIGN tt-fibro.arquivo = i-arquivo. 
    END.

    FOR EACH tt-fibro.
        ASSIGN tt-fibro.sl1 = tt-fibro.sl1 / 100
               tt-fibro.sl2 = tt-fibro.sl2 / 100
               tt-fibro.unif = tt-fibro.unif / 100
               tt-fibro.z = ''.
        IF tt-fibro.fardo = 0 THEN
           DELETE tt-fibro.
    END.

    FOR EACH gw-fibro.
        DELETE gw-fibro.
    END.

    FOR EACH tt-fibro.
        CREATE gw-fibro.
        BUFFER-COPY tt-fibro TO gw-fibro.
    END.

    OS-DELETE VALUE(c-arq-err).
    OS-DELETE VALUE(c-arq-dump).
END PROCEDURE.

