def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.txt" CONVERT SOURCE "ibm850".
    
FOR each espec._file NO-LOCK.
    FOR EACH espec._field WHERE espec._field._file-recid = recid(espec._file)
                            AND espec._field._field-name MATCHES "*ob*".
        DISP espec._file._file-name
             espec._field._field-name.
    END.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "wordpad.exe", input "c:\lixo\lixo.txt").
delete procedure h-prog.

