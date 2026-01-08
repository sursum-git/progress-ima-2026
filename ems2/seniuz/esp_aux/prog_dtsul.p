FOR EACH prog_dtsul WHERE /*prog_dtsul.cod_prog_dtsul BEGINS "cd" 
                      AND */ prog_dtsul.des_prog_dtsul MATCHES "*Man*"
                      AND prog_dtsul.des_prog_dtsul MATCHES "*Fornec*"
                    NO-LOCK.
    DISP prog_dtsul.cod_prog_dtsul
         prog_dtsul.des_prog_dtsul.
END.
