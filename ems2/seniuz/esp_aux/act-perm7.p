FOR EACH prog_dtsul WHERE
         prog_dtsul.cod_prog_dtsul BEGINS "FNC".

    FIND prog_dtsul_segur OF prog_dtsul WHERE 
         prog_dtsul_segur.cod_grp_usuar = "*" NO-LOCK NO-ERROR. 

    IF NOT AVAIL prog_dtsul_segur THEN DO.
       CREATE prog_dtsul_segur.
       ASSIGN prog_dtsul_segur.cod_prog_dtsul = prog_dtsul.cod_prog_dtsul
              prog_dtsul_segur.cod_grp_usuar = "*".
    END.
END.
