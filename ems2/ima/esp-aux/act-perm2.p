DEF BUFFER b-prog_dtsul_segur FOR prog_dtsul_segur.

DEF VAR i-idi_dtsul AS INT.

FOR LAST prog_dtsul_segur BY prog_dtsul_segur.idi_dtsul.
    ASSIGN i-idi_dtsul = prog_dtsul_segur.idi_dtsul.
END.

FOR EACH prog_dtsul_segur BREAK BY prog_dtsul_segur.cod_prog_dtsul.

    IF FIRST-OF(prog_dtsul_segur.cod_prog_dtsul) THEN DO.

        FIND prog_dtsul WHERE 
             prog_dtsul.cod_prog_dtsul = prog_dtsul_segur.cod_prog_dtsul
             NO-LOCK NO-ERROR.

       FIND b-prog_dtsul_segur WHERE
            b-prog_dtsul_segur.cod_prog_dtsul = prog_dtsul_segur.cod_prog_dtsul AND
            b-prog_dtsul_segur.cod_grp_usuar  = 'sup'
            NO-LOCK NO-ERROR.

       IF NOT AVAIL b-prog_dtsul_segur THEN DO.
          FIND grp_usuar WHERE
               grp_usuar.cod_grp_usuar = 'SUP' NO-LOCK NO-ERROR.

          DISP prog_dtsul_segur.cod_prog_dtsul FORMAT "x(30)".
          PAUSE 0 .

          ASSIGN i-idi_dtsul = i-idi_dtsul + 1. 

          CREATE b-prog_dtsul_segur.                                                
          ASSIGN b-prog_dtsul_segur.cod_prog_dtsul = prog_dtsul_segur.cod_prog_dtsul
                 b-prog_dtsul_segur.cod_grp_usuar = 'SUP'
                 b-prog_dtsul_segur.idi_dtsul = i-idi_dtsul
                 b-prog_dtsul_segur.idi_dtsul_prog_dtsul = prog_dtsul.idi_dtsul
                 b-prog_dtsul_segur.idi_dtsul_grp_usuar = grp_usuar.idi_dtsul. 

       END.
    END.
END.

