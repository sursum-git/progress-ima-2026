DEF BUFFER b-prog_dtsul_segur FOR prog_dtsul_segur.

OUTPUT TO c:\temp\t1.txt.

FOR EACH prog_dtsul_segur WHERE 
         prog_dtsul_segur.cod_grp_usuar = '*' EXCLUSIVE-LOCK.

    FIND prog_dtsul WHERE
         prog_dtsul.cod_prog_dtsul = prog_dtsul_segur.cod_prog_dtsul
         NO-LOCK NO-ERROR.

    IF prog_dtsul.idi_template = 1 OR
       prog_dtsul.idi_template = 13 OR
       prog_dtsul.idi_template = 18 OR
       prog_dtsul.idi_template = 27 THEN NEXT.

    IF prog_dtsul_segur.cod_prog_dtsul BEGINS 'g0' OR
       prog_dtsul_segur.cod_prog_dtsul BEGINS 'g1' THEN NEXT.

    IF prog_dtsul_segur.cod_prog_dtsul BEGINS 'b0' OR
       prog_dtsul_segur.cod_prog_dtsul BEGINS 'bo' 
       THEN NEXT.

    IF prog_dtsul_segur.cod_prog_dtsul BEGINS 'z0' OR
       prog_dtsul_segur.cod_prog_dtsul BEGINS 'z2' OR
       prog_dtsul_segur.cod_prog_dtsul BEGINS 'z6' OR
       prog_dtsul_segur.cod_prog_dtsul BEGINS 'z9' OR
       prog_dtsul_segur.cod_prog_dtsul BEGINS 'z1' THEN NEXT.

    /*
    FIND b-prog_dtsul_segur WHERE
         b-prog_dtsul_segur.cod_prog_dtsul = prog_dtsul_segur.cod_prog_dtsul AND
         b-prog_dtsul_segur.cod_grp_usuar  = 'sup'
         NO-LOCK NO-ERROR.

    DISP prog_dtsul_segur.cod_prog_dtsul FORMAT "x(30)".
    PAUSE 0.

    IF NOT AVAIL b-prog_dtsul_segur THEN DO.
       ASSIGN prog_dtsul_segur.log_livre_2 = YES.

       ASSIGN prog_dtsul_segur.cod_grp_usuar = 'SUP'.
    END.
    */

    EXPORT prog_dtsul_segur.

    DELETE prog_dtsul_segur.
END.

OUTPUT CLOSE.

