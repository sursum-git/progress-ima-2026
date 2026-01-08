FOR EACH prog_dtsul NO-LOCK.
    FOR EACH prog_dtsul_segur OF prog_dtsul.
        RUN pi-proced (INPUT prog_dtsul.cod_proced).
    END. 
END.

PROCEDURE pi-proced.
    DEF INPUT PARAMETER p-prog AS CHAR.
    FIND procedimento WHERE
         procedimento.cod_proced = p-prog NO-ERROR.

    RUN pi-modulo (INPUT procedimento.cod_modul_dtsul).

    FIND proced_segur OF procedimento WHERE
         proced_segur.cod_grp_usuar = prog_dtsul_segur.cod_grp_usuar NO-ERROR.

    IF NOT AVAIL proced_segur THEN DO.
       CREATE proced_segur.
       ASSIGN proced_segur.cod_proced = procedimento.cod_proced
              proced_segur.cod_grp_usuar = prog_dtsul_segur.cod_grp_usuar.
    END.
END PROCEDURE.

PROCEDURE pi-modulo.
    DEF INPUT PARAMETER p-modulo AS CHAR.
    FIND modul_dtsul WHERE
         modul_dtsul.cod_modul_dtsul = p-modulo NO-ERROR.

    RUN pi-sistema (INPUT modul_dtsul.cod_sist_dtsul).

    FIND modul_dtsul_segur OF modul_dtsul WHERE
         modul_dtsul_segur.cod_grp_usuar = prog_dtsul_segur.cod_grp_usuar NO-ERROR.

    IF NOT AVAIL modul_dtsul_segur THEN DO.
       CREATE modul_dtsul_segur.
       ASSIGN modul_dtsul_segur.cod_modul_dtsul = modul_dtsul.cod_modul_dtsul
              modul_dtsul_segur.cod_grp_usuar = prog_dtsul_segur.cod_grp_usuar.
    END.
END PROCEDURE.

PROCEDURE pi-sistema.
    DEF INPUT PARAMETER p-sistema AS CHAR.
    FIND sist_dtsul WHERE
         sist_dtsul.cod_sist_dtsul = p-sistema NO-ERROR.

    RUN pi-aplic (INPUT sist_dtsul.cod_aplicat_dtsul).

    FIND sist_dtsul_segur OF sist_dtsul WHERE
         sist_dtsul_segur.cod_grp_usuar = prog_dtsul_segur.cod_grp_usuar NO-ERROR.

    IF NOT AVAIL sist_dtsul_segur THEN DO.
       CREATE sist_dtsul_segur.
       ASSIGN sist_dtsul_segur.cod_sist_dtsul = sist_dtsul.cod_sist_dtsul
              sist_dtsul_segur.cod_grp_usuar = prog_dtsul_segur.cod_grp_usuar.
    END.
END PROCEDURE.

PROCEDURE pi-aplic.
    DEF INPUT PARAMETER p-aplic AS CHAR.
    FIND aplicat_dtsul WHERE
         aplicat_dtsul.cod_aplicat_dtsul = p-aplic NO-ERROR.

    IF p-aplic = "tec" AND prog_dtsul_segur.cod_grp_usuar <> "sup" THEN NEXT.    
    
    FIND aplicat_dtsul_segur OF aplicat_dtsul WHERE
         aplicat_dtsul_segur.cod_grp_usuar = prog_dtsul_segur.cod_grp_usuar NO-ERROR.

    IF NOT AVAIL aplicat_dtsul_segur THEN DO.
       CREATE aplicat_dtsul_segur.
       ASSIGN aplicat_dtsul_segur.cod_aplicat_dtsul = aplicat_dtsul.cod_aplicat_dtsul
              aplicat_dtsul_segur.cod_grp_usuar = prog_dtsul_segur.cod_grp_usuar.
    END.
END PROCEDURE.


