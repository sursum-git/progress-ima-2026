DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF VAR l-acesso AS LOG.
ASSIGN l-acesso = NO.

FOR EACH usuar_grp_usuar WHERE
         usuar_grp_usuar.cod_usuario = c-seg-usuario NO-LOCK.
    FIND prog_dtsul_segur WHERE
         prog_dtsul_segur.cod_prog_dtsul = "{1}" AND
         prog_dtsul_segur.cod_grp_usuar = usuar_grp_usuar.cod_grp_usuar
         NO-LOCK NO-ERROR.
    IF AVAIL prog_dtsul_segur THEN DO:
        ASSIGN l-acesso = YES.
    END.

END.
IF l-acesso = NO THEN DO:
    RUN utp/ut-msgs.p(INPUT "show",
                       INPUT 2858,
                       INPUT RETURN-VALUE).
    RETURN.
END.

