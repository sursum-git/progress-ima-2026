DEF VAR c-seg-usuario AS CHAR INIT "kleyderson".
DEF VAR c-grupos AS CHAR FORMAT "x(20)".
DEF VAR l-ok AS LOG.

ASSIGN l-ok = NO.
FIND FIRST espec.param-dis NO-LOCK NO-ERROR.
ASSIGN c-grupos = espec.param-dis.grp-alt-ped.
FOR EACH usuar_grp_usuar WHERE usuar_grp_usuar.cod_usuario = c-seg-usuario NO-LOCK:
    IF INDEX(c-grupos,usuar_grp_usuar.cod_grp_usuar) <> 0 THEN DO:
       ASSIGN l-ok = YES.
       LEAVE.
    END.
END.
DISP IF l-ok THEN "Pode"
             ELSE "NÆo pode".
