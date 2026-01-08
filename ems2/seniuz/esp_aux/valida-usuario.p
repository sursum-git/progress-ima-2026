DEF VAR c-ok AS CHAR.
DEF VAR c-usuarios AS CHAR.

FOR EACH usuar_grp_usuar WHERE 
         usuar_grp_usuar.cod_grp_usuar = "EP0" NO-LOCK.
    ASSIGN c-usuarios = c-usuarios + usuar_grp_usuar.cod_usuar + ','.
END.

DISP c-usuarios.

RUN btb/btb910zc.p (INPUT c-usuarios, INPUT YES, INPUT YES, OUTPUT c-ok).

MESSAGE c-ok
   VIEW-AS ALERT-BOX INFO BUTTONS OK.
