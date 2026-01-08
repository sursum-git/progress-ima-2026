DEF VAR c-programa AS CHAR.
DEF VAR c-prog2 LIKE prog_dtsul.cod_prog_dtsul.

UPDATE c-programa.

FIND prog_dtsul WHERE
     prog_dtsul.nom_prog_ext MATCHES  '*' + c-programa  + '*' NO-LOCK NO-ERROR.

ASSIGN c-prog2 = prog_dtsul.cod_prog_dtsul.

UPDATE c-prog2 FORMAT "x(34)".


