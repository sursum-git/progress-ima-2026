DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR c-connect  AS CHARACTER FORMAT "x(100)" NO-UNDO.

IF CONNECTED ("dbaux") THEN DISCONNECT dbaux.
/*
FIND fnd_usuar_univ WHERE
     fnd_usuar_univ.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.

CASE fnd_usuar_univ.cod_empresa.
*/
FIND FIRST ped-venda NO-LOCK NO-ERROR.
CASE ped-venda.cod-estabel.
    WHEN '1' THEN DO.
       FIND bco_empres WHERE
            bco_empres.cod_empresa = '5' AND
            bco_empres.cod_bco_fisic MATCHES '*MED*' NO-LOCK NO-ERROR.

       ASSIGN c-connect = '-db ' + bco_empres.cod_bco_fisic + ' -ld dbaux ' + bco_empres.cod_param_conex.
    END.
    WHEN '5' THEN DO.
       FIND bco_empres WHERE
            bco_empres.cod_empresa = '1' AND
            bco_empres.cod_bco_fisic MATCHES '*IMA*' NO-LOCK NO-ERROR.

       ASSIGN c-connect = '-db ' + bco_empres.cod_bco_fisic + ' -ld dbaux ' + bco_empres.cod_param_conex.
    END.
END CASE.

IF c-connect <> "" THEN CONNECT VALUE(c-connect) NO-ERROR.

