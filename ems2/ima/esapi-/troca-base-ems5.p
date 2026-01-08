/*DEF INPUT PARAMETER p-base-para AS CHAR.*/

DEF VAR c-connect AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEF VAR c-service-para AS CHAR.
DEF VAR i-alias AS INT.

/*FIND im-param WHERE
     im-param.cod-param = "SERVICES_DATABASE" NO-LOCK NO-ERROR.

ASSIGN c-service-para = ENTRY(LOOKUP(p-base-para,im-param.val-param) - 1,im-param.val-param).
*/
ASSIGN c-service-para = '10'.

FIND bco_empres WHERE
     bco_empres.cod_empresa = '1' AND
     bco_empres.cod_bco_fisic MATCHES '*ems5*' NO-LOCK NO-ERROR.

ASSIGN c-connect = '-db ' + bco_empres.cod_bco_fisic + ' -ld db-aux ' + bco_empres.cod_param_conex.
ASSIGN c-connect = REPLACE(c-connect,"40",c-service-para).
/*
IF CONNECTED ("ems5") THEN DO. 
   DISCONNECT ems5.
   DO i-alias = 1 TO NUM-ENTRIES(bco_empres.cod_alias_bco).
      DELETE ALIAS VALUE(ENTRY(i-alias,bco_empres.cod_alias_bco)).
   END.
END.
*/
IF c-connect <> "" THEN DO.
   CONNECT VALUE(c-connect).
   /*
   DO i-alias = 1 TO NUM-ENTRIES(bco_empres.cod_alias_bco).
      CREATE ALIAS VALUE(ENTRY(i-alias,bco_empres.cod_alias_bco)) FOR DATABASE db-aux.
   END.
   */
END.

