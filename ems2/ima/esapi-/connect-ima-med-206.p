DEFINE VARIABLE base AS CHARACTER.

DEF TEMP-TABLE tt-pf FIELD linha  AS CHARACTER FORMAT "x(200)".

DEFINE VARIABLE c-dir-ems         AS CHARACTER INITIAL "m:\ems206\scripts" NO-UNDO.
DEFINE VARIABLE c-connect         AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE VARIABLE v-base-connect    AS CHARACTER EXTENT 2        NO-UNDO.
DEFINE VARIABLE v-base-arquivo-pf AS CHARACTER EXTENT 2        NO-UNDO.
DEFINE VARIABLE i                 AS INTEGER                   NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

FIND fnd_usuar_univ WHERE
     fnd_usuar_univ.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.

ASSIGN base = IF fnd_usuar_univ.cod_empresa = '1' 
            THEN 'IMA' ELSE 'MED'.

IF CONNECTED ("bd-IMA") and CONNECTED ("bd-MED") THEN LEAVE.

IF CONNECTED ("bd-IMA") THEN DISCONNECT bd-IMA.
IF CONNECTED ("bd-MED") THEN DISCONNECT bd-MED.

IF base MATCHES "*oficial*" THEN
   ASSIGN v-base-connect[1]    = "ima"
          v-base-arquivo-pf[1] = c-dir-ems + "\BaseIMA\pf\ems206ima-pro.pf"
          v-base-connect[2]    = "med"
          v-base-arquivo-pf[2] = c-dir-ems + "\BaseMED\pf\ems206med-pro.pf".
ELSE
   ASSIGN v-base-connect[1]    = "ima"
          v-base-arquivo-pf[1] = c-dir-ems + "\BaseIMA\pf\ems206ima-tst.pf"
          v-base-connect[2]    = "med"
          v-base-arquivo-pf[2] = c-dir-ems + "\BaseMED\pf\ems206med-tst.pf".


/*FOR EACH TT-PF:
    EXPORT TT-PF.
END.*/



DO i = 1 TO 2.
   /*MESSAGE 'ARQUIVOS:' v-base-arquivo-pf[i]
       VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
   IF SEARCH(v-base-arquivo-pf[i]) <> ? THEN DO:
      INPUT FROM VALUE(v-base-arquivo-pf[i]).
      REPEAT:
         CREATE tt-pf.
         IMPORT DELIMITER "#$&" tt-pf.linha.
         /*EXPORT TT-PF.LINHA.*/
             
      END.
      INPUT CLOSE.
   END.

   FOR EACH tt-pf WHERE
            tt-pf.linha BEGINS "-db" AND
            tt-pf.linha MATCHES "*" + v-base-connect[i] + "*" 
            NO-LOCK.

       ASSIGN c-connect = replace(tt-pf.linha,"ld ems206","ld bd-" + v-base-connect[i]).
       IF c-connect <> "" THEN CONNECT VALUE(c-connect).
   END.
END.

