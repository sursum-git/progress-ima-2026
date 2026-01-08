DEFINE VARIABLE h-server AS HANDLE  NO-UNDO.

DEF VAR c-erros AS CHAR.
DEF VAR i-ct    AS INT.

CREATE SERVER h-server.
h-server:CONNECT("-AppService datasul-progress-integra-pro -H 192.168.0.38 -S 5162") NO-ERROR.
/* h-server:CONNECT("-AppService datasul-progress-integra-bases -H 192.168.0.38 -S 5162") NO-ERROR. */

IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO.
   DO i-ct = 0 TO ERROR-STATUS:NUM-MESSAGES:
      IF ERROR-STATUS:GET-MESSAGE(i-ct) <> '' THEN
         ASSIGN c-erros = c-erros + ERROR-STATUS:GET-MESSAGE(i-ct) + '   '. 
   END.
   MESSAGE c-erros VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
ELSE 
   IF h-server:CONNECTED() THEN DO.
      RUN esapi/conecta-base.p ON h-server (INPUT "MED-OFICIAL").
      RUN esp-aux/verifica.p ON h-server.
      RUN esapi/disconecta-base.p ON h-server.
   END.
