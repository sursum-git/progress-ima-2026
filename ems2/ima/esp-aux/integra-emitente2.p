/* Temp-Table Definitions */
DEF TEMP-TABLE tt-emitente LIKE emitente.
DEF TEMP-TABLE tt-cont-emit LIKE cont-emit.

DEFINE TEMP-TABLE tt-erros-login
    FIELD cod-erro  AS INTEGER
    FIELD desc-erro AS CHARACTER FORMAT "X(256)":U
    FIELD desc-arq  AS CHARACTER.

/* Parameters Definitions */
DEF INPUT PARAMETER p-cod-emit LIKE emitente.cod-emitente.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Defini‡Æo de Variaveis */
DEFINE VARIABLE h-server AS HANDLE NO-UNDO.
DEFINE VARIABLE c-erros  AS CHAR.
DEFINE VARIABLE i-ct     AS INT.



CREATE SERVER h-server.
h-server:CONNECT("-AppService datasul-progress-integra-pro -H 192.168.0.38 -S 5162") NO-ERROR.
/* h-server:CONNECT("-AppService datasul-progress-integra-bases -H 192.168.0.38 -S 5162") NO-ERROR. */

IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO.
   DO i-ct = 0 TO ERROR-STATUS:NUM-MESSAGES:
      IF ERROR-STATUS:GET-MESSAGE(i-ct) <> '' THEN
         ASSIGN c-erros = c-erros + ERROR-STATUS:GET-MESSAGE(i-ct) + '   '. 
   END.
END.

IF NOT h-server:CONNECTED() THEN DO.
    RETURN 'NOK'.
END.


IF h-server:CONNECTED() THEN DO.
   FIND emitente WHERE
        emitente.cod-emitente = p-cod-emit NO-LOCK NO-ERROR.

   IF AVAIL emitente THEN DO.
      CREATE tt-emitente.
      BUFFER-COPY emitente TO tt-emitente.

      FOR EACH cont-emit OF emitente NO-LOCK.
          CREATE tt-cont-emit.
          BUFFER-COPY cont-emit TO tt-cont-emit.
      END.
   END.
                  
   RUN esapi/conecta-base.p ON SERVER h-server (INPUT "FOUNDATION").
   RUN btb/btapi910za.p ON SERVER h-server (INPUT "integra":U,    /* Login automatico no EMS5 */
                                            INPUT "integra":U, 
                                            OUTPUT TABLE tt-erros-login).
   FIND FIRST tt-erros-login NO-ERROR.
   IF AVAIL tt-erros-login THEN DO.
      RUN esapi/disconecta-base.p ON SERVER h-server.
      h-server:DISCONNECT().
      DELETE OBJECT h-server.
      RETURN 'NOK'.
   END. 
   
   /* Cria Emitente na IMA BKP */
   RUN esapi/conecta-base.p ON SERVER h-server (INPUT "IMA-BKP").
   RUN esapi/importa-emitente.p ON SERVER h-server (INPUT TABLE tt-emitente,
                                                    INPUT TABLE tt-cont-emit).
   RUN esapi/disconecta-base.p ON SERVER h-server.

   h-server:DISCONNECT().
   DELETE OBJECT h-server.

   RETURN 'OK'.
END.





