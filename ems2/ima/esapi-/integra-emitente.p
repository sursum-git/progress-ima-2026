/* Temp-Table Definitions */
DEF TEMP-TABLE tt-emitente LIKE emitente.
DEF TEMP-TABLE tt-cont-emit LIKE cont-emit.
DEF TEMP-TABLE tt-loc-entr LIKE loc-entr.

DEFINE TEMP-TABLE tt-erros-login
    FIELD cod-erro  AS INTEGER
    FIELD desc-erro AS CHARACTER FORMAT "X(256)":U
    FIELD desc-arq  AS CHARACTER.

/* Parameters Definitions */
DEF INPUT PARAMETER p-cod-emit LIKE emitente.cod-emitente.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* Definiá∆o de Variaveis */
DEFINE VARIABLE h-server  AS HANDLE NO-UNDO.
DEFINE VARIABLE c-erros   AS CHAR.
DEFINE VARIABLE c-base    AS CHAR.
DEFINE VARIABLE i-ct      AS INT.
DEFINE VARIABLE c-arq-log AS CHAR.
DEFINE VARIABLE h-acomp   AS HANDLE  NO-UNDO.

FIND im-param WHERE
     im-param.cod-param = "INTEGRA_BASES" NO-LOCK NO-ERROR.

IF AVAIL im-param AND
   LOGICAL(im-param.val-param) = NO THEN DO.
   RETURN 'OK'.
END.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Integrando Bases..").

ASSIGN c-arq-log = "p:\integra-emitente" + STRING(TODAY,"99-99-9999") + ".txt".

/* Main Block */
OUTPUT TO VALUE(c-arq-log) APPEND.
   PUT UNFORMATTED "INTEGRANDO EMITENTE: " 
       p-cod-emit
       "  Usuario: " c-seg-usuario
       "  " 
       NOW
       SKIP.
OUTPUT CLOSE.

RUN pi-acompanhar IN h-acomp (INPUT "Conectando AppServer Integra-Base").

OUTPUT TO VALUE(c-arq-log) APPEND.
   PUT UNFORMATTED "Conectando Appserver" AT 5
       c-erros
       NOW
       SKIP.
OUTPUT CLOSE.

CREATE SERVER h-server.
h-server:CONNECT("-AppService datasul-progress-integra-bases -H 192.168.0.38 -S 5162") NO-ERROR.

IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO.
   DO i-ct = 0 TO ERROR-STATUS:NUM-MESSAGES:
      IF ERROR-STATUS:GET-MESSAGE(i-ct) <> '' THEN
         ASSIGN c-erros = c-erros + ERROR-STATUS:GET-MESSAGE(i-ct) + '   '. 
   END.
   OUTPUT TO VALUE(c-arq-log) APPEND.
       PUT UNFORMATTED "ERRO ao Conectar Appserver" AT 5
           c-erros
           NOW
           SKIP.
   OUTPUT CLOSE.
   RUN pi-finalizar in h-acomp.
   RETURN 'NOK'.
END.

IF NOT h-server:CONNECTED() THEN DO.
   OUTPUT TO VALUE(c-arq-log) APPEND.
      PUT UNFORMATTED "ERRO Appserver n∆o Conectado" AT 5
          NOW
          SKIP.
    OUTPUT CLOSE.
    RUN pi-finalizar in h-acomp.
    RETURN 'NOK'.
END.

IF h-server:CONNECTED() THEN DO.
   OUTPUT TO VALUE(c-arq-log) APPEND.
       PUT UNFORMATTED "Appserver Conectado " AT 5
           NOW
           SKIP.
   OUTPUT CLOSE.

   FIND emitente WHERE
        emitente.cod-emitente = p-cod-emit NO-LOCK NO-ERROR.

   IF AVAIL emitente THEN DO.
      CREATE tt-emitente.
      BUFFER-COPY emitente TO tt-emitente.

      FOR EACH cont-emit OF emitente NO-LOCK.
          CREATE tt-cont-emit.
          BUFFER-COPY cont-emit TO tt-cont-emit.
      END.

      FOR EACH loc-entr WHERE
               loc-entr.nome-abrev = emitente.nome-abrev NO-LOCK.
          CREATE tt-loc-entr.
          BUFFER-COPY loc-entr TO tt-loc-entr.
      END.
   END.
                  
   RUN pi-acompanhar IN h-acomp (INPUT "Efetuando Loign Autom†tico").

   RUN esapi/conecta-base.p ON SERVER h-server (INPUT "FOUNDATION").
   RUN btb/btapi910za.p ON SERVER h-server (INPUT "integra":U,    /* Login automatico no EMS5 */
                                            INPUT "integra":U, 
                                            OUTPUT TABLE tt-erros-login).
   FIND FIRST tt-erros-login NO-ERROR.
   IF AVAIL tt-erros-login THEN DO.
      OUTPUT TO VALUE(c-arq-log) APPEND.
          PUT UNFORMATTED "ERRO no Login Autom†tico" AT 5
              SKIP.
          FOR EACH tt-erros-login NO-LOCK.
              PUT UNFORMATTED tt-erros-login.cod-erro 
                              tt-erros-login.desc-erro SKIP.
          END.
      OUTPUT CLOSE.

      RUN esapi/disconecta-base.p ON SERVER h-server.
      h-server:DISCONNECT().
      DELETE OBJECT h-server.

      RUN pi-finalizar in h-acomp.
      RETURN 'NOK'.
   END. 
   
   OUTPUT TO VALUE(c-arq-log) APPEND.
       PUT UNFORMATTED "Conectado Foundation " AT 5
           NOW
           SKIP.
   OUTPUT CLOSE.

   FIND FIRST param-global NO-LOCK NO-ERROR.

   FIND im-param WHERE
        im-param.cod-param = "BASE" NO-LOCK NO-ERROR.
   IF AVAIL im-param THEN
      ASSIGN c-base = im-param.val-param.

   CASE param-global.empresa-prin:
       WHEN '1' THEN DO.
          /* Cria Emitente na MED OFICIAL */
          RUN pi-acompanhar IN h-acomp (INPUT "Conectando Bancos MED").

          OUTPUT TO VALUE(c-arq-log) APPEND.
              PUT UNFORMATTED SKIP
                  "Conectando base MED " AT 5
                  c-base 
                  " "
                  NOW
                  SKIP.
          OUTPUT CLOSE.

          IF c-base = 'PRODUCAO' THEN
             RUN esapi/conecta-base.p ON SERVER h-server (INPUT "MED-PRODUCAO").
          ELSE
             RUN esapi/conecta-base.p ON SERVER h-server (INPUT "MED-TESTE").          

          RUN pi-acompanhar IN h-acomp (INPUT "Integrando Base MED").
          RUN esapi/importa-emitente.p ON SERVER h-server (INPUT TABLE tt-emitente,
                                                           INPUT TABLE tt-cont-emit,
                                                           INPUT TABLE tt-loc-entr).
          RUN esapi/disconecta-base.p ON SERVER h-server.
           
          OUTPUT TO VALUE(c-arq-log) APPEND.
              PUT UNFORMATTED "Base MED Integrada com SUCESSO " AT 5
                  NOW
                  SKIP.
          OUTPUT CLOSE.
       END.
       WHEN '5' THEN DO.
           /* Cria Emitente na IMA OFICIAL */
           RUN pi-acompanhar IN h-acomp (INPUT "Conectando Bancos IMA").

           OUTPUT TO VALUE(c-arq-log) APPEND.
               PUT UNFORMATTED SKIP
                   "Conectando base IMA " AT 5
                   c-base 
                   " "
                   NOW
                   SKIP.
           OUTPUT CLOSE.

           IF c-base = 'PRODUCAO' THEN
              RUN esapi/conecta-base.p ON SERVER h-server (INPUT "IMA-PRODUCAO").
           ELSE
              RUN esapi/conecta-base.p ON SERVER h-server (INPUT "IMA-TESTE").

           RUN pi-acompanhar IN h-acomp (INPUT "Integrando Base IMA").
           RUN esapi/importa-emitente.p ON SERVER h-server (INPUT TABLE tt-emitente,
                                                            INPUT TABLE tt-cont-emit,
                                                            INPUT TABLE tt-loc-entr).
           RUN esapi/disconecta-base.p ON SERVER h-server.

           OUTPUT TO VALUE(c-arq-log) APPEND.
               PUT UNFORMATTED "Base IMA Integrada com SUCESSO " AT 5
                   NOW
                   SKIP.
           OUTPUT CLOSE.
       END.
   END CASE.

   FIND im-param WHERE
        im-param.cod-param = "BASE_BKP_NO_AR" NO-LOCK NO-ERROR.
   
   IF AVAIL im-param AND
      LOGICAL(im-param.val-param) = NO THEN DO.
      RUN pi-finalizar in h-acomp.
      RETURN 'NOK'.
   END.

   /* Cria Emitente na IMA BKP */
   RUN pi-acompanhar IN h-acomp (INPUT "Conectando Bancos IMA-BACKUP").

   OUTPUT TO VALUE(c-arq-log) APPEND.
       PUT UNFORMATTED SKIP
           "Conectando base IMA BACKUP " AT 5
           c-base 
           " "
           NOW
           SKIP.
   OUTPUT CLOSE.

   IF c-base = 'PRODUCAO' THEN
      RUN esapi/conecta-base.p ON SERVER h-server (INPUT "IMA-BKP").
   ELSE
      RUN esapi/conecta-base.p ON SERVER h-server (INPUT "IMA-BKT").

   RUN pi-acompanhar IN h-acomp (INPUT "Integrando IMA-BACKUP").
   RUN esapi/importa-emitente.p ON SERVER h-server (INPUT TABLE tt-emitente,
                                                    INPUT TABLE tt-cont-emit,
                                                    INPUT TABLE tt-loc-entr).
   RUN esapi/disconecta-base.p ON SERVER h-server.

   OUTPUT TO VALUE(c-arq-log) APPEND.
       PUT UNFORMATTED "Base IMA BACKUP Integrada com SUCESSO " AT 5
           NOW
           SKIP.
   OUTPUT CLOSE.

   /* Cria Emitente na MED BKP */

   RUN pi-acompanhar IN h-acomp (INPUT "Conectando Bancos MED-BACKUP").

   OUTPUT TO VALUE(c-arq-log) APPEND.
       PUT UNFORMATTED SKIP
           "Conectando base MED BACKUP " AT 5
           c-base 
           " "
           NOW
           SKIP.
   OUTPUT CLOSE.

   IF c-base = 'PRODUCAO' THEN
      RUN esapi/conecta-base.p ON SERVER h-server (INPUT "MED-BKP").
   ELSE
      RUN esapi/conecta-base.p ON SERVER h-server (INPUT "MED-BKT").

   RUN pi-acompanhar IN h-acomp (INPUT "Integrando MED-BACKUP").
   RUN esapi/importa-emitente.p ON SERVER h-server (INPUT TABLE tt-emitente,
                                                    INPUT TABLE tt-cont-emit,
                                                    INPUT TABLE tt-loc-entr).
   RUN esapi/disconecta-base.p ON SERVER h-server.

   OUTPUT TO VALUE(c-arq-log) APPEND.
       PUT UNFORMATTED "Base MED BACKUP Integrada com SUCESSO " AT 5
           NOW
           SKIP.
   OUTPUT CLOSE.

   h-server:DISCONNECT().
   DELETE OBJECT h-server.

   OUTPUT TO VALUE(c-arq-log) APPEND.
      PUT UNFORMATTED "TERMINO INTEGRAÄ«O DO EMITENTE: " 
          p-cod-emit
          "  " 
          NOW
          SKIP.
   OUTPUT CLOSE.

   RUN pi-finalizar in h-acomp.
   RETURN 'OK'.
END.





