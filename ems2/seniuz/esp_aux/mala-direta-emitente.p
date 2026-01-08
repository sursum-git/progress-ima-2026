DEFINE VARIABLE com_email   AS INTEGER INITIAL 0 NO-UNDO.
DEFINE VARIABLE sem_email   AS INTEGER INITIAL 0 NO-UNDO.
DEFINE VARIABLE d-dt-emis-nota LIKE dbaux.nota-fiscal.dt-emis-nota NO-UNDO.

ASSIGN d-dt-emis-nota = 09.24.2015.

DEF TEMP-TABLE tt-emitente NO-UNDO
    FIELD cod-emitente LIKE ems2ima.cont-emit.cod-emitente
    FIELD nome-emit    LIKE ems2ima.emitente.nome-emit
    FIELD cnpj         LIKE ems2ima.emitente.cgc
    FIELD dt-emis-nota LIKE ems2ima.nota-fiscal.dt-emis-nota.

DEF TEMP-TABLE tt-contato
    FIELD cod-emitente LIKE ems2ima.cont-emit.cod-emitente
    FIELD e-mail       LIKE ems2ima.cont-emit.e-mail.


/* BANCO IMA */
FOR EACH ems2ima.nota-fiscal WHERE
         ems2ima.nota-fiscal.cod-estabel  = "1"
     AND ems2ima.nota-fiscal.serie        = "3"
     AND ems2ima.nota-fiscal.dt-emis-nota > d-dt-emis-nota
     AND ems2ima.nota-fiscal.dt-cancela   = ?
     NO-LOCK.

    FIND ems2ima.emitente WHERE 
         ems2ima.emitente.cod-emitente = ems2ima.nota-fiscal.cod-emitente
         NO-LOCK NO-ERROR.

     FIND tt-emitente WHERE 
          tt-emitente.cod-emitente = ems2ima.nota-fiscal.cod-emitente
          NO-LOCK NO-ERROR.

     IF NOT AVAIL tt-emitente THEN DO:
        CREATE tt-emitente.
        ASSIGN tt-emitente.cod-emitente = ems2ima.nota-fiscal.cod-emitente
               tt-emitente.nome-emit    = ems2ima.emitente.nome-emit
               tt-emitente.cnpj         = ems2ima.emitente.cgc.
     END.
     IF tt-emitente.dt-emis-nota = ? OR
        tt-emitente.dt-emis-nota < ems2ima.nota-fiscal.dt-emis THEN
        ASSIGN tt-emitente.dt-emis-nota = ems2ima.nota-fiscal.dt-emis.
END.

/* BANCO med */
FOR EACH dbaux.nota-fiscal WHERE
         dbaux.nota-fiscal.cod-estabel  = "1"
     AND dbaux.nota-fiscal.serie        = "3"
     AND dbaux.nota-fiscal.dt-emis-nota > d-dt-emis-nota
     AND dbaux.nota-fiscal.dt-cancela   = ?
     NO-LOCK.

    FIND dbaux.emitente WHERE 
         dbaux.emitente.cod-emitente = dbaux.nota-fiscal.cod-emitente
         NO-LOCK NO-ERROR.

     FIND tt-emitente WHERE 
          tt-emitente.cod-emitente = dbaux.nota-fiscal.cod-emitente
          NO-LOCK NO-ERROR.

     IF NOT AVAIL tt-emitente THEN DO:
        CREATE tt-emitente.
        ASSIGN tt-emitente.cod-emitente = dbaux.nota-fiscal.cod-emitente
               tt-emitente.nome-emit    = dbaux.emitente.nome-emit
               tt-emitente.cnpj         = dbaux.emitente.cgc.
     END.
     IF tt-emitente.dt-emis-nota = ? OR
        tt-emitente.dt-emis-nota < dbaux.nota-fiscal.dt-emis THEN
        ASSIGN tt-emitente.dt-emis-nota = dbaux.nota-fiscal.dt-emis.
END.



FOR EACH tt-emitente NO-LOCK.
    FOR EACH ems2ima.cont-emit WHERE
             ems2ima.cont-emit.cod-emitente = tt-emitente.cod-emitente
             NO-LOCK.
        FIND tt-contato WHERE
             tt-contato.cod-emitente = ems2ima.cont-emit.cod-emitente AND
             tt-contato.e-mail = ems2ima.cont-emit.e-mail NO-ERROR.
        IF NOT AVAIL tt-contato THEN DO.
           CREATE tt-contato.
           ASSIGN tt-contato.cod-emitente = ems2ima.cont-emit.cod-emitente
                  tt-contato.e-mail = ems2ima.cont-emit.e-mail.
        END.
    END.
    FIND FIRST tt-contato WHERE 
               tt-contato.cod-emitente = ems2ima.cont-emit.cod-emitente NO-ERROR.
    IF NOT AVAIL tt-contato THEN DO.
       FIND ems2ima.emitente WHERE 
            ems2ima.emitente.cod-emitente = tt-contato.cod-emitente
            NO-LOCK NO-ERROR.
       IF ems2ima.emitente.e-mail <> '' THEN DO.
          CREATE tt-contato.
          ASSIGN tt-contato.cod-emitente = ems2ima.emitente.cod-emitente
                 tt-contato.e-mail = ems2ima.emitente.e-mail.
       END.
    END.


    FOR EACH dbaux.cont-emit WHERE
             dbaux.cont-emit.cod-emitente = tt-emitente.cod-emitente
             NO-LOCK.
        FIND tt-contato WHERE
             tt-contato.cod-emitente = dbaux.cont-emit.cod-emitente AND
             tt-contato.e-mail = dbaux.cont-emit.e-mail NO-ERROR.
        IF NOT AVAIL tt-contato THEN DO.
           CREATE tt-contato.
           ASSIGN tt-contato.cod-emitente = dbaux.cont-emit.cod-emitente
                  tt-contato.e-mail = dbaux.cont-emit.e-mail.
        END.
    END.
    FIND FIRST tt-contato WHERE 
               tt-contato.cod-emitente = dbaux.cont-emit.cod-emitente NO-ERROR.
    IF NOT AVAIL tt-contato THEN DO.
       FIND dbaux.emitente WHERE 
            dbaux.emitente.cod-emitente = tt-contato.cod-emitente
            NO-LOCK NO-ERROR.
       IF dbaux.emitente.e-mail <> '' THEN DO.
          CREATE tt-contato.
          ASSIGN tt-contato.cod-emitente = dbaux.emitente.cod-emitente
                 tt-contato.e-mail = dbaux.emitente.e-mail.
       END.
    END.
END.



OUTPUT TO "c:\temp\nota-fiscal.txt".
FOR EACH tt-emitente NO-LOCK:
    FIND FIRST tt-contato WHERE
               tt-contato.cod-emitente = tt-emitente.cod-emitente NO-ERROR.
    IF NOT AVAIL tt-contato THEN DO.
       ASSIGN sem_email = sem_email + 1.
       PUT UNFORMAT 
           tt-emitente.cod-emitente ";"
           tt-emitente.nome-emit    ";"
           tt-emitente.cnpj         ";"
           tt-emitente.dt-emis-not  ";"
           SKIP.
    END.
    ELSE DO.
        ASSIGN com_email = com_email + 1.
        FOR EACH tt-contato WHERE
                 tt-contato.cod-emitente = tt-emitente.cod-emitente
                 NO-LOCK.
            PUT UNFORMAT tt-contato.e-mail SKIP.
        END.
    END.
END.

PUT UNFORMAT "COM EMAIL: " STRING(com_email)   SKIP.
PUT UNFORMAT "SEM EMAIL: " STRING(sem_email)   SKIP.

OUTPUT CLOSE.















/*
    MESSAGE SKIP "tt-emitente"
            SKIP total_email
            SKIP tt-emitente.cod-emitente
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
        MESSAGE SKIP "ems2ima.cont-emit"
                SKIP com_email
                SKIP ems2ima.cont-emit.cod-emitente
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
*/

/*
    FIND ems2ima.cont-emit WHERE
         ems2ima.cont-emit.sequencia    = 10
         ems2ima.cont-emit.cod-emitente = tt-emitente.cod-emitente
         NO-LOCK NO-ERROR.
IF AVAIL ems2ima.cont-emit THEN
       ASSIGN com-email = com-email + 1.
    ELSE
        ASSIGN sem-email = sem-email + 1.
        
        
       MESSAGE ems2ima.nota-fiscal.dt-emis-nota SKIP
               ems2ima.nota-fiscal.cod-emitente SKIP
               VIEW-AS ALERT-BOX INFO BUTTONS OK.

        
*/


/*
           PUT UNFORMAT tt-emitente.cod-estabel 
                    ";" tt-emitente.serie        
                    ";" tt-emitente.dt-emis-nota
                    ";" tt-emitente.nr-nota-fis 
                    ";" tt-emitente.cod-emitente
                    ";" ems2ima.cont-emit.e-mail
                    SKIP.
*/










/*    PUT UNFORMAT 
        ems2ima.nota-fiscal.dt-emis-nota
        SKIP. */

