DEF BUFFER repres FOR repres.
DEF BUFFER emitente FOR emitente.

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    field classifica       as integer
    field desc-classifica  as char format "x(40)"
    field modelo-rtf       as char format "x(35)"
    field l-habilitaRtf    as LOG
    FIELD cod-emitente-ini LIKE emitente.cod-emitente
    FIELD cod-emitente-fim LIKE emitente.cod-emitente
    FIELD nome-abrev-ini   LIKE emitente.nome-abrev
    FIELD nome-abrev-fim   LIKE emitente.nome-abrev
    FIELD cod-rep-ini      LIKE emitente.cod-rep
    FIELD cod-rep-fim      LIKE emitente.cod-rep
    FIELD nome-ab-rep-ini  LIKE repres.nome-abrev
    FIELD nome-ab-rep-fim  LIKE repres.nome-abrev
    FIELD cidade-ini       LIKE emitente.cidade
    FIELD cidade-fim       LIKE emitente.cidade
    FIELD uf-ini           LIKE emitente.estado
    FIELD uf-fim           LIKE emitente.estado
    FIELD cod-ramo-ini     LIKE ext-emitente.cod-ramo-ativ
    FIELD cod-ramo-fim     LIKE ext-emitente.cod-ramo-ativ
    FIELD rs-dias          AS INT
    FIELD dt-ini           AS DATE
    FIELD dt-fim           AS DATE
    FIELD ativo            AS LOG
    FIELD inativo          AS LOG
    FIELD semcompra        AS LOG
    FIELD ordenar          AS INT
    FIELD tp-relatorio     AS INTEGER.

DEF TEMP-TABLE tt-cli NO-UNDO
    FIELD cod-emitente AS INT  FORMAT "<<<<9"
    FIELD nome-emit    AS CHAR FORMAT "x(40)"
    FIELD endereco     AS CHAR FORMAT "x(40)"
    FIELD bairro       AS CHAR FORMAT "x(30)"
    FIELD cidade       AS CHAR FORMAT "x(25)"
    FIELD estado       AS CHAR FORMAT "x(02)"
    FIELD nome-ab-rep  AS CHAR FORMAT "x(12)"
    FIELD c-status     AS CHAR
    FIELD tel1         AS CHAR
    FIELD tel2         AS CHAR
    FIELD i-dias       AS INT
    INDEX rep IS PRIMARY c-status nome-ab-rep i-dias  .

DEF INPUT PARAMETER TABLE FOR tt-param.
DEF OUTPUT PARAMETER TABLE FOR tt-cli.

DEF VAR h-acomp        AS HANDLE  NO-UNDO.
DEF VAR rw-nota-fiscal AS ROWID NO-UNDO.

FIND FIRST tt-param NO-LOCK NO-ERROR.
FIND FIRST ems2ima.para-ped NO-LOCK NO-ERROR.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Processando...").

FOR EACH repres WHERE /*1*/
         repres.cod-rep >= tt-param.cod-rep-ini        AND
         repres.cod-rep <= tt-param.cod-rep-fim        AND
         repres.nome-abrev >= tt-param.nome-ab-rep-ini AND
         repres.nome-abrev <= tt-param.nome-ab-rep-fim BY repres.nome-abrev:

    RUN pi-acompanhar IN h-acomp (INPUT repres.nome-abrev).
    FOR EACH emitente WHERE /*2*/
             emitente.identific    <> 2  AND          
             emitente.cod-rep       =  repres.cod-rep AND 
             emitente.cod-emitente >= tt-param.cod-emitente-ini AND 
             emitente.cod-emitente <= tt-param.cod-emitente-fim AND
             emitente.nome-abrev   >= tt-param.nome-abrev-ini   AND
             emitente.nome-abrev   <= tt-param.nome-abrev-fim   AND
             emitente.cidade       >= tt-param.cidade-ini       AND
             emitente.cidade       <= tt-param.cidade-fim       AND
             emitente.estado       >= tt-param.uf-ini           AND
             emitente.estado       <= tt-param.uf-fim           AND
             emitente.ind-cre-cli  <> 4
             BY emitente.nome-abrev: 

        RUN pi-acompanhar IN h-acomp (INPUT repres.nome-abrev + '-' + emitente.nome-abrev).

        FIND FIRST ext-emitente WHERE
                   ext-emitente.cod-emitente = emitente.cod-emitente AND
                   ext-emitente.cod-ramo-ativ >= tt-param.cod-ramo-ini AND
                   ext-emitente.cod-ramo-ativ <= tt-param.cod-ramo-fim NO-LOCK NO-ERROR.
        IF NOT AVAIL ext-emitente THEN NEXT.

        IF tt-param.semcompra = YES THEN DO: /*3*/
           FIND FIRST dbaux.nota-fiscal WHERE 
                      dbaux.nota-fiscal.cod-emitente = emitente.cod-emitente AND
                      dbaux.nota-fiscal.nome-ab-cli  = emitente.nome-abrev   
                      NO-LOCK NO-ERROR.
           IF NOT AVAIL nota-fiscal THEN DO: /*4*/
              FIND FIRST nota-fiscal WHERE 
                         nota-fiscal.cod-emitente = emitente.cod-emitente AND
                         nota-fiscal.nome-ab-cli  = emitente.nome-abrev   
                         NO-LOCK NO-ERROR.
              IF NOT AVAIL nota-fiscal THEN DO: /*5*/
                 CREATE tt-cli.
                 ASSIGN tt-cli.cod-emitente   = emitente.cod-emitente
                        tt-cli.nome-emit      = emitente.nome-emit
                        tt-cli.endereco       = emitente.endereco            
                        tt-cli.bairro         = emitente.bairro
                        tt-cli.cidade         = emitente.cidade              
                        tt-cli.estado         = emitente.estado              
                        tt-cli.nome-ab-rep    = repres.nome-abrev         
                        tt-cli.tel1           = emitente.telefone[1]
                        tt-cli.c-status       = "Sem Compra"
                        tt-cli.i-dias         = tt-param.dt-fim - emitente.data-implant.
              END.
           END. /*IF NOT AVAIL dbaux.nota-fiscal 4*/
        END. /* sem compra = yes 3 */
       
        
        IF tt-param.semcompra = NO THEN DO: /*8*/  
           RUN pi-acompanhar IN h-acomp (INPUT repres.nome-abrev + '-' + emitente.nome-abrev).

           ASSIGN rw-nota-fiscal = ?.
           FOR EACH dbaux.nota-fiscal WHERE
                    dbaux.nota-fiscal.cod-emitente = emitente.cod-emitente AND
                    dbaux.nota-fiscal.dt-emis-nota <= tt-param.dt-fim 
                    NO-LOCK BY dbaux.nota-fiscal.dt-emis-nota DESCENDING.
               FIND dbaux.natur-oper OF dbaux.nota-fiscal NO-LOCK NO-ERROR.
               IF NOT dbaux.natur-oper.emite-duplic THEN NEXT.
               ASSIGN rw-nota-fiscal = ROWID(dbaux.nota-fiscal).
               LEAVE.
           END.

           IF rw-nota-fiscal <> ? THEN DO.
              FOR LAST dbaux.nota-fiscal WHERE /*10*/
                       dbaux.nota-fiscal.cod-emitente = emitente.cod-emitente AND
                       dbaux.nota-fiscal.dt-emis-nota <= tt-param.dt-fim
                       BY dbaux.nota-fiscal.dt-emis-nota.
                  CREATE tt-cli.
                  ASSIGN tt-cli.cod-emitente   =   emitente.cod-emitente
                         tt-cli.nome-emit      =   emitente.nome-emit
                         tt-cli.endereco       =   emitente.endereco
                         tt-cli.bairro         =   emitente.bairro
                         tt-cli.cidade         =   emitente.cidade              
                         tt-cli.estado         =   emitente.estado              
                         tt-cli.nome-ab-rep    =   repres.nome-abrev         
                         tt-cli.tel1           =   emitente.telefone[1]
                         tt-cli.tel2           =   emitente.telefone[2]
                         tt-cli.i-dias         =   tt-param.dt-fim - dbaux.nota-fiscal.dt-emis-nota.

                  IF dbaux.nota-fiscal.dt-emis-nota < tt-param.dt-ini THEN 
                     ASSIGN tt-cli.c-status = "Inativo".
                  ELSE
                     ASSIGN tt-cli.c-status = "Ativo".
              END. /* for last dbaux.nota-fiscal 10*/
           END.  /* if avail dbaux.nota-fiscal 9 */
          

           ASSIGN rw-nota-fiscal = ?.
           FOR EACH ems2ima.nota-fiscal WHERE
                    ems2ima.nota-fiscal.cod-emitente = emitente.cod-emitente AND
                    ems2ima.nota-fiscal.dt-emis-nota <= tt-param.dt-fim
                    NO-LOCK BY ems2ima.nota-fiscal.dt-emis-nota DESCENDING.
               FIND ems2ima.natur-oper OF ems2ima.nota-fiscal NO-LOCK NO-ERROR.
               IF NOT ems2ima.natur-oper.emite-duplic THEN NEXT.
               ASSIGN rw-nota-fiscal = ROWID(ems2ima.nota-fiscal).
               LEAVE.
           END.

           IF rw-nota-fiscal <> ? THEN DO.
              FOR LAST ems2ima.nota-fiscal WHERE /*12*/
                       ems2ima.nota-fiscal.nome-ab-cli = emitente.nome-abrev 
                       NO-LOCK BY nota-fiscal.dt-emis-nota .
                  FIND tt-cli WHERE  
                       tt-cli.cod-emitente = nota-fiscal.cod-emitente NO-ERROR.
                  IF AVAIL tt-cli THEN DO:
                     IF ems2ima.nota-fiscal.dt-emis-nota > (tt-param.dt-fim - tt-cli.i-dias) THEN DO:
                        ASSIGN tt-cli.i-dias = tt-param.dt-fim - ems2ima.nota-fiscal.dt-emis-nota.
                        IF ems2ima.nota-fiscal.dt-emis-nota < tt-param.dt-ini THEN
                           ASSIGN tt-cli.c-status = "Inativo".
                        ELSE
                           ASSIGN tt-cli.c-status = "Ativo".
                     END. 
                  END. 
                  ELSE DO:
                     CREATE tt-cli.
                     ASSIGN tt-cli.cod-emitente   =   emitente.cod-emitente
                            tt-cli.nome-emit      =   emitente.nome-emit
                            tt-cli.endereco       =   emitente.endereco
                            tt-cli.bairro         =   emitente.bairro
                            tt-cli.cidade         =   emitente.cidade              
                            tt-cli.estado         =   emitente.estado              
                            tt-cli.nome-ab-rep    =   repres.nome-abrev         
                            tt-cli.tel1           =   emitente.telefone[1]
                            tt-cli.tel2           =   emitente.telefone[2]
                            tt-cli.i-dias         =   tt-param.dt-fim - ems2ima.nota-fiscal.dt-emis-nota.

                     IF ems2ima.nota-fiscal.dt-emis-nota < tt-param.dt-ini THEN
                        ASSIGN tt-cli.c-status = "Inativo".
                     ELSE
                        ASSIGN tt-cli.c-status = "Ativo".
                  END. /*else dbaux.nota-fiscal.dt-emis-nota > (tt-param.dt-fim - tt-cli.i-dias) 13*/
              END. /*FOR LAST dbaux.nota-fiscal 12*/
           END. /*IF AVAIL dbaux.nota-fiscal 11*/
        END.  /*sem compra = no 8*/
    END. /*for each emitente 2*/
END. /*for each repres 1*/
RUN pi-finalizar IN h-acomp. 

DISCONNECT dbaux.
