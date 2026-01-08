DISABLE TRIGGERS FOR LOAD OF cont-emit.

DEF TEMP-TABLE tt-aux LIKE emitente.
DEF TEMP-TABLE tt-cont-emit LIKE cont-emit.
DEF TEMP-TABLE tt-loc-entr LIKE loc-entr.

DEF BUFFER empresa FOR mgadm.empresa.
DEF BUFFER cliente FOR ems5.cliente.
DEF BUFFER fornecedor FOR ems5.fornecedor.

DEFINE VAR h-api                AS HANDLE NO-UNDO.
DEFINE VAR c-arquivo-saida      AS CHAR FORMAT "x(20)"     INITIAL "c:\temp\api006.lst". 
DEFINE VAR c-arquivo-integra    AS CHAR FORMAT "x(20)"     INITIAL "c:\temp\integra.lst". 
DEFINE VAR l-erro               AS LOGICAL FORMAT "yes/no" INITIAL NO.
DEFINE VAR c-arq-log            AS CHAR.
DEFINE VAR l-new-record         AS LOGICAL FORMAT "yes/no" INITIAL NO.
DEFINE VAR c_cod_telef_sem_edic LIKE telef_pessoa.cod_telef_sem_edic.
DEFINE VAR i-num-pessoa         AS INT.
DEFINE VAR c-desc               AS CHAR.
DEFINE VAR c-ambiente           AS CHAR.
DEFINE VAR i-banco              AS INTEGER. 

DEF INPUT PARAMETER TABLE FOR tt-aux.
DEF INPUT PARAMETER TABLE FOR tt-cont-emit.
DEF INPUT PARAMETER TABLE FOR tt-loc-entr.

FIND FIRST param-global NO-LOCK NO-ERROR.

ASSIGN c-arq-log = "\\ima-srv-file\publico\integra-emitente" + STRING(TODAY,"99-99-9999") + ".txt".

DO i-banco = 1 TO NUM-DBS:
   IF LDBNAME(i-banco) = 'ems5' THEN DO.
      IF ENTRY(2,ENTRY(3,DBPARAM(i-banco)),"") BEGINS "10" THEN
         ASSIGN c-ambiente = "OFICIAL".
      IF ENTRY(2,ENTRY(3,DBPARAM(i-banco)),"") BEGINS "30" THEN
         ASSIGN c-ambiente = "BACKUP".
   END.
END.

{esinc/cdapi366b.i}
RUN cdp/cdapi366b.p PERSISTENT SET h-api.

FOR EACH tt-emitente.
    DELETE tt-emitente.
END.

FOR EACH tt-aux.
    CREATE tt-emitente.
    BUFFER-COPY tt-aux TO tt-emitente.

    OUTPUT TO VALUE(c-arq-log) APPEND.
        PUT UNFORMATTED "Iniciando Integra‡Æo " AT 7
            NOW
            SKIP.
    OUTPUT CLOSE.

    FIND emitente WHERE
         emitente.cod-emitente = tt-emitente.cod-emitente SHARE-LOCK NO-ERROR.
    
    IF AVAIL emitente THEN DO.
       BUFFER-COPY tt-emitente EXCEPT cod-emitente nome-abrev TO emitente.
       OUTPUT TO VALUE(c-arq-log) APPEND.
           PUT UNFORMATTED "Alterado Emitente " AT 7
               NOW
               SKIP.
       OUTPUT CLOSE.
       ASSIGN l-new-record = YES.
    END.
    ELSE DO.
       BUFFER-COPY tt-emitente TO emitente.
       ASSIGN l-new-record = YES.
       OUTPUT TO VALUE(c-arq-log) APPEND.
           PUT UNFORMATTED "Criado novo Emitente" AT 7
               NOW
               SKIP.
       OUTPUT CLOSE.
    END.

    OUTPUT TO VALUE(c-arq-log) APPEND.
        PUT UNFORMATTED "Integrando Contatos do Emitente " AT 7
            NOW
            SKIP.
    OUTPUT CLOSE.
    FOR EACH cont-emit OF emitente.
        DELETE cont-emit.
    END.
    FOR EACH tt-cont-emit NO-LOCK:
        CREATE cont-emit.
        BUFFER-COPY tt-cont-emit TO cont-emit.         
    END.
    OUTPUT TO VALUE(c-arq-log) APPEND.
        PUT UNFORMATTED "Contatos Integrados com SUCESSO " AT 7
            NOW
            SKIP.
    OUTPUT CLOSE.


    OUTPUT TO VALUE(c-arq-log) APPEND.
        PUT UNFORMATTED "Integrando Local de Entrega do Emitente " AT 7
            NOW
            SKIP.
    OUTPUT CLOSE.
    FOR EACH loc-entr WHERE
             loc-entr.nome-abrev = emitente.nome-abrev EXCLUSIVE-LOCK.
        DELETE loc-entr.
    END.
    FOR EACH tt-loc-entr NO-LOCK.
        CREATE loc-entr.
        BUFFER-COPY tt-loc-entr TO loc-entr. 
    END.
    OUTPUT TO VALUE(c-arq-log) APPEND.
        PUT UNFORMATTED "Local de Entrega Integrado com SUCESSO " AT 7
            NOW
            SKIP.
    OUTPUT CLOSE.

    IF l-new-record THEN DO.
       FIND FIRST tab-ocor WHERE
                  tab-ocor.cod-tab   = 098 AND
                  tab-ocor.descricao = "Inclui Emit" USE-INDEX descricao
                  SHARE-LOCK NO-ERROR.
       IF AVAIL tab-ocor THEN DO.
          IF tab-ocor.i-campo[1] < tt-emitente.cod-emitente THEN
             ASSIGN tab-ocor.i-campo[1] = tt-emitente.cod-emitente.
       END.
    END.

    IF c-ambiente = "BACKUP" THEN DO.
        OUTPUT TO VALUE(c-arq-log) APPEND.
            PUT UNFORMATTED "Integrando EMS 5.00 " AT 7
                NOW
                SKIP.
        OUTPUT CLOSE.
    
        RUN cdp/cd1608.p (INPUT emitente.cod-emitente,
                          INPUT emitente.cod-emitente,
                          INPUT emitente.identific,
                          INPUT YES,
                          INPUT 1,
                          INPUT 0,
                          INPUT c-arquivo-integra,
                          INPUT "Arquivo":U,
                          INPUT ""). 
    
        OUTPUT TO VALUE(c-arq-log) APPEND.
            PUT UNFORMATTED "FIM Integra‡Æo EMS 5.00 " AT 7
                NOW
                SKIP.
        OUTPUT CLOSE.
    END.
      
    /*
    /* Atualizando Telefones do EMS 5 */
    FIND emitente WHERE
         emitente.cod-emitente = tt-emitente.cod-emitente NO-LOCK NO-ERROR.

    ASSIGN i-num-pessoa = 0.
    IF emitente.identific = 2 THEN DO.
       FIND FIRST fornecedor WHERE
                  fornecedor.cdn_fornec = emitente.cod-emit NO-LOCK NO-ERROR.
       IF NOT AVAIL fornecedor THEN NEXT.
       ASSIGN i-num-pessoa = fornecedor.num_pessoa.
    END.
    ELSE DO.
       FIND FIRST cliente WHERE
                  cliente.cdn_cliente = emitente.cod-emit NO-LOCK NO-ERROR.
       IF NOT AVAIL cliente THEN NEXT.
       ASSIGN i-num-pessoa = cliente.num_pessoa.
    END.
    IF i-num-pessoa = 0 THEN NEXT.

    FOR EACH telef_pessoa WHERE
             telef_pessoa.num_pessoa = i-num-pessoa EXCLUSIVE-LOCK.
        DELETE telef_pessoa.
    END.

    FOR EACH tt-cont-emit NO-LOCK.
        RUN pi-cria-telef (INPUT tt-cont-emit.telefone,
                           INPUT "Fixo",
                           INPUT "Telefone Fixo").
    
        FIND ext-cont-emit WHERE
             ext-cont-emit.cod-emitente = tt-cont-emit.cod-emitente AND
             ext-cont-emit.sequencia = tt-cont-emit.sequencia
             NO-LOCK NO-ERROR.
        IF AVAIL ext-cont-emit THEN DO.
           IF ext-cont-emit.celular1 <> '' THEN DO.
              ASSIGN c-desc = "Celular SEM Messenger".
              IF ext-cont-emit.messenger1 THEN
                 ASSIGN c-desc = "Celular COM Messenger: " + ext-cont-emit.aplicativo1.
    
              RUN pi-cria-telef (INPUT ext-cont-emit.celular1,
                                 INPUT "Celular",
                                 INPUT c-desc).
           END.
    
           IF ext-cont-emit.celular2 <> '' THEN DO.
              ASSIGN c-desc = "Celular SEM Messenger".
              IF ext-cont-emit.messenger2 THEN
                 ASSIGN c-desc = "Celular COM Messenger: " + ext-cont-emit.aplicativo2.
    
              RUN pi-cria-telef (INPUT ext-cont-emit.celular2,
                                 INPUT "Celular",
                                 INPUT c-desc).
           END.
    
           IF ext-cont-emit.celular3 <> '' THEN DO.
              ASSIGN c-desc = "Celular SEM Messenger".
              IF ext-cont-emit.messenger3 THEN
                 ASSIGN c-desc = "Celular COM Messenger: " + ext-cont-emit.aplicativo3.
    
              RUN pi-cria-telef (INPUT ext-cont-emit.celular3,
                                 INPUT "Celular",
                                 INPUT c-desc).
           END.
        END.
    END.
    */
END.

DELETE PROCEDURE h-api.

/* fim de programa */

/*
PROCEDURE pi-cria-telef.
    DEF INPUT PARAMETER p-cod-telef AS CHAR.
    DEF INPUT PARAMETER p-tipo-telef AS CHAR.
    DEF INPUT PARAMETER p-desc AS CHAR.

    ASSIGN c_cod_telef_sem_edic = ''.
    RUN pi-formata (INPUT p-cod-telef).

    FIND telef_pessoa WHERE
         telef_pessoa.num_pessoa = i-num-pessoa AND
         telef_pessoa.cod_telef_sem_edic = c_cod_telef_sem_edic 
         SHARE-LOCK NO-ERROR.

    IF NOT AVAIL telef_pessoa THEN DO.
       CREATE telef_pessoa.
       ASSIGN telef_pessoa.cod_telef_sem_edic = c_cod_telef_sem_edic        
              telef_pessoa.num_pessoa = i-num-pessoa
              telef_pessoa.dat_livre_1 = emitente.data-implant              
              telef_pessoa.dat_livre_2 = emitente.data-implant.

       FIND FIRST telef WHERE 
                  telef.cod_telef_sem_edic = telef_pessoa.cod_telef_sem_edic AND
                  telef.ind_tip_telef_pessoa = p-tipo-telef NO-ERROR.
       IF NOT AVAIL telef THEN DO.
          CREATE telef.
          ASSIGN telef.cod_telef_sem_edic = telef_pessoa.cod_telef_sem_edic
                 telef.ind_tip_telef_pessoa = p-tipo-telef.
       END.
    END.
    ASSIGN telef_pessoa.cod_telefone = p-cod-telef
           telef_pessoa.des_telefone = p-desc.

END PROCEDURE.

PROCEDURE pi-formata.
    DEF INPUT PARAMETER p-cod-telef LIKE cont-emit.telefone.
    DEF VAR i-ct AS INT.
    DO i-ct = 1 TO LENGTH(p-cod-telef).
       IF SUBSTR(p-cod-telef,i-ct,1) >= '0' AND
          SUBSTR(p-cod-telef,i-ct,1) <= '9' THEN
          ASSIGN c_cod_telef_sem_edic = c_cod_telef_sem_edic + SUBSTR(p-cod-telef,i-ct,1).
    END.
END PROCEDURE.


*/
