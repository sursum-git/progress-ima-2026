/****************************************************************************
** Programa : TWAD098 - trigger de Write para a tabela b-emit-new  - TONINHO
** Data     : Novmembro 2014
** Objetivo : trigger de Write para a tabela b-emit-new 
** Empresa  : IMA 
** Vers∆o   : 2.04.001
** Alterado : 
** Fluxo    : Integrar b-emit-new com as Demais Bases
*****************************************************************************/

DEFINE PARAMETER BUFFER b-emit-new FOR emitente.
DEFINE PARAMETER BUFFER b-emit-old FOR emitente.  

DEFINE VAR i-num-pessoa LIKE ems5.fornecedor.num_pessoa.
DEFINE VAR c_cod_telef_sem_edic LIKE telef_pessoa.cod_telef_sem_edic.
DEFINE VAR c-desc AS CHAR.

IF AVAIL b-emit-new THEN DO:
   /* Atualizando Telefones do EMS 5 */
   /*
   ASSIGN i-num-pessoa = 0.
   IF b-emit-new.identific = 2 THEN DO.
      FIND FIRST ems5.fornecedor WHERE
                 ems5.fornecedor.cdn_fornec = b-emit-new.cod-emit NO-LOCK NO-ERROR.
      IF NOT AVAIL ems5.fornecedor THEN NEXT.
      ASSIGN i-num-pessoa = ems5.fornecedor.num_pessoa.
   END.
   ELSE DO.
      FIND FIRST ems5.cliente WHERE
                 ems5.cliente.cdn_cliente = b-emit-new.cod-emit NO-LOCK NO-ERROR.
      IF NOT AVAIL ems5.cliente THEN NEXT.
      ASSIGN i-num-pessoa = ems5.cliente.num_pessoa.
   END.
   IF i-num-pessoa = 0 THEN NEXT.

    FOR EACH telef_pessoa WHERE
             telef_pessoa.num_pessoa = i-num-pessoa EXCLUSIVE-LOCK.
        DELETE telef_pessoa.
    END.

    FOR EACH cont-emit NO-LOCK.
        RUN pi-cria-telef (INPUT cont-emit.telefone,
                           INPUT "Fixo",
                           INPUT "Telefone Fixo").
    
        FIND ext-cont-emit WHERE
             ext-cont-emit.cod-emitente = cont-emit.cod-emitente AND
             ext-cont-emit.sequencia = cont-emit.sequencia
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




/********************** PROCEDURES ***********************/
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
              telef_pessoa.dat_livre_1 = b-emit-new.data-implant              
              telef_pessoa.dat_livre_2 = b-emit-new.data-implant.

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

