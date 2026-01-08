DISABLE TRIGGERS FOR LOAD OF med.emitente.
DISABLE TRIGGERS FOR LOAD OF med.cont-emit.
DISABLE TRIGGERS FOR LOAD OF med.cliente.
DISABLE TRIGGERS FOR LOAD OF imabkp.emitente.
DISABLE TRIGGERS FOR LOAD OF medbkp.emitente.
DISABLE TRIGGERS FOR LOAD OF imabkp.cont-emit.
DISABLE TRIGGERS FOR LOAD OF medbkp.cont-emit.
DISABLE TRIGGERS FOR LOAD OF imabkp.cliente.
DISABLE TRIGGERS FOR LOAD OF medbkp.cliente.
DISABLE TRIGGERS FOR LOAD OF ems5bkp.cliente.
DISABLE TRIGGERS FOR LOAD OF ems5bkp.clien_financ.
DISABLE TRIGGERS FOR LOAD OF ems5bkp.pessoa_jurid.
DISABLE TRIGGERS FOR LOAD OF ems5bkp.clien_analis_cr.
DISABLE TRIGGERS FOR LOAD OF ems5bkp.telef_pessoa.
DISABLE TRIGGERS FOR LOAD OF ems5bkp.contato.
DEFINE INPUT  PARAMETER pCliente AS INTEGER     NO-UNDO.
DEFINE VARIABLE cAcao AS CHARACTER   NO-UNDO FORMAT 'x(100)'.

FIND FIRST ems2ima.emitente
    WHERE ems2ima.emitente.cod-emitente = pCliente NO-LOCK NO-ERROR.
IF AVAIL ems2ima.emitente THEN DO:
   FIND   med.emitente 
       WHERE med.emitente.cod-emitente = ems2ima.emitente.cod-emitente NO-LOCK NO-ERROR.
   IF NOT AVAIL  med.emitente THEN DO:
      CREATE med.emitente.
      BUFFER-COPY ems2ima.emitente TO med.emitente NO-ERROR.
      ASSIGN cAcao = 'Emitente Criado na MED Oficial'.
   END.
   ELSE 
      ASSIGN cAcao = 'Emitente J  Existia na MED Oficial'.
   
   CREATE imabkp.emitente.
   BUFFER-COPY ems2ima.emitente TO imabkp.emitente.
   ASSIGN cAcao = cAcao + CHR(10) + 'Emitente Criado na IMA BKP'.
   
   CREATE medbkp.emitente.
   BUFFER-COPY ems2ima.emitente TO medbkp.emitente.
   ASSIGN cAcao = cAcao + CHR(10) + 'Emitente Criado na MED BKP'.

   FOR EACH ems2ima.cont-emit OF ems2ima.emitente NO-LOCK.
       IF AVAIL  ems2ima.cont-emit THEN DO:
          CREATE med.cont-emit.
          BUFFER-COPY ems2ima.cont-emit TO med.cont-emit NO-ERROR.
          ASSIGN cAcao = cAcao + CHR(10) + 'Contato do Cliente Criado - MED Oficial'.
          
          CREATE imabkp.cont-emit.
          BUFFER-COPY ems2ima.cont-emit TO imabkp.cont-emit NO-ERROR.
          ASSIGN cAcao = cAcao + CHR(10) + 'Contato do Cliente Criado - IMA BKP'.
       
          CREATE medbkp.cont-emit.
          BUFFER-COPY ems2ima.cont-emit TO medbkp.cont-emit NO-ERROR.
          ASSIGN cAcao = cAcao + CHR(10) + 'Contato do Cliente Criado - MED BKP'.
          
       END.
   END.
   FIND FIRST ems2ima.cliente OF ems2ima.emitente 
   NO-LOCK NO-ERROR.
   IF AVAIL ems2ima.cliente THEN DO:

      CREATE med.cliente.
      BUFFER-COPY ems2ima.cliente TO med.cliente NO-ERROR.
      ASSIGN cAcao = cAcao + CHR(10) + 'Cliente x Emitente Criado - MED Oficial'.

      CREATE imabkp.cliente.
      BUFFER-COPY ems2ima.cliente TO imabkp.cliente NO-ERROR.
      ASSIGN cAcao = cAcao + CHR(10) + 'Cliente x Emitente Criado - IMA Bkp'.

      CREATE medbkp.cliente.
      BUFFER-COPY ems2ima.cliente TO medbkp.cliente NO-ERROR.
      ASSIGN cAcao = cAcao + CHR(10) + 'Cliente x Emitente Criado - MED Bkp'.

   END.
END.

FOR EACH ems5.cliente
    WHERE ems5.cliente.cdn_cliente = pCliente NO-LOCK.
    CREATE ems5bkp.cliente.
    BUFFER-COPY ems5.cliente TO ems5bkp.cliente.
   ASSIGN cAcao = cAcao + CHR(10) + 'Cliente ems5 Criado - BKP' .
   FOR EACH ems5.clien_financ OF ems5.cliente NO-LOCK.
       CREATE  ems5bkp.clien_financ.
       BUFFER-COPY ems5.clien_financ TO ems5bkp.clien_financ NO-ERROR.
       ASSIGN cAcao = cAcao + CHR(10) + 'Cliente Financeiro ems5 Criado - BKP' .
   END.
   FOR EACH ems5.clien_analis_cr OF ems5.cliente NO-LOCK:
       CREATE ems5bkp.clien_analis_cr.
       BUFFER-COPY ems5.clien_analis_cr TO ems5bkp.clien_analis_cr NO-ERROR.
       ASSIGN cAcao = cAcao + CHR(10) + 'Cliente Financeiro - Analise Cr‚dito - ems5 Criado - BKP' .
   END.
   FIND FIRST ems5.pessoa_jurid 
       WHERE ems5.pessoa_jurid.num_pessoa_jurid =  ems5.cliente.num_pessoa NO-LOCK NO-ERROR.
   IF AVAIL ems5.pessoa_jurid THEN DO:
      CREATE ems5bkp.pessoa_jurid.
      BUFFER-COPY ems5.pessoa_jurid TO ems5bkp.pessoa_jurid NO-ERROR.
      ASSIGN cAcao = cAcao + CHR(10) + 'Pessoa Juridica do Cliente Criada  - BKP' .
      FOR EACH ems5.contato OF ems5.pessoa_jurid NO-LOCK:
          CREATE ems5bkp.contato.
          BUFFER-COPY ems5.contato TO ems5bkp.contato.
          ASSIGN cAcao = cAcao + CHR(10) + 'Contato da Pessoa Juridica do Cliente Criado- BKP' .

      END.
      FOR EACH ems5.telef_pessoa
          WHERE ems5.telef_pessoa.num_pessoa =  ems5.pessoa_jurid.num_pessoa_jurid NO-LOCK:
          CREATE ems5bkp.telef_pessoa.
          BUFFER-COPY ems5.telef_pessoa TO ems5bkp.telef_pessoa.
          ASSIGN cAcao = cAcao + CHR(10) + 'Telefones da Pessoa Juridica do Cliente Criado- BKP' .

      END.
   END.


END.


MESSAGE cAcao
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
   /*
emitente OF estatist (cod-emitente)
    emitente OF emitente-cex (cod-emitente)
    emitente OF cliente (nome-abrev)
      emitente OF cliente-sfa (nome-abrev)
    emitente OF cliente-sfa (nome-abrev)
    emitente OF dist-emitente (cod-emitente)
    cont-emit
     */
