DISABLE TRIGGERS FOR LOAD OF ima.emitente.
DISABLE TRIGGERS FOR LOAD OF ima.cont-emit.
DISABLE TRIGGERS FOR LOAD OF ima.cliente.
DISABLE TRIGGERS FOR LOAD OF med.emitente.
DISABLE TRIGGERS FOR LOAD OF med.cont-emit.
DISABLE TRIGGERS FOR LOAD OF med.cliente.
DISABLE TRIGGERS FOR LOAD OF imabkp.emitente.
DISABLE TRIGGERS FOR LOAD OF imabkp.emitente.
DISABLE TRIGGERS FOR LOAD OF imabkp.cont-emit.
DISABLE TRIGGERS FOR LOAD OF imabkp.cont-emit.
DISABLE TRIGGERS FOR LOAD OF imabkp.cliente.
DISABLE TRIGGERS FOR LOAD OF imabkp.cliente.
DISABLE TRIGGERS FOR LOAD OF ems5bkp.cliente.
DISABLE TRIGGERS FOR LOAD OF ems5bkp.clien_financ.
DISABLE TRIGGERS FOR LOAD OF ems5bkp.pessoa_jurid.
DISABLE TRIGGERS FOR LOAD OF ems5bkp.clien_analis_cr.
DISABLE TRIGGERS FOR LOAD OF ems5bkp.telef_pessoa.
DISABLE TRIGGERS FOR LOAD OF ems5bkp.contato.
DEFINE INPUT  PARAMETER pCliente AS INTEGER     NO-UNDO.
DEFINE VARIABLE cAcao AS CHARACTER   NO-UNDO FORMAT 'x(1000)'.
DEFINE VARIABLE cArquivo AS CHARACTER   NO-UNDO INIT "c:\temp\logcliente.txt".
OUTPUT TO VALUE(cArquivo).
FIND FIRST med.emitente
    WHERE med.emitente.cod-emitente = pCliente NO-LOCK NO-ERROR.
IF AVAIL med.emitente THEN DO:
   PUT "cliente(" pcliente ") encontrado na base Med" SKIP. 
   FIND  FIRST ima.emitente 
       WHERE ima.emitente.cod-emitente = med.emitente.cod-emitente NO-LOCK NO-ERROR.
   IF NOT AVAIL  ima.emitente THEN DO:
       PUT "cliente(" 
           pcliente 
           ") NAO encontrado na base IMA. O cliente foi Criado" SKIP. 
      CREATE ima.emitente.
      BUFFER-COPY med.emitente TO ima.emitente.
      MESSAGE "cliente criado" ima.emitente.cod-emitente SKIP
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ASSIGN cAcao = 'Emitente Criado na ima Oficial'.
   END.
   ELSE DO: 
      PUT "cliente(" pcliente ") encontrado na base IMA. NAO foi criado o cliente." SKIP. 
      ASSIGN cAcao = 'Emitente J  Existia na ima Oficial'.
   END.
      
  /* PUT "cliente(" pcliente ") criado na base medbkp , sem verifica‡Æo se j  existia." SKIP. 
   CREATE medbkp.emitente.
   BUFFER-COPY med.emitente TO medbkp.emitente.
   ASSIGN cAcao = cAcao + CHR(10) + 'Emitente Criado na IMA BKP'.
   PUT "cliente(" pcliente ") criado na base imabkp , sem verifica‡Æo se j  existia." SKIP. 
   CREATE imabkp.emitente.
   BUFFER-COPY med.emitente TO imabkp.emitente.
   ASSIGN cAcao = cAcao + CHR(10) + 'Emitente Criado na ima BKP'.*/
   FOR EACH med.cont-emit OF med.emitente NO-LOCK.
          
          CREATE ima.cont-emit.
          BUFFER-COPY med.cont-emit TO ima.cont-emit NO-ERROR.
          ASSIGN cAcao = cAcao + CHR(10) + 'Contato do Cliente Criado - ima Oficial'.
          
          CREATE imabkp.cont-emit.
          BUFFER-COPY med.cont-emit TO imabkp.cont-emit NO-ERROR.
          ASSIGN cAcao = cAcao + CHR(10) + 'Contato do Cliente Criado - IMA BKP'.
       
          CREATE imabkp.cont-emit.
          BUFFER-COPY med.cont-emit TO imabkp.cont-emit NO-ERROR.
          ASSIGN cAcao = cAcao + CHR(10) + 'Contato do Cliente Criado - ima BKP'.
       
   END.

   /*FIND FIRST ems2ima.cliente OF ems2ima.emitente 
   NO-LOCK NO-ERROR.
   IF AVAIL ems2ima.cliente THEN DO:

      CREATE ima.cliente.
      BUFFER-COPY ems2ima.cliente TO ima.cliente NO-ERROR.
      ASSIGN cAcao = cAcao + CHR(10) + 'Cliente x Emitente Criado - ima Oficial'.

      CREATE imabkp.cliente.
      BUFFER-COPY ems2ima.cliente TO imabkp.cliente NO-ERROR.
      ASSIGN cAcao = cAcao + CHR(10) + 'Cliente x Emitente Criado - IMA Bkp'.

      CREATE imabkp.cliente.
      BUFFER-COPY ems2ima.cliente TO imabkp.cliente NO-ERROR.
      ASSIGN cAcao = cAcao + CHR(10) + 'Cliente x Emitente Criado - ima Bkp'.

   END.*/
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
OUTPUT CLOSE.

   /*
emitente OF estatist (cod-emitente)
    emitente OF emitente-cex (cod-emitente)
    emitente OF cliente (nome-abrev)
      emitente OF cliente-sfa (nome-abrev)
    emitente OF cliente-sfa (nome-abrev)
    emitente OF dist-emitente (cod-emitente)
    cont-emit
     */
