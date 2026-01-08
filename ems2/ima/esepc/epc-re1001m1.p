/* Programa: epc-re1001m1.p
** Objetivo: Emitir NF de Remessa para Deposito Fechado
**           
** Autor...: PGS - Toninho  Maráo/2022
*/

DEF TEMP-TABLE tt-nota-fisc LIKE nota-fiscal.
DEF TEMP-TABLE tt-it-nota-fisc LIKE it-nota-fisc.

/* Variaveis do Recebimento Fiscal ---                                  */
DEF TEMP-TABLE tt-docum-est NO-UNDO LIKE docum-est 
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE tt-item-doc-est NO-UNDO LIKE item-doc-est
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-notas-geradas NO-UNDO
    FIELD rw-nota-fiscal AS   ROWID
    FIELD nr-nota        LIKE nota-fiscal.nr-nota-fis
    FIELD seq-wt-docto   LIKE wt-docto.seq-wt-docto.

DEF BUFFER b-estabelec FOR estabelec.

DEF NEW GLOBAL SHARED VAR gr-row-in090 AS ROWID.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR l-dep-externo AS LOG INIT NO.

FIND docum-est WHERE
     ROWID(docum-est) = gr-row-in090 NO-LOCK NO-ERROR.

// Valide o que quiser aqui...

FIND FIRST item-doc-est OF docum-est NO-LOCK NO-ERROR.

FIND processo-imp WHERE
     processo-imp.num-pedido = item-doc-est.num-pedido NO-LOCK NO-ERROR.
IF AVAIL processo-imp AND 
   processo-imp.num-pedido <> 0 THEN DO.   // Ç Importaá∆o

   FIND pp-container WHERE
        pp-container.nr-container = INTEGER(processo-imp.nr-proc-imp)
        NO-LOCK NO-ERROR.

   IF NOT AVAIL pp-container THEN DO.
      MESSAGE 'Container n∆o Encontrado com o Processo: ' processo-imp.nr-proc-imp
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN 'ADM-ERROR'.
   END.

   IF pp-container.cod-depos = '' THEN DO.
      MESSAGE 'Dep¢sito de Descarga do Container, n∆o foi Informado' SKIP
              'Verifique Programa espp001'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN 'ADM-ERROR'.
   END.

   FIND usuar-depos WHERE
        usuar-depos.cod-estab = pp-container.cod-estab AND
        //usuar-depos.cod-depos = pp-container.cod-depos AND
        usuar-depos.cod-usuar = c-seg-usuario 
        NO-LOCK NO-ERROR.
   IF NOT AVAIL usuar-depos THEN DO.
      MESSAGE 'Usu†rio n∆o est† relacionado ao Dep¢sito do Container' SKIP
              'Ou existe em mais de um deposito no mesmo Estabelecimento' 
              'Utilize cd1760' 
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
      RETURN 'ADM-ERROR'.
   END.

   FIND deposito WHERE
        deposito.cod-depos =  usuar-depos.cod-depos NO-LOCK NO-ERROR.

   // 2 - armazen geral, 3 - deposito fechado
   IF deposito.ind-tipo-dep = 2 OR deposito.ind-tipo-dep = 3 THEN
      ASSIGN l-dep-externo = YES. 

END.

// Executa a atualizaá∆o
APPLY 'choose' TO SELF.

// Coloquei sa emiss∆o aqui, porque na trigger da erro
FIND CURRENT docum-est NO-LOCK NO-ERROR.
IF docum-est.ce-atual = YES AND
   l-dep-externo THEN DO.

   IF deposito.ind-tipo-dep = 2 THEN DO.     // LISA  (LOGISTICA EXTERNA)
      RUN pi-trata-armazem-geral.
      IF RETURN-VALUE = 'ADM-ERROR' THEN
         RETURN 'ADM-ERROR'.
   END.
   /*
   IF deposito.ind-tipo-dep = 3 THEN DO.   // DEF  (Deposito da MED)
      RUN pi-trata-dep-fechado.
      IF RETURN-VALUE = 'ADM-ERROR' THEN
         RETURN 'ADM-ERROR'.
   END.
   */
END.

// ------------ PROCEDURES --------------- 
PROCEDURE pi-trata-dep-fechado.

   FIND estabelec WHERE
        estabelec.cod-estab = '5' NO-LOCK NO-ERROR.

   FIND b-estabelec WHERE
        b-estabelec.cod-estab = '504' NO-LOCK NO-ERROR.

   RUN pi-transf-dep-fechado.
   IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
      MESSAGE 'Erro ao Transferir Quantidade para Dep¢sito Fechado...' SKIP
              'Atualizaá∆o abortada... (esepc/epc-re1001m1.p)'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN 'ADM-ERROR'.  
   END.

   // se chegar aqui Ç porque gerou a NF de Saida
   DO TRANSACTION:
      RUN pi-recebimento.
      IF RETURN-VALUE = "ADM-ERROR" THEN
         RETURN 'ADM-ERROR'.

      RUN esapi/atu-nota-re1001.p (INPUT TABLE tt-docum-est).
      IF RETURN-VALUE = "ADM-ERROR" THEN
         RETURN 'ADM-ERROR'.
   END.

   MESSAGE 'Transferencia para Dep¢sito Fechado efetuada com Sucesso...'
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.


PROCEDURE pi-trata-armazem-geral.
    FIND estabelec WHERE
         estabelec.cod-estab = docum-est.cod-estab NO-LOCK NO-ERROR.


    FIND lisa-integra WHERE
         lisa-integra.cod-trans = 'NotaRemessa' AND
         lisa-integra.chave = STRING(pp-container.nr-container) 
         NO-LOCK NO-ERROR.
    IF NOT AVAIL lisa-integra THEN DO.
       CREATE lisa-integra.
       ASSIGN lisa-integra.cod-trans = 'NotaRemessa'
              lisa-integra.chave = STRING(pp-container.nr-container)
              lisa-integra.acao = 'GERAR'
              lisa-integra.ind-situacao = 1.  
    
       FIND lisa-integra WHERE
            lisa-integra.cod-trans = 'PackingList' AND
            lisa-integra.chave = STRING(pp-container.nr-container) SHARE-LOCK NO-ERROR.
       IF NOT AVAIL lisa-integra THEN DO. 
          CREATE lisa-integra.
          ASSIGN lisa-integra.cod-trans = 'PackingList'
                 lisa-integra.chave = STRING(pp-container.nr-container)
                 lisa-integra.acao = 'ENVIAR'
                 lisa-integra.ind-situacao = 1.
       END.
    END.
END PROCEDURE.


PROCEDURE pi-transf-armazem-geral.
    DEF VAR c-natur-oper AS CHAR.
    DEF VAR i-nr-seq AS INT.

/*
    ASSIGN c-natur-oper = '59207I'.

    FIND emitente WHERE
         emitente.nome-abrev = deposito.nome-abrev NO-LOCK NO-ERROR.

    CREATE tt-nota-fisc.
    ASSIGN tt-nota-fisc.cod-estabel = estabelec.cod-estabel  
           tt-nota-fisc.serie = estabelec.serie
           tt-nota-fisc.nome-ab-cli = emitente.nome-abrev
           tt-nota-fisc.nat-oper = c-natur-oper 
           tt-nota-fisc.dt-emis-nota = docum-est.dt-emis
           tt-nota-fisc.nro-proc-entrada = pp-container.nr-container. 

    ASSIGN i-nr-seq = 0.

    FOR EACH item-doc-est OF docum-est NO-LOCK.
        FIND ITEM WHERE
             ITEM.it-codigo = item-doc-est.it-codigo NO-LOCK NO-ERROR.
    
        ASSIGN i-nr-seq = i-nr-seq + 10.
        
        CREATE tt-it-nota-fisc.
        ASSIGN tt-it-nota-fisc.nr-seq-fat = i-nr-seq
               tt-it-nota-fisc.cod-estabel = tt-nota-fisc.cod-estabel  
               tt-it-nota-fisc.serie = tt-nota-fisc.serie
               tt-it-nota-fisc.nat-oper = tt-nota-fisc.nat-oper
               tt-it-nota-fisc.dt-emis-nota = tt-nota-fisc.dt-emis-nota
               tt-it-nota-fisc.it-codigo = item-doc-est.it-codigo
               tt-it-nota-fisc.cod-refer = item-doc-est.cod-refer
               tt-it-nota-fisc.cod-depos = 'ARM'
               tt-it-nota-fisc.un-fatur[1] = item.un
               tt-it-nota-fisc.un-fatur[2] = item.un
               tt-it-nota-fisc.qt-faturada[1] = item-doc-est.quantidade
               tt-it-nota-fisc.qt-faturada[2] = item-doc-est.quantidade
               tt-it-nota-fisc.vl-preuni = ROUND(item-doc-est.preco-unit[1],2).
    END.
    
    RUN esapi/cria-nota-ft4003.p (INPUT TABLE tt-nota-fisc,
                                  INPUT TABLE tt-it-nota-fisc,
                                  OUTPUT TABLE tt-notas-geradas).

    FIND FIRST tt-notas-geradas NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-notas-geradas THEN 
       RETURN 'ADM-ERROR'.
    */   
END PROCEDURE.


PROCEDURE pi-transf-dep-fechado.
    DEF VAR c-natur-oper AS CHAR.
    DEF VAR i-nr-seq AS INT.

    ASSIGN c-natur-oper = '51207M'. 

    FIND emitente WHERE
         emitente.cod-emit = b-estabelec.cod-emit NO-LOCK NO-ERROR.

    CREATE tt-nota-fisc.
    ASSIGN tt-nota-fisc.cod-estabel = estabelec.cod-estabel  
           tt-nota-fisc.serie = estabelec.serie
           tt-nota-fisc.nome-ab-cli = emitente.nome-abrev
           tt-nota-fisc.nat-oper = c-natur-oper 
           tt-nota-fisc.dt-emis-nota = docum-est.dt-emis. 

    ASSIGN i-nr-seq = 0.

    FOR EACH item-doc-est OF docum-est NO-LOCK.
        FIND ITEM WHERE
             ITEM.it-codigo = item-doc-est.it-codigo NO-LOCK NO-ERROR.
    
        ASSIGN i-nr-seq = i-nr-seq + 10.
        
        CREATE tt-it-nota-fisc.
        ASSIGN tt-it-nota-fisc.nr-seq-fat = i-nr-seq
               tt-it-nota-fisc.cod-estabel = tt-nota-fisc.cod-estabel  
               tt-it-nota-fisc.serie = tt-nota-fisc.serie
               tt-it-nota-fisc.nat-oper = tt-nota-fisc.nat-oper
               tt-it-nota-fisc.dt-emis-nota = tt-nota-fisc.dt-emis-nota
               tt-it-nota-fisc.it-codigo = item-doc-est.it-codigo
               tt-it-nota-fisc.cod-refer = item-doc-est.cod-refer
               tt-it-nota-fisc.cod-depos = 'ARM'
               tt-it-nota-fisc.un-fatur[1] = item.un
               tt-it-nota-fisc.un-fatur[2] = item.un
               tt-it-nota-fisc.qt-faturada[1] = item-doc-est.quantidade
               tt-it-nota-fisc.qt-faturada[2] = item-doc-est.quantidade
               tt-it-nota-fisc.vl-preuni = ROUND(item-doc-est.preco-unit[1],2).
    END.
    
    RUN esapi/cria-nota-ft4003.p (INPUT TABLE tt-nota-fisc,
                                  INPUT TABLE tt-it-nota-fisc,
                                  OUTPUT TABLE tt-notas-geradas).

    FIND FIRST tt-notas-geradas NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-notas-geradas THEN 
       RETURN 'ADM-ERROR'.
END PROCEDURE.

PROCEDURE pi-recebimento.
    DEF VAR de-tot-valor AS DEC.
    DEF VAR de-tot-qtde AS DEC.

    FIND FIRST nota-fiscal WHERE
         ROWID(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal
         NO-ERROR.
    
    /* Cria Temp-Table para o Recebimento*/
    CREATE tt-docum-est.
    ASSIGN tt-docum-est.cod-emit = estabelec.cod-emit
           tt-docum-est.serie-docto = nota-fiscal.serie
           tt-docum-est.nro-docto = nota-fiscal.nr-nota-fis
           tt-docum-est.cod-estabel = b-estabelec.cod-estabel
           tt-docum-est.nat-operacao = '11207m'
           tt-docum-est.dt-emissao = nota-fiscal.dt-emis
           tt-docum-est.dt-trans = TODAY
           tt-docum-est.cod-chave-aces-nf-eletro = nota-fiscal.cod-chave-aces-nf-eletro.

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK
             BREAK BY it-nota-fisc.it-codigo.

        FIND item WHERE
             item.it-codigo = tt-item-doc-est.it-codigo NO-LOCK NO-ERROR.

        FIND FIRST fat-ser-lote OF it-nota-fisc NO-LOCK NO-ERROR.

        /* Cria os Itens do Recebimento */
        CREATE tt-item-doc-est.
        ASSIGN tt-item-doc-est.nro-docto      = tt-docum-est.nro-docto
               tt-item-doc-est.serie-docto    = tt-docum-est.serie-docto
               tt-item-doc-est.cod-emitente   = tt-docum-est.cod-emit
               tt-item-doc-est.nat-operacao   = tt-docum-est.nat-operacao
               tt-item-doc-est.sequencia      = it-nota-fisc.nr-seq-fat
               tt-item-doc-est.cod-depos      = 'ARM'
               tt-item-doc-est.it-codigo      = it-nota-fisc.it-codigo
               tt-item-doc-est.cod-refer      = it-nota-fisc.cod-refer
               tt-item-doc-est.lote           = fat-ser-lote.nr-serlote
               tt-item-doc-est.qt-do-forn     = it-nota-fisc.qt-faturada[1]
               tt-item-doc-est.preco-total[1] = it-nota-fisc.vl-tot-item
               tt-item-doc-est.preco-unit[1]  = it-nota-fisc.vl-preuni
               tt-item-doc-est.peso-bruto     = it-nota-fisc.qt-faturada[1]
               tt-item-doc-est.base-icm       = it-nota-fisc.vl-tot-item
               tt-item-doc-est.base-ipi       = it-nota-fisc.vl-tot-item
               tt-item-doc-est.dt-vali-lote   = 12.31.9999
               tt-item-doc-est.narrativa      = "Recebimento Autom†tico".

        ASSIGN de-tot-qtde = de-tot-qtde + it-nota-fisc.qt-faturada[1]
               de-tot-valor = de-tot-valor + it-nota-fisc.vl-tot-item.
    END.
    ASSIGN tt-docum-est.tot-valor = de-tot-valor 
           tt-docum-est.tot-peso = de-tot-qtde
           tt-docum-est.valor-mercad = de-tot-valor
           tt-docum-est.base-icm = de-tot-valor  
           tt-docum-est.base-ipi = de-tot-valor 
           tt-docum-est.despesa-nota = 0.


    // Cria docum-est
    RUN esapi/cria-nota-re1001.p (INPUT TABLE tt-docum-est,
                                  INPUT TABLE tt-item-doc-est).
    IF RETURN-VALUE = 'ADM-ERROR' THEN
       RETURN 'ADM-ERROR'.

END PROCEDURE.

