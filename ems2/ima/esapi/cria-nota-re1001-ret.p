/*
Exemplo de como chamar essa api

/* Variaveis do Recebimento Fiscal ---                                  */
DEF TEMP-TABLE tt-docum-est NO-UNDO LIKE docum-est 
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE tt-item-doc-est NO-UNDO LIKE item-doc-est
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-item-terc NO-UNDO
    FIELD rw-saldo-terc   AS ROWID
    FIELD quantidade      LIKE saldo-terc.quantidade
    FIELD preco-total     LIKE componente.preco-total EXTENT 0
    FIELD desconto        LIKE componente.desconto    EXTENT 0    
    FIELD cod-depos       LIKE saldo-terc.cod-depos
    FIELD nr-ord-prod     LIKE saldo-terc.nr-ord-prod
    FIELD nat-of          LIKE item-doc-est.nat-of.

    DEF VAR de-tot-valor AS DEC.
    DEF VAR de-tot-qtde AS DEC.

    FIND FIRST nota-fiscal WHERE
         ROWID(nota-fiscal) = tt-notas-geradas.rw-nota-fiscal
         NO-ERROR.

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK
        BREAK BY it-nota-fisc.it-codigo.

        FOR EACH saldo-terc WHERE
                 saldo-terc.cod-emit = b-estabelec.cod-emit AND  // 504
                 saldo-terc.nro-docto = it-nota-fisc.nr-docum AND 
                 saldo-terc.it-codigo = it-nota-fisc.it-codigo and
                 saldo-terc.cod-refer = it-nota-fisc.cod-refer NO-LOCK.
        
            CREATE tt-item-terc.
            ASSIGN tt-item-terc.rw-saldo-terc = ROWID(saldo-terc)
                   tt-item-terc.quantidade    = it-nota-fisc.qt-faturada[1]
                   tt-item-terc.preco-total   = it-nota-fisc.vl-tot-item
                   tt-item-terc.desconto      = saldo-terc.dec-2
                   tt-item-terc.cod-depos     = saldo-terc.cod-depos
                   tt-item-terc.nr-ord-prod   = saldo-terc.nr-ord-prod.
        END.
    
        ASSIGN de-tot-qtde = de-tot-qtde + it-nota-fisc.qt-faturada[1]
               de-tot-valor = de-tot-valor + it-nota-fisc.vl-tot-item.

    END.
    
    /* Cria Temp-Table para o Recebimento*/
    CREATE tt-docum-est.
    ASSIGN tt-docum-est.cod-emit = b-estabelec.cod-emit
           tt-docum-est.serie-docto = nota-fiscal.serie
           tt-docum-est.nro-docto = nota-fiscal.nr-nota-fis
           tt-docum-est.cod-estabel = estabelec.cod-estabel
           tt-docum-est.nat-operacao = '11208m'
           tt-docum-est.dt-emissao = nota-fiscal.dt-emis
           tt-docum-est.dt-trans = TODAY
           tt-docum-est.cod-chave-aces-nf-eletro = nota-fiscal.cod-chave-aces-nf-eletro.
    
    ASSIGN tt-docum-est.tot-valor = de-tot-valor 
           tt-docum-est.tot-peso = de-tot-qtde
           tt-docum-est.valor-mercad = de-tot-valor
           tt-docum-est.base-icm = de-tot-valor  
           tt-docum-est.base-ipi = de-tot-valor 
           tt-docum-est.despesa-nota = 0.

    // Cria docum-est
    RUN esapi/cria-nota-re1001-ret.p (INPUT TABLE tt-docum-est,
                                      INPUT TABLE tt-item-terc).
    IF RETURN-VALUE = 'ADM-ERROR' THEN
       RETURN 'ADM-ERROR'.

*/



/* Variaveis do Recebimento Fiscal ---                                  */
DEF TEMP-TABLE tt-docum-est NO-UNDO LIKE docum-est 
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-item-doc-est NO-UNDO LIKE item-doc-est
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-item-terc NO-UNDO
    FIELD rw-saldo-terc   AS ROWID
    FIELD quantidade      LIKE saldo-terc.quantidade
    FIELD preco-total     LIKE componente.preco-total EXTENT 0
    FIELD desconto        LIKE componente.desconto    EXTENT 0    
    FIELD cod-depos       LIKE saldo-terc.cod-depos
    FIELD nr-ord-prod     LIKE saldo-terc.nr-ord-prod
    FIELD nat-of          LIKE item-doc-est.nat-of.


/* Temp-Table tt-erro Definitions ---*/
{method/dbotterr.i}

DEF VAR h-boin090      AS HANDLE.
DEF VAR h-boin176      AS HANDLE.

DEF INPUT PARAMETER TABLE FOR tt-docum-est.
DEF INPUT PARAMETER TABLE FOR tt-item-terc.

IF NOT VALID-HANDLE(h-boin090) OR
   h-boin090:TYPE      <> "PROCEDURE":U OR
   h-boin090:FILE-NAME <> "inbo/boin090.p":U THEN
   RUN inbo/boin090.p PERSISTENT SET h-boin090.

IF NOT VALID-HANDLE(h-boin176) OR 
   h-boin176:TYPE      <> "PROCEDURE":U OR
   h-boin176:FILE-NAME <> "inbo/boin176.p":U THEN
   RUN inbo/boin176.p PERSISTENT SET h-boin176.



DO TRANSACTION ON ERROR UNDO:


    /* Cria Recebimento Fiscal */
    RUN openQueryStatic IN h-boin090 (INPUT "Main":U).
    RUN emptyRowErrors IN h-boin090.
    RUN setRecord IN h-boin090 (INPUT TABLE tt-docum-est).
    RUN createRecord IN h-boin090.
    RUN getRowErrors IN h-boin090 (OUTPUT TABLE RowErrors).
    
    IF CAN-FIND(FIRST RowErrors WHERE 
                      RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
    
       RUN mostrarErro.
       IF VALID-HANDLE(h-boin090) THEN
          DELETE PROCEDURE h-boin090.
    
       IF VALID-HANDLE(h-boin176) THEN
          DELETE PROCEDURE h-boin176.
    
       RETURN 'ADM-ERROR'.
    END.
    
    /* Cria Itens do Recebimento*/
    RUN createItemOfComponente IN h-boin176 (INPUT h-boin090,
                                             INPUT TABLE tt-item-terc ).
    
    RUN getRowErrors IN h-boin176 (OUTPUT TABLE RowErrors).
    IF CAN-FIND(FIRST RowErrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
    
       RUN mostrarErro.
       IF VALID-HANDLE(h-boin090) THEN
          DELETE PROCEDURE h-boin090.
    
       IF VALID-HANDLE(h-boin176) THEN
          DELETE PROCEDURE h-boin176.
    
       RETURN 'ADM-ERROR'.
    END.
    
    
    IF VALID-HANDLE(h-boin090) THEN
       DELETE PROCEDURE h-boin090.
    
    IF VALID-HANDLE(h-boin176) THEN
       DELETE PROCEDURE h-boin176.

END.


PROCEDURE mostrarErro:

FOR EACH rowerrors WHERE
        RowErrors.ErrorSubType = "ERROR":U:
    MESSAGE "Erro ao Gerar o Recebimento" SKIP
            "Sequencia:" rowerrors.errorSequence SKIP
            "Numero:" rowerrors.errornumber SKIP
            "Descri‡Æo:"rowerrors.errordescription SKIP
            "Ajuda:" rowerrors.errorhelp SKIP
            "Parametros:" rowerrors.errorParameters SKIP
           VIEW-AS ALERT-BOX.
END.

END PROCEDURE.

          

