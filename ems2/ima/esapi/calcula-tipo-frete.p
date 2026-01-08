DEFINE INPUT  PARAMETER p-nr-pedcli  LIKE ped-venda.nr-pedcli.
DEFINE OUTPUT PARAMETER p-tipo-frete AS INTEGER.

DEF BUFFER unid-feder FOR ems2cad.unid-feder.
DEF BUFFER cotacao FOR ems2cad.cotacao.

DEFINE VARIABLE de-tot-ped    AS DECIMAL.
DEFINE VARIABLE de-tot-prazo  AS INT.
DEFINE VARIABLE i-ct          AS INT.
DEFINE VARIABLE i-prazo-medio AS INT.
DEFINE VARIABLE l-preco-tab   AS LOG.
DEFINE VARIABLE l-outlet      AS LOG.
DEFINE VARIABLE c-nr-tabpre   AS CHAR.
DEFINE VARIABLE i-prz         AS INT.
DEF VAR de-preco-tab           AS DEC.
DEF VAR de-vlReal          AS DECIMAL.
DEF VAR de-vlDolar         AS DECIMAL.
DEF VAR i-tipo-preco    AS INTEGER.

FIND ped-venda WHERE
     ped-venda.nr-pedcli = p-nr-pedcli SHARE-LOCK NO-ERROR.

FIND ped-venda-ext WHERE
     ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
     ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.

FIND tbs_preco WHERE
     tbs_preco.tb_preco_id = ped-venda-ext.tb_preco_id
     NO-LOCK NO-ERROR.

/* Prazo MÇdio */
FIND cond-pagto WHERE
     cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag NO-LOCK NO-ERROR.
IF AVAIL cond-pagto THEN DO.
   ASSIGN i-prazo-medio = cond-pagto.qtd-dias-prazo-medio.
END.
ELSE DO.
    FOR EACH cond-ped WHERE
             cond-ped.nr-pedido = ped-venda.nr-pedido NO-LOCK.
        ASSIGN de-tot-prazo = de-tot-prazo + cond-ped.nr-dias-venc.
        ASSIGN i-ct = i-ct + 1.
    END.
    ASSIGN i-prazo-medio = de-tot-prazo / i-ct.
END.

ASSIGN de-tot-ped = 0.
FOR EACH ped-item OF ped-venda WHERE
         ped-item.cod-sit-item = 1 NO-LOCK.
    ASSIGN de-tot-ped = de-tot-ped + (ped-item.qt-pedida * ped-item.vl-preori).
END.

// Se o Pedido for em Dolar, converte para Real para Aplicar a Regra
IF ped-venda.mo-codigo = 3 THEN DO.
   FIND cotacao WHERE
        cotacao.mo-codigo = ped-venda.mo-codigo AND
        cotacao.ano-periodo = STRING(YEAR(TODAY),"9999") + 
                              STRING(MONTH(TODAY),"99") NO-LOCK NO-ERROR.
   IF AVAIL cotacao THEN DO.
      IF cotacao.cotacao[DAY(TODAY)] <> 0 THEN
         ASSIGN de-tot-ped = de-tot-ped * cotacao.cotacao[DAY(TODAY)].
      ELSE
         ASSIGN de-tot-ped = de-tot-ped * cotacao.cota-media.
   END.
   ELSE DO.
      FIND cotacao WHERE
           cotacao.mo-codigo = ped-venda.mo-codigo AND
           cotacao.ano-periodo = STRING(YEAR(ped-venda.dt-implant),"9999") + 
                                 STRING(MONTH(ped-venda.dt-implant),"99") NO-LOCK NO-ERROR.
      IF AVAIL cotacao THEN DO.
         IF cotacao.cotacao[DAY(ped-venda.dt-implant)] <> 0 THEN
            ASSIGN de-tot-ped = de-tot-ped * cotacao.cotacao[DAY(ped-venda.dt-implant)].
         ELSE
            ASSIGN de-tot-ped = de-tot-ped * cotacao.cota-media.
      END.
   END.
END.

// Inicializa com Frete CIF
ASSIGN p-tipo-frete = 1.  // CIF

FIND unid-feder WHERE
     unid-feder.estado = ped-venda.estado NO-LOCK NO-ERROR.
IF NOT AVAIL unid-feder THEN 
   RETURN.

IF AVAIL tbs_preco THEN DO.
   IF tbs_preco.num_tipo = 2 THEN DO.  // Ç Rubi
      FIND im-param WHERE
           im-param.cod-param = "VL_MIN_FRETE_CIF_RUBI" NO-LOCK NO-ERROR.

      IF de-tot-ped < DEC(im-param.val-param) THEN DO.   // 2500,00
         ASSIGN p-tipo-frete = 3.  // Fob
         RETURN.
      END.

      IF unid-feder.char-2 = 'Sudeste' THEN DO.
         ASSIGN p-tipo-frete = 1.  // CIF
      END.
      ELSE IF ped-venda.nome-tr-red <> '' THEN 
         ASSIGN p-tipo-frete = 2.  // CIF atÇ Resespacho
      ELSE
          ASSIGN p-tipo-frete = 3.  // Fob

      RETURN.
   END.
END.

// Se chegar aqui Ç porque n∆o Ç rubi ou n∆o tem tabela, ent∆o
// efetua calculo normal...

/* TotPedido < VL_MIN_FRETE_CIF */
FIND im-param WHERE
     im-param.cod-param = "VL_MIN_FRETE_CIF_"  + ped-venda.cod-estabel 
     NO-LOCK NO-ERROR.
IF NOT AVAIL im-param THEN
   FIND im-param WHERE
     im-param.cod-param = "VL_MIN_FRETE_CIF" 
     NO-LOCK NO-ERROR.

IF de-tot-ped < DEC(im-param.val-param) THEN DO.
   ASSIGN p-tipo-frete = 3.  // Fob
   RETURN.
END.


/* Pedido  VL_MAX_FRETE_FOB */
FIND im-param WHERE
     im-param.cod-param = "VL_MAX_FRETE_FOB" NO-LOCK NO-ERROR.

IF de-tot-ped > DEC(im-param.val-param) THEN DO.
   ASSIGN p-tipo-frete = 1.  // CIF

   IF unid-feder.char-2 = 'Norte' AND 
      ped-venda.nome-tr-red <> '' THEN DO:
      ASSIGN p-tipo-frete = 2.  // CIF atÇ Resespacho
   END.
      
   RETURN.
END.

// 'pedido com cliente fora da regi∆o sudeste, setado como frete igual a fob' 
IF unid-feder.char-2 <> 'Sudeste' THEN DO.
   ASSIGN p-tipo-frete = 3.  // Fob
   RETURN.
END.

/* Se cheqer aqui Ç porque o Pedido Ç maior que o MIN para CIF e
   menor que o MAX para FOB e para o Sudeste  */

//PUT 'entrei no sudeste' 
IF ped-venda.tp-preco = 1 THEN DO. /* Informado */
  //'entrei no preáo informado' 
  IF AVAIL cond-pagto THEN DO.
     ASSIGN de-tot-prazo = 0
            i-ct = 0.
     DO i-prz = 1 TO EXTENT(cond-pagto.prazo).
        IF cond-pagto.prazo[i-prz] <> 0 THEN DO.
           ASSIGN de-tot-prazo = de-tot-prazo + cond-pagto.prazo[i-prz].
           ASSIGN i-ct = i-ct + 1.
        END.
     END.
     ASSIGN i-prazo-medio = de-tot-prazo / i-ct.
  END.
  ELSE DO.
     ASSIGN de-tot-prazo = 0
            i-ct = 0.
     FOR EACH cond-ped WHERE
              cond-ped.nr-pedido = ped-venda.nr-pedido NO-LOCK.

         IF cond-ped.nr-dias-venc <> 0 THEN 
            ASSIGN de-tot-prazo = de-tot-prazo + cond-ped.nr-dias-venc.
         ELSE
            ASSIGN de-tot-prazo = de-tot-prazo + (cond-ped.data-pagto - TODAY).

         ASSIGN i-ct = i-ct + 1.
     END.
     ASSIGN i-prazo-medio = de-tot-prazo / i-ct.
  END.

  ASSIGN l-preco-tab = YES.
  FOR EACH ped-item OF ped-venda WHERE
           ped-item.cod-sit-item = 1 NO-LOCK.
      
      FIND ped-item-ext WHERE
           ped-item-ext.cod-estabel  = ped-venda.cod-estabel AND
           ped-item-ext.nome-abrev   = ped-item.nome-abrev AND
           ped-item-ext.nr-pedcli    = ped-item.nr-pedcli AND
           ped-item-ext.nr-sequencia = ped-item.nr-sequencia 
           NO-LOCK NO-ERROR.

      RUN pi-busca-preco (INPUT  ped-item.it-codigo,
                          INPUT  ped-item.cod-refer,
                          INPUT  "", // Campanha
                          OUTPUT de-vlReal,  
                          OUTPUT de-vlDolar).

      /*
      RUN pi-busca-preco-por-id (INPUT  ped-item-ext.cod_controle_preco,
                                 OUTPUT de-vlReal,  
                                 OUTPUT de-vlDolar,
                                 OUTPUT i-tipo-preco). 
      */                           
     
      IF ped-venda.mo-codigo = 0 THEN
         ASSIGN de-preco-tab = de-vlReal.
      ELSE
         ASSIGN de-preco-tab = de-vlDolar.

      IF ped-item.vl-preuni < de-preco-tab THEN
         ASSIGN l-preco-tab = NO.
  END.

  IF l-preco-tab AND 
     i-prazo-medio <= 90 THEN DO:
     //PUT 'preco dentro da tabela e prazo medio:' i-prazo-medio ' menor igual a 90 dias' 
     ASSIGN p-tipo-frete = 1.  // CIF
  END.
  ELSE DO: 
     //PUT 'preco abaixo da tabela ou prazo medio:' i-prazo-medio ' maior que 90 dias ' 
     ASSIGN p-tipo-frete = 3.  // Fob
  END.
     
END.
ELSE DO.   /* Preáo de Tabela */
  // 'tipo de preáo do pedido:tabela'
  IF l-outlet = NO AND i-prazo-medio <= 90 THEN DO:
     // 'pedido sem item outlet e com prazo medio:' i-prazo-medio ' menor igual a 90 dias' 
     ASSIGN p-tipo-frete = 1.  // CIF
  END.
  ELSE DO: 
      // 'pedido com item outlet ou com prazo medio:' i-prazo-medio ' maior que 90 dias' 
      ASSIGN p-tipo-frete = 3.  // Fob
  END.
     
END.


RETURN.


// Procedures------------------------------
PROCEDURE pi-busca-preco:
    DEF INPUT  PARAMETER p-it-codigo AS CHAR.
    DEF INPUT  PARAMETER p-cod-refer AS CHAR.
    DEF INPUT  PARAMETER p-campanha  AS CHAR.
    DEF OUTPUT PARAMETER p-vlReal    AS DECIMAL NO-UNDO. 
    DEF OUTPUT PARAMETER p-vlDolar   AS DECIMAL NO-UNDO.

    DEF VAR h-bo        AS HANDLE    NO-UNDO.
    DEF VAR i-tp-preco  AS INT.
    DEF VAR i-tp-busca  AS INT.
    DEF VAR iControlePreco AS INTEGER.

    ASSIGN i-tp-busca = 1.  // PE
    IF p-campanha = '' AND  // N∆o Exste campnha para PI
       ped-venda.tp-pedido = 'PI' AND
       ped-venda-ext.nr-container <> 0 THEN 
       ASSIGN i-tp-busca = 2.  // PI
    
    RUN esbo/boPrecosItemRef.p PERSISTENT SET h-bo.

    RUN iniciarBos      IN h-bo.
    RUN limparTTPreco   IN h-bo.
    RUN limparTTMsg     IN h-bo.
    RUN setTbPreco      IN h-bo (INPUT ped-venda-ext.tb_preco_id). 
    RUN setItem         IN h-bo (INPUT p-it-codigo). 
    RUN setRef          IN h-bo (INPUT p-cod-refer). 
    RUN setNrContainer  IN h-bo (INPUT ped-venda-ext.nr-container).
    RUN setTipoBusca    IN h-bo (INPUT i-tp-busca). 
    RUN setPrazoMedio   IN h-bo (INPUT i-prazo-medio).
    RUN buscarPrecos    IN h-bo.

    IF p-campanha <> '' THEN  
       RUN getPrecoPrazo   IN h-bo (INPUT p-campanha,
                                    OUTPUT p-vlReal,
                                    OUTPUT p-vlDolar,
                                    OUTPUT iControlePreco).
    ELSE
       RUN getPrecoPrazo   IN h-bo (INPUT ped-venda.tp-pedido,
                                    OUTPUT p-vlReal,
                                    OUTPUT p-vlDolar,
                                    OUTPUT iControlePreco).

    RUN finalizarBos IN h-bo.
    IF VALID-HANDLE(h-bo) THEN
       DELETE PROCEDURE h-bo.
    
END PROCEDURE.

PROCEDURE pi-busca-preco-por-id.
    DEF INPUT  PARAMETER p-id AS INTEGER.
    DEF OUTPUT PARAMETER p-vlReal    AS DECIMAL NO-UNDO. 
    DEF OUTPUT PARAMETER p-vlDolar   AS DECIMAL NO-UNDO.
    DEF OUTPUT PARAMETER p-tipo-preco   AS INTEGER.

    DEF VAR h-boPrecosItemRef        AS HANDLE    NO-UNDO.

    RUN esbo/boPrecosItemRef.p PERSISTENT SET h-boPrecosItemRef.

    RUN iniciarBos      IN h-boPrecosItemRef.
    RUN limparTTPreco   IN h-boPrecosItemRef.
    RUN limparTTMsg     IN h-boPrecosItemRef.
    RUN buscarPrecos    IN h-boPrecosItemRef.

    RUN getPrecoPrazoPorId IN h-boPrecosItemRef (INPUT p-id,
                                                 OUTPUT p-vlReal,
                                                 OUTPUT p-vlDolar,
                                                 OUTPUT p-tipo-preco).

    RUN finalizarBos IN h-boPrecosItemRef.
    IF VALID-HANDLE(h-boPrecosItemRef) THEN
       DELETE PROCEDURE h-boPrecosItemRef.

END PROCEDURE.


