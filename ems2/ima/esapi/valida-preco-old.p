DEFINE INPUT  PARAMETER p-nr-pedcli LIKE ped-venda.nr-pedcli.
DEFINE OUTPUT PARAMETER p-ok AS LOG INIT YES.

DEFINE BUFFER unid-feder FOR mgadm.unid-feder.

DEFINE VARIABLE c-tab-preco AS CHARACTER NO-UNDO.
DEFINE VARIABLE de-ind-finan AS DECIMAL NO-UNDO.
DEFINE VARIABLE de-perc-aceito AS DECIMAL NO-UNDO.

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEFINE VARIABLE de-preco-venda    AS DEC.
DEFINE VARIABLE de-perc-acrescimo AS DEC.
DEFINE VARIABLE i-prazo-medio     AS INT.
DEFINE VARIABLE de-tot-prazo      AS INT.
DEFINE VARIABLE i-ct              AS INT.
DEFINE VARIABLE i-prz             AS INT.

FIND ped-venda WHERE
     ped-venda.nr-pedcli = p-nr-pedcli SHARE-LOCK NO-ERROR.

/* Pedidos PI sempre dever∆o ser aprovados */
IF ped-venda.tp-pedido = 'PI' THEN DO.
   ASSIGN ped-venda.log-ped-bonif-pendente = NO
          ped-venda.cod-sit-preco = 1
          p-ok = NO.
   RETURN.
END.

/* Inicia como se o Preoáo estivesse OK... */
ASSIGN ped-venda.log-ped-bonif-pendente = NO  /* Sem Pendencias */
       ped-venda.cod-sit-preco = 2   /* Aprovado */
       p-ok = YES.

FIND ped-venda-ext WHERE
     ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
     ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
IF AVAIL ped-venda-ext AND
   (ped-venda-ext.tp-pedido = "Exportaá∆o" OR 
    ped-venda-ext.tp-pedido = "Bonificaá∆o") THEN RETURN.

/* Aprovaá∆o Autom†tica */
FIND im-param WHERE
     im-param.cod-param = "USR_APROV_AUTOM" NO-LOCK NO-ERROR.

IF AVAIL im-param AND
   LOOKUP(c-seg-usuario,im-param.val-param) > 0 THEN
   RETURN.

/* Se preco for informado, valida para ver o % de diferenáa da Tabela Padr∆o */
IF ped-venda.tp-preco = 1 THEN DO.

   /* TABELA DE PREÄO */
   IF ped-venda.cod-estabel = '1' THEN DO.
      FIND unid-feder WHERE 
           unid-feder.estado = ped-venda.estado NO-LOCK NO-ERROR.
      IF unid-feder.char-2 = 'SUL' OR
         (unid-feder.char-2 = 'SUDESTE' AND unid-feder.estado <> "ES") THEN
         FIND im-param WHERE
              im-param.cod-param = "TABELA_IMA12" NO-LOCK NO-ERROR.
      ELSE
         FIND im-param WHERE
              im-param.cod-param = "TABELA_IMA07" NO-LOCK NO-ERROR.
   END.
   ELSE
      FIND im-param WHERE
           im-param.cod-param = "TABELA_MED" NO-LOCK NO-ERROR.

   IF NOT AVAIL im-param THEN DO.
      MESSAGE "Tabelas n∆o Parametrizadas, verifique o programa esim001.w "
               VIEW-AS ALERT-BOX.

      ASSIGN ped-venda.log-ped-bonif-pendente = YES
             ped-venda.cod-sit-preco = 1.

      ASSIGN p-ok = NO.
      RETURN.
   END.
   ASSIGN c-tab-preco = im-param.val-param.

   /* % ACREECIMO PARA CORTE */
   FIND im-param WHERE
        im-param.cod-param = "PERC_ACRESCIMO_CORTE" NO-LOCK NO-ERROR.
   ASSIGN de-perc-acrescimo = DEC(im-para.val-param).


   /* INDICE DE FINANCIAMENTO */
   FIND cond-pagto WHERE
        cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag NO-LOCK NO-ERROR.

   ASSIGN de-ind-finan = 1.
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

      IF cond-pagto.nr-tab-finan <> 0 AND
         cond-pagto.nr-ind-finan <> 0 THEN DO.
         FIND tab-finan WHERE
              tab-finan.nr-tab-finan = cond-pagto.nr-tab-finan NO-LOCK NO-ERROR.
         IF tab-finan.tab-ind-fin[cond-pagto.nr-ind-finan] <> 0 THEN
            ASSIGN de-ind-finan = tab-finan.tab-ind-fin[cond-pagto.nr-ind-finan].
      END.
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

      /* Calcula Indice de Financiamento das Cond. Pagto. Informada */
      FIND FIRST tab-finan WHERE 
                 tab-finan.dt-ini-val <= TODAY AND 
                 tab-finan.dt-fim-val >= TODAY NO-LOCK NO-ERROR.
         
      DO i-ct = 1 TO EXTENT(tab-finan.tab-dia-fin).
         IF tab-finan.tab-dia-fin[i-ct] >= i-prazo-medio THEN
            LEAVE. 
      END.
      IF i-ct > EXTENT(tab-finan.tab-ind-fin) THEN
         ASSIGN i-ct = EXTENT(tab-finan.tab-ind-fin).

      ASSIGN de-ind-finan = tab-finan.tab-ind-fin[i-ct].
   END.

   IF i-prazo-medio = 0 THEN DO.
      ASSIGN ped-venda.log-ped-bonif-pendente = YES
             ped-venda.cod-sit-preco = 1.

      ASSIGN p-ok = NO.
      RETURN.
   END.

   IF de-ind-finan = 0 THEN
      ASSIGN de-ind-finan = 1.

   /* % MAX ACEITE DO PREÄO */
   IF i-prazo-medio < 90 THEN DO.
      IF ped-venda.cod-estabel = '1' THEN 
         FIND im-param WHERE
              im-param.cod-param = "PERC_DIF_IMA_ATE_90" NO-LOCK NO-ERROR.
      ELSE
         FIND im-param WHERE
              im-param.cod-param = "PERC_DIF_MED_ATE_90" NO-LOCK NO-ERROR.
   END.
   ELSE DO.
       IF ped-venda.cod-estabel = '1' THEN 
          FIND im-param WHERE
               im-param.cod-param = "PERC_DIF_IMA_MAIOR_90" NO-LOCK NO-ERROR.
       ELSE
          FIND im-param WHERE
               im-param.cod-param = "PERC_DIF_MED_MAIOR_90" NO-LOCK NO-ERROR.
   END.

   IF AVAIL im-param THEN DO.
      ASSIGN de-perc-aceito = DEC(im-para.val-param).
   END.
   ELSE DO.
      MESSAGE "Percentual de Diferenáa n∆o Parametrizado, verifique o programa esim001.w "
               VIEW-AS ALERT-BOX.

      ASSIGN ped-venda.log-ped-bonif-pendente = YES
             ped-venda.cod-sit-preco = 1.

      ASSIGN p-ok = NO.
      RETURN.
   END.


   /* VERIFICA PREÄOS */
   ASSIGN p-ok = YES.
   FOR EACH ped-item OF ped-venda NO-LOCK.
       IF ped-item.cod-sit-item = 6 THEN NEXT.  // Cancelado

       FIND ped-item-ext WHERE 
            ped-item-ext.cod-estabel  = ped-venda.cod-estabel AND
            ped-item-ext.nome-abrev   = ped-venda.nome-abrev  AND
            ped-item-ext.nr-pedcli    = ped-item.nr-pedcli    AND
            ped-item-ext.nr-sequencia = ped-item.nr-sequencia 
            NO-LOCK NO-ERROR.

       IF AVAIL ped-item-ext AND
          i-prazo-medio < 90 AND
          ped-item-ext.vl-pre-min <> 0 AND
          ped-item.vl-preori >= ped-item-ext.vl-pre-min THEN
          NEXT.  /* N∆o preicsa validar o Preáo; Est† maior que o Minimo */

       ASSIGN de-perc-aceito = DEC(im-para.val-param).
       FIND item-ext WHERE
            item-ext.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
       IF AVAIL item-ext AND
          item-ext.var-max-preco <> 0 AND 
          ped-venda.cod-estabel = '5' THEN
          ASSIGN de-perc-aceito = item-ext.var-max-preco.

       FIND preco-item WHERE
            preco-item.nr-tabpre = c-tab-preco AND
            preco-item.it-codigo = ped-item.it-codigo AND
            preco-item.cod-refer = ped-item.cod-refer
            NO-LOCK NO-ERROR.
        
       IF NOT AVAIL preco-item THEN DO.
          MESSAGE "N∆o foi Encontrado Tabela de Preáo para o Item " ped-item.it-codigo ped-item.cod-refer
                  VIEW-AS ALERT-BOX.

          ASSIGN ped-venda.log-ped-bonif-pendente = YES
                 ped-venda.cod-sit-preco = 1.

          ASSIGN p-ok = NO.
          RETURN.  /* Se tiver um item com erro, n∆o precisa olhar o restante */
       END.
       ASSIGN de-preco-venda = TRUNCATE(preco-item.preco-venda * de-ind-finan,2).

       FIND liquida-ima WHERE
            liquida-ima.num-id-liquida = ped-item-ext.num-id-liquida
            NO-LOCK NO-ERROR.
       IF AVAIL liquida-ima AND liquida-ima.preco-item > 0 THEN
          ASSIGN de-preco-venda = liquida-ima.preco-item.

       IF AVAIL ped-item-ext AND 
          ped-item-ext.retirar-corte THEN 
          ASSIGN de-preco-venda = de-preco-venda * (1 + de-perc-acrescimo / 100).

       IF i-prazo-medio > 90 OR
          (AVAIL ped-item-ext AND ped-item-ext.retirar-corte) OR
          (100 - (ped-item.vl-preori / de-preco-venda * 100)) > de-perc-aceito THEN DO.
          ASSIGN ped-venda.log-ped-bonif-pendente = YES
                 ped-venda.cod-sit-preco = 1.

          ASSIGN p-ok = NO. 
          RETURN. /* Se tiver um item com erro, n∆o precisa olhar o restante */
       END.
   END.
END.

