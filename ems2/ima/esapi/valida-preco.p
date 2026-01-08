/*************************************************************************
historico de alteraá∆o:
rn-tc01 - 29/05/2025 - Thiago Cassimiro - Solicitado por Ana Fl†via
objetivo: todos os pedidos da tabela safira devem cair para aprovaá∆o
***************************************************************************/

DEFINE INPUT  PARAMETER p-nr-pedcli LIKE ped-venda.nr-pedcli.
DEFINE OUTPUT PARAMETER p-ok        AS   LOG INIT YES.
 
DEFINE BUFFER unid-feder FOR mgadm.unid-feder.

{esp/espd4000.i}

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEFINE VARIABLE de-perc-aceito    AS DECIMAL NO-UNDO.
DEFINE VARIABLE de-preco-venda    AS DECIMAL.
DEFINE VARIABLE de-vlReal         AS DECIMAL.
DEFINE VARIABLE de-vlDolar        AS DECIMAL.
DEFINE VARIABLE de-perc-acrescimo AS DECIMAL.
DEFINE VARIABLE i-prazo-medio     AS INTEGER.
DEFINE VARIABLE de-tot-prazo      AS INTEGER.
DEFINE VARIABLE i-ct              AS INTEGER.
DEFINE VARIABLE i-prz             AS INTEGER.
DEFINE VARIABLE hBoCondPagtoPed   AS HANDLE NO-UNDO.

RUN esbo/boCondPagtoPed.p PERSISTENT SET hboCondPagtoPed.

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

/* Pedidos de Campanha X (rubi-X) sempre dever∆o ser aprovados */
IF AVAIL ped-venda-ext AND ped-venda-ext.l_tab_x = YES THEN DO.
    ASSIGN ped-venda.log-ped-bonif-pendente = NO
           ped-venda.cod-sit-preco = 1
           p-ok = NO.
    RETURN.
END.

// Todos os pedidos da tabela Safira devem ser aprovados.
IF ped-venda-ext.tb_preco_id = 3 THEN
DO:
    ASSIGN ped-venda.cod-sit-preco = 1.
END.


/* Aprovaá∆o Autom†tica */
FIND im-param WHERE
     im-param.cod-param = "USR_APROV_AUTOM" NO-LOCK NO-ERROR.

IF AVAIL im-param AND
   LOOKUP(c-seg-usuario,im-param.val-param) > 0 THEN
   RETURN.

/* Se preco for informado, valida para ver o % de diferenáa da Tabela Padr∆o */
IF ped-venda.tp-preco = 1 THEN DO.
   /* % ACREECIMO PARA CORTE */
   FIND im-param WHERE
        im-param.cod-param = "PERC_ACRESCIMO_CORTE" NO-LOCK NO-ERROR.
   ASSIGN de-perc-acrescimo = DEC(im-para.val-param).

   /* CALCULA PRAZO MêDIO */
   ASSIGN i-prazo-medio = 0.
   RUN pi-prazo-medio.

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

   IF NOT AVAIL im-param THEN DO.
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

       ASSIGN de-perc-aceito = DEC(im-param.val-param).
       FIND item-ext WHERE
            item-ext.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
       IF AVAIL item-ext AND
          item-ext.var-max-preco <> 0 AND 
          substr(ped-venda.cod-estabel,1,1) = '5' THEN
          ASSIGN de-perc-aceito = item-ext.var-max-preco.

       // Mostrar Preáo autorizado para Venda
       RUN pi-busca-preco (INPUT  ped-item.it-codigo,
                           INPUT  ped-item.cod-refer,
                           INPUT  "OUTLET", // Campanha
                           OUTPUT de-vlReal,  
                           OUTPUT de-vlDolar).
       IF de-vlReal = 0 AND de-vlDolar = 0 THEN 
          RUN pi-busca-preco (INPUT  ped-item.it-codigo,
                              INPUT  ped-item.cod-refer,
                              INPUT  "", // Campanha
                              OUTPUT de-vlReal,  
                              OUTPUT de-vlDolar).


       IF de-vlReal = 0 AND de-vlDolar = 0 THEN DO.
          ASSIGN ped-venda.log-ped-bonif-pendente = YES
                 ped-venda.cod-sit-preco = 1.

          ASSIGN p-ok = NO.
          RETURN.  /* Se tiver um item com erro, n∆o precisa olhar o restante */
       END.

       IF ped-venda.mo-codigo = 0 THEN
          ASSIGN de-preco-venda = de-vlReal.
       ELSE
          ASSIGN de-preco-venda = de-vlDolar.

       IF AVAIL ped-item-ext AND 
          ped-item-ext.retirar-corte THEN 
          ASSIGN de-preco-venda = de-preco-venda * (1 + de-perc-acrescimo / 100).

       ASSIGN de-preco-venda = ROUND(de-preco-venda,2).

       IF i-prazo-medio > 90 THEN DO.
          ASSIGN ped-venda.log-ped-bonif-pendente = YES
                 ped-venda.cod-sit-preco = 1.

          ASSIGN p-ok = NO. 
          RETURN. /* Se tiver um item com erro, n∆o precisa olhar o restante */
       END.

       IF (AVAIL ped-item-ext AND ped-item-ext.retirar-corte) THEN DO.
           ASSIGN ped-venda.log-ped-bonif-pendente = YES
                  ped-venda.cod-sit-preco = 1.

           ASSIGN p-ok = NO. 
           RETURN. /* Se tiver um item com erro, n∆o precisa olhar o restante */
       END.

       IF (100 - (ped-item.vl-preori / de-preco-venda * 100)) > de-perc-aceito THEN DO.
          ASSIGN ped-venda.log-ped-bonif-pendente = YES
                 ped-venda.cod-sit-preco = 1.

          ASSIGN p-ok = NO. 
          RETURN. /* Se tiver um item com erro, n∆o precisa olhar o restante */
       END.
       //rn-tc01 - regra para tabela SAFIRA
       
        IF ped-venda-ext.tb_preco_id = 2 AND
          CAN-FIND(FIRST  ped-item-ext
                    WHERE ped-venda.nome-abrev = ped-item-ext.nome-abrev
                    AND   ped-venda.nr-pedcli  = ped-item-ext.nr-pedcli
                    AND (ped-item-ext.liquida-ima = YES OR ped-item-ext.num-id-liquida-ima <> ''))           
          THEN DO:
          
          ASSIGN ped-venda.log-ped-bonif-pendente = YES
                 ped-venda.cod-sit-preco = 1.           
       END.
       
       
       
   END.
END.

//------------- Procedure --------------

PROCEDURE pi-busca-preco :
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

    RUN expttMsg IN h-bo(SESSION:TEMP-DIRECTORY + "/boPrecosItemRef_item_" + p-it-codigo + '_ref_' + p-cod-refer + 'nrContainer_' + STRING(ped-venda-ext.nr-container) + "_" +  STRING(TIME) + '.txt' ).

    RUN finalizarBos IN h-bo.
    IF VALID-HANDLE(h-bo) THEN
       DELETE PROCEDURE h-bo.
    
END PROCEDURE.


PROCEDURE pi-prazo-medio.
    FIND cond-pagto WHERE
         cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag
         NO-LOCK NO-ERROR.
    IF AVAIL cond-pagto THEN
       ASSIGN i-prazo-medio = cond-pagto.qtd-dias-prazo-medio.
    ELSE DO.
       FOR EACH cond-ped OF ped-venda NO-LOCK.
           CREATE tt-cond-ped.
           BUFFER-COPY cond-ped TO tt-cond-ped.
       END.
        
       RUN setTipoCalc IN hBoCondPagtoPed (INPUT 3).
       RUN setTTCondPed IN hBoCondPagtoPed (INPUT TABLE tt-cond-ped).
       RUN calcularPrazoMedio IN hBoCondPagtoPed.
       RUN getPrazoMedio IN hBoCondPagtoPed (OUTPUT i-prazo-medio).
    END.
END PROCEDURE.
