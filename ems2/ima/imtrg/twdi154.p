/*****************************************************************************
**
**     Programa: TWDI154.P
**     Data....: Outubro DE 2002
**     Objetivo: Grava o item na tabela de aloca‡äes.
**
*****************************************************************************/

DEFINE PARAMETER BUFFER b-ped-item-new FOR ped-item.
DEFINE PARAMETER BUFFER b-ped-item-old FOR ped-item. 

{include/i-prgvrs.i TWDI154 2.04.00.001}

DEF TEMP-TABLE tt-saldo-estoq
    FIELD  cod-estabel   LIKE movadm.saldo-estoq.cod-estabel 
    FIELD  empresa       LIKE mgadm.empresa.nome
    FIELD  it-codigo     LIKE movadm.saldo-estoq.it-codigo
    FIELD  cod-depos     LIKE movadm.saldo-estoq.cod-depos
    FIELD  cod-refer     LIKE movadm.saldo-estoq.cod-refer 
    FIELD  qt-disponivel LIKE movadm.saldo-estoq.qtidade-atu
    FIELD  qt-aloc-ped   LIKE movadm.saldo-estoq.qt-aloc-ped 
    FIELD  qt-alocada    LIKE movadm.saldo-estoq.qt-alocada 
    FIELD  qtidade-atu   LIKE movadm.saldo-estoq.qtidade-atu
    FIELD  qt-aloc-prod  LIKE movadm.saldo-estoq.qt-aloc-prod
    FIELD  qt-aloc-pi    LIKE movadm.saldo-estoq.qtidade-atu.

DEF BUFFER b-ped-venda FOR ped-venda.
DEF BUFFER b-ped-venda-ext FOR ped-venda-ext.
DEF BUFFER b-peds_web FOR peds_web.    
DEF BUFFER b-itens_ped_web FOR itens_ped_web.
DEF BUFFER b-pp-it-container FOR pp-it-container.    

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR c-msg-erro-di154 AS CHAR NO-UNDO.

DEF VAR de-qtidade-atu     LIKE saldo-estoq.qtidade-atu.
DEF VAR de-qt-aloc-web AS DECIMAL.    
DEF VAR l-ok AS LOG.

DEF VAR i-ct AS INT.
DEF VAR l-verifica AS LOGICAL.

DEFINE VARIABLE h-registro-old  AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-registro-new  AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-campo-old     AS HANDLE     NO-UNDO.
DEFINE VARIABLE h-campo-new     AS HANDLE     NO-UNDO.
DEFINE VARIABLE c-campos        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i-cont          AS INTEGER    NO-UNDO.
DEF VAR c-ocorrencia AS CHAR    FORMAT "x(500)".
DEF VAR c-arquivo AS CHAR.
ASSIGN c-arquivo = "P:\Log_PedVenda\ocorrencias-ped-item-" + STRING(DAY(TODAY),"99") + STRING(MONTH(TODAY),"99") + ".txt".

IF NOT NEW b-ped-item-new THEN DO.
   BUFFER-COMPARE b-ped-item-new TO b-ped-item-old SAVE RESULT IN c-campos NO-ERROR.
   IF c-campos <> "" THEN DO:
      ASSIGN h-registro-new = BUFFER b-ped-item-new:HANDLE
             h-registro-old = BUFFER b-ped-item-old:HANDLE.
       
      OUTPUT TO VALUE(c-arquivo) APPEND.
      DO i-cont = 1 TO NUM-ENTRIES(c-campos).
         ASSIGN h-campo-old = h-registro-old:BUFFER-FIELD(ENTRY(i-cont,c-campos))
                h-campo-new = h-registro-new:BUFFER-FIELD(ENTRY(i-cont,c-campos)).

         ASSIGN c-ocorrencia = "Alterado o Campo: " + ENTRY(i-cont,c-campos) + " " + 
                               "De: " + TRIM(h-campo-old:STRING-VALUE) + " "  +
                               "Para: " + TRIM(h-campo-new:STRING-VALUE).

         PUT UNFORMATTED 
             b-ped-item-new.nr-pedcli " " 
             b-ped-item-new.nome-abrev " " 
             b-ped-item-new.nr-sequencia " " 
             c-seg-usuario " " 
             STRING(TODAY,"99/99/9999") " " 
             STRING(TIME,"HH:MM:SS") " " 
             c-ocorrencia
             SKIP.
      END.
      OUTPUT CLOSE.
   END.
END.

ASSIGN l-verifica = YES.
DO i-ct = 1 TO 10.
   IF PROGRAM-NAME(i-ct) = ? THEN LEAVE.

   IF PROGRAM-NAME(i-ct) MATCHES "*essp0154*" THEN DO.
      ASSIGN l-verifica = NO.
      LEAVE.
   END.
END.

IF NOT l-verifica THEN RETURN.

/* Se ‚ um novo Pedido ou 
   Se Alterou a Condi‡Æo de Pagamento, Volta Pedido para Aprova‡Æo de Pre‡o */
IF NEW b-ped-item-new OR
   b-ped-item-new.vl-preuni <> b-ped-item-old.vl-preuni THEN DO.

   IF b-ped-item-new.tp-preco = 1 THEN
      RUN esapi/valida-preco.p (INPUT b-ped-item-new.nr-pedcli,
                                OUTPUT l-ok).
END.

// Se for novo item, Cancelou a Qude ou Alteraou a Qtde
IF NEW b-ped-item-new OR               
   b-ped-item-new.cod-sit-item = 6 OR 
   b-ped-item-new.qt-pedida <> b-ped-item-old.qt-pedida THEN DO.

   FIND im-param WHERE
        im-param.cod-param = 'USR_NAO_VERIFICA_SALDO' NO-LOCK NO-ERROR.

   FIND b-ped-venda OF b-ped-item-new NO-LOCK NO-ERROR.
   FIND b-ped-venda-ext WHERE
        b-ped-venda-ext.cod-estabel = b-ped-venda.cod-estabel AND
        b-ped-venda-ext.nr-pedido = b-ped-venda.nr-pedido
        NO-LOCK NO-ERROR.
    
   IF b-ped-venda-ext.nr-container <> 0 THEN DO.
      FIND b-pp-it-container WHERE 
           b-pp-it-container.nr-container = b-ped-venda-ext.nr-container AND
           b-pp-it-container.it-comprado  = b-ped-item-new.it-codigo AND
           b-pp-it-container.ref-comprada = b-ped-item-new.cod-refer
           SHARE-LOCK NO-ERROR.
    
      IF AVAIL b-pp-it-container THEN DO.
         ASSIGN de-qt-aloc-web = 0.
         FOR EACH b-peds_web WHERE   // situacao: 1,2,5,8,9
                  b-peds_web.ind_sit_ped_web <= 2 OR 
                  b-peds_web.ind_sit_ped_web = 5 OR
                  b-peds_web.ind_sit_ped_web >= 8 NO-LOCK,
             EACH b-itens_ped_web WHERE
                  b-itens_ped_web.ped_web_id = b-peds_web.ped_web_id AND 
                  b-itens_ped_web.it_codigo = b-pp-it-container.it-codigo AND
                  b-itens_ped_web.cod_refer = b-pp-it-container.ref-comprada NO-LOCK.
    
             IF b-peds_web.nr_container <> b-pp-it-container.nr-container THEN NEXT.

             IF b-peds_web.cod_tipo_pedido <> 'PI' THEN NEXT.
             IF b-peds_web.ped_web_id = b-ped-venda-ext.ped_web_id THEN NEXT.
    
             ASSIGN de-qt-aloc-web = de-qt-aloc-web + b-itens_ped_web.qt_pedida. 
         END.
    
         IF NEW b-ped-item-new THEN DO.
            IF b-pp-it-container.qt-vendida + b-ped-item-new.qt-pedida + de-qt-aloc-web > (b-pp-it-container.qt-pedida * b-pp-it-container.perc-dsp-venda / 100) THEN DO.
               ASSIGN c-msg-erro-di154 = 'Item ' + b-ped-item-new.it-codigo + ' Referˆncia ' + b-ped-item-new.cod-refer + ' Sem saldo Dispon¡vel no Container...' + CHR(10) +
                                         'Qtde Comprada: ' + STRING(b-pp-it-container.qt-pedida) + CHR(10) + 
                                         'Qtde Disponivel Venda:' + STRING( (b-pp-it-container.qt-pedida * b-pp-it-container.perc-dsp-venda / 100) ) + CHR(10) + CHR(10) +                             
                                         'Qtde Vendida Acumulada: ' + STRING(b-pp-it-container.qt-vendida + de-qt-aloc-web) + CHR(10) + 
                                         'Nova Qtde do Pedido: ' + STRING(b-ped-item-new.qt-pedida) + CHR(10) + CHR(10) +                             
                                         '(Controle CPD: imtrg/twdi154.p)'.
    

               IF NOT AVAIL im-param or
                  (AVAIL im-param AND LOOKUP(c-seg-usuario,im-param.val-param) > 0 ) THEN DO.
                  IF b-ped-venda-ext.origem <> 5 THEN
                     MESSAGE c-msg-erro-di154
                          VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
                  //RETURN ERROR.
               END.
            END.
            ASSIGN b-pp-it-container.qt-vendida = b-pp-it-container.qt-vendida + b-ped-item-new.qt-pedida. 
         END.
         ELSE DO.
            IF b-ped-item-old.cod-sit-item <> 6 AND
               b-ped-item-new.cod-sit-item = 6 THEN /* Cancelou o Item*/
               ASSIGN b-pp-it-container.qt-vendida = b-pp-it-container.qt-vendida - b-ped-item-new.qt-pedida. 
    
            IF b-ped-item-old.qt-pedida <> b-ped-item-new.qt-pedida THEN DO. /* Alterou a Quantidade */
               IF b-pp-it-container.qt-vendida + de-qt-aloc-web - b-ped-item-old.qt-pedida + b-ped-item-new.qt-pedida > (b-pp-it-container.qt-pedida * b-pp-it-container.perc-dsp-venda / 100) THEN DO.
                  ASSIGN c-msg-erro-di154 = 'Item ' + b-ped-item-new.it-codigo + ' Referˆncia ' + b-ped-item-new.cod-refer + ' Sem saldo dispon¡vel no Container...' + CHR(10) + 
                                            'Qtde Comprada: ' + STRING(b-pp-it-container.qt-pedida) + CHR(10) + 
                                            'Qtde Disponivel Venda:' + STRING( (b-pp-it-container.qt-pedida * b-pp-it-container.perc-dsp-venda / 100) ) + CHR(10) + 
                                            'Qtde Vendida Acumulada: ' + STRING(b-pp-it-container.qt-vendida + de-qt-aloc-web) + CHR(10) + 
                                            'Qtde Anterior do Pedido: ' + STRING(b-ped-item-old.qt-pedida) + CHR(10) + 
                                            'Nova Qtde do Pedido: ' + STRING(b-ped-item-new.qt-pedida) + CHR(10) + CHR(10) + 
                                            '(Controle CPD: imtrg/twdi154.p)'.
    
                  IF NOT AVAIL im-param or
                     (AVAIL im-param AND LOOKUP(c-seg-usuario,im-param.val-param) > 0 ) THEN DO.
                     IF b-ped-venda-ext.origem <> 5 THEN
                        MESSAGE c-msg-erro-di154
                            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
                     //RETURN ERROR.
                  END.
               END.
                
               ASSIGN b-pp-it-container.qt-vendida = b-pp-it-container.qt-vendida - b-ped-item-old.qt-pedida + b-ped-item-new.qt-pedida. 
            END.
         END.
      END.
   END.
   ELSE DO.
      ASSIGN de-qt-aloc-web = 0.
      FOR EACH b-peds_web WHERE
               b-peds_web.ind_sit_ped_web <= 2 OR 
               b-peds_web.ind_sit_ped_web = 5 NO-LOCK,
          EACH b-itens_ped_web WHERE
               b-itens_ped_web.ped_web_id = b-peds_web.ped_web_id AND 
               b-itens_ped_web.it_codigo  = b-ped-item-new.it-codigo AND
               b-itens_ped_web.cod_refer  = b-ped-item-new.cod-refer NO-LOCK.
    
          IF b-peds_web.cod_tipo_pedido <> 'PE' THEN NEXT.
          IF b-peds_web.ped_web_id = b-ped-venda-ext.ped_web_id THEN
             ASSIGN de-qt-aloc-web = de-qt-aloc-web + b-itens_ped_web.qt_pedida. 
      END.
    
      RUN esrp/esimce025rp.p (INPUT b-ped-item-new.it-codigo,
                              OUTPUT TABLE tt-saldo-estoq).
    
      FOR EACH tt-saldo-estoq WHERE 
               tt-saldo-estoq.cod-refer = b-ped-item-new.cod-refer NO-LOCK.
          ASSIGN de-qtidade-atu = de-qtidade-atu + tt-saldo-estoq.qt-disponivel.
      END.
      ASSIGN de-qtidade-atu = de-qtidade-atu + b-ped-item-new.qt-pedida.
    
        
      // Soma a Qtde do Item em questÆo, pois o imce025 considera essa qtde como Alocada, uma vez que a ped-item j  est  criada...
      IF NEW b-ped-item-new THEN DO.
         IF b-ped-item-new.qt-pedida > de-qtidade-atu + de-qt-aloc-web THEN DO.
            ASSIGN c-msg-erro-di154 = 'Item ' + b-ped-item-new.it-codigo + ' Referˆncia ' + b-ped-item-new.cod-refer + ' Sem saldo Dispon¡vel em Estoque...' + CHR(10) +
                                      'Qtde Dispon¡vel: ' + STRING(de-qtidade-atu) + CHR(10) + 
                                      'Qtde Alocada WEB: ' + STRING(de-qt-aloc-web) + CHR(10) + 
                                      'Nova Qtde do Pedido: ' + STRING(b-ped-item-new.qt-pedida) + CHR(10) + CHR(10) +
                                      '(Controle CPD: imtrg/twdi154.p)'.
        
            IF NOT AVAIL im-param or
               (AVAIL im-param AND LOOKUP(c-seg-usuario,im-param.val-param) > 0 ) THEN DO.
               IF b-ped-venda-ext.origem <> 5 THEN
                  MESSAGE c-msg-erro-di154
                       VIEW-AS ALERT-BOX INFO BUTTONS OK.
            
               //RETURN ERROR.
            END.
         END.
      END.
      ELSE DO.
         IF b-ped-item-old.qt-pedida <> b-ped-item-new.qt-pedida THEN DO. /* Alterou a Quantidade */
            IF b-ped-item-new.qt-pedida > de-qtidade-atu + de-qt-aloc-web + b-ped-item-old.qt-pedida THEN DO.
               ASSIGN c-msg-erro-di154 = 'Item ' + b-ped-item-new.it-codigo + ' Referˆncia ' + b-ped-item-new.cod-refer + ' Sem saldo Dispon¡vel em Estoque...' + CHR(10) +
                                         'Qtde Dispon¡vel: ' + STRING(de-qtidade-atu) + CHR(10) + 
                                         'Qtde Alocada WEB: ' + STRING(de-qt-aloc-web) + CHR(10) + 
                                         'Qtde Anterior do Pedido: ' + STRING(b-ped-item-old.qt-pedida) + CHR(10) + 
                                         'Nova Qtde do Pedido: ' + STRING(b-ped-item-new.qt-pedida) + CHR(10) + CHR(10) + 
                                         '(Controle CPD: imtrg/twdi154.p)'.
    
               IF NOT AVAIL im-param or
                  (AVAIL im-param AND LOOKUP(c-seg-usuario,im-param.val-param) > 0 ) THEN DO.
                  IF b-ped-venda-ext.origem <> 5 THEN
                     MESSAGE c-msg-erro-di154
                         VIEW-AS ALERT-BOX INFO BUTTONS OK.
        
                  //RETURN ERROR.
               END.
            END.
         END.
      END.
   END.
END.
