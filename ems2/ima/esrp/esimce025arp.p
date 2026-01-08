DEF NEW SHARED TEMP-TABLE tt-ped-venda
    FIELD  cod-estabel  LIKE movadm.saldo-estoq.cod-estabel   
    FIELD  nome-abrev   LIKE movadm.ped-item.nome-abrev  
    FIELD  nr-pedcli    LIKE movadm.ped-item.nr-pedcli   
    FIELD  qt-pedida    LIKE movadm.ped-item.qt-pedida   
    FIELD  qt-log-aloca LIKE movadm.ped-item.qt-log-aloca
    FIELD  qt-alocada   LIKE movadm.ped-item.qt-alocada
    FIELD  nr-embarque  LIKE movadm.pre-fatur.nr-embarque.

DEFINE NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEFINE NEW SHARED VARIABLE var-cod-estabel AS CHARACTER   NO-UNDO.
DEFINE NEW SHARED VARIABLE var-it-codigo   AS CHARACTER   NO-UNDO.
DEFINE NEW SHARED VARIABLE var-cod-refer   AS CHARACTER   NO-UNDO.
DEFINE NEW SHARED VARIABLE var-cod-depos   AS CHARACTER   NO-UNDO.

DEFINE INPUT PARAMETER p-var-cod-estabel AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER p-var-it-codigo   AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER p-var-cod-refer   AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER p-var-cod-depos   AS CHARACTER   NO-UNDO.
      
DEF BUFFER pre-fatur FOR movadm.pre-fatur.

DEFINE VARIABLE c-nome-abrev LIKE emitente.nome-abrev.

ASSIGN var-cod-estabel = p-var-cod-estabel
       var-it-codigo   = p-var-it-codigo  
       var-cod-refer   = p-var-cod-refer  
       var-cod-depos   = p-var-cod-depos.

// Trata Pedidos WEB
FOR EACH peds_web WHERE
         peds_web.ind_sit_ped_web <= 2 OR 
         peds_web.ind_sit_ped_web  = 5 OR 
         peds_web.ind_sit_ped_web >= 8 NO-LOCK,
    EACH itens_ped_web WHERE
         itens_ped_web.ped_web_id = peds_web.ped_web_id AND 
         itens_ped_web.it_codigo = var-it-codigo AND
         itens_ped_web.cod_refer = var-cod-refer NO-LOCK.

    IF peds_web.cod_tipo_pedido = 'PI' THEN NEXT.
    IF peds_web.cod_estab <> p-var-cod-estabel THEN NEXT.

    /*
    IF peds_web.log_novo_cliente AND
       NOT CAN-FIND(FIRST emitente WHERE
                          emitente.cgc = peds_web.cnpj_novo_cliente) THEN NEXT.

    IF peds_web.log_novo_cliente_triang AND
       NOT CAN-FIND(FIRST emitente WHERE
                          emitente.cgc = peds_web.cnpj_novo_cliente_triang) THEN NEXT.
    */
    IF peds_web.log_novo_cliente THEN
       FIND emitente WHERE
            emitente.cgc = peds_web.cnpj_novo_cliente NO-LOCK NO-ERROR.
    ELSE
       FIND emitente WHERE  
   		    emitente.cod-emit = peds_web.cliente_id NO-LOCK NO-ERROR.

       IF AVAIL emitente THEN DO.
          ASSIGN c-nome-abrev = emitente.nome-abrev.

          IF emitente.cod-emit = 0 THEN DO.
             IF peds_web.log_novo_cliente THEN
                ASSIGN c-nome-abrev = 'CLIENTE NOVO'.
             ELSE
                ASSIGN c-nome-abrev = 'NÇO INFORMADO'.
          END.
       END.
       ELSE IF peds_web.log_novo_cliente THEN
          ASSIGN c-nome-abrev = 'CLIENTE NOVO'.
       ELSE
          ASSIGN c-nome-abrev = 'NÇO INFORMADO'.

    FIND tt-ped-venda WHERE 
         tt-ped-venda.cod-estabel  = var-cod-estabel and 
         tt-ped-venda.nome-abrev   = c-nome-abrev and 
         tt-ped-venda.nr-pedcli    = 'WEB-' + STRING(peds_web.ped_web_id) NO-ERROR.

    IF NOT AVAIL tt-ped-venda THEN DO.
       CREATE tt-ped-venda.
       ASSIGN tt-ped-venda.cod-estabel = var-cod-estabel 
              tt-ped-venda.nome-abrev  = c-nome-abrev
              tt-ped-venda.nr-pedcli   = 'WEB-' + STRING(peds_web.ped_web_id).
    END.
    ASSIGN tt-ped-venda.qt-pedida = tt-ped-venda.qt-pedida + itens_ped_web.qt_pedida.
END.


FOR EACH movadm.ped-item WHERE 
         movadm.ped-item.cod-sit-item = 1               AND
         movadm.ped-item.it-codigo    = var-it-codigo   AND 
         movadm.ped-item.cod-refer    = var-cod-refer   NO-LOCK, 
   FIRST movadm.ped-venda OF movadm.ped-item WHERE
         movadm.ped-venda.tp-pedido <> "PI" NO-LOCK.
  
   IF ped-venda.cod-estabel <> p-var-cod-estabel THEN NEXT.

   FIND tt-ped-venda WHERE 
        tt-ped-venda.cod-estabel  = movadm.ped-venda.cod-estabel and 
        tt-ped-venda.nome-abrev   = movadm.ped-item.nome-abrev and 
        tt-ped-venda.nr-pedcli    = movadm.ped-item.nr-pedcli NO-ERROR.

   IF NOT AVAIL tt-ped-venda THEN DO.
      CREATE tt-ped-venda.
      ASSIGN tt-ped-venda.cod-estabel  = movadm.ped-venda.cod-estabel
             tt-ped-venda.nome-abrev   = movadm.ped-item.nome-abrev       
             tt-ped-venda.nr-pedcli    = movadm.ped-item.nr-pedcli        
             tt-ped-venda.qt-pedida    = movadm.ped-item.qt-pedida        
             tt-ped-venda.qt-log-aloca = movadm.ped-item.qt-log-aloca     
             tt-ped-venda.qt-alocada   = movadm.ped-item.qt-alocada.      
   END.
END.

/*
FOR EACH dbaux.ped-item WHERE 
         dbaux.ped-item.cod-sit-item = 1 AND
         dbaux.ped-item.it-codigo    = var-it-codigo   AND 
         dbaux.ped-item.cod-refer    = var-cod-refer   NO-LOCK,
   FIRST dbaux.ped-venda OF dbaux.ped-item WHERE
         dbaux.ped-venda.tp-pedido <> "PI" NO-LOCK.
  
   FIND tt-ped-venda WHERE 
        tt-ped-venda.cod-estabel = dbaux.ped-venda.cod-estabel and 
        tt-ped-venda.nome-abrev  = dbaux.ped-item.nome-abrev and 
        tt-ped-venda.nr-pedcli   = dbaux.ped-item.nr-pedcli NO-ERROR.

    IF NOT AVAIL tt-ped-venda THEN DO.
       CREATE tt-ped-venda.
       ASSIGN tt-ped-venda.cod-estabel  = dbaux.ped-venda.cod-estabel
              tt-ped-venda.nome-abrev   = dbaux.ped-item.nome-abrev       
              tt-ped-venda.nr-pedcli    = dbaux.ped-item.nr-pedcli        
              tt-ped-venda.qt-pedida    = dbaux.ped-item.qt-pedida        
              tt-ped-venda.qt-log-aloca = dbaux.ped-item.qt-log-aloca     
              tt-ped-venda.qt-alocada   = dbaux.ped-item.qt-alocada.      
    END.
END.


DISCONNECT dbaux.
*/

RUN esp/esimce025a.w.
