{include/i-epc200.i1} /* Defini‡Æo da temp-table tt-epc */

DEFINE INPUT PARAMETER P-ind-event AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.

DEF NEW GLOBAL SHARED VAR h-bodi317va AS HANDLE.

DEFINE VAR l-nao-reserv AS LOGICAL.
DEFINE VAR de-vl-frete     LIKE nota-fiscal.vl-frete.
DEFINE VAR de-qt-separ     AS DEC.

DEFINE VAR c-desc LIKE  ped-item-res.desc-dentro.
DEFINE VAR c-mesg AS CHAR.      
DEF VAR i-tot-etq AS INT.
DEF VAR c-ncm LIKE item.class-fiscal.

FIND FIRST tt-epc WHERE
           tt-epc.cod-event     = "AfterValidaItemdaNota" AND
           tt-epc.cod-parameter = "Object-Handle" NO-ERROR.
IF AVAIL tt-epc AND VALID-HANDLE(WIDGET-HANDLE(tt-epc.val-parameter)) THEN
   ASSIGN h-bodi317va = WIDGET-HANDLE(tt-epc.val-parameter).

FIND FIRST tt-epc WHERE
           tt-epc.cod-event     = "AfterValidaItemdaNota" AND
           tt-epc.cod-parameter = "Table-Rowid" NO-ERROR.

IF AVAIL tt-epc THEN DO:

   FIND FIRST wt-docto WHERE
        ROWID(wt-docto) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.

   FIND ped-venda WHERE
        ped-venda.cod-estabel = wt-docto.cod-estabel AND  
        ped-venda.nr-pedido = INTEGER(wt-docto.nr-pedcli)
        NO-LOCK NO-ERROR.

   IF AVAIL ped-venda THEN DO.
      FIND natur-oper WHERE
           natur-oper.nat-operacao = wt-docto.nat-operacao NO-LOCK NO-ERROR.

      /*
        FOR EACH relacto-item-uf-aliq.
            DISP relacto-item-uf-aliq.cod-item
                 relacto-item-uf-aliq.val-aliq.
        END.
      */
      /*
      FOR EACH wt-it-docto OF wt-docto WHERE
               wt-it-docto.selecionado = "*" AND
               wt-it-docto.log-2 = NO NO-LOCK.
          FIND item WHERE
               item.it-codigo = wt-it-docto.it-codigo NO-LOCK NO-ERROR.
          IF AVAIL item AND 
             INT(item.codigo-orig) = 1 THEN DO.
             IF natur-oper.aliquota-icm <> 4 THEN DO.
                ASSIGN c-mesg = "Al¡quota de ICM " + STRING(natur-oper.aliquota-icm) +  "% Inv lida para o Pedido " + wt-docto.nr-pedcli + 
                                " Verfique a Natureza de Opera‡Æo...".
    
                RUN _inserterrormanual IN h-bodi317va (INPUT 0, /*numero do erro */
                                                       INPUT "OUTROS", /* tipos */
                                                       INPUT "ERROR",
                                                       INPUT "Al¡quota de ICM Inv lida",
                                                       INPUT c-mesg,
                                                       INPUT "").
             END.
          END.
      END.
      */

      IF ped-venda.mo-codigo <> 0 THEN DO.
         ASSIGN c-mesg = "Moeda do Pedido " + ped-venda.nr-pedcli + 
                         " NÆo permite Faturamento...".
    
         RUN _inserterrormanual IN h-bodi317va (INPUT 0, /*numero do erro */
                                                INPUT "OUTROS", /* tipos */
                                                INPUT "ERROR",
                                                INPUT "Moeda Inv lida",
                                                INPUT c-mesg,
                                                INPUT "").
      END.
   END.

   FIND emitente WHERE
        emitente.cod-emitente = wt-docto.cod-emitente
        NO-LOCK NO-ERROR.

   ASSIGN l-nao-reserv = NO.
   FOR EACH wt-it-docto OF wt-docto WHERE
            wt-it-docto.selecionado = "*" AND
            wt-it-docto.log-2 = NO.

       FIND ped-item-ext WHERE 
            ped-item-ext.cod-estabel  = wt-docto.cod-estabel AND  
            ped-item-ext.nome-abrev   = emitente.nome-abrev AND 
            ped-item-ext.nr-pedcli    = wt-it-docto.nr-pedcli AND 
            ped-item-ext.nr-sequencia = wt-it-docto.nr-sequencia 
            NO-LOCK NO-ERROR.

       /* Esse corte NÆo Precisa  de Reserva */
       IF ped-item-ext.corte-comerc = 'L' THEN NEXT. 
       
       FIND ITEM WHERE
            ITEM.it-codigo = ped-item-ext.it-codigo NO-LOCK NO-ERROR.

       IF ITEM.ge-codigo < 50 OR
          ITEM.ge-codigo > 60 THEN NEXT.

       IF ITEM.fm-cod-com = '88' THEN NEXT.

       FIND ped-item-res WHERE 
            ped-item-res.cod-estabel  = wt-docto.cod-estabel AND
            ped-item-res.nome-abrev   = emitente.nome-abrev AND 
            ped-item-res.nr-pedcli    = wt-it-docto.nr-pedcli AND 
            ped-item-res.it-codigo    = wt-it-docto.it-codigo AND 
            ped-item-res.cod-refer    = wt-it-docto.cod-refer AND
            ped-item-res.nr-sequencia = wt-it-docto.nr-sequencia 
            NO-LOCK NO-ERROR.
       
       IF NOT AVAIL ped-item-res AND VALID-HANDLE(h-bodi317va) THEN DO.
          ASSIGN c-mesg = "Item " + wt-it-docto.it-codigo + 
                          " Refer. " + wt-it-docto.cod-refer + 
                          " sem Reservas. NÆo poder  ser Faturado...".

          RUN _inserterrormanual IN h-bodi317va (INPUT 0, /*numero do erro */
                                                 INPUT "OUTROS", /* tipos */
                                                 INPUT "ERROR",
                                                 INPUT "Item sem Reservas",
                                                 INPUT c-mesg,
                                                 INPUT "").
       END.
       
       /*
       IF ped-item-res.qt-pedida <> wt-it-docto.quantidade[1] THEN DO.
          ASSIGN c-mesg = "Item " + wt-it-docto.it-codigo + " Seq.: " + STRING(wt-it-docto.nr-sequencia) + " est  Inconsistente." +
                          " Qt Faturada " + STRING(wt-it-docto.quantidade[1]) +
                          " <> Qt Reservada " + STRING(ped-item-res.qt-pedida) +
                          " NÆo poder  ser Faturado...". 

          RUN _inserterrormanual IN h-bodi317va (INPUT 0, /*numero do erro */
                                                 INPUT "OUTROS", /* tipos */
                                                 INPUT "ERROR",
                                                 INPUT c-mesg,
                                                 INPUT "Sequencia Inconsistente",
                                                 INPUT "").
       END.
       */

       ASSIGN de-qt-separ = 0.
       FOR EACH ped-item-rom WHERE 
                ped-item-rom.nome-abrev   = emitente.nome-abrev AND 
                ped-item-rom.nr-pedcli    = wt-it-docto.nr-pedcli AND
                ped-item-rom.nr-sequencia = wt-it-docto.nr-sequencia AND
                ped-item-rom.cod-estabel  = wt-docto.cod-estabel NO-LOCK.
           ASSIGN de-qt-separ = de-qt-separ + ped-item-rom.quantidade
                  i-tot-etq = i-tot-etq + 1.
       END.

       /*
       IF wt-it-docto.quantidade[1] <> de-qt-separ THEN DO:
          MESSAGE "Item " wt-it-docto.it-codigo "est  Inconsistente." SKIP
                  "Qt Faturada" wt-it-docto.quantidade[1] "<> Qt Separada" de-qt-separ SKIP
                  "Deseja Continuar ?"
                   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                   UPDATE choice AS LOGICAL.
           IF NOT choice AND VALID-HANDLE(h-bodi317va) THEN DO.
              ASSIGN c-mesg = "Item " + wt-it-docto.it-codigo + 
                              " Inconsistente. NÆo poder  ser Faturado...". 

              RUN _inserterrormanual IN h-bodi317va (INPUT 0, /*numero do erro */
                                                     INPUT "OUTROS", /* tipos */
                                                     INPUT "ERROR",
                                                     INPUT "Item Inconsistente",
                                                     INPUT c-mesg,
                                                     INPUT "").
           END.
       END.
       */


       ASSIGN wt-it-docto.log-2 = YES.
   END.

   FIND ped-venda-ext WHERE
        ped-venda-ext.cod-estabel = wt-docto.cod-estabel AND  
        ped-venda-ext.nr-pedido = INTEGER(wt-docto.nr-pedcli)
        NO-LOCK NO-ERROR.
   IF AVAIL ped-venda-ext AND 
      ped-venda-ext.qt-fardos > 0 THEN            /* Altera o Total de Etiquetas */
      ASSIGN i-tot-etq = ped-venda-ext.qt-fardos. /* pelo Total de Fardos */


   IF wt-docto.nro-proc-entrada <> 0 THEN DO.
      ASSIGN i-tot-etq = 0.
      FOR EACH ob-etiqueta WHERE
               ob-etiqueta.nr-container = wt-docto.nro-proc-entrada NO-LOCK.
          ASSIGN i-tot-etq = i-tot-etq + 1.
      END.
   END.

   FIND wt-nota-embal WHERE
        wt-nota-embal.seq-wt-docto = wt-docto.seq-wt-docto AND
        wt-nota-embal.sigla-emb = 'VL' NO-ERROR.

   IF NOT AVAIL wt-nota-embal THEN DO.
      CREATE wt-nota-embal.
      ASSIGN wt-nota-embal.seq-wt-docto = wt-docto.seq-wt-docto
             wt-nota-embal.sigla-emb   = 'VL'
             wt-nota-embal.desc-vol   = 'VIDE ROMANEIO'.
   END.
   ASSIGN wt-nota-embal.qt-volumes = IF i-tot-etq > 0
                                     THEN i-tot-etq
                                     ELSE wt-nota-embal.qt-volumes.
END.

/*
RUN dibo/upcbodi317va.p (INPUT P-ind-event,
                         INPUT-OUTPUT TABLE tt-epc).
*/

