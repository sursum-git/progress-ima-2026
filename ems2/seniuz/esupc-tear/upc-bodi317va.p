{include/i-epc200.i1} /* Definiá∆o da temp-table tt-epc */

DEFINE INPUT PARAMETER P-ind-event AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.

DEF NEW GLOBAL SHARED VAR h-bodi317va AS HANDLE.
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEFINE VAR i-ct         AS INTEGER.
DEFINE VAR i-qtd-parc   AS INTEGER.
DEFINE VAR l-nao-reserv AS LOGICAL.
DEFINE VAR l-erro       AS LOGICAL.
DEFINE VAR de-vl-frete  LIKE nota-fiscal.vl-frete.
DEFINE VAR de-qt-separ  AS DEC.
DEFINE VAR c-desc       LIKE  ped-item-res.desc-dentro.
DEFINE VAR c-mesg       AS CHAR.      
DEFINE VAR i-tot-etq    AS INT.
DEFINE VAR c-ncm        LIKE item.class-fiscal.
DEFINE VAR l-traducao   AS LOGICAL.

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

   FIND ext-ser-estab WHERE
        ext-ser-estab.cod-estabel = wt-docto.cod-estabel AND
        ext-ser-estab.serie = wt-docto.serie
        NO-LOCK NO-ERROR.

   IF NOT AVAIL ext-ser-estab OR 
      (AVAIL ext-ser-estab AND ext-ser-estab.cod-grp-usuar = "") THEN DO.
      ASSIGN c-mesg = "Estabelecimento " + wt-docto.cod-estabel +   
                      "   SÇrie " + wt-docto.serie + CHR(10) +
                      " N∆o foi parametrizado Grupo de Faturamento." + CHR(10) + 
                      " Faturamento n∆o permitido...".

      RUN _inserterrormanual IN h-bodi317va (INPUT 0, /*numero do erro */
                                             INPUT "OUTROS", /* tipos */
                                             INPUT "ERROR",
                                             INPUT c-mesg,
                                             INPUT "Grupo de Fat. n∆o Informado",
                                             INPUT "").
   END.
   
   IF AVAIL ext-ser-estab  THEN DO.
      ASSIGN l-erro = YES.
      DO i-ct = 1 TO NUM-ENTRIES(ext-ser-estab.cod-grp-usuar).
         FIND usuar_grp_usuar WHERE
              usuar_grp_usuar.cod_grp_usuar = ENTRY(i-ct,ext-ser-estab.cod-grp-usuar) AND
              usuar_grp_usuar.cod_usuario = c-seg-usuario NO-LOCK NO-ERROR.  
           
         IF AVAIL usuar_grp_usuar THEN
            ASSIGN l-erro = NO.
      END.
    
      IF l-erro = YES THEN DO.
         ASSIGN c-mesg = "Estabelecimento " + wt-docto.cod-estabel +   
                         "   SÇrie " + wt-docto.serie + CHR(10) +
                         " Usu†rio " + c-seg-usuario + " N∆o tem permiss∆o para Faturar nessa SÇrie/Estabelecimento..." + CHR(10) + 
                         " Faturamento n∆o permitido...".
        
         RUN _inserterrormanual IN h-bodi317va (INPUT 0, /*numero do erro */
                                                INPUT "OUTROS", /* tipos */
                                                INPUT "ERROR",
                                                INPUT "Usu†rio sem Permiss∆o",
                                                INPUT c-mesg,
                                                INPUT "").
         NEXT.
      END.
   END.

   IF wt-docto.cod-estabel <> '1' THEN NEXT.
   IF wt-docto.nat-operacao = "799A" THEN NEXT.

   FIND ped-venda WHERE
        ped-venda.nr-pedido = INTEGER(wt-docto.nr-pedcli)
        NO-LOCK NO-ERROR.

   IF AVAIL ped-venda THEN DO.
      ASSIGN i-qtd-parc = 0.
      IF ped-venda.cod-cond-pag = 0 THEN DO.  /* Especial */ 
          ASSIGN i-qtd-parc = 0.
          FOR EACH cond-ped OF ped-venda NO-LOCK.
              ASSIGN i-qtd-parc = i-qtd-parc + 1.
          END.
      END.
      ELSE DO.
          FIND cond-pagto WHERE
               cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag NO-LOCK NO-ERROR.
          ASSIGN i-qtd-parc = cond-pagto.num-parcelas.
      END.

      IF i-qtd-parc > 9 THEN 
         RUN _inserterrormanual IN h-bodi317va (INPUT 0, /*numero do erro */
                                                INPUT "OUTROS", /* tipos */
                                                INPUT "ERROR",
                                                INPUT "Duplicatas Inv†lidas",
                                                INPUT "Permitido o M†ximo de 9 (Nove) Parcelas",
                                                INPUT "").
   END.

   FIND emitente WHERE
        emitente.cod-emitente = wt-docto.cod-emitente
        NO-LOCK NO-ERROR.

   FIND FIRST ref-item-cli WHERE
              ref-item-cli.cod-emitente = emitente.cod-emitente
              NO-LOCK NO-ERROR.

   ASSIGN l-nao-reserv = NO
          i-tot-etq = 0
          l-traducao = AVAIL ref-item-cli.

   FOR EACH wt-it-docto OF wt-docto WHERE
            wt-it-docto.selecionado = "*" AND
            wt-it-docto.log-2 = NO.

       FIND item WHERE
            item.it-codigo = wt-it-docto.it-codigo NO-LOCK NO-ERROR.

       IF l-traducao THEN DO.
          FIND ref-item-cli WHERE 
               ref-item-cli.cod-refer    = wt-it-docto.cod-refer AND 
               ref-item-cli.it-codigo    = wt-it-docto.it-codigo AND 
               ref-item-cli.cod-emitente = emitente.cod-emitente
               NO-LOCK NO-ERROR.

          IF NOT AVAIL ref-item-cli AND VALID-HANDLE(h-bodi317va) THEN DO.
             ASSIGN c-mesg = "Cliente " + emitente.nome-abrev + " usa traduá∆o de C¢digo TEAR x CLIENTE." +
                             "Item " + wt-it-docto.it-codigo + " Refer. " + wt-it-docto.cod-refer + " n∆o possui Cadastro de Traduá∆o." +
                             "Pedido n∆o ser† Faturado enquanto n∆o for Cadastrado...".

             RUN _inserterrormanual IN h-bodi317va (INPUT 0, /*numero do erro */
                                                    INPUT "OUTROS", /* tipos */
                                                    INPUT "ERROR",
                                                    INPUT c-mesg,
                                                    INPUT "Item Sem Traduá∆o",
                                                    INPUT "").
          END.
       END.

       /* Item Serial, n∆o valida NADA */
       IF item.tipo-con-est = 1 THEN NEXT.  

       IF wt-it-docto.vl-preuni = 0.01 THEN DO.
          ASSIGN c-mesg = "Item " + wt-it-docto.it-codigo + 
                          " Refer. " + wt-it-docto.cod-refer + 
                          " com preáo de 0.01 (um centavo). N∆o poder† ser Faturado...".

          RUN _inserterrormanual IN h-bodi317va (INPUT 0, /*numero do erro */
                                                 INPUT "OUTROS", /* tipos */
                                                 INPUT "ERROR",
                                                 INPUT c-mesg,
                                                 INPUT "Item com Preáo de 0.01",
                                                 INPUT "").
       END.

       FIND ped-item-ext WHERE 
            ped-item-ext.nome-abrev   = emitente.nome-abrev AND 
            ped-item-ext.nr-pedcli    = wt-it-docto.nr-pedcli AND 
            ped-item-ext.it-codigo    = wt-it-docto.it-codigo AND 
            ped-item-ext.cod-refer    = wt-it-docto.cod-refer AND
            ped-item-ext.nr-sequencia = wt-it-docto.nr-sequencia 
            NO-LOCK NO-ERROR.

       /* Esse corte (Peáa Confeccionada) N∆o Precisa de Reserva */
       IF ped-item-ext.corte-comerc = 'L' THEN NEXT. 

       IF NOT ped-item-ext.it-codigo BEGINS '5' THEN NEXT.
       
       /* Retire o Comentario para faturar CA sem Reserva */
       /* IF ped-item-ext.lote BEGINS 'CA' THEN NEXT.  */

       /* Retire o Comentario para faturar SC sem Reserva */
       /*IF ped-item-ext.lote BEGINS 'SC' THEN NEXT.   */

       FIND ped-item-res WHERE 
            ped-item-res.nome-abrev   = emitente.nome-abrev AND 
            ped-item-res.nr-pedcli    = wt-it-docto.nr-pedcli AND 
            ped-item-res.nr-sequencia = wt-it-docto.nr-sequencia 
            NO-LOCK NO-ERROR.
       
       IF NOT AVAIL ped-item-res AND VALID-HANDLE(h-bodi317va) THEN DO.
          ASSIGN c-mesg = "Item " + wt-it-docto.it-codigo + 
                          " Refer. " + wt-it-docto.cod-refer + 
                          " sem Reservas. N∆o poder† ser Faturado...".

          RUN _inserterrormanual IN h-bodi317va (INPUT 0, /*numero do erro */
                                                 INPUT "OUTROS", /* tipos */
                                                 INPUT "ERROR",
                                                 INPUT c-mesg,
                                                 INPUT "Item sem Reservas",
                                                 INPUT "").
       END.
       
       IF ped-item-res.qt-pedida <> wt-it-docto.quantidade[1] THEN DO.
          ASSIGN c-mesg = "Item " + wt-it-docto.it-codigo + " Seq.: " + STRING(wt-it-docto.nr-sequencia) + " est† Inconsistente." +
                          " Qt Faturada " + STRING(wt-it-docto.quantidade[1]) +
                          " <> Qt Reservada " + STRING(ped-item-res.qt-pedida) +
                          " N∆o poder† ser Faturado...". 

          RUN _inserterrormanual IN h-bodi317va (INPUT 0, /*numero do erro */
                                                 INPUT "OUTROS", /* tipos */
                                                 INPUT "ERROR",
                                                 INPUT c-mesg,
                                                 INPUT "Sequencia Inconsistente",
                                                 INPUT "").
       END.

       ASSIGN de-qt-separ = 0.
       FOR EACH ped-item-rom WHERE 
                ped-item-rom.nome-abrev   = emitente.nome-abrev AND 
                ped-item-rom.nr-pedcli    = wt-it-docto.nr-pedcli AND
                ped-item-rom.nr-sequencia = wt-it-docto.nr-sequencia  NO-LOCK.
           ASSIGN de-qt-separ = de-qt-separ + ped-item-rom.quantidade
                  i-tot-etq = i-tot-etq + 1.
       END.

       IF wt-it-docto.quantidade[1] <> de-qt-separ THEN DO:
          ASSIGN c-mesg = "Item " + wt-it-docto.it-codigo + " Seq.: " + STRING(wt-it-docto.nr-sequencia) + "est† Inconsistente." + CHR(10) +
                          " Qt Faturada " + STRING(wt-it-docto.quantidade[1]) +
                          " <> Qt Separada " + STRING(de-qt-separ) + CHR(10) +
                          " N∆o poder† ser Faturado...". 

          RUN _inserterrormanual IN h-bodi317va (INPUT 0, /*numero do erro */
                                                 INPUT "OUTROS", /* tipos */
                                                 INPUT "ERROR",
                                                 INPUT c-mesg,
                                                 INPUT "Romaneio Incorreto",
                                                 INPUT "").
       END.

       FOR EACH ped-item-rom WHERE 
                ped-item-rom.nome-abrev   = wt-docto.nome-abrev AND 
                ped-item-rom.nr-pedcli    = wt-it-docto.nr-pedcli AND
                ped-item-rom.nr-sequencia = wt-it-docto.nr-sequencia NO-LOCK.
           FIND ob-etiqueta WHERE
                ob-etiqueta.cod-estabel = ped-item-rom.cod-estabel AND
                ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                NO-LOCK NO-ERROR.

           /*
           IF wt-it-docto.it-codigo <> ob-etiqueta.it-codigo OR
              wt-it-docto.cod-refer <> ob-etiqueta.cod-refer OR 
              SUBSTR(ped-item-ext.lote,1,2) <> ob-etiqueta.nr-lote OR
              ped-item-ext.corte-comerc <> ob-etiqueta.corte-comerc THEN DO.
              ASSIGN c-mesg = " Seq.: " + STRING(wt-it-docto.nr-sequencia) + "est† Inconsistente." + CHR(10) +
                              "Faturado Item: " + wt-it-docto.it-codigo + " Ref: " + wt-it-docto.cod-refer + " Lote: " + SUBSTR(ped-item-ext.lote,1,2) + CHR(10) +
                              "Etiqueta Item: " + ob-etiqueta.it-codigo + " Ref: " + ob-etiqueta.cod-refer + " Lote: " + ob-etiqueta.nr-lote + CHR(10) +
                              " N∆o poder† ser Faturado...  (UPC-BODI317VA.P)". 

              RUN _inserterrormanual IN h-bodi317va (INPUT 0, /*numero do erro */
                                                     INPUT "OUTROS", /* tipos */
                                                     INPUT "ERROR",
                                                     INPUT c-mesg,
                                                     INPUT "Item/Referencia da Etiqueta Diferente da Nota",
                                                     INPUT "").
           END.
           */
       END.

       /*
       ASSIGN c-desc = IF c-desc = "" AND ped-item-res.desc-dentro <> ""
                       THEN ped-item-res.desc-dentro
                       ELSE c-desc.
    
       FIND embalag WHERE
            embalag.sigla-emb = ped-item-res.sigla-emb NO-ERROR.

       FIND wt-nota-embal WHERE
            wt-nota-embal.seq-wt-docto = wt-docto.seq-wt-docto AND
            wt-nota-embal.sigla-emb = embalag.descricao NO-ERROR.

       IF NOT AVAIL wt-nota-embal THEN DO.
          CREATE wt-nota-embal.
          ASSIGN wt-nota-embal.seq-wt-docto = wt-docto.seq-wt-docto
                 wt-nota-embal.sigla-emb   = embalag.descricao.
       END.
       ASSIGN wt-nota-embal.qt-volumes = IF INT(wt-docto.nr-volum) =  0
                                         THEN wt-nota-embal.qt-volumes + i-tot-etq
                                         ELSE INT(wt-docto.nr-volume).

       */

       
       IF AVAIL item THEN DO:
          /* Verifica Composicao e Classificiacao Fiscal */
          RUN esapi/calcula-ncm.p (INPUT wt-it-docto.it-codigo,
                                   INPUT wt-it-docto.cod-refer,
                                   OUTPUT c-ncm).

          IF int(c-ncm) = 0 OR
             int(c-ncm) = 99999999 THEN DO:
             ASSIGN tt-epc.cod-event = "ERROR"
                    tt-epc.cod-parameter = "EPC-ERROR" 
                    tt-epc.val-parameter = "Item " + wt-it-docto.it-codigo + 
                                           " n∆o tem Classificaá∆o Fiscal. N∆o poder† ser Faturado...".
          END.
          ASSIGN wt-it-docto.class-fiscal = c-ncm.

          /* Verifica Composicao e Composicao */
          FIND item-ext WHERE 
               item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.
    
          FIND composi WHERE 
               composi.cod-composi = item-ext.cod-composi NO-LOCK NO-ERROR.
          IF NOT AVAIL composi AND VALID-HANDLE(h-bodi317va) THEN DO:
             ASSIGN c-mesg = "Item " + wt-it-docto.it-codigo + 
                             " n∆o tem C¢digo de Composiá∆o. N∆o poder† ser Faturado...".
 
             RUN _inserterrormanual IN h-bodi317va (INPUT 0, /*numero do erro */
                                                    INPUT "OUTROS", /* tipos */
                                                    INPUT "ERROR",
                                                    INPUT c-mesg,
                                                    INPUT "Item sem Composiá∆o",
                                                    INPUT "").
          END.
       END.
       ASSIGN wt-it-docto.log-2 = YES.
   END.


   FIND ped-venda-ext WHERE
        ped-venda-ext.nr-pedido = INTEGER(wt-docto.nr-pedcli)
        NO-LOCK NO-ERROR.
   IF AVAIL ped-venda-ext AND 
      ped-venda-ext.qt-fardos > 0 THEN            /* Altera o Total de Etiquetas */
      ASSIGN i-tot-etq = ped-venda-ext.qt-fardos. /* pelo Total de Fardos */

   IF i-tot-etq <> 0 THEN DO.  /* N∆o foi Romaneado */
      FIND wt-nota-embal WHERE
           wt-nota-embal.seq-wt-docto = wt-docto.seq-wt-docto AND
           wt-nota-embal.sigla-emb = 'VL' NO-ERROR.
    
      IF NOT AVAIL wt-nota-embal THEN DO.
         CREATE wt-nota-embal.
         ASSIGN wt-nota-embal.seq-wt-docto = wt-docto.seq-wt-docto
                wt-nota-embal.sigla-emb   = 'VL'
                wt-nota-embal.desc-vol   = 'VIDE ROMANEIO'.
         ASSIGN wt-nota-embal.qt-volumes = i-tot-etq.
      END.
   END.
END.


