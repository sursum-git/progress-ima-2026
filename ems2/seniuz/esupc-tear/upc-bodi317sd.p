/* Programa: upc-bodi317sd.p
** Objetivo: Criar a popular a temp-table fat-ser-lote.
** Autor...: Prodb - Toninho  Maráo/2004
*/

{include/i-epc200.i1} /* Definiá∆o da temp-table tt-epc */

DEFINE INPUT PARAMETER P-ind-event AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.

/* Variaveis de uso especifico da Tear */
DEF VAR c-lote        LIKE ped-item-res.lote.
DEF VAR c-gramatura   AS CHAR FORMAT "x(7)".
def var c-cod-composi LIKE composi.cod-composi.
def var i-rlgp        LIKE item-ext.cod-rlgp.
DEF VAR c-narr-it-nota LIKE wt-it-docto.narrativa.
DEF VAR c-cod-depos    LIKE item-uni-estab.deposito-pad.

FIND FIRST tt-epc WHERE
           tt-epc.cod-event     = "afterCriaWtDocto" AND
           tt-epc.cod-parameter = "Table-Rowid" NO-LOCK NO-ERROR.
IF AVAIL tt-epc THEN DO:
   FIND FIRST wt-docto WHERE
        ROWID(wt-docto) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.

   IF AVAIL wt-docto THEN DO.
      FIND ped-venda WHERE
           ped-venda.nome-abrev = wt-docto.nome-abrev AND
           ped-venda.nr-pedcli =  wt-docto.nr-pedcli
           NO-LOCK NO-ERROR.

      IF AVAIL ped-venda THEN DO.
         FIND ped-venda-ext WHERE
              ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
         IF AVAIL ped-venda-ext AND
            ped-venda-ext.compl-observ <> "" THEN 
            ASSIGN wt-docto.observ-nota = ped-venda-ext.compl-observ.
    
         IF wt-docto.nome-tr-red <> "" AND 
            SUBSTR(wt-docto.nat-oper,1,1) <> '7' THEN DO:
            FIND transporte-ext WHERE
                 transporte-ext.nome-transp = wt-docto.nome-transp NO-LOCK NO-ERROR.
         END.
      END.
      ELSE DO.
          FOR EACH wt-it-docto OF wt-docto.
              ASSIGN wt-it-docto.narrativa = "".
          END.
      END.
   END.
END.

FIND FIRST tt-epc WHERE
           tt-epc.cod-event     = "aftergeraWtItDoctoComItensDoPedido" AND
           tt-epc.cod-parameter = "Table-Rowid" NO-LOCK NO-ERROR.
IF AVAIL tt-epc THEN DO.
   FIND FIRST wt-docto WHERE
        ROWID(wt-docto) = TO-ROWID(tt-epc.val-parameter) NO-ERROR.

   IF AVAIL wt-docto THEN DO.
      FIND ped-venda WHERE
           ped-venda.nome-abrev = wt-docto.nome-abrev AND
           ped-venda.nr-pedcli =  wt-docto.nr-pedcli
           NO-LOCK NO-ERROR.

      IF AVAIL ped-venda THEN DO.
         FOR EACH wt-it-docto OF wt-docto.
        
             FIND emitente WHERE
                  emitente.cod-emitente = wt-docto.cod-emitente
                  NO-LOCK NO-ERROR.

             FIND item-uni-estab WHERE
                  item-uni-estab.cod-estabel = wt-docto.cod-estabel AND
                  item-uni-estab.it-codigo = wt-it-docto.it-codigo
                  NO-LOCK NO-ERROR.

             ASSIGN c-cod-depos = 'EXP'.
             IF AVAIL item-uni-estab THEN
                ASSIGN c-cod-depos = item-uni-estab.deposito-pad.

             FIND ped-item-res WHERE 
                  ped-item-res.nome-abrev   = emitente.nome-abrev AND 
                  ped-item-res.nr-pedcli    = wt-it-docto.nr-pedcli AND 
                  ped-item-res.nr-sequencia = wt-it-docto.nr-seq-ped
                  NO-LOCK NO-ERROR.
        
             IF AVAIL ped-item-res AND
                ped-item-res.lote <> '' THEN DO.
        
                FIND wt-fat-ser-lote WHERE
                     wt-fat-ser-lote.seq-wt-docto = wt-docto.seq-wt-docto AND
                     wt-fat-ser-lote.seq-wt-it-docto = wt-it-docto.seq-wt-it-docto AND
                     wt-fat-ser-lote.it-codigo = wt-it-docto.it-codigo AND
                     wt-fat-ser-lote.cod-depos = c-cod-depos  AND
                     wt-fat-ser-lote.cod-refer = wt-it-docto.cod-refer 
                     NO-LOCK NO-ERROR.
                IF NOT AVAIL wt-fat-ser-lote THEN DO.
                   CREATE wt-fat-ser-lote.
                   ASSIGN wt-fat-ser-lote.seq-wt-docto = wt-docto.seq-wt-docto
                          wt-fat-ser-lote.seq-wt-it-docto = wt-it-docto.seq-wt-it-docto
                          wt-fat-ser-lote.it-codigo = wt-it-docto.it-codigo
                          wt-fat-ser-lote.cod-depos = c-cod-depos
                          wt-fat-ser-lote.lote = ped-item-res.lote
                          wt-fat-ser-lote.cod-refer = wt-it-docto.cod-refer
                          wt-fat-ser-lote.dt-vali-lote = 12.31.9999
                          wt-fat-ser-lote.quantidade = ped-item-res.qt-pedida.
                END.
                ELSE DO.
                   IF AMBIGUOUS wt-fat-ser-lote THEN DO.
                      ASSIGN tt-epc.cod-event = "ERROR"
                             tt-epc.cod-parameter = "EPC-ERROR" 
                             tt-epc.val-parameter = "Existem 2 (duas) ou mais Reservas " +
                                                    "para o mesmo item " +
                                                    wt-it-docto.it-codigo + ' ' + wt-it-docto.cod-refer + ' ' +
                                                    STRING(wt-it-docto.seq-wt-it-docto).
                   END.
                END.
             END.
             ELSE DO.
                 FIND ped-item-ext WHERE 
                      ped-item-ext.nome-abrev   = emitente.nome-abrev AND 
                      ped-item-ext.nr-pedcli    = wt-it-docto.nr-pedcli AND 
                      ped-item-ext.it-codigo    = wt-it-docto.it-codigo AND 
                      ped-item-ext.cod-refer    = wt-it-docto.cod-refer AND
                      ped-item-ext.nr-sequencia = wt-it-docto.nr-sequencia 
                      NO-LOCK NO-ERROR.

                 IF AVAIL ped-item-ext THEN DO.
                    FIND wt-fat-ser-lote WHERE
                         wt-fat-ser-lote.seq-wt-docto = wt-docto.seq-wt-docto AND
                         wt-fat-ser-lote.seq-wt-it-docto = wt-it-docto.seq-wt-it-docto AND
                         wt-fat-ser-lote.it-codigo = wt-it-docto.it-codigo AND
                         wt-fat-ser-lote.cod-depos = c-cod-depos  AND
                         wt-fat-ser-lote.cod-refer = wt-it-docto.cod-refer 
                         NO-LOCK NO-ERROR.
                    IF NOT AVAIL wt-fat-ser-lote THEN DO.
                       CREATE wt-fat-ser-lote.
                       ASSIGN wt-fat-ser-lote.seq-wt-docto = wt-docto.seq-wt-docto
                              wt-fat-ser-lote.seq-wt-it-docto = wt-it-docto.seq-wt-it-docto
                              wt-fat-ser-lote.it-codigo = wt-it-docto.it-codigo
                              wt-fat-ser-lote.cod-depos = c-cod-depos
                              wt-fat-ser-lote.cod-refer = wt-it-docto.cod-refer.
                    END.
                    ASSIGN wt-fat-ser-lote.lote = ped-item-ext.lote
                           wt-fat-ser-lote.dt-vali-lote = 12.31.9999
                           wt-fat-ser-lote.quantidade = wt-it-docto.quantidade[1].
                 END.
             END.
        
             FIND ITEM WHERE
                  ITEM.it-codigo = wt-it-docto.it-codigo NO-LOCK NO-ERROR.
        
             /*--- Complemento do codigo do Item para os controlados por referencia ---*/
             ASSIGN c-lote = ''
                    c-narr-it-nota = ""
                    c-gramatura = "".
        
             FIND wt-fat-ser-lote WHERE
                  wt-fat-ser-lote.seq-wt-docto = wt-docto.seq-wt-docto AND
                  wt-fat-ser-lote.seq-wt-it-docto = wt-it-docto.seq-wt-it-docto AND
                  wt-fat-ser-lote.it-codigo = wt-it-docto.it-codigo AND
                  wt-fat-ser-lote.cod-refer = wt-it-docto.cod-refer 
                  NO-LOCK NO-ERROR.
        
             /*--- Complemento da descricao do Item: gramatura por metro ---*/
             IF ITEM.ge-codigo >= 50 AND ITEM.ge-codigo <= 59 AND
                ITEM.peso-liquido <> 0 AND item.un <> 'kg' THEN
                ASSIGN c-narr-it-nota = IF item.peso-liquido <> 0 
                                        THEN " G/M:" + STRING((ITEM.peso-liquido * 1000),">>9")
                                        ELSE c-narr-it-nota.
        
             /*--- Complemento da descricao do Item: Composicao e Recomendacoes de lavagem ---*/
             FIND item-ext WHERE
                  item-ext.it-codigo = item.it-codigo no-lock no-error.
        
             IF AVAIL item-ext THEN DO.
                ASSIGN c-narr-it-nota = IF item-ext.cod-composi <> "" 
                                        THEN c-narr-it-nota + ' CP:' + item-ext.cod-composi
                                        ELSE c-narr-it-nota.
        
                ASSIGN c-narr-it-nota = IF item-ext.cod-rlgp <> 0
                                        THEN c-narr-it-nota + ' RL:' + STRING(item-ext.cod-rlgp,"9")
                                        ELSE c-narr-it-nota.
             END.
        
             ASSIGN c-narr-it-nota = IF AVAIL wt-fat-ser-lote
                                     THEN c-narr-it-nota + " " + wt-fat-ser-lote.lote
                                     ELSE c-narr-it-nota.
        
             ASSIGN wt-it-docto.narrativa = c-narr-it-nota.
         END.
      END.

      /* Acrescenta a observaá∆o 885 para as Notas de Venda para outros estados
         do Brasil, cujo o Frete seja CIF (tipo 1)
         Solicitaá∆o de inclus∆o feita por Robson em 17/08/2006
         Incluido em 18/08/2006 por Toninho 
      */   
       
      IF wt-docto.nat-operacao BEGINS '6' AND
         (wt-docto.cidade-cif <> '' OR wt-docto.nome-tr-red <> '') AND
         wt-docto.cod-emit <> 8632 THEN DO.
    
         CREATE wt-msg-docto.
         ASSIGN wt-msg-docto.seq-wt-docto = wt-docto.seq-wt-docto
                wt-msg-docto.cod-mensagem = 885.
      END. 
   END.
END.
