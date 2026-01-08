/* Programa: upc-bodi317sd.p
** Objetivo: 
** Autor...: Prodb - Toninho  Mar‡o/2004
*/

{include/i-epc200.i1} /* Defini‡Æo da temp-table tt-epc */

DEFINE INPUT PARAMETER P-ind-event AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAM TABLE FOR tt-epc.

DEF BUFFER b-wt-fat-ser-lote FOR wt-fat-ser-lote.

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

         FIND ped-venda-ext WHERE
              ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND 
              ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.

         IF AVAIL ped-venda-ext AND
            ped-venda-ext.tp-frete = 'Cif Destaque NF' THEN
            ASSIGN wt-docto.vl-frete-inf = ped-venda.val-frete.
      END.

      IF wt-docto.cod-estabel = '505' THEN DO.
         FOR EACH wt-fat-ser-lote OF wt-docto WHERE
                  wt-fat-ser-lote.cod-depos <> 'ITA' SHARE-LOCK.
             FIND b-wt-fat-ser-lote OF wt-docto WHERE
                  b-wt-fat-ser-lote.cod-depos = 'ITA' AND
                  b-wt-fat-ser-lote.lote = wt-fat-ser-lote.lote AND
                  b-wt-fat-ser-lote.it-codigo = wt-fat-ser-lote.it-codigo AND
                  b-wt-fat-ser-lote.cod-refer = wt-fat-ser-lote.cod-refer
                  SHARE-LOCK NO-ERROR.
             IF AVAIL b-wt-fat-ser-lote THEN DO.
                ASSIGN b-wt-fat-ser-lote.quantidade[1] = b-wt-fat-ser-lote.quantidade[1] + 
                                                         wt-fat-ser-lote.quantidade[1].
                DELETE wt-fat-ser-lote.
             END.   
             ELSE
                ASSIGN wt-fat-ser-lote.cod-depos = 'ITA'.
         END.
      END.
   END.
END.
