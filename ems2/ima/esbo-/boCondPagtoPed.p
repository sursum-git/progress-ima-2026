/*
programa: boCondPagtoPed
objetivo: prover todos os calculos necess rios
aos prazos da cond.pagto do pedido.
Seja uma condi‡Æo cadastrada, seja uma condi‡Æo
especial
*/
{esp/espd4000.i}
DEFINE VARIABLE rRowidPedido    AS ROWID     NO-UNDO.
DEFINE VARIABLE iTipoCalc       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCondPagto      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPrazoMedio     AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDiasCondPagto  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lSemParcelas    AS LOGICAL     NO-UNDO INIT NO.
DEFINE VARIABLE lCondNaoEncotrada AS LOGICAL     NO-UNDO.

PROCEDURE setTipoCalc:
DEFINE INPUT  PARAMETER pTipoCalc AS INTEGER     NO-UNDO.
//1- pedido de venda, 2-cond.pagto 3-cond-ped 4-dias
ASSIGN iTipoCalc = pTipoCalc.

END PROCEDURE.

PROCEDURE setRowidPedido:
DEFINE INPUT  PARAMETER pRowidPedido AS ROWID    NO-UNDO.
ASSIGN rRowidPedido = pRowidPedido.

END PROCEDURE.

PROCEDURE setTTCondPed:
DEFINE INPUT PARAMETER TABLE FOR tt-cond-ped.
END PROCEDURE.

PROCEDURE setCondPagto:
DEFINE INPUT  PARAMETER pCondPagto AS INTEGER     NO-UNDO.
ASSIGN iCondPagto = pCondPagto.
END PROCEDURE.

PROCEDURE limpatTTs:
EMPTY TEMP-TABLE tt-cond-ped.
END PROCEDURE.

PROCEDURE setDiasCondPagto:
    DEFINE INPUT  PARAMETER pDias AS CHARACTER  FORMAT 'x(200)' NO-UNDO.
    ASSIGN cDiasCondPagto = pDias.
END PROCEDURE.

PROCEDURE calcularPrazoMedio:
DEFINE VARIABLE qtDias AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtParc AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCont       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iDia        AS INTEGER     NO-UNDO.
CASE iTipoCalc:
    WHEN 1 THEN DO:
        FIND ped-venda
            WHERE rowid(ped-venda) = rRowidPedido
            NO-LOCK NO-ERROR. 
        IF AVAIL ped-venda THEN DO:
           IF ped-venda.cod-cond-pag <> 0 THEN DO:
              FIND cond-pagto OF ped-venda
                  NO-LOCK NO-ERROR.
              IF AVAIL cond-pagto THEN DO:
                 ASSIGN iPrazoMedio = cond-pagto.qtd-dias-prazo-medio.       
              END.
              ELSE DO:
                  ASSIGN qtParc = 0
                         qtDias = 0.
                  FOR EACH cond-ped OF ped-venda NO-LOCK.
                      ASSIGN qtParc = qtParc + 1.
                       IF tt-cond-ped.data-pagto <> ? THEN
                          ASSIGN qtDias = qtDias + (tt-cond-ped.data-pagto - TODAY).
                       ELSE
                          ASSIGN qtDias = qtDias + cond-ped.nr-dias-venc .
                  END.
                  ASSIGN iPrazoMedio = qtDias / QtParc.
              END.
           END.
        END.
    END.
    WHEN 2 THEN DO:
        FIND cond-pagto WHERE
             cond-pagto.cod-cond-pag = iCondPagto NO-LOCK NO-ERROR.
        IF AVAIL cond-pagto THEN DO:
            MESSAGE 'ENTREI AQUI'
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
           ASSIGN iPrazoMedio = cond-pagto.qtd-dias-prazo-medio.  
        END.
        ELSE DO:
           ASSIGN iPrazoMedio = 0 .  
        END.

    END.
    WHEN 3 THEN DO:
         ASSIGN qtParc = 0
                qtDias = 0.
        FOR EACH tt-cond-ped:
            /*MESSAGE tt-cond-ped.nr-dias-venc
                VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
            ASSIGN qtParc = qtParc + 1.
            IF tt-cond-ped.data-pagto <> ? THEN
              ASSIGN qtDias = qtDias + (tt-cond-ped.data-pagto - TODAY).
            ELSE
              ASSIGN qtDias = qtDias + tt-cond-ped.nr-dias-venc .
        END.
        IF qtParc > 0 THEN
           ASSIGN iPrazoMedio = qtDias / QtParc.
        ELSE DO:
           ASSIGN iPrazoMedio = 0
                  lSemParcelas = YES.

        END.
        
    END.
    WHEN 4 THEN DO:
        ASSIGN qtParc = NUM-ENTRIES(cDiasCondPagto,',').
        REPEAT iCont = 1 TO qtParc.
            ASSIGN iDia = int(ENTRY(iCont,cDiasCondPagto,","))
                   qtDias = qtDias + iDia.
        END.
        ASSIGN iPrazoMedio = qtDias / qtParc.
    END.

END CASE.


END PROCEDURE.



PROCEDURE getPrazoMedio:
DEFINE OUTPUT PARAMETER pPrazoMedio AS INTEGER     NO-UNDO.
ASSIGN pPrazoMedio = iPrazoMedio.
END PROCEDURE.

PROCEDURE getSemParcelas:
DEFINE OUTPUT PARAMETER pSemParcelas AS LOGICAL     NO-UNDO.
ASSIGN lSemParcelas = pSemParcelas.

END PROCEDURE.
