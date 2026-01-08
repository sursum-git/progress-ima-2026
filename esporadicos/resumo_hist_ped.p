DEFINE VARIABLE cListaCampos AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iQt          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSituacao    AS INTEGER     NO-UNDO.
DEFINE VARIABLE i            AS INT         NO-UNDO.
DEFINE VARIABLE nomeAbrev    AS CHARACTER   NO-UNDO FORMAT 'x(12)'.
DEFINE VARIABLE qtItens      AS INTEGER     NO-UNDO.
DEFINE VARIABLE qtCanc       AS INTEGER     NO-UNDO.
DEFINE VARIABLE tipoPed      AS CHARACTER   NO-UNDO.
DEFINE BUFFER bfIma FOR ems2med.ped-venda.
DEFINE BUFFER bfMed FOR emsima.ped-venda.


DEFINE TEMP-TABLE tt
      FIELD codEstab    AS CHAR
      FIELD nrPedido    AS CHAR FORMAT 'x(20)'
      FIELD qts         AS INT EXTENT 20
      FIELD qtItens     AS INT
      FIELD percCriacao AS DEC
      FIELD percALt     AS DEC
      FIELD percCanc    AS DEC
      FIELD repres      AS CHAR FORMAT 'x(12)'
      FIELD valor       AS DEC
      FIELD codSitped   AS INT
      FIELD tipo        AS CHAR
      FIELD qtscont     AS INT EXTENT 20. 

DEFINE TEMP-TABLE ttResumo
    FIELD codEstab   AS CHAR
    FIELD codSitPed  AS INT
    FIELD repres     AS CHAR FORMAT 'x(12)'
    FIELD qtPedido   AS INT
    FIELD qts        AS INT EXTENT 20.

ASSIGN cListaCampos = "Bloqueio Faturamento,Cancelamento ,Cond.Pagto,Dt.Entrega,Eliminacao de Reserva,Eliminar Reserva,Emitir Nota Fiscal,Inclusao de Item,Nao AProvar,Observacao,Preco,Prioridade,Qte.Item,Redespacho,Tipo de Entrega(prazo),Tipo de Frete,Tipo De Pagto,Tipo de Preco,Transportadora,Total Geral".

INPUT FROM u:\hist_pedidodesde052015ate022016.csv.

REPEAT:
    CREATE tt.
    IMPORT DELIMITER ','  tt.
END.
INPUT CLOSE.

FOR EACH tt WHERE tt.codEstab <> '' :
    /*DISP tt WITH 1 COL WIDTH 550.*/
    /*ASSIGN iSituacao = 10
           nomeAbrev = ''.*/

    IF tt.codEstab = '1' THEN DO:
       FIND FIRST emsima.ped-venda
           WHERE ped-venda.nr-pedido = INT(tt.nrPedido) NO-LOCK NO-ERROR.
       IF AVAIL emsima.ped-venda THEN DO:
          /*ASSIGN iSituacao = ped-venda.cod-sit-ped
                 nomeAbrev = ped-venda.no-ab-reppri.*/
          ASSIGN qtItens = 0.
          IF  emsima.ped-venda.cod-sit-ped = 6 THEN
              ASSIGN qtCanc = tt.qts[2] - 1.
          ELSE
             ASSIGN qtCanc = tt.qts[2].
          FOR EACH emsima.ped-item OF ped-venda NO-LOCK.
              ASSIGN qtItens = qtItens + 1.
          END.
          IF emsima.ped-venda.tp-pedido = 'pe' THEN DO:
             FIND FIRST bfIma
              WHERE bfIma.nr-pedido = emsima.ped-venda.nr-pedido
              AND   bfima.tp-pedido = 'pi' NO-LOCK NO-ERROR.
             IF AVAIL bfima THEN
                ASSIGN tipoPed = 'PI'.
             ELSE
                ASSIGN tipoPed = 'PE'.
          END.
          ELSE 
             ASSIGN tipoPEd = emsima.ped-venda.tp-pedido.
             

          ASSIGN tt.qtItens     = qtItens
                 tt.percCriacao = tt.qts[8]  / tt.qtItens
                 tt.percAlt     = tt.qts[13] / tt.qtItens
                 tt.percCanc    = qtcanc     / tt.qtItens
                 tt.repres      = ped-venda.no-ab-reppri
                 tt.valor       = emsima.ped-venda.vl-tot-ped
                 tt.codSitPed   = emsima.ped-venda.cod-sit-ped
                 tt.tipo        = tipoped.
       END.
    END.
    ELSE DO:
        FIND FIRST ems2med.ped-venda
           WHERE ems2med.ped-venda.nr-pedido = INT(tt.nrPedido) NO-LOCK NO-ERROR.
       IF AVAIL ems2med.ped-venda THEN DO:
          /*ASSIGN iSituacao = ped-venda.cod-sit-ped
                 nomeAbrev = ped-venda.no-ab-reppri.*/
          ASSIGN qtItens = 0.
          IF  ems2med.ped-venda.cod-sit-ped = 6 THEN
              ASSIGN qtCanc = tt.qts[2] - 1.
          ELSE
             ASSIGN qtCanc = tt.qts[2].
          FOR EACH ems2med.ped-item OF ems2med.ped-venda NO-LOCK.
              ASSIGN qtItens = qtItens + 1.
          END.
          IF ems2med.ped-venda.tp-pedido = 'pe' THEN DO:
             FIND FIRST bfmed
              WHERE bfmed.nr-pedido = ems2med.ped-venda.nr-pedido
              AND   bfmed.tp-pedido = 'pi' NO-LOCK NO-ERROR.
             IF AVAIL bfmed THEN
                ASSIGN tipoPed = 'PI'.
             ELSE
                ASSIGN tipoPed = 'PE'.
          END.
          ELSE 
             ASSIGN tipoPEd = ems2med.ped-venda.tp-pedido.

          ASSIGN tt.qtItens     = qtItens
                 tt.percCriacao = tt.qts[8]  / tt.qtItens
                 tt.percAlt     = tt.qts[13] / tt.qtItens
                 tt.percCanc    = qtcanc     / tt.qtItens
                 tt.repres      = ems2med.ped-venda.no-ab-reppri
                 tt.valor       = ems2med.ped-venda.vl-tot-ped
                 tt.codSitPed   = ems2med.ped-venda.cod-sit-ped
                  tt.tipo       = ems2med.ped-venda.tp-pedido.
       END.
    END.
    REPEAT i = 1 TO 20:
        IF tt.qts[i] > 0 THEN
           ASSIGN tt.qtscont[i] = 1.
        ELSE 
          ASSIGN tt.qtscont[i]  = 0.
    END. 

    FIND FIRST ttResumo
        WHERE ttResumo.codEstab  = tt.codEstab
        AND   ttResumo.codSitPed = tt.codSitPed 
        AND   ttResumo.repres    = tt.repres NO-ERROR.
    IF NOT AVAIL ttResumo THEN DO:
       CREATE ttResumo.
       ASSIGN ttResumo.codEstab  = tt.codEstab
              ttResumo.codSitPed = tt.codSitPed
              ttResumo.repres    = tt.repres.
    END.
    ASSIGN ttResumo.qtPedido = ttResumo.QtPedido + 1.
    REPEAT i = 1 TO 20:
        IF tt.qts[i] > 0 THEN
           ASSIGN ttResumo.qts[i] = ttResumo.qts[i] + 1.
    END.
    /*DISP ttresumo WITH 1 COL WIDTH 550.*/
END.

OUTPUT TO c:\temp\resumo.txt.
FOR EACH ttResumo:
    EXPORT DELIMITER "|" ttREsumo.

END.
OUTPUT CLOSE.
OUTPUT TO c:\temp\tt.txt.
FOR EACH tt:
    EXPORT DELIMITER "|" tt.

END.

OUTPUT CLOSE.









