{esapi/ttRemessaTerc.i}
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttRetorno.
DEFINE TEMP-TABLE ttItemRef LIKE itens_pedido_lisa 
       FIELD qtPlanilha AS DECIMAL.
DEFINE TEMP-TABLE ttPlanLisa NO-UNDO
    FIELD itCodigo      AS CHAR
    FIELD descricao     AS CHAR
    FIELD quantidade    AS DECIMAL
    FIELD nfRetorno     AS CHAR FORMAT 'x(20)'
    FIELD pedidoLisa    AS CHAR
    FIELD nfRemessa     AS CHAR FORMAT 'X(20)'
    FIELD codRefer      AS CHAR
    FIELD nrPedido      AS CHAR FORMAT 'x(20)'
    FIELD qtEncontrada  AS DECIMAL
    FIELD linhas        AS INT
    INDEX ind-pri IS PRIMARY itCodigo codRefer.
{esp/util.i}
DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
INPUT FROM c:\temp\retornos.csv.
ASSIGN iCont = 1.
REPEAT:
    CREATE ttPlanLisa.
    ASSIGN iCont = iCont + 1.
    IMPORT DELIMITER ";" ttPlanLisa NO-ERROR.
    ASSIGN ttPlanLisa.linha = iCont.
END.

FOR EACH ttPlanLisa
    WHERE ttPlanLisa.itCodigo <> '':
    ASSIGN ttPlanLisa.itCodigo = REPLACE(ttPlanLisa.itCodigo,'ima','').
    //DISP tt WITH 1 COL 1 DOWN.
    ASSIGN iCont = iCont + 1.
END.

//DISP iCont.
FIND LAST ttRetorno NO-ERROR.
IF AVAIL ttRetorno THEN
   ASSIGN iCont = ttRetorno.id.
ELSE
   ASSIGN iCont = 0.


FOR EACH ttPlanLisa
    WHERE ttPlanLisa.nfRetorno <> '':
    FIND LAST itens_pedido_lisa NO-LOCK
        WHERE int(itens_pedido_lisa.nf_retorno) = int(ttPlanLisa.nfRetorno)
        AND itens_pedido_lisa.it_codigo = ttPlanLisa.itCodigo
        AND itens_pedido_lisa.cod_refer = ttPlanLisa.codRefer
        AND int(itens_pedido_lisa.nf_origem) = int(ttPlanLisa.nfRemessa)
        NO-ERROR.
    FIND pedidos_lisa NO-LOCK
        WHERE pedidos_lisa.pedido_lisa_id = itens_pedido_lisa.pedido_lisa_id  NO-ERROR.
    CREATE ttRetorno.
    ASSIGN iCont                   = iCont + 1
              ttRetorno.id         = iCont
              ttRetorno.itCodigo   = ttPlanLisa.itCodigo
              ttRetorno.codRefer   = ttPlanLisa.codRefer
              ttRetorno.nrNota     = ttPlanLisa.nfRetorno
              ttRetorno.nfBaixada  = ttPlanLisa.nfRemessa
              ttRetorno.dtEntrada  = IF AVAIL pedidos_lisa THEN pedidos_lisa.dt_expedicao ELSE ?
              ttRetorno.quantidade = ttPlanLisa.quantidade
              ttRetorno.valorUnit  = 0
              ttRetorno.valorTotal = 0
              /*ttRetorno.nfErp      = ''
              ttRetorno.logLancErp = NO*/
              ttRetorno.nrPedido   = IF AVAIL pedidos_lisa THEN STRING(pedidos_lisa.nr_pedido) ELSE ''
              .

END.


/*
{esp/exportarTabelacsv3.i tt " " " " "ttPlanilhaLisa" }
{esp/exportarTabelacsv3.i ttItemRef " " " " "ttItemPedidoLisa" }
  */
