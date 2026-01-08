DEFINE TEMP-TABLE ttSaldo NO-UNDO
    FIELD itCodigo          AS CHAR       COLUMN-LABEL "Item"
    FIELD codRefer          AS CHAR       COLUMN-LABEL "Ref."
    FIELD qtSaldoPE         AS DECIMAL    COLUMN-LABEL "Qt.Saldo PE"
    FIELD qtDeposFechado    AS DECIMAL    COLUMN-LABEL "Qt.Depos.Fechado"
    FIELD qtSaldoPI         AS DECIMAL    COLUMN-LABEL "Qt.Saldo PI"
    FIELD qtSaldoPISemPerc  AS DECIMAL    COLUMN-LABEL "Qt.Saldo S/%"
    FIELD qtPedidaPE        AS DECIMAL    COLUMN-LABEL "Qt.Pedida PE"
    FIELD qtPedidaPI        AS DECIMAL    COLUMN-LABEL "Qt.Pedida PI"
    FIELD qtCarrinhoPE      AS DECIMAL    COLUMN-LABEL "Qt.Carrinho PE"
    FIELD qtCarrinhoLoginPE AS DECIMAL    COLUMN-LABEL "Qt.Carrinho Login PE"
    FIELD qtCarrinhoPI      AS DECIMAL    COLUMN-LABEL "Qt.Carrinho PI"
    FIELD qtCarrinhoLoginPI AS DECIMAL    COLUMN-LABEL "Qt.Carrinho Login PI"
    /*FIELD qtDispPE          AS DECIMAL
    FIELD qtDispPI          AS DECIMAL*/
    INDEX ind AS PRIMARY UNIQUE itCodigo codRefer.

DEFINE TEMP-TABLE ttPedidosEmAberto NO-UNDO
    FIELD tipoPed       AS CHAR COLUMN-LABEL "Tipo"
    FIELD pedwebId      AS INT  COLUMN-LABEL "Pedido Web."
    FIELD codEstab      AS CHAR COLUMN-LABEL "Estab."
    FIELD nrPedido      LIKE ped-venda.nr-pedido    COLUMN-LABEL "Pedido ERP."
    FIELD itCodigo      LIKE ped-item.it-codigo     COLUMN-LABEL "Item"
    FIELD codRefer      LIKE ped-item.cod-refer     COLUMN-LABEL "Refer."
    FIELD quantidade    AS DECIMAL                  COLUMN-LABEL "Quant."
    FIELD indSitPedweb  AS INT
    INDEX ind-pedido tipoPed itCodigo codRefer pedWebId codEstab nrPedido
    INDEX ind-sit AS PRIMARY  tipoPed itCodigo codRefer indSitPedWeb pedWebId codEstab nrPedido .

DEFINE TEMP-TABLE ttEstabDepos
    FIELD codEstab AS CHAR
    FIELD codDepos AS CHAR.
