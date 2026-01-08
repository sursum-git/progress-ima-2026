
DEFINE TEMP-TABLE ttDados
    FIELD codEstab     AS CHAR
    FIELD numPedido    LIKE ordem-compra.num-pedido
    FIELD numOrdem     LIKE ordem-compra.numero-ordem
    FIELD codigo       LIKE ordem-compra.it-codigo
    FIELD descItem     LIKE ITEM.desc-item
    FIELD fornec       LIKE ordem-compra.cod-emitente
    FIELD descFornec   LIKE emitente.nome-abrev
    FIELD situacao     AS CHAR FORMAT "X(20)"
    FIELD dataOrdem    LIKE ordem-compra.dat-ordem
    FIELD dataEntrega  LIKE prazo-compr.data-entrega
    FIELD precoUnit    LIKE ordem-compra.preco-unit
    FIELD quantidade   LIKE prazo-compr.quantidade
    FIELD preco        AS   DEC FORMAT ">>>,>>>,>>9.99"
    FIELD narrativa    LIKE ordem-compra.narrativa.

DEFINE TEMP-TABLE ttPrazo LIKE ttDados
    FIELD parcelas    AS INT
    FIELD dtVenc      AS DATE
    FIELD codDespesa  AS INT
    FIELD descDespesa AS CHAR FORMAT "x(50)".
