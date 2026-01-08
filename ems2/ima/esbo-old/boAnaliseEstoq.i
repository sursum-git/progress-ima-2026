DEFINE TEMP-TABLE ttMovto NO-UNDO
    FIELD itCodigo          LIKE saldo-estoq.it-codigo      COLUMN-LABEL "Produto"
    FIELD codRefer          LIKE saldo-estoq.cod-refer      COLUMN-LABEL "Referencia"
    FIELD descItem          LIKE ITEM.desc-item             COLUMN-LABEL "Desc.Produto"
    FIELD un                LIKE ITEM.un                    COLUMN-LABEL "Unidade"
    FIELD especDocto        AS INT                          COLUMN-LABEL "Esp.Docto"
    FIELD desEspecDocto     AS CHAR                         COLUMN-LABEL "Desc.Esp.Docto"
    FIELD nrDocto           AS CHAR                         COLUMN-LABEL "Nr.Docto"
    FIELD serie             AS CHAR                         COLUMN-LABEL "Serie"
    FIELD codEmitente       AS INT                          COLUMN-LABEL "Emitente"
    FIELD nomeEmitente      AS CHAR                         COLUMN-LABEL "Nome Emitente"
    FIELD tipoTrans         LIKE movto-estoq.tipo-trans     COLUMN-LABEL "Tp.Trans."
    FIELD desTipoTrans      AS CHAR                         COLUMN-LABEL "Descri‡Æo Tp.Trans."
    FIELD natOperacao       LIKE movto-estoq.nat-operacao   COLUMN-LABEL "Nat.Operacao"
    FIELD codDepos          AS CHAR                         COLUMN-LABEL "Deposito"
    FIELD codLocaliz        AS CHAR                         COLUMN-LABEL "Localiz."
    FIELD qtMovto           LIKE movto-estoq.quantidade     COLUMN-LABEL "Quantidade"
    FIELD vlMovto           LIKE movto-estoq.valor-mat-m[1] COLUMN-LABEL "Valor"
    FIELD codClassifMovto   AS INT                          COLUMN-LABEL "Cod.Classif.Movto"
    FIELD desClassifMovto   AS CHAR                         COLUMN-LABEL "Desc.Classif.Movto"
    FIELD dtTrans           AS DATE                         COLUMN-LABEL "Dt.Trans."
    FIELD ano               AS INT                          COLUMN-LABEL "Ano"
    FIELD mes               AS INT                          COLUMN-LABEL "Mes"
    FIELD dia               AS INT                          COLUMN-LABEL "Dia"
    FIELD agrup             AS CHAR                         COLUMN-LABEL "Agrup."
    FIELD ordem             AS INT                          COLUMN-LABEL "Ordem"
    FIELD qtSaldoAcum       AS DECIMAL                      COLUMN-LABEL "Qt.Saldo Acumulado" FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD id                AS INTEGER
    INDEX princ IS PRIMARY itCodigo codRefer
    INDEX ind-class codClassifMovto
    INDEX ind-data  itCodigo codRefer dtTrans ordem
    INDEX ind-agrup agrup itCodigo codRefer
    INDEX ind-comp itCodigo codRefer ano mes
    .


DEFINE TEMP-TABLE ttSaldo NO-UNDO
    FIELD itCodigo          LIKE ITEM.it-codigo                      COLUMN-LABEL "Produto"   
    FIELD codRefer          AS CHAR                                  COLUMN-LABEL "Refer."
    FIELD dtInicial         AS DATE                                  COLUMN-LABEL "Dt.Inicial"
    FIELD qtSaldoAnterior   AS DECIMAL FORMAT "->>>,>>>,>>9.99"      COLUMN-LABEL "Qt.Saldo Anterior"
    FIELD qtEntrada         AS DECIMAL FORMAT "->>>,>>>,>>9.99"      COLUMN-LABEL "Qt.Entrada"
    FIELD qtSaida           AS DECIMAL FORMAT "->>>,>>>,>>9.99"      COLUMN-LABEL "Qt.Sa¡da"
    FIELD qtSaldoAtual      AS DECIMAL FORMAT "->>>,>>>,>>9.99"      COLUMN-LABEL "Qt.Saldo Atual"
    INDEX princ IS PRIMARY itCodigo codRefer
    .

DEFINE TEMP-TABLE ttProdRef NO-UNDO
    FIELD itCodigo          AS CHAR
    FIELD descitem          AS CHAR
    FIELD codRefer          AS CHAR
    FIELD qtImportacoes     AS INT
    FIELD qtImportada       AS DECIMAL
    FIELD dtUltImportacao   AS DATE
    FIELD qtVendas          AS INT
    FIELD qtVendida         AS DECIMAL
    FIELD dtUltimaVenda     AS DATE
    FIELD qtDiasSemVenda    AS INT
    FIELD qtSaldoAnterior   AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD qtSaldoAtual      AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD percRedSaldo      AS DECIMAL FORMAT ">>9"
    INDEX princ IS PRIMARY itCodigo codRefer
    INDEX vl qtSaldoAtual DESC percredSaldo DESC
    .


DEFINE TEMP-TABLE ttProd NO-UNDO
    FIELD itCodigo          AS CHAR
    FIELD descitem          AS CHAR
    FIELD qtImportacoes     AS INT
    FIELD qtImportada       AS DECIMAL
    FIELD dtUltImportacao   AS DATE
    FIELD qtVendas          AS INT
    FIELD qtVendida         AS DECIMAL
    FIELD dtUltimaVenda     AS DATE
    FIELD qtDiasSemVenda    AS INT
    FIELD qtSaldoAnterior   AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD qtSaldoAtual      AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD percRedSaldo      AS DECIMAL FORMAT ">>9"
    INDEX princ IS PRIMARY itCodigo 
    INDEX vl qtSaldoAtual DESC percredSaldo DESC
    .


DEFINE TEMP-TABLE ttData NO-UNDO
    FIELD itCodigo          AS CHAR
    FIELD descitem          AS CHAR
    FIELD codRefer          AS CHAR
    FIELD data              AS DATE
    FIELD qtSaldo             AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    INDEX primario IS PRIMARY itCodigo codRefer data
    .

DEFINE TEMP-TABLE ttComp NO-UNDO
    FIELD itCodigo          AS CHAR
    FIELD descitem          AS CHAR
    FIELD codRefer          AS CHAR
    FIELD ano               AS INT
    FIELD mes               AS INT
    FIELD qtSaldo             AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    INDEX primario IS PRIMARY itCodigo codRefer ano mes
    .

