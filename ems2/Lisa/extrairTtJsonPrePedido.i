DEFINE TEMP-TABLE ttPedido NO-UNDO
    FIELD situacao          AS CHAR
    FIELD descrSituacao     AS CHAR FORMAT 'x(20)'
    FIELD dtInclusao        AS DATE
    FIELD horaInclusao      AS CHAR
    FIELD obs               AS CHAR FORMAT 'x(100)'
    FIELD obsSeparacao      AS CHAR FORMAT 'x(100)'
    FIELD logNfeEnviada     AS LOGICAL
    FIELD logEnviadoAPi     AS LOGICAL
    FIELD pedidoLisa        AS INT
    FIELD pedidoCliente     AS INT
    FIELD situacaoPed       AS CHAR
    FIELD dtExpedido        AS DATE
    FIELD horaExpedido      AS CHAR
    FIELD dtRomaneio        AS DATE   
    FIELD nfCliente         AS CHAR
    FIELD pesoBruto         AS DECIMAL
    FIELD pesoLiquido       AS DECIMAL
    FIELD qtCaixa           AS INT 
    FIELD codRomaneio       AS CHAR
    FIELD prePedido         AS INT
    FIELD codEstabel        AS CHAR.

DEFINE TEMP-TABLE ttPedItem NO-UNDO
    FIELD itCodigo          AS CHAR COLUMN-LABEL "Produto"
    FIELD descricao         AS CHAR COLUMN-LABEL "Descri‡Æo"
    FIELD codRefer          AS CHAR COLUMN-LABEL "Refer."
    FIELD qtSolicitada      AS DECIMAL  COLUMN-LABEL "Qt.Solicitada"
    FIELD qtSeparada        AS DECIMAL  COLUMN-LABEL "Qt.Separada"
    FIELD qtDiferenca       AS DECIMAL  COLUMN-LABEL "Qt.Diferen‡a"
    FIELD agrup             AS INT .

DEFINE TEMP-TABLE ttPedItemFat  NO-UNDO
       FIELD itCodigo       AS CHAR FORMAT 'x(12)' COLUMN-LABEL "Produto"   
       FIELD codRefer       AS CHAR COLUMN-LABEL "Refer."    
       FIELD nfOrigem       AS CHAR FORMAT 'x(15)'COLUMN-LABEL "NF Origem"
       FIELD serieNfOrigem  AS CHAR COLUMN-LABEL "Serie NF Orig." 
       FIELD itemNfOrigem   AS CHAR FORMAT 'x(12)' COLUMN-LABEL "Item NF Orig." 
       FIELD nfRetorno      AS CHAR FORMAT 'x(12)' COLUMN-LABEL "NF Retorno" 
       FIELD serieNfRetorno AS CHAR COLUMN-LABEL "Serie NF Retorno"
       FIELD qtFaturada     AS DECIMAL COLUMN-LABEL "Qt.Faturada"
       FIELD idNfOrigem     AS CHAR FORMAT 'x(12)' COLUMN-LABEL "ID NF Origem"
       FIELD agrup          AS INT.


DEFINE TEMP-TABLE ttPedItemEtq  NO-UNDO
    FIELD itCodigo      AS CHAR
    FIELD codRefer      AS CHAR
    FIELD rolo          AS CHAR                 COLUMN-LABEL "Rolo"
    FIELD idLisa        AS CHAR FORMAT 'x(10)'  COLUMN-LABEL "Id LISA"
    FIELD nrContainer   AS INTEGER              COLUMN-LABEL "Container"
    FIELD nrSeq         AS CHAR                 COLUMN-LABEL "Nr.Seq."
    FIELD data          AS DATE                 COLUMN-LABEL "Data"
    FIELD hora          AS CHAR                 COLUMN-LABEL "Hora"
    FIELD endereco      AS CHAR FORMAT 'x(20)'  COLUMN-LABEL "Endere‡o"
    FIELD quantidade    AS DECIMAL              COLUMN-LABEL "Quantidade"
    FIELD agrup         AS INT
    INDEX ind-pri IS PRIMARY itCodigo codRefer nrSeq rolo nrContainer 
    INDEX ind-nrseq nrSeq
    INDEX ind-idlisa idLisa
    INDEX ind-dt-hr data hora
    INDEX ind-container nrContainer itCodigo codRefer rolo
    INDEX ind-agrup agrup
    INDEX ind-item-ref itCodigo codRefer
    .
   
