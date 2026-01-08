DEFINE TEMP-TABLE ttItemNf NO-UNDO
    FIELD Id              AS INT
    FIELD itemPedidoLisaId AS INT
    FIELD nfOriginal      AS CHAR FORMAT 'x(12)' COLUMN-LABEL "Nf.Original"
    FIELD nfSubstituta    AS CHAR FORMAT 'x(12)' COLUMN-LABEL "Nf.Substituta"
    FIELD codReferSubst   AS CHAR
    FIELD itCodigo        AS CHAR FORMAT 'x(12)' COLUMN-LABEL "Produto"
    FIELD codRefer        AS CHAR COLUMN-LABEL "Refer."
    FIELD pedidoLisaId    AS INT
    FIELD sequencia       AS INT
    FIELD qtNfSubst       AS DECIMAL COLUMN-LABEL "Qt.NF.Subst."
    FIELD qtNfOriginal    AS DECIMAL COLUMN-LABEL "Qt.Nf.Orig." 
    FIELD qtFaturada      AS DECIMAL COLUMN-LABEL "Qt.faturada"
    FIELD qtRetorno       AS DECIMAL COLUMN-LABEL "Qt.Retorno"  
    FIELD retornoLisaId   AS INT
    FIELD nfRetorno       AS CHAR COLUMN-LABEL "Nf.Retorno"
    FIELD codReferRet     AS CHAR COLUMN-LABEL "Refer."
    FIELD qtSaldoDisp     AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" COLUMN-LABEL "Qt.Disp." 
    FIELD qtExcedente     AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99" COLUMN-LABEL "Qt.Excedente" 
    FIELD logCalcSist     AS LOGICAL  COLUMN-LABEL "Calc.Sistema?"
    FIELD codReferOri     AS CHAR  COLUMN-LABEL "Refer.Original"
    INDEX pri IS PRIMARY pedidoLisaId itemPedidoLisaId retornoLisaId itCodigo codRefer
    INDEX ind-item-ped itemPedidoLisaId
    INDEX ind-id id
    INDEX ind-ref-ori   pedidoLisaId itCodigo codReferOri 
    .





