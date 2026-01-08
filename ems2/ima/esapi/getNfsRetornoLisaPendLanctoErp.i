DEFINE TEMP-TABLE ttNfPend NO-UNDO
    FIELD codEstabel        AS CHAR  COLUMN-LABEL "Estab."
    FIELD serie             AS CHAR  COLUMN-LABEL "S‚rie"
    FIELD nrNotaFis         AS CHAR  COLUMN-LABEL "Nr.NF" FORMAT 'x(10)'
    FIELD nrPedido          AS CHAR  FORMAT "x(20)" COLUMN-LABEL "Nr.Pedido"
    FIELD dtEmisNota        AS DATE  COLUMN-LABEL "Dt.Emis.NF"  FORMAT "99/99/9999"
    FIELD chave             AS CHAR FORMAT 'x(70)'
    FIELD logRe1001         AS LOGICAL COLUMN-LABEL "Re1001 Gerado" FORMAT "Sim/NÆo"
    FIELD dtTransacao       AS DATE COLUMN-LABEL "DtTransa‡Æo"  FORMAT "99/99/9999"
    FIELD prePedido         AS INT  COLUMN-LABEL "Pr‚-Pedido" FORMAT '999999'
    FIELD retornoLisaId     AS INT
    FIELD descrErro         AS CHAR FORMAT 'x(80)' COLUMN-LABEL "Inconsistˆncia"
    INDEX ind-pri IS PRIMARY codEstabel serie nrNotaFis
    INDEX ind-dt  dtEmisNota .


