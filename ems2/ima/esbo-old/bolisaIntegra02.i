DEFINE TEMP-TABLE ttRemessas
    FIELD codEstab        AS CHAR COLUMN-LABEL "Estab."
    FIELD serie           AS CHAR COLUMN-LABEL "S‚rie"
    FIELD nf              AS CHAR COLUMN-LABEL "NF"
    FIELD dtCancel        AS DATE COLUMN-LABEL "Dt.Cancelamento"
    FIELD RROWID          AS ROWID
    INDEX index-cancel AS PRIMARY dtcancel .


PROCEDURE criarTTRemessa:

    DEFINE INPUT  PARAMETER pCodEstab AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pSerie    AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pNF       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pDtCancel AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pRowid    AS ROWID       NO-UNDO.

    CREATE ttRemessas.
    ASSIGN ttRemessas.codEstab = pCodEstab
           ttRemessas.serie    = pSerie
           ttRemessas.nf       = pNf
           ttRemessas.dtCancel = pDtCancel
           ttRemessas.rRowid   = pRowid.




END PROCEDURE.


