DEFINE TEMP-TABLE {1} NO-UNDO
    FIELD data      AS DATE EXTENT 2    
    FIELD estab     AS CHAR EXTENT 2
    FIELD repres    AS INT  EXTENT 2
    FIELD uf        AS CHAR  EXTENT 2
    .
    



DEFINE TEMP-TABLE {2} NO-UNDO
FIELD estab     AS CHAR                 COLUMN-LABEL "Estab."
FIELD cidade    LIKE emitente.cidade    COLUMN-LABEL "Cidade"
FIELD uf        LIKE emitente.estado    COLUMN-LABEL "UF"
FIELD mes       AS INT                  COLUMN-LABEL "Mˆs"
FIELD ano       AS INT                  COLUMN-LABEL "Ano"
FIELD data      AS DATE                 COLUMN-LABEL "Data"
FIELD vlFat     AS DECIMAL              COLUMN-LABEL "Vl.Faturamento"
FIELD vlDesc        AS DECIMAL          COLUMN-LABEL "Vl.Desconto"
FIELD vlDevol       AS DECIMAL          COLUMN-LABEL "Vl.Devolu‡Æo"
FIELD vlDevolDesc   AS DECIMAL          COLUMN-LABEL "Vl.Desconto Devolu‡Æo"
INDEX ind-primario IS PRIMARY mes ano cidade uf estab.
