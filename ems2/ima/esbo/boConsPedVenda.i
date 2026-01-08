DEFINE TEMP-TABLE {1} NO-UNDO
    FIELD data      AS DATE EXTENT 2    
    FIELD estab     AS CHAR EXTENT 2
    FIELD repres    AS CHAR  EXTENT 2
    FIELD cliente   AS INT  EXTENT 2
    
.



/* DEFINE TEMP-TABLE {2} NO-UNDO
    FIELD estab     AS CHAR                  COLUMN-LABEL "Estab."
    FIELD data      AS DATE                  COLUMN-LABEL "Data"
    FIELD nrPedido  AS INT                   COLUMN-LABEL "Pedido"
    FIELD cliente   AS INT COLUMN-LABEL "Cliente"
    FIELD repres    AS CHAR                  COLUMN-LABEL "Repres".*/
