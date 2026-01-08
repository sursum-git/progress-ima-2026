DEFINE TEMP-TABLE ttEtq
    FIELD numEtq        AS INT                  COLUMN-LABEL "Nr.Etq."
    FIELD qtEtq         AS DECIMAL              COLUMN-LABEL "Qt.Etq."
    FIELD idEtqLisa     AS CHAR FORMAT 'X(20)'  COLUMN-LABEL "Id.Etq.Lisa"
    FIELD qtLisa        AS DECIMAL              COLUMN-LABEL "Qt.Lisa"
    FIELD situacao      AS CHAR FORMAT 'x(20)'  COLUMN-LABEL  "Situa‡Æo"
    FIELD observ        AS CHAR FORMAT 'x(100)' COLUMN-LABEL  "Observa‡Æo"
    INDEX pri IS PRIMARY numEtq 
    .
