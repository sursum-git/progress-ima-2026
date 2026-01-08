DEFINE TEMP-TABLE ttNotaItemRef
    FIELD itCodigo  AS CHAR
    FIELD refer     AS CHAR
    FIELD qt        AS DECIMAL
    FIELD nota      AS CHAR
    INDEX primario AS PRIMARY itCodigo refer
    .
DEFINE TEMP-TABLE ttEtqItemRef
    FIELD itCodigo  AS CHAR COLUMN-LABEL "Produto"
    FIELD refer     AS CHAR COLUMN-LABEL "Refer"   
    FIELD etq       AS CHAR COLUMN-LABEL "Num.Etq."
    FIELD nota      AS CHAR COLUMN-LABEL "NF Remessa"
    INDEX primario AS PRIMARY itCodigo refer
    .    
    
    
{esp/util.i}    
    
INPUT FROM VALUE('P:\inventario 2025\remessa\REMESSA_UNIFICADO_CORRETO.csv') .

    REPEAT:
        CREATE ttNotaItemRef.
        IMPORT DELIMITER ";" ttNotaItemRef.
        ASSIGN ttNotaItemRef.ref = IF LENGTH(ttNotaItemRef.ref) < 3 THEN  FILL('0',3 - LENGTH(ttNotaItemRef.ref)) +   ttNotaItemRef.ref ELSE ttNotaItemRef.refer .
    END.

INPUT CLOSE.

    
INPUT FROM VALUE('P:\inventario 2025\etqs_base_conc_lisa_apos_inventario_final.csv') .

    REPEAT:
        CREATE ttEtqItemRef.
        IMPORT DELIMITER ";" ttEtqItemRef.
        ASSIGN ttEtqItemRef.ref = IF LENGTH(ttEtqItemRef.ref) < 3 THEN  FILL('0',3 - LENGTH(ttEtqItemRef.ref)) +   ttEtqItemRef.ref ELSE ttEtqItemRef.refer .
    END.

INPUT CLOSE.

FOR EACH ttEtqItemREf    :
    FIND FIRST ttNotaItemRef 
    WHERE ttNotaItemRef.Itcodigo  = ttEtqItemRef.itCodigo
    AND   ttNotaItemRef.refer     = ttEtqItemRef.refer
    NO-ERROR.
    IF AVAIL ttNotaItemRef THEN
    DO:
       ASSIGN ttEtqItemRef.nota = "00" + ttNotaItemRef.nota. 
    END.
END.                           

{esp/exportarTabelaCsv3.i ttEtqItemREf " " " " "Relac-etq-nf"}
