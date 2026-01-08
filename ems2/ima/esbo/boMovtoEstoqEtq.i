
DEFINE TEMP-TABLE ttEtq  NO-UNDO
    FIELD cod-estabel   LIKE ob-etiqueta.cod-estabel
    FIELD num-etiqueta  LIKE ob-etiqueta.num-etiqueta    
    FIELD nr-container  LIKE ob-etiqueta.nr-container
    FIELD quantidade    LIKE ob-etiqueta.quantidade
    
    .
DEFINE TEMP-TABLE ttContainer  NO-UNDO
    FIELD nr-container  LIKE ob-etiqueta.nr-container
    FIELD quantidade    LIKE ob-etiqueta.quantidade
    INDEX ind-container IS PRIMARY nr-container
    .   
