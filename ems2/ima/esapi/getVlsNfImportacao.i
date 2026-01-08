DEF TEMP-TABLE {1}
    FIELD vlPisNota        LIKE nota-fiscal.vl-tot-nota   LABEL "Vl.PIS.NOTA"
    FIELD vlCofinsNota     LIKE nota-fiscal.vl-tot-nota   LABEL "Vl.COFINS.NOTA"
    FIELD vlIINota         LIKE nota-fiscal.vl-tot-nota   LABEL "Vl.II.NOTA"
    FIELD ICMSufreMet      LIKE nota-fiscal.vl-tot-nota   LABEL "Vl.ICMS"
    FIELD vlFOB            LIKE nota-fiscal.vl-tot-nota   LABEL "Vl.FOB"
    FIELD vlDespesas       LIKE nota-fiscal.vl-tot-nota   LABEL "Vl.DESPESAS"
    FIELD vlFrete          LIKE nota-fiscal.vl-tot-nota   LABEL "Vl.FRETE"
    FIELD vlSeguro         LIKE nota-fiscal.vl-tot-nota   LABEL "Vl.SEGURO"
    FIELD di               AS CHAR  LABEL "DI"   FORMAT 'x(20)'
    FIELD pedsImp          AS CHAR  LABEL "PED.IMP" FORMAT 'x(60)'
    FIELD nrContainer      AS INT   LABEL "Nr.CONTAINER"
    .
    
