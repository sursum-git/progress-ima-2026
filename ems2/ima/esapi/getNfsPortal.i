 DEFINE TEMP-TABLE ttNF
FIELD nr-nota-fis           LIKE nota-fiscal.nr-nota-fis
FIELD serie                 LIKE nota-fiscal.serie
FIELD cod-emitente          LIKE nota-fiscal.cod-emitente
FIELD no-ab-reppri          LIKE nota-fiscal.no-ab-reppri
FIELD cod-rep               LIKE nota-fiscal.cod-rep
FIELD nr-pedcli             LIKE nota-fiscal.nr-pedcli
FIELD vl-tot-nota           LIKE nota-fiscal.vl-tot-nota
FIELD val-desconto-total    LIKE nota-fiscal.val-desconto-total
FIELD esp-docto             LIKE nota-fiscal.esp-docto
FIELD cod-estabel           LIKE nota-fiscal.cod-estabel
FIELD cidade                LIKE nota-fiscal.cidade
FIELD estado                LIKE  nota-fiscal.estado
FIELD dt-emis-nota          AS DATE
. 
