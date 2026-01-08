/**************************************************************************
Programa:esapi/getNumEtqPorRolo.p
Autor: tadeu Silva Parreiras 
Objetivo: Retornar o numero da etiqueta da MED 
a partir do item, referencia, container e rolo passados por parametro
Data:08/2024

*****************************************************************************/

DEFINE INPUT  PARAMETER pItem       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pRefer      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pContainer  AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pNumRolo    AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER numEtq      AS INTEGER     NO-UNDO.

    FOR LAST ob-etiqueta FIELDS(it-codigo cod-refer nr-container num-etiqueta) NO-LOCK
    WHERE ob-etiqueta.it-codigo     = pItem
    AND   ob-etiqueta.cod-refer     = pRefer
    AND   ob-etiqueta.nr-container  = pContainer
    AND   ob-etiqueta.num-rolo      = pNumRolo .   
        ASSIGN numEtq = ob-etiqueta.num-etiqueta .
    END.

