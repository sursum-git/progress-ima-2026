/**************************************************************************
Programa:esapi/getNumEtqMed.p
Autor: tadeu Silva Parreiras 
Objetivo: Retornar o numero da etiqueta da MED 
a partir do numero da etiqueta da lisa
Data:08/2024

*****************************************************************************/

DEFINE INPUT  PARAMETER pEtqLisa AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER numEtq   AS INTEGER     NO-UNDO.

FIND LAST etiqueta_lisa NO-LOCK
    WHERE etiqueta_lisa.id_etq_lisa = pEtqLisa
    NO-ERROR.
IF AVAIL etiqueta_lisa THEN DO:
   ASSIGN numEtq = etiqueta_lisa.num_etiqueta .

END.

