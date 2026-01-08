/**************************************************************************
Programa:esapi/getNumEtqMed2.p
Autor: tadeu Silva Parreiras 
Objetivo: Retornar o numero da etiqueta da MED 
a partir do numero da etiqueta da lisa e se existe outra etiqueta
com ac chave estab + item + ref + container + rolo igual
Data:07/2025

*****************************************************************************/

DEFINE INPUT  PARAMETER pEtqLisa        AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER numEtq          AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER lAmbiguidade    AS LOGICAL     NO-UNDO.

DEFINE BUFFER bf FOR ob-etiqueta. 

FOR LAST etiqueta_lisa fields(cod_estabel num_etiqueta id_etq_lisa) NO-LOCK
    WHERE etiqueta_lisa.id_etq_lisa = pEtqLisa  .
END.
IF AVAIL etiqueta_lisa THEN DO:
   ASSIGN numEtq = etiqueta_lisa.num_etiqueta .
   FOR FIRST ob-etiqueta 
   FIELDS(cod-estabel num-etiqueta it-codigo cod-refer nr-container num-rolo) NO-LOCK
   WHERE ob-etiqueta.cod-estabel   = etiqueta_lisa.cod_estabel
   AND   ob-etiqueta.num-etiqueta  = etiqueta_lisa.num_etiqueta    .
   END.
   IF NOT AVAIL ob-etiqueta THEN NEXT.  
   
   ASSIGN lAmbiguidade =  CAN-FIND(FIRST bf 
   WHERE bf.it-codigo          = ob-etiqueta.it-codigo
   AND   bf.cod-estabel        = ob-etiqueta.cod-estabel
   AND   bf.cod-refer          = ob-etiqueta.cod-refer
   AND   bf.nr-container       = ob-etiqueta.nr-container
   AND   bf.num-rolo           = ob-etiqueta.num-rolo
   AND ROWID(bf)              <> ROWID(ob-etiqueta)
   AND bf.situacao = 3) .

END.

