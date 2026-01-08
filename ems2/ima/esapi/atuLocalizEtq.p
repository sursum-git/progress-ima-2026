/**************************************************************************
Programa:esapi/atuLocalizEtq.p
Autor: tadeu Silva Parreiras 
Objetivo: Atualiza a localiza‡Æo da Etiqueta
Data:08/2024

*****************************************************************************/
DEFINE INPUT  PARAMETER pEstab      AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNumEtq     AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pLocaliz    AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE ttEtq LIKE ob-etiqueta .

DEFINE VARIABLE hBo AS HANDLE      NO-UNDO.

RUN esbo/boEs049.p PERSIST SET hBo.
RUN iniciar IN hBo.
FIND ob-etiqueta NO-LOCK
    WHERE ob-etiqueta.cod-estabel  = pEstab
    AND   ob-etiqueta.num-etiqueta = pNumEtq
    NO-ERROR.
IF AVAIL ob-etiqueta THEN DO:
   CREATE ttEtq.
   BUFFER-COPY ob-etiqueta TO ttEtq.
   ASSIGN ttEtq.localiz = pLocaliz.
   RUN setTTReg IN hbo(INPUT TABLE ttEtq).
   IF ob-etiqueta.localiz <> pLocaliz THEN
   DO:
    RUN alterarLocaliz IN hBo.    
   END.
   
END.

RUN finalizar IN hBo.

