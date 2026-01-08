/*
programa: esapi/gravarEtqLisa.p
objetivo: incluir a rela‡Æo entre etiqueta MED e etiqueta LISA
autor: Tadeu
data: 07/2024
*/
DEFINE INPUT  PARAMETER pCodEstabel  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNumEtiqueta AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pItCodigo    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodRefer    AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNrContainer AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pNrPedido    AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pPrePedido   AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pIdEtqLisa   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pOrigem      AS INTEGER     NO-UNDO.


DEFINE TEMP-TABLE ttReg LIKE etiqueta_lisa.

DEFINE VARIABLE hBo AS HANDLE      NO-UNDO.
RUN esbo/bolisa13.p PERSIST SET hBo.
RUN iniciar IN hBo.
CREATE  ttReg.
ASSIGN ttReg.cod_estabel   = pCodEstabel
       ttReg.num_etiqueta  = pNumEtiqueta
       ttReg.id_etq_lisa   = pIDEtqLisa
       ttReg.nr_pedido     = pNrPedido
       ttReg.pre_pedido    = pPrePedido
       ttReg.num_origem    = pOrigem 
       .
RUN setTtReg IN hBo(TABLE ttReg).
RUN incluir IN hBo.

RUN finalizar IN hBo.

