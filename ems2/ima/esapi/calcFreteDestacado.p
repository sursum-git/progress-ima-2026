DEFINE INPUT  PARAMETER pNrPedido           AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pCodEstabel         AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pTpFrete            AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodCli             AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pCodCliTri          AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pVlTotPed           AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER vlFreteDestacado    AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER logFreteDestacado   AS LOGICAL     NO-UNDO.

DEFINE VARIABLE hBo01       AS HANDLE      NO-UNDO.
RUN esbo/boCalcFreteDestacado.p PERSIST SET hBo01.
RUN iniciar                 IN hBo01.
RUN setProp                 IN hBo01('nrPedido',          0,STRING(pNrPedido)).
RUN setProp                 IN hBo01('codEstabel',        0,STRING(pCodEstabel)).
RUN setProp                 IN hBo01('codTipoFrete',      0,STRING(pTpFrete)).
RUN setProp                 IN hBo01('codCliente',        0,STRING(pCodCli)).
RUN setProp                 IN hBo01('codClienteTriang',  0,STRING(pCodCliTri)).
RUN setProp                 IN hBo01('vlTotPed',          0,STRING(pVlTotPed)).
RUN exec                    IN hBo01.
RUN getVlFreteDestacado     IN hBo01(OUTPUT vlFreteDestacado).
RUN getLogFreteDestacado    IN hBo01(OUTPUT logFreteDestacado).
RUN gravarLogCalculo        IN hBo01.
RUN finalizar               IN hBo01.

