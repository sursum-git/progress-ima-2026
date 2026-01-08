/********************************************************************************************
Programa    : esapi/verifExistPedidoLisa.p
Autor       : Tadeu Silva Parreiras
Objetivo    : A Partir dos campos cod-estabel e nr-pedido verifica se o pedido LISA
              foi registrado.
Data        : 01/2024 
Modificacoes:
*********************************************************************************************/
DEFINE INPUT  PARAMETER pCodEstabel AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pNrPedido   AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER logExiste   AS LOGICAL     NO-UNDO.

DEFINE VARIABLE hBoLisa10 AS HANDLE      NO-UNDO.

RUN esbo/boLisa10.p PERSIST SET hBoLisa10.

RUN iniciar IN hBolisa10.

RUN setProp IN hBoLisa10('cod_estabel',0,pCodEstabel).
RUN setProp IN hBoLisa10('nr_pedido',0,pNrPedido).

RUN verifExist IN hBoLisa10(OUTPUT logExiste).


RUN finalizar IN hBoLisa10.

