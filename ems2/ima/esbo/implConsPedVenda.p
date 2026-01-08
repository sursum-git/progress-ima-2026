{esbo/boConsPedVenda.i ttParam ttLocal}
DEFINE VARIABLE hbo AS HANDLE      NO-UNDO.
DEFINE TEMP-TABLE tt LIKE fats_repres_clientes.
RUN esbo/boConsPedVenda.p PERSIST SET hBo.
RUN iniciar IN hBo.
RUN finalizar IN hBo.
RUN exec IN hBo.

/*FOR EACH ttLocal:
DISP ttLocal WITH WIDTH 550.
END. */
