{esbo/boFatLocal.i ttParam ttLocal}
DEFINE VARIABLE hbo AS HANDLE      NO-UNDO.
DEFINE TEMP-TABLE tt LIKE fats_repres_clientes.
RUN esbo/boFatLocal.p PERSIST SET hBo.
RUN iniciar IN hBo.
RUN setValsIni IN hBo.
RUN setProp IN hBo('data',1,string(11.01.2024)).
RUN setProp IN hBo('data',2,string(11.30.2024)).
RUN exec    IN hBo.
RUN getTTResult IN hBo(OUTPUT TABLE tt).
RUN gerarTTlocal IN hbo.
RUN getTTLocal IN hBo(OUTPUT TABLE ttLocal ).
RUN finalizar IN hBo.

FOR EACH ttLocal:
DISP ttLocal WITH WIDTH 550.
END.
