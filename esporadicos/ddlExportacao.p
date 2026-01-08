DEFINE VARIABLE hBo             AS HANDLE      NO-UNDO.
DEFINE VARIABLE clistaTabelas   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCont           AS INTEGER   NO-UNDO.
{esp/util.i}
{esbo/boMeTaDados.i}
RUN esbo/boMeTaDados.p  PERSIST SET hBo.

/*RUN incrValor(INPUT-OUTPUT cListaTabelas,'ped-venda',',').
RUN incrValor(INPUT-OUTPUT cListaTabelas,'ped-venda-ext',',').
RUN incrValor(INPUT-OUTPUT cListaTabelas,'ped-item',',').
RUN incrValor(INPUT-OUTPUT cListaTabelas,'ped-item-ext',',').
RUN incrValor(INPUT-OUTPUT cListaTabelas,'it-nota-fisc',',').*/
RUN incrValor(INPUT-OUTPUT cListaTabelas,'emitente',',').


REPEAT iCont = 1 TO NUM-ENTRIES(cListaTabelas) :
    CREATE ttTbsDireto.
    ASSIGN ttTbsDireto.tabela = ENTRY(iCont,cListaTabelas).
END.
FOR EACH ttTbsDireto:
    DISP ttTbsDireto.tabela.
END.

RUN setTipoRetCpExtent IN hBo('extent'). 
RUN setTabelasDireto   IN Hbo(INPUT TABLE ttTbsDireto).
RUN getDadosTbsDireto  IN hBo. 
RUN gerarDDlMySQL      IN hBo("c:\temp\ddl4.sql",FALSE).

