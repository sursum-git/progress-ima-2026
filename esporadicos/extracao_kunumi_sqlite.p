DEFINE VARIABLE hBoConsDin          AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBoMD               AS HANDLE      NO-UNDO.
DEFINE VARIABLE cListaCampos        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTabela             AS CHARACTER   NO-UNDO FORMAT 'x(20)'.
{util.i}
{esbo/boMetaDados.i}
DEFINE TEMP-TABLE tt NO-UNDO
    FIELD tabela AS CHAR FORMAT 'x(30)'.
RUN esbo/boConsdin.p    PERSIST SET hBoConsDin.
RUN esbo/boMetaDados.p  PERSIST SET hBoMD.
RUN iniciarBos IN hBoConsDin.

INPUT FROM c:\temp\kunumi\tbs13.csv.
REPEAT:
    CREATE tt. 
    IMPORT  tt.

END.
INPUT CLOSE.
FOR EACH tt
    WHERE tt.tabela <> '':
    DISP tt.tabela. PAUSE 0.
    RUN limparFiltros IN hBoMD.
    RUN setTabela IN hBoMD(tt.tabela).
    IF tt.tabela = 'cliente' OR tt.tabela = 'fornecedor' THEN
       RUN setBanco IN hBoMd('ems5').
    RUN limparTTCampos IN hBoMD.
    RUN getCpsTb IN hBoMD.
    RUN getTTCps IN hBoMD(OUTPUT TABLE ttCampos).
    RUN extrairListaCpsTbPorOrdem(TEMP-TABLE ttCampos:DEFAULT-BUFFER-HANDLE,'nome','',' BY ordem ', OUTPUT cListaCampos).
    RUN limparTTs IN hBoConsDin.
    RUN setArqInsertSqlite    IN hBoConsDin('t:\exportacao\' + tt.tabela + '.sql').
    RUN setBancoTabela IN hBoConsDin('cliente','ems5').
    RUN setBancoTabela IN hBoConsDin('fornecedor','ems5').
    RUN setDadosConsulta IN hBoConsDin(tt.tabela,cListaCampos,'1=1','').
    
    RUN execConsulta IN hBoConsDin('sqlite').
END.
 
RUN finalizarBos IN hBoConsDin.
IF VALID-HANDLE(hBoconsDin) THEN
   DELETE PROCEDURE hBoConsDin.
IF VALID-HANDLE(hBoMD) THEN
   DELETE PROCEDURE hboMd.



/*UPDATE cTabela.

//RUN setBanco IN hBoMD(cBancoSel).




OUTPUT TO c:\temp\ttCampos.txt.
FOR EACH ttCampos:
    DISP ttCampos.nome .
END.
OUTPUT CLOSE.

*/

