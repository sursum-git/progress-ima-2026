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

INPUT FROM c:\temp\tbs.csv.
REPEAT:
    CREATE tt. 
    IMPORT  tt.

END.
INPUT CLOSE.
OUTPUT TO VALUE("t:\exportacao\opcoes_campos2.csv") NO-CONVERT.
PUT "tabela;campo;valores" SKIP.
FOR EACH tt
    WHERE tt.tabela <> '':
    //DISP tt.tabela. PAUSE 0.
    RUN limparFiltros IN hBoMD.
    RUN setTabela IN hBoMD(tt.tabela).
    RUN limparTTCampos IN hBoMD.
    RUN getCpsTb IN hBoMD. 
    RUN getTTCps IN hBoMD(OUTPUT TABLE ttCampos).
    FOR EACH ttcampos
        WHERE ttCampos.tipo = 'integer' OR (ttCampos.tipo <> 'integer' AND ttCampos.lista <> ''):
        EXPORT DELIMITER ";" tt.tabela ttcampos.nome ttCampos.lista .
    END.                                                            
END.
OUTPUT CLOSE.

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

