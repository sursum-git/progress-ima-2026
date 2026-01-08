/****************************************************************************
programa:esapi/moverArqRetornoRegistrado.p
objetivo: A partir da verificaá∆o ao registro do arquivo pelo nome do mesmo
na tabela retornos_lisa, move o arquivo para a pasta de arquivos registrados.
Caso o arquivo n∆o seja encontrado na tabela retornos_lisa o mesmo n∆o Ç movido
data:12/2023
autor: tadeu silva 
****************************************************************************/
{esp/util.i}

DEFINE INPUT  PARAMETER nomeArquivo  AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cNovoArquivo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDir                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBoConsParam         AS HANDLE      NO-UNDO.

FIND FIRST retornos_lisa NO-LOCK
   WHERE retornos_lisa.arquivo_json = nomeArquivo NO-ERROR.
IF AVAIL retornos_lisa THEN DO:
   RUN esbo/boConsParam.p PERSIST SET hBoConsParam.
   RUN incrValor(INPUT-OUTPUT cNovoArquivo,retornos_lisa.nr_pedido,"-").
   RUN incrValor(INPUT-OUTPUT cNovoArquivo,retornos_lisa.pre_pedido,"-").
   RUN incrValor(INPUT-OUTPUT cNovoArquivo,retornos_lisa.serie,"-").
   RUN incrValor(INPUT-OUTPUT cNovoArquivo,retornos_lisa.nr_nota_fis,"-").
   RUN getDirCompletoRetornoProcessadoLisa IN hBoConsParam(OUTPUT cDir).
   ASSIGN cNovoArquivo = cDir  + "\" + cNovoArquivo + ".json".
   OS-RENAME value(nomeArquivo) value(cNovoArquivo).
   IF VALID-HANDLE(hBoConsParam) THEN
      DELETE PROCEDURE hBoConsParam.
END.
        
            
