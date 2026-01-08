/*
programa: esbo/boLogsCalculos.p
Objetivo: Manter a tabela de log de calculos a ser utilizada por diversos programas
data: 10/2021
*/

DEFINE VARIABLE idTransacao   AS INT64       NO-UNDO.
DEFINE VARIABLE iTipo         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCalculo      AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDescricao    LIKE logs_calculos.descricao    NO-UNDO.
DEFINE VARIABLE hBoTransacoes AS HANDLE      NO-UNDO.
DEFINE TEMP-TABLE ttLogs LIKE logs_calculos.

{utp/ut-glob.i}


PROCEDURE iniciarBos:
   IF NOT VALID-HANDLE(hBoTransacoes) THEN
      RUN esbo/boTransacoes.p PERSISTENT SET hBoTransacoes.



END PROCEDURE.


PROCEDURE finalizarBos:
    IF VALID-HANDLE(hBoTransacoes) THEN
       DELETE PROCEDURE hBoTransacoes.

END PROCEDURE.

PROCEDURE setTransacao:

   DEFINE INPUT  PARAMETER iTransacao AS INTEGER     NO-UNDO.
   ASSIGN idTransacao = iTransacao.

END PROCEDURE.


PROCEDURE setTipo:

   DEFINE INPUT  PARAMETER pTipo AS INTEGER     NO-UNDO.
   ASSIGN iTipo  = pTipo.

END PROCEDURE.

PROCEDURE setDescricao:
   DEFINE INPUT  PARAMETER pDescricao LIKE logs_calculos.descricao  NO-UNDO.
   ASSIGN  cDescricao = pDescricao.

END PROCEDURE.

PROCEDURE setCalculo :
    DEFINE INPUT  PARAMETER pID AS INTEGER     NO-UNDO.
    ASSIGN iCalculo = pId.



END PROCEDURE.


PROCEDURE criarLog:

    CREATE logs_calculos.
    ASSIGN logs_calculos.LOG_calculo_id = NEXT-VALUE(seq_log_calculo)
           logs_calculos.transacao_id   = idTransacao
           logs_calculos.num_tipo       = iTipo
           logs_calculos.dt_hr_registro = NOW
           logs_calculos.descricao      = cDescricao
           logs_calculos.calculo_id     = iCalculo .
   

END PROCEDURE.

PROCEDURE getLogsUltTransPorChave:
   DEFINE INPUT  PARAMETER pChave AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR ttLogs.
   DEFINE VARIABLE idTransacao    AS INT64       NO-UNDO.
   DEFINE BUFFER logs FOR logs_calculos.
   RUN setChave IN hBoTransacoes(pChave).
   RUN getUltIDPorChave IN hBoTransacoes(OUTPUT idTransacao).
   /*MESSAGE idTransacao SKIP
           iCalculo 
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
   FOR EACH logs
       WHERE logs.transacao_id = idTransacao
       AND   logs.calculo_id = iCalculo:
       /*MESSAGE 'entrei'
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
       CREATE ttLogs.
       BUFFER-COPY logs TO ttLogs.
   END.



END PROCEDURE.

PROCEDURE getLogsPorChave:

     
     DEFINE INPUT  PARAMETER pChave AS CHARACTER   NO-UNDO.
     DEFINE OUTPUT PARAMETER TABLE FOR ttLogs.
     DEFINE VARIABLE cListaIdTransacoes AS CHARACTER   NO-UNDO.
     DEFINE BUFFER logs FOR logs_calculos.
     RUN setChave IN hBoTransacoes(pChave).
     RUN getIdsPorChave IN hBoTransacoes(INPUT 0, // 0 todas situa‡äes - acima de zero filtra a situa‡Æo passada por parametro
                        OUTPUT cListaIdTransacoes).

     /*MESSAGE idTransacao SKIP
             iCalculo 
         VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
     RUN limparTT.
     FOR EACH logs
         WHERE lookup(string(logs.transacao),cListaIdTransacoes,',') > 0
         AND   logs.calculo_id = iCalculo:
         /*MESSAGE 'entrei'
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
         CREATE ttLogs.
         BUFFER-COPY logs TO ttLogs.
     END.




END PROCEDURE.


    
PROCEDURE limparTT:

    EMPTY TEMP-TABLE ttLogs.

END PROCEDURE.







