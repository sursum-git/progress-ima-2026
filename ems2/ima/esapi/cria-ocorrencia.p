DEF INPUT PARAMETER p-cod-tab   LIKE tab-ocor.cod-tab.
DEF INPUT PARAMETER p-cod-ocor  LIKE tab-ocor.cod-ocor.
DEF INPUT PARAMETER p-campo     AS CHAR.
DEF INPUT PARAMETER p-descricao AS CHAR.

/*
FIND tab-ocor WHERE
     tab-ocor.cod-tab = p-cod-tab AND
     tab-ocor.cod-ocor = p-cod-ocor AND
     tab-ocor.c-campo[1] = p-campo
     NO-LOCK NO-ERROR.
IF NOT AVAIL tab-ocor THEN DO.
   CREATE tab-ocor.
   ASSIGN tab-ocor.cod-tab     = p-cod-tab
          tab-ocor.cod-ocor    = p-cod-ocor
          tab-ocor.descricao   = p-descricao 
          tab-ocor.c-campo[1]  = p-campo
          tab-ocor.da-campo[1] = TODAY.
END.
*/

/*
RUN esapi/cria-ocorrencia.p (INPUT 159,
                             INPUT 1,
                             INPUT ped-venda.nr-pedcli,
                             INPUT "Alterado Cond Pagto").

FIND tab-ocor WHERE
     tab-ocor.cod-tab = p-cod-tab AND
     tab-ocor.cod-ocor = p-cod-ocor AND
     tab-ocor.c-campo[1] = p-campo
     NO-ERROR.
DELETE tab-ocor.
FIND FIRST tab-ocor NO-LOCK NO-ERROR.
*/
