/*
  Pegar valor de uma celula

*/

{utp/utapi006.i}


CREATE tt-dados.
ASSIGN tt-dados.versao-integracao = 1
       tt-dados.arquivo           = "c:\temp\vendas por periodo.xls"
       tt-dados.planilha          = 1
       tt-dados.celula-linha      = 4
       tt-dados.celula-coluna     = 2.

RUN utp/utapi006.p (INPUT-OUTPUT TABLE tt-dados,
                    OUTPUT TABLE tt-erros).

IF RETURN-VALUE = "nok" THEN DO: 
   FOR EACH tt-erros: 
       DISP tt-erros WITH 1 COL WIDTH 500. 
   END.
END.                  

FIND FIRST tt-dados NO-LOCK NO-ERROR.
MESSAGE tt-dados.valor         SKIP /* valor da Celula */
        tt-dados.formula-local SKIP /* formula local da celula */
        tt-dados.formula       SKIP /* formula padrao da celula */
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
