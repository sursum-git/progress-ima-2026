DEF NEW GLOBAL SHARED VAR wh-bt-atu AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE h_esbofin328a AS HANDLE      NO-UNDO.
{utp/ut-glob.i}
/*  MESSAGE 'entrei no bot∆o de atualizaá∆o'  */
/*      VIEW-AS ALERT-BOX INFO BUTTONS OK.    */
RUN esbo/esbofin328a.p PERSISTENT SET h_esbofin328a.
RUN modificarPermissaoAPB IN h_esbofin328a (YES).
RUN criarLog IN h_esbofin328a('Aprovaá∆o concedida a ' +  c-seg-usuario +  ' - programa apb713aa2-epc - no clique do bot∆o de atualizaá∆o').
IF VALID-HANDLE(h_esbofin328a) THEN
   DELETE PROCEDURE h_esbofin328a.

APPLY 'choose' TO wh-bt-atu.

