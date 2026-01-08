DEF NEW GLOBAL SHARED VAR wh-janela         AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-cancel      AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE h_esbofin328a AS HANDLE      NO-UNDO.
{utp/ut-glob.i}
/*  MESSAGE 'cancelei a operaá∆o'          */
/*      VIEW-AS ALERT-BOX INFO BUTTONS OK. */

RUN esbo/esbofin328a.p PERSISTENT SET h_esbofin328a.
RUN modificarPermissaoAPB IN h_esbofin328a (NO).
RUN criarLog IN h_esbofin328a('Aprovaá∆o RETIRADA  a ' +  c-seg-usuario +  ' - programa apb713ab1-epc - no clique do bot∆o cancelar ').
IF VALID-HANDLE(h_esbofin328a) THEN
   DELETE PROCEDURE h_esbofin328a.
APPLY 'esc' TO wh-janela.


