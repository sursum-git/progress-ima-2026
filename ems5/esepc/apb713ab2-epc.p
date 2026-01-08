DEF NEW GLOBAL SHARED VAR wh-bt-ok      AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE h_esbofin328a           AS HANDLE  NO-UNDO.
{utp/ut-glob.i}
RUN esbo/esbofin328a.p PERSISTENT SET h_esbofin328a.
/* MESSAGE 'modificar aprovador para n∆o '  */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.   */
RUN modificarPermissaoAPB IN h_esbofin328a (NO).
RUN criarLog IN h_esbofin328a('Aprovaá∆o RETIRADA a ' +  c-seg-usuario +  ' - programa apb713ab2-epc - no clique do bot∆o de ok').
IF VALID-HANDLE(h_esbofin328a) THEN
   DELETE PROCEDURE h_esbofin328a.
APPLY 'choose' TO wh-bt-ok.
