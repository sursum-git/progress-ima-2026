

def input param p-ind-event      as char          no-undo.
def input param p-ind-object     as char          no-undo.
def input param p-wgh-object     as handle        no-undo.
def input param p-wgh-frame      as widget-handle no-undo.
def input param p-cod-table      as char          no-undo.
def input param p-row-table      as recid         no-undo.

DEF NEW GLOBAL SHARED VAR h-campo       AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-ok      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-bt-cancel  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-janela     AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hesbofin328a AS HANDLE      NO-UNDO.
{utp/ut-glob.i}
DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
 
DEF VAR h-frame AS WIDGET-HANDLE NO-UNDO.
/*MESSAGE

    p-ind-event   
    p-ind-object  
    p-wgh-object  
    p-wgh-frame   
    p-cod-table   
    p-row-table   

    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

IF p-ind-event = 'initialize' AND p-ind-object = 'viewer' THEN DO:

  /*MESSAGE 'vou aplicar a permiss∆o correta'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
  RUN esbo/esbofin328a.p PERSISTENT SET hesbofin328a.
  RUN modificarPermissaoAPB IN hesbofin328a(NO).
  RUN criarLog IN hesbofin328a(' Permiss∆o RETIRADA do usuario:' + c-seg-usuario + ' - programa apb710zf-epc.p(parametros itens  - bordero) - viewer - initialize').
  IF VALID-HANDLE(hesbofin328a) THEN
      DELETE PROCEDURE hesbofin328a.





END.











