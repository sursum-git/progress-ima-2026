/******************************************************************************
*   Programa .....: imcc0300a.p                                               *
*   Data .........: 18/01/2005                                                *
*   Cliente ......: Ima Tecidos                                               *
*   Programador ..: Viviane Fonseca Moreira                                   *
*   Objetivo .....: Extens∆o da tabela pedido de compra                       *
******************************************************************************/



/****** Parametros ******/
DEF INPUT PARAMETER p-ind-event  AS CHARACTER.              
DEF INPUT PARAMETER p-ind-object AS CHARACTER.              
DEF INPUT PARAMETER p-wgh-object AS HANDLE.                 
DEF INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.          
DEF INPUT PARAMETER p-cod-table  AS CHARACTER.              
DEF INPUT PARAMETER p-row-table  AS ROWID.                  

/****** Vari†veis ******/
DEF NEW GLOBAL SHARED VAR wh-tp-pedido            AS  WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-tp-pedido-label      AS  WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-num-pedido-cc0300a   AS  WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-data-pedido-cc0300a  AS  WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cod-emitente-cc0300a AS  WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-nome-fornec-cc0300a  AS  WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-impr-pedido-cc0300a  AS  WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-btOK-cc0300a         AS  WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-btSave-cc0300a       AS  WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-btOK-novo-a          AS  WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-btSave-novo-a        AS  WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-frame-pedido         AS  WIDGET-HANDLE NO-UNDO.
def new global shared var l-modifica-cc0300 as logical.

DEF VAR c-char  AS CHAR.
DEF VAR h-frame AS WIDGET-HANDLE NO-UNDO.
DEF VAR h-campo AS WIDGET-HANDLE NO-UNDO.

DEF BUFFER b-pedido-compr FOR pedido-compr.
DEF BUFFER b-im-ped-compra-pi FOR im-ped-compra-pi.

IF VALID-HANDLE(p-wgh-object) THEN
   ASSIGN c-char = ENTRY(NUM-ENTRIES(p-wgh-object:FILE-NAME,"~/"), p-wgh-object:FILE-NAME,"~/").


/* MESSAGE p-ind-event                          */
/*         p-ind-object skip                    */
/*         "Objeto:" string(p-wgh-object) skip  */
/*         "frame:" p-wgh-frame skip            */
/*         "frame:" string(p-wgh-frame) skip    */
/*         "Cod table:" p-cod-table skip        */
/*         "Rowid:" string(p-row-table) SKIP    */
/*         c-char  VIEW-AS ALERT-BOX.           */


if p-ind-event  = 'BEFORE-INITIALIZE' and
   p-ind-object = 'CONTAINER' and
   c-char       = 'cc0300a.w' then do:

   assign h-campo = p-wgh-frame:FIRST-CHILD
          h-campo = h-campo:FIRST-CHILD.
   do while valid-handle(h-campo):
      if h-campo:name = 'num-pedido' then
         assign wh-num-pedido-cc0300a = h-campo.

      if h-campo:name = 'data-pedido' then
         assign wh-data-pedido-cc0300a = h-campo.

      if h-campo:name = 'cod-emitente' then
         assign wh-cod-emitente-cc0300a = h-campo.

      if h-campo:name = 'c-nome-fornec' then
         assign wh-nome-fornec-cc0300a = h-campo.
      
      if h-campo:name = 'impr-pedido' then
         assign wh-impr-pedido-cc0300a = h-campo:handle.
      
      if h-campo:name = 'btOK' then
         assign wh-btOK-cc0300a = h-campo.

      if h-campo:name = 'btSave' then
         assign wh-btSave-cc0300a = h-campo.

      if h-campo:TYPE = 'field-group'
         then assign h-campo = h-campo:FIRST-CHILD.
         else assign h-campo = h-campo:NEXT-SIBLING.
   end.
   
   assign wh-frame-pedido = p-wgh-frame.
   run cria-campos.
end.


if p-ind-event  = 'AFTER-DISPLAY' and
   p-ind-object = 'CONTAINER' and
   c-char       = 'cc0300a.w' then do:
   
   run mostra-campos.
end.


if p-ind-event  = 'AFTER-ENABLE' and
   p-ind-object = 'CONTAINER' and
   c-char       = 'cc0300a.w' then do:
   
   if wh-btOK-cc0300a:sensitive = yes then
      assign wh-btOK-novo-a:sensitive = yes.
   else
      assign wh-btOK-novo-a:sensitive = no.

   if wh-btSave-cc0300a:sensitive = yes then
      assign wh-btSave-novo-a:sensitive = yes.
   else
      assign wh-btSave-novo-a:sensitive = no.

   if wh-tp-pedido:screen-value = "PI" and l-modifica-cc0300 = yes then
      assign wh-tp-pedido:sensitive = no.
end.


if p-ind-event  = 'AFTER-ASSIGN' and
   p-ind-object = 'CONTAINER' and
   c-char       = 'cc0300a.w' then do:
   
   run grava-campos.
end.


PROCEDURE cria-campos:
    
    IF NOT VALID-HANDLE(wh-tp-pedido) THEN DO:
       CREATE TEXT wh-tp-pedido-label
       ASSIGN ROW               = 5.1
              COLUMN            = 68
              FRAME             = wh-frame-pedido /*p-wgh-frame*/
              SENSITIVE         = NO
              VISIBLE           = YES
              HEIGHT-CHARS      = 0.88
              WIDTH-CHARS       = 10
              FORMAT            = 'x(12)'
              SCREEN-VALUE      = 'Tipo Pedido:'.
       
       CREATE FILL-IN wh-tp-pedido
       ASSIGN ROW               = 5.1
              COLUMN            = 77
              FRAME             = wh-frame-pedido
              SENSITIVE         = yes
              VISIBLE           = YES
              HEIGHT-CHARS      = 0.88
              WIDTH-CHARS       = 4
              FORMAT            = 'x(2)'
              SCREEN-VALUE      = ''
       TRIGGERS:
           ON F5 PERSISTENT RUN imepc/imcc0300z.p.
           ON mouse-select-dblclick PERSISTENT RUN imepc/imcc0300z.p.
           /*ON LEAVE PERSISTENT RUN scepc/scepc0821z2.p.*/
       END TRIGGERS.

       wh-tp-pedido:LOAD-MOUSE-POINTER("image\lupa.cur").
       wh-tp-pedido:MOVE-AFTER-TAB-ITEM(wh-impr-pedido-cc0300a:HANDLE).

       CREATE BUTTON wh-btOK-novo-a
       ASSIGN frame     = wh-frame-pedido
              width     = 10
              height    = 1
              row       = 18.71
              col       = 2
              label     = 'OK'
              visible   = yes
              sensitive = yes
              tooltip   = ''
       TRIGGERS:
           on choose persistent run "imepc/imcc0300c.p".
       END TRIGGERS.

       CREATE BUTTON wh-btSave-novo-a
       ASSIGN frame     = wh-frame-pedido
              width     = 10
              height    = 1
              row       = 18.71
              col       = 13
              label     = 'Salvar'
              visible   = yes
              sensitive = yes
              tooltip   = ''
       TRIGGERS:
           on choose persistent run "imepc/imcc0300d.p".
       END TRIGGERS.
    END.

END PROCEDURE.

PROCEDURE mostra-campos:

    FIND FIRST b-pedido-compr WHERE ROWID(b-pedido-compr) = p-row-table NO-LOCK NO-ERROR.

    IF NOT AVAIL b-pedido-compr THEN DO:
       IF VALID-HANDLE(wh-tp-pedido) THEN
          ASSIGN wh-tp-pedido:SCREEN-VALUE = "".
    END.
    
    IF AVAIL b-pedido-compr THEN DO:
       FIND FIRST im-ext-pedido-compr WHERE
            im-ext-pedido-compr.num-pedido = b-pedido-compr.num-pedido NO-LOCK NO-ERROR.
       IF AVAIL im-ext-pedido-compr THEN DO:
          IF VALID-HANDLE(wh-tp-pedido) THEN
             ASSIGN wh-tp-pedido:SCREEN-VALUE = im-ext-pedido-compr.cod-tipo-ped.
       END.
       ELSE DO:
          IF VALID-HANDLE(wh-tp-pedido) THEN
             ASSIGN wh-tp-pedido:SCREEN-VALUE = "".
       END.
    END.

END PROCEDURE.

PROCEDURE grava-campos:

    FIND FIRST b-pedido-compr WHERE ROWID(b-pedido-compr) = p-row-table NO-LOCK NO-ERROR.
    IF AVAIL b-pedido-compr THEN DO:
       FIND FIRST im-ext-pedido-compr WHERE
            im-ext-pedido-compr.num-pedido = b-pedido-compr.num-pedido NO-ERROR.
       IF NOT AVAIL im-ext-pedido-compr THEN DO:
          CREATE im-ext-pedido-compr.
          ASSIGN im-ext-pedido-compr.num-pedido = b-pedido-compr.num-pedido.
       END.
       
       IF VALID-HANDLE(wh-tp-pedido) THEN DO:
          ASSIGN im-ext-pedido-compr.cod-tipo-ped = wh-tp-pedido:SCREEN-VALUE.

          if wh-tp-pedido:screen-value = "PI" then do:
             find first b-im-ped-compra-pi where
                  b-im-ped-compra-pi.num-pedido-compra = int(wh-num-pedido-cc0300a:screen-value) and
                  b-im-ped-compra-pi.nome-abrev        = wh-nome-fornec-cc0300a:screen-value and
                  b-im-ped-compra-pi.cod-emitente      = int(wh-cod-emitente-cc0300a:screen-value)
                  no-lock no-error.
             if not avail b-im-ped-compra-pi then do:
                create im-ped-compra-pi.
                assign im-ped-compra-pi.num-pedido-compra = int(wh-num-pedido-cc0300a:screen-value)
                       im-ped-compra-pi.nome-abrev        = wh-nome-fornec-cc0300a:screen-value
                       im-ped-compra-pi.cod-emitente      = int(wh-cod-emitente-cc0300a:screen-value)
                       im-ped-compra-pi.cod-tipo-pedido   = "PI"
                       im-ped-compra-pi.dt-emissao-pedido = date(wh-data-pedido-cc0300a:screen-value)
                       im-ped-compra-pi.sit-ped-compra    = 1.  /*Aberto*/
             end.
          end.
       END.
    END.

END PROCEDURE.
