/****************************************************************************
** Programa: upc-re1001a.p 
** Objetivo: Criar um campo de digitacao Onde ser† informado o 
**           codigo da Rejeiá∆o 
**
**           Este Novo campo sera gravado na tabela docum-est.int-2 e
**           Na Trigger da Tabela devol-cli, repassado para o campo
**           devol-cli.codigo-rejei
**
** Autor   : TONINHO - ABRIL/2017
*****************************************************************************/
/* Parameter Definitions ****************************************************/

DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.
{utp/ut-glob.i}
/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR wh-lbl-cod-rejei  AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cod-rejei      AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-desc-rejei     AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cod-estabel    AS WIDGET-HANDLE NO-UNDO.

/* Variable Definitions *****************************************************/
DEF VAR h-objeto          AS WIDGET-HANDLE NO-UNDO.
DEF VAR c-objeto          AS CHAR NO-UNDO.

/* Variavies criadas na viewer dinamicamente*********************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/
IF p-ind-event = "AFTER-INITIALIZE" THEN DO:
   CREATE TEXT wh-lbl-cod-rejei
        ASSIGN FRAME         = p-wgh-frame
               FORMAT        = "x(15)"
               WIDTH         = 20
               SCREEN-VALUE  = "Cod. Devoluá∆o:"
               ROW           = 10.1
               COL           = 40.8
               VISIBLE       = YES.

   CREATE FILL-IN wh-cod-rejei
       ASSIGN FRAME             = p-wgh-frame
              SIDE-LABEL-HANDLE = wh-lbl-cod-rejei
              DATA-TYPE         = "integer"
              FORMAT            = ">>9"
              WIDTH             = 4
              ROW               = 9.9
              COL               = 52.8
              VISIBLE           = YES
              SENSITIVE         = YES
              HEIGHT            = 0.88
              TOOLTIP           = "C¢digo da Devoluá∆o"
              TRIGGERS:
                  ON "MOUSE-SELECT-DBLCLICK":U PERSISTENT RUN esepc/epc-re2001a-z.p.
                  ON "LEAVE":U PERSISTENT RUN esepc/epc-re2001a-s1.p.
              END TRIGGERS.

   CREATE FILL-IN wh-desc-rejei
      ASSIGN FRAME             = p-wgh-frame
             DATA-TYPE         = "CHARACTER"
             FORMAT            = "x(30)"
             WIDTH             = 30
             ROW               = 9.9
             COL               = 57.5
             VISIBLE           = YES
             SENSITIVE         = NO
             HEIGHT            = 0.88.

   wh-cod-rejei:LOAD-MOUSE-POINTER("image/lupa.cur":U).

   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = 'dt-trans' THEN 
         wh-cod-rejei:MOVE-AFTER-TAB-ITEM(h-objeto).
      IF h-objeto:NAME = 'cod-estabel' THEN DO:
         ASSIGN wh-cod-estabel = h-objeto:HANDLE.
         ASSIGN h-objeto:SCREEN-VALUE = i-ep-codigo-usuario.
         /*IF i-ep-codigo-usuario = '5' THEN
            ASSIGN h-objeto:SENSITIVE = NO.*/
      END.
      IF h-objeto:NAME = 'c-estabel' THEN DO:
         FIND FIRST estabelec
             WHERE estabelec.cod-estabel = i-ep-codigo-usuario
             NO-LOCK NO-ERROR.
         IF AVAIL estabelec THEN
           ASSIGN h-objeto:SCREEN-VALUE = estabelec.nome.
         
      END.
      IF h-objeto:NAME = 'cb-cod-observa' THEN 
         ON "VALUE-CHANGED":U OF h-objeto PERSISTENT RUN esepc/epc-re1001a-s2.p.

      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
END.

IF p-ind-event = "BEFORE-ASSIGN" THEN DO.
   IF wh-cod-rejei:SENSITIVE THEN DO.
      FIND cod-rejeicao WHERE 
           cod-rejeicao.codigo-rejei = INT(wh-cod-rejei:SCREEN-VALUE) NO-LOCK NO-ERROR.
      IF NOT AVAIL cod-rejeicao THEN DO:
         MESSAGE "C¢digo de Rejeiá∆o n∆o Cadastrado..."
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY 'ENTRY' TO wh-cod-rejei.
         RETURN "NOK".
      END.

      IF INT(wh-cod-rejei:SCREEN-VALUE) = 0 THEN DO.
         MESSAGE "               A T E N Ä « O  !!!" SKIP(1)
                 "C¢digo de Rejeiá∆o ser† gravado com C¢digo ZERO" SKIP
                 "Favor Verificar..." 
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
   END.
   IF substr(wh-cod-estabel:SCREEN-VALUE,1,1) <> i-ep-codigo-usuario AND (wh-cod-estabel:SCREEN-VALUE <> '8' AND i-ep-codigo-usuario <> '1' )    THEN DO:
      MESSAGE 'ATENÄ«O!!' SKIP 
          'Vocà digitou o estabelecimento ' + wh-cod-estabel:SCREEN-VALUE +  ' que diverge da empresa corrente:' + i-ep-codigo-usuario
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO wh-cod-estabel.
      RETURN ERROR.
      /*RETURN "NOK".*/
   END.
   
END.

IF p-ind-event = "AFTER-ASSIGN" THEN DO:
   IF wh-cod-rejei:SENSITIVE THEN DO.
      FIND docum-est WHERE
           ROWID(docum-est) = p-row-table NO-ERROR.
      IF AVAIL docum-est THEN
         ASSIGN docum-est.int-2 = INTEGER(wh-cod-rejei:SCREEN-VALUE).
   END.
END.

