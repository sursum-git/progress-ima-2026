/****************************************************************************
** Programa: upc-re2001a.p 
** Objetivo: Criar um campo de digitacao Onde ser† informado o 
**           codigo da Rejeiá∆o 
**
**           Este Novo campo sera gravado na tabela DOC-FISICO.int-2 e
**           Na Trigger da Tabela devol-cli, repassado para o campo
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


/* Global Variable Definitions **********************************************/
DEF NEW GLOBAL SHARED VAR wh-lbl-cod-rejei AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-cod-rejei AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR wh-desc-rejei AS WIDGET-HANDLE NO-UNDO.
DEF NEW GLOBAL SHARED VAR h-tipo-nota AS WIDGET-HANDLE NO-UNDO.
/*
DEF NEW GLOBAL SHARED VAR wh-cod-estabel AS WIDGET-HANDLE NO-UNDO.
*/

{utp/ut-glob.i}
/* Variable Definitions *****************************************************/
DEF VAR h-objeto          AS WIDGET-HANDLE NO-UNDO.
DEF VAR c-objeto          AS CHAR NO-UNDO.

/* Variavies criadas na viewer dinamicamente*********************************/
ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

/*    MESSAGE p-ind-event SKIP             */
/*           p-ind-object SKIP             */
/*           c-objeto     SKIP             */
/*      VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*                                         */


/***** TONINHO 09/11

IF p-ind-event = "ADD" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05in089.w" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = 'cod-estabel' THEN DO:
/*           MESSAGE 'entrei aqui.....................................' */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                     */
          ASSIGN wh-cod-estabel = h-objeto:HANDLE.
         ASSIGN h-objeto:SCREEN-VALUE = i-ep-codigo-usuario.
         /*ASSIGN h-objeto:SENSITIVE = NO*/ .
      END.
/*       IF h-objeto:NAME = 'fi-estabel' THEN DO:                 */
/*          FIND FIRST estabelec                                  */
/*              WHERE estabelec.cod-estabel = i-ep-codigo-usuario */
/*              NO-LOCK NO-ERROR.                                 */
/*          IF AVAIL estabelec THEN                               */
/*            ASSIGN h-objeto:SCREEN-VALUE = estabelec.nome.      */
/*       END.                                                     */
      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
END.

***** TONINHO */



IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05in089.w" THEN DO:

   CREATE TEXT wh-lbl-cod-rejei
        ASSIGN FRAME         = p-wgh-frame
               FORMAT        = "x(15)"
               WIDTH         = 15
               SCREEN-VALUE  = "Cod. Devoluá∆o:"
               ROW           = 13.2
               COL           = 10.8
               VISIBLE       = YES.

   CREATE FILL-IN wh-cod-rejei
       ASSIGN FRAME             = p-wgh-frame
              SIDE-LABEL-HANDLE = wh-lbl-cod-rejei
              DATA-TYPE         = "integer"
              FORMAT            = ">>9"
              WIDTH             = 4
              ROW               = 13
              COL               = 22.8
              VISIBLE           = YES
              SENSITIVE         = NO
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
             ROW               = 13
             COL               = 27.5
             VISIBLE           = YES
             SENSITIVE         = NO
             HEIGHT            = 0.88.

   wh-cod-rejei:LOAD-MOUSE-POINTER("image/lupa.cur":U).


   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:NAME = 'c-tipo-nota' THEN DO.
         ASSIGN h-tipo-nota = h-objeto.
         ON "LEAVE":U OF h-objeto PERSISTENT RUN esepc/epc-re2001a-s2.p.
      END.
      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.

END.

IF p-ind-event = "ENABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05in089.w" THEN DO.

   IF h-tipo-nota:SCREEN-VALUE = "Devoluá∆o" THEN
      ASSIGN wh-cod-rejei:SENSITIVE = YES.
END.


IF p-ind-event = "DISABLE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05in089.w" THEN 
   ASSIGN wh-cod-rejei:SENSITIVE = NO.

IF p-ind-event = "ADD" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05in089.w" THEN DO: 
   ASSIGN wh-cod-rejei:SCREEN-VALUE = " "
          wh-desc-rejei:SCREEN-VALUE = " ".
   ASSIGN wh-cod-rejei:LABEL = "Cod. Devoluá∆o:".
END.

IF p-ind-event = "DISPLAY" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05in089.w" AND 
   VALID-HANDLE(wh-cod-rejei)THEN DO:

   ASSIGN wh-cod-rejei:SCREEN-VALUE = " "
          wh-desc-rejei:SCREEN-VALUE = " ".
   ASSIGN wh-cod-rejei:LABEL = "Cod. Devoluá∆o:".
   FIND doc-fisico WHERE
        ROWID(doc-fisico) = p-row-table NO-LOCK NO-ERROR.
   IF AVAIL doc-fisico THEN DO.
      ASSIGN wh-cod-rejei:SCREEN-VALUE = STRING(doc-fisico.int-2).
      FIND cod-rejeicao WHERE 
           cod-rejeicao.codigo-rejei = INT(wh-cod-rejei:SCREEN-VALUE) NO-LOCK NO-ERROR.
      IF AVAIL cod-rejeicao THEN 
         ASSIGN wh-desc-rejei:SCREEN-VALUE = UPPER(cod-rejeicao.descricao).
   END.
END.

IF p-ind-event = "VALIDATE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05in089.w" THEN DO.

   /**** TONINHO
   IF SUBSTR(wh-cod-estabel:SCREEN-VALUE,1,1) <> i-ep-codigo-usuario THEN DO:
      MESSAGE 'Estabelecimento diferente da empresa corrente. Favor modificar estabelecimento ou mudar a empresa.'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'ENTRY' TO wh-cod-estabel.
      RETURN 'NOK'.

   END.
   **** TONINHO */

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
                 "Confirma ?" 
                 VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-confirma AS LOGICAL.
         IF NOT l-confirma THEN DO.
            APPLY 'ENTRY' TO wh-cod-rejei.
            RETURN "NOK".
         END.
      END.
   END.
END.

IF p-ind-event = "ASSIGN" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05in089.w" THEN DO:
   IF VALID-HANDLE(wh-cod-rejei) AND wh-cod-rejei:SENSITIVE THEN DO.
      FIND doc-fisico WHERE
           ROWID(doc-fisico) = p-row-table NO-ERROR.
      IF AVAIL doc-fisico THEN
         ASSIGN doc-fisico.int-2 = INTEGER(wh-cod-rejei:SCREEN-VALUE).
   END.
END.



