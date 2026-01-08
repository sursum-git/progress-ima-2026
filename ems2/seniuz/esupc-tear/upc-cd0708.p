/* Programa: upc-cd0708.p
** Objetivo: Dar manuten‡Æo nos Dados complementares das Representatens,
**           (cota-rer) referentes …s customiza‡äes da Tear Tˆxtil Ind.Com.Ltda.
** Autor...: Prodb - Toninho  Mar‡o/2004
*/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEFINE NEW GLOBAL SHARED VAR adm-broker-hdl AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-folder       AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-browse       AS HANDLE NO-UNDO.

/* Global Variable Definitions for CEPONLINE *******************************/
DEFINE NEW GLOBAL SHARED VAR h-cep          AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-endereco     AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-bairro       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-cidade       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-estado       AS HANDLE.

/* Variable Definitions *****************************************************/
DEFINE VAR c-folder       AS CHARACTER NO-UNDO.
DEFINE VAR c-objects      AS CHARACTER NO-UNDO.
DEFINE VAR h-object       AS HANDLE    NO-UNDO.
DEFINE VAR i-objects      AS INTEGER   NO-UNDO.
DEFINE VAR l-record       AS LOGICAL   NO-UNDO INITIAL NO.

DEFINE VAR l-group-assign AS LOGICAL   NO-UNDO INITIAL NO.

DEF VAR h-objeto AS WIDGET-HANDLE NO-UNDO.
DEF VAR c-objeto AS CHAR NO-UNDO.

/* Temp-Tables definidas ***************************************************/

ASSIGN c-objeto = ENTRY(NUM-ENTRIES(p-wgh-object:PRIVATE-DATA, "~/"), p-wgh-object:PRIVATE-DATA, "~/").

/* Main Block ***************************************************************/

IF p-ind-event = "BEFORE-INITIALIZE" AND
   p-ind-object = "VIEWER" AND
   c-objeto = "v05ad229.w" THEN DO:
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
       IF h-objeto:TYPE <> "field-group" THEN DO:
          IF h-objeto:NAME = "endereco" THEN DO.
             ASSIGN h-endereco = h-objeto.
             ON 'entry':U OF h-objeto PERSISTENT RUN esupc/upc-cd0401b.p.
          END.

          IF h-objeto:NAME = "bairro" THEN
             ASSIGN h-bairro = h-objeto.

          IF h-objeto:NAME = "cidade" THEN
             ASSIGN h-cidade = h-objeto.

          IF h-objeto:NAME = "estado" THEN
             ASSIGN h-estado = h-objeto.

          IF h-objeto:NAME = "cep" THEN DO.
             ASSIGN h-cep = h-objeto.
             ON 'leave':U OF h-objeto PERSISTENT RUN esupc/upc-cd0401a.p.
          END.
          ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
       END.
       ELSE 
          ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.
 
IF p-ind-event = "BEFORE-INITIALIZE" AND
   c-objeto = "v05ad229.w" THEN DO: 
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto):
      IF h-objeto:TYPE <> "field-group" THEN DO:
         IF h-objeto:NAME = "e-mail" THEN
            ASSIGN h-objeto:FORMAT = "x(62)".  
         
         ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
      END.
      ELSE 
         ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   END.
END.
 
IF p-ind-event = "INITIALIZE" AND p-ind-object = "CONTAINER" THEN DO:

    RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                           INPUT "PAGE-SOURCE":U,
                                           OUTPUT c-folder).

    ASSIGN h-folder = WIDGET-HANDLE(c-folder) NO-ERROR.
    
    IF VALID-HANDLE(h-folder) THEN DO:
       RUN create-folder-page IN h-folder (INPUT 5, INPUT "Cotas":U).
       RUN create-folder-label IN h-folder (INPUT 5, INPUT "Cotas":U).
       
       RUN select-page IN p-wgh-object (INPUT 5).
       
       RUN init-object IN p-wgh-object (INPUT "esbrw/b01ad229.w":U,
                                        INPUT p-wgh-frame,
                                        INPUT "Layout = ":U,
                                        OUTPUT h-browse).
       
       RUN set-position IN h-browse ( 6.55, 3).
       
       RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                              INPUT "CONTAINER-TARGET":U,
                                              OUTPUT c-objects).
       
       DO i-objects = 1 TO NUM-ENTRIES(c-objects):
          ASSIGN h-object = WIDGET-HANDLE(ENTRY(i-objects, c-objects)).
          
          IF INDEX(h-object:PRIVATE-DATA, "qry") <> 0 AND   
             NOT l-record THEN DO:
             ASSIGN l-record = YES.
             
             RUN add-link IN adm-broker-hdl (INPUT h-object,
                                             INPUT "Record":U,
                                             INPUT h-browse).
          END.
          
          IF INDEX(h-object:PRIVATE-DATA, "vwr") <> 0 AND /* Voce deve verificar se e a viewer principal */
             NOT l-group-assign THEN DO:
             ASSIGN l-group-assign = YES.

             RUN add-link IN adm-broker-hdl (INPUT h-object, 
                                             INPUT "Group-Assign":U,
                                             INPUT h-browse).
          END.
          
       END.
       RUN dispatch IN h-browse ("initialize":U).
       RUN select-page IN p-wgh-object (INPUT 1).
    END. 
END.
 

