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
DEFINE NEW GLOBAL SHARED VAR h_v02es015       AS HANDLE NO-UNDO.

/* Variable Definitions *****************************************************/
DEFINE VAR c-folder       AS CHARACTER NO-UNDO.
DEFINE VAR c-objects      AS CHARACTER NO-UNDO.
DEFINE VAR h-object       AS HANDLE    NO-UNDO.
DEFINE VAR i-objects      AS INTEGER   NO-UNDO.
DEFINE VAR l-record       AS LOGICAL   NO-UNDO INITIAL NO.
DEFINE VAR l-group-assign AS LOGICAL   NO-UNDO INITIAL NO.

/* Main Block ***************************************************************/
IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "CONTAINER" THEN DO:
   RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                          INPUT "PAGE-SOURCE":U,
                                          OUTPUT c-folder).
   ASSIGN h-folder = WIDGET-HANDLE(c-folder) NO-ERROR.
    
   IF VALID-HANDLE(h-folder) THEN DO:
      RUN create-folder-page IN h-folder (INPUT 5, INPUT "Compl.TEAR":U).
      RUN create-folder-label IN h-folder (INPUT 5, INPUT "Compl.TEAR":U).
       
      RUN select-page IN p-wgh-object (INPUT 5).
       
      RUN init-object IN p-wgh-object (INPUT "esvwr/v02es015.w":U, /* Nome do Objeto Viewer */
                                       INPUT p-wgh-frame,
                                       INPUT "Layout = ":U,
                                       OUTPUT h_v02es015).
       
      RUN set-position IN h_v02es015 ( 7.10, 3.00).
       
      RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                             INPUT "CONTAINER-TARGET":U,
                                             OUTPUT c-objects).
        
      DO i-objects = 1 TO NUM-ENTRIES(c-objects):
         ASSIGN h-object = WIDGET-HANDLE(entry(i-objects, c-objects)).
          
         IF INDEX(h-object:PRIVATE-DATA, "qry") <> 0 AND  /* Vocˆ deve verificar se e a query principal */
            NOT l-record THEN DO:
            ASSIGN l-record = YES.
             
            RUN add-link IN adm-broker-hdl (INPUT h-object,
                                            INPUT "Record":U,
                                            INPUT h_v02es015).
         END.
          
         IF INDEX(h-object:PRIVATE-DATA, "vwr") <> 0 AND /* Voce deve verificar se e a viewer principal */
            NOT l-group-assign THEN DO:
            ASSIGN l-group-assign = YES.
             
            RUN add-link IN adm-broker-hdl (INPUT h-object, 
                                            INPUT "Group-Assign":U,
                                            INPUT h_v02es015).
         END.
      END.
       
      RUN dispatch IN h_v02es015 ("initialize":U).

      RUN select-page IN p-wgh-object (INPUT 1).
   END. 
END.

