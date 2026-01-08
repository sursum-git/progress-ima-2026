/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions **********************************************/
DEFINE NEW GLOBAL SHARED VAR adm-broker-hdl   AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h-folder         AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR h_v03es015       AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR c-seg-usuario    AS CHAR NO-UNDO.

/* Variable Definitions *****************************************************/
DEFINE VAR c-folder       AS CHARACTER NO-UNDO.
DEFINE VAR c-usuarios     AS CHARACTER NO-UNDO.
DEFINE VAR c-objects      AS CHARACTER NO-UNDO.
DEFINE VAR h-object       AS HANDLE    NO-UNDO.
DEFINE VAR i-objects      AS INTEGER   NO-UNDO.
DEFINE VAR l-record       AS LOGICAL   NO-UNDO INITIAL NO.
DEFINE VAR l-group-assign AS LOGICAL   NO-UNDO INITIAL NO.

/* Variable Definitions *****************************************************/

/* Main Block ***************************************************************/

/*
MESSAGE "p-wgh-frame  " p-wgh-frame         SKIP
        "p-ind-event  " p-ind-event         SKIP
        "p-ind-object " p-ind-object        SKIP
        "p-cod-table  " STRING(p-cod-table) SKIP 
        "p-row-table  " STRING(P-row-table)    SKIP
        "c-objeto     " c-objects             SKIP
        "p-wgh-object " p-wgh-object:FILE-NAME SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/

IF p-ind-event = "INITIALIZE" AND
   p-ind-object = "CONTAINER" THEN DO:
   RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                          INPUT "PAGE-SOURCE":U,
                                          OUTPUT c-folder).
   ASSIGN h-folder = WIDGET-HANDLE(c-folder) NO-ERROR.
    
   IF VALID-HANDLE(h-folder) THEN DO:
      RUN create-folder-page IN h-folder (INPUT 3, INPUT "Param PCP":U).
      RUN create-folder-label IN h-folder (INPUT 3, INPUT "Param PCP":U).
       
      RUN select-page IN p-wgh-object (INPUT 3).
       
      RUN init-object IN p-wgh-object (INPUT "esvwr/v03es015.w":U, /* Nome do Objeto Viewer */
                                       INPUT p-wgh-frame,
                                       INPUT "Layout = ":U,
                                       OUTPUT h_v03es015).
       
      RUN set-position IN h_v03es015 ( 7.70, 3.10).
       
      RUN get-link-handle IN adm-broker-hdl (INPUT p-wgh-object,
                                             INPUT "CONTAINER-TARGET":U,
                                             OUTPUT c-objects).
       /* 
      /* Desabilita Folders e Habitilita se usuario for do Grupo PCP */
      RUN disable-folder-page IN h-folder (INPUT 3).
      ASSIGN c-usuarios = "".
      FOR EACH usuar_grp_usuar WHERE 
               usuar_grp_usuar.cod_grp_usuar = "PCP" NO-LOCK. /* Grupo 'REIMPRESSÇO ETIQUETA CRU' */
              ASSIGN c-usuarios = c-usuarios + usuar_grp_usuar.cod_usuar + ','.
      END.
      IF LOOKUP(c-seg-usuario,c-usuarios) > 0 OR
         c-seg-usuario = 'super' THEN
         RUN enable-folder-page IN h-folder (INPUT 3).
      */

      DO i-objects = 1 TO NUM-ENTRIES(c-objects):
         ASSIGN h-object = WIDGET-HANDLE(entry(i-objects, c-objects)).
          
         IF INDEX(h-object:PRIVATE-DATA, "qry") <> 0 AND  /* Vocˆ deve verificar se e a query principal */
            NOT l-record THEN DO:
            ASSIGN l-record = YES.
             
            RUN add-link IN adm-broker-hdl (INPUT h-object,
                                            INPUT "Record":U,
                                            INPUT h_v03es015).
         END.
          
         IF INDEX(h-object:PRIVATE-DATA, "vwr") <> 0 AND /* Voce deve verificar se e a viewer principal */
            h-object:NAME = 'invwr/v12in172.w' AND 
            NOT l-group-assign  THEN DO:

            ASSIGN l-group-assign = YES.
             
            RUN add-link IN adm-broker-hdl (INPUT h-object, 
                                            INPUT "Group-Assign":U,
                                            INPUT h_v03es015).
         END.
      END.
       
      RUN dispatch IN h_v03es015 ("initialize":U).

      RUN select-page IN p-wgh-object (INPUT 1).
   END. 
END.

  
