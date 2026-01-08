&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME d-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS d-vapara 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i G01ES021 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
define output parameter     p-row-tabela    as rowid    no-undo.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartVaPara
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME d-vapara

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS mov-est-acbm.data-mov mov-est-acbm.num-lote ~
mov-est-acbm.it-codigo mov-est-acbm.cod-refer 
&Scoped-define ENABLED-TABLES mov-est-acbm
&Scoped-define FIRST-ENABLED-TABLE mov-est-acbm
&Scoped-Define ENABLED-OBJECTS bt-ok bt-cancela bt-ajuda rt-button 
&Scoped-Define DISPLAYED-FIELDS mov-est-acbm.data-mov mov-est-acbm.num-lote ~
mov-est-acbm.it-codigo mov-est-acbm.cod-refer 
&Scoped-define DISPLAYED-TABLES mov-est-acbm
&Scoped-define FIRST-DISPLAYED-TABLE mov-est-acbm
&Scoped-Define DISPLAYED-OBJECTS fi-desc-item fi-desc-refer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-bt-ajuda 
       MENU-ITEM mi-sobre       LABEL "Sobre..."      .


/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-refer AS CHARACTER FORMAT "x(32)" 
     VIEW-AS FILL-IN 
     SIZE 27 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 70 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME d-vapara
     mov-est-acbm.data-mov AT ROW 1.29 COL 14.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .88
     mov-est-acbm.num-lote AT ROW 2.29 COL 14.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.86 BY .88
     mov-est-acbm.it-codigo AT ROW 3.29 COL 14.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.14 BY .88
     fi-desc-item AT ROW 3.29 COL 31.86 COLON-ALIGNED NO-LABEL
     mov-est-acbm.cod-refer AT ROW 4.29 COL 14.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.86 BY .88
     fi-desc-refer AT ROW 4.29 COL 23.57 COLON-ALIGNED HELP
          "Descriá∆o da referància" NO-LABEL
     bt-ok AT ROW 5.67 COL 2.14
     bt-cancela AT ROW 5.67 COL 13
     bt-ajuda AT ROW 5.67 COL 59.86
     rt-button AT ROW 5.42 COL 1
     SPACE(1.13) SKIP(0.11)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "V† Para Movto Estoque Acabados"
         DEFAULT-BUTTON bt-ok CANCEL-BUTTON bt-cancela.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartVaPara
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB d-vapara 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-vapara.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX d-vapara
   L-To-R                                                               */
ASSIGN 
       FRAME d-vapara:SCROLLABLE       = FALSE
       FRAME d-vapara:HIDDEN           = TRUE.

ASSIGN 
       bt-ajuda:POPUP-MENU IN FRAME d-vapara       = MENU POPUP-MENU-bt-ajuda:HANDLE.

/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME d-vapara
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-refer IN FRAME d-vapara
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX d-vapara
/* Query rebuild information for DIALOG-BOX d-vapara
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX d-vapara */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME d-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL d-vapara d-vapara
ON GO OF FRAME d-vapara /* V† Para Movto Estoque Acabados */
DO:
  find mov-est-acbm no-lock 
      where mov-est-acbm.data-mov  = input frame {&frame-name} mov-est-acbm.data-mov 
        AND mov-est-acbm.num-lote  = input frame {&frame-name} mov-est-acbm.num-lote
        AND mov-est-acbm.it-codigo = input frame {&frame-name} mov-est-acbm.it-codigo no-error.
  IF NOT AVAIL mov-est-acbm THEN DO:
     {utp/ut-table.i espec mov-est-acbm 1}
     run utp/ut-msgs.p (input "show":U,
                        input 2,
                        input RETURN-VALUE).
     return no-apply.
  END.
  ASSIGN p-row-tabela = ROWID(mov-est-acbm).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL d-vapara d-vapara
ON WINDOW-CLOSE OF FRAME d-vapara /* V† Para Movto Estoque Acabados */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda d-vapara
ON CHOOSE OF bt-ajuda IN FRAME d-vapara /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-est-acbm.cod-refer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-est-acbm.cod-refer d-vapara
ON LEAVE OF mov-est-acbm.cod-refer IN FRAME d-vapara /* Referencia */
DO:
   FIND referencia WHERE referencia.cod-refer = INPUT FRAME {&FRAME-NAME} mov-est-acbm.cod-refer
                   NO-LOCK NO-ERROR.
   IF NOT AVAIL referencia THEN DO:
      MESSAGE "Referància inv†lida." VIEW-AS ALERT-BOX.
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN fi-desc-refer:SCREEN-VALUE IN FRAME {&FRAME-NAME} = referencia.descricao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mov-est-acbm.it-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mov-est-acbm.it-codigo d-vapara
ON LEAVE OF mov-est-acbm.it-codigo IN FRAME d-vapara /* Item */
DO:
   FIND item WHERE item.it-codigo = INPUT FRAME {&FRAME-NAME} mov-est-acbm.it-codigo NO-LOCK NO-ERROR.
   IF NOT AVAIL item THEN DO.
      MESSAGE "Item n∆o Cadastrado..." VIEW-AS ALERT-BOX.
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = item.desc-item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre d-vapara
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK d-vapara 


/* ***************************  Main Block  *************************** */

assign p-row-tabela = ?.

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects d-vapara  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available d-vapara  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI d-vapara  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME d-vapara.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI d-vapara  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY fi-desc-item fi-desc-refer 
      WITH FRAME d-vapara.
  IF AVAILABLE mov-est-acbm THEN 
    DISPLAY mov-est-acbm.data-mov mov-est-acbm.num-lote mov-est-acbm.it-codigo 
          mov-est-acbm.cod-refer 
      WITH FRAME d-vapara.
  ENABLE mov-est-acbm.data-mov mov-est-acbm.num-lote mov-est-acbm.it-codigo 
         mov-est-acbm.cod-refer bt-ok bt-cancela bt-ajuda rt-button 
      WITH FRAME d-vapara.
  VIEW FRAME d-vapara.
  {&OPEN-BROWSERS-IN-QUERY-d-vapara}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy d-vapara 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .
  {include/i-logfin.i}

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize d-vapara 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut9000.i "G01ES021" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records d-vapara  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartVaPara, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed d-vapara 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

