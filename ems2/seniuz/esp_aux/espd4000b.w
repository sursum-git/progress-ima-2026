&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*:T*******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESPD4000B 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
  
DEF INPUT-OUTPUT PARAMETER c-nr-pedcli LIKE ped-item.nr-pedcli.
DEF INPUT-OUTPUT PARAMETER l-todos-itens AS LOG.
DEF INPUT-OUTPUT PARAMETER l-aberto AS LOG.
DEF INPUT-OUTPUT PARAMETER l-atendido-parcial AS LOG.
DEF INPUT-OUTPUT PARAMETER l-atendido-total AS LOG.
DEF INPUT-OUTPUT PARAMETER l-pendente AS LOG.
DEF INPUT-OUTPUT PARAMETER l-suspenso AS LOG.
DEF INPUT-OUTPUT PARAMETER l-cancelado AS LOG.
DEF INPUT-OUTPUT PARAMETER l-fat-balcao AS LOG.
DEF INPUT-OUTPUT PARAMETER c-it-codigo-ini AS CHAR.                              
DEF INPUT-OUTPUT PARAMETER c-it-codigo-fin AS CHAR.                              
DEF INPUT-OUTPUT PARAMETER c-cod-refer-ini AS CHAR.                              
DEF INPUT-OUTPUT PARAMETER c-cod-refer-fin AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-nr-pedcli tg-todos-itens tg-aberto ~
tg-suspenso tg-atendido-parcial tg-cancelado tg-atendido-total ~
tg-fat-balcao tg-pendente fi-it-codigo-ini fi-it-codigo-fin ~
fi-cod-refer-ini fi-cod-refer-fin bt-ok bt-cancela bt-ajuda IMAGE-1 IMAGE-2 ~
IMAGE-51 IMAGE-52 RECT-45 RECT-46 RECT-49 rt-buttom 
&Scoped-Define DISPLAYED-OBJECTS fi-nr-pedcli fi-nome-abrev tg-todos-itens ~
tg-aberto tg-suspenso tg-atendido-parcial tg-cancelado tg-atendido-total ~
tg-fat-balcao tg-pendente fi-it-codigo-ini fi-it-codigo-fin ~
fi-cod-refer-ini fi-cod-refer-fin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini ~
fi-cod-refer-fin 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

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

DEFINE VARIABLE fi-cod-refer-fin AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer-ini AS CHARACTER FORMAT "X(8)":U 
     LABEL "Referància" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-fin AS CHARACTER FORMAT "X(16)":U INITIAL "ZZZZZZZZZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo-ini AS CHARACTER FORMAT "X(16)":U 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-abrev AS CHARACTER FORMAT "X(13)":U 
     VIEW-AS FILL-IN 
     SIZE 18.29 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli AS CHARACTER FORMAT "999999999":U 
     LABEL "Nß Pedido" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-2
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-51
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-52
     FILENAME "image\im-las":U
     SIZE 3 BY .79.

DEFINE RECTANGLE RECT-45
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 66 BY 1.63.

DEFINE RECTANGLE RECT-46
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 66 BY 5.

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 66 BY 2.75.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 68 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE tg-aberto AS LOGICAL INITIAL yes 
     LABEL "Aberto" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .83 NO-UNDO.

DEFINE VARIABLE tg-atendido-parcial AS LOGICAL INITIAL yes 
     LABEL "Atendido Parcial" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .83 NO-UNDO.

DEFINE VARIABLE tg-atendido-total AS LOGICAL INITIAL yes 
     LABEL "Atendido Total" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .83 NO-UNDO.

DEFINE VARIABLE tg-cancelado AS LOGICAL INITIAL yes 
     LABEL "Cancelado" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .83 NO-UNDO.

DEFINE VARIABLE tg-fat-balcao AS LOGICAL INITIAL yes 
     LABEL "Faturamento Balá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE tg-pendente AS LOGICAL INITIAL yes 
     LABEL "Pendente" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .83 NO-UNDO.

DEFINE VARIABLE tg-suspenso AS LOGICAL INITIAL yes 
     LABEL "Suspendo" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .83 NO-UNDO.

DEFINE VARIABLE tg-todos-itens AS LOGICAL INITIAL yes 
     LABEL "Copiar Todos os Itens" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .88 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-nr-pedcli AT ROW 1.92 COL 12 COLON-ALIGNED
     fi-nome-abrev AT ROW 1.92 COL 21.57 COLON-ALIGNED NO-LABEL
     tg-todos-itens AT ROW 1.92 COL 46.14
     tg-aberto AT ROW 4.5 COL 16
     tg-suspenso AT ROW 4.5 COL 41
     tg-atendido-parcial AT ROW 5.5 COL 16
     tg-cancelado AT ROW 5.5 COL 41
     tg-atendido-total AT ROW 6.5 COL 16
     tg-fat-balcao AT ROW 6.5 COL 41
     tg-pendente AT ROW 7.5 COL 16
     fi-it-codigo-ini AT ROW 9.75 COL 14 COLON-ALIGNED
     fi-it-codigo-fin AT ROW 9.75 COL 43 COLON-ALIGNED NO-LABEL
     fi-cod-refer-ini AT ROW 10.75 COL 14 COLON-ALIGNED
     fi-cod-refer-fin AT ROW 10.75 COL 43 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 12.54 COL 3
     bt-cancela AT ROW 12.54 COL 14
     bt-ajuda AT ROW 12.54 COL 59
     IMAGE-1 AT ROW 9.75 COL 33
     IMAGE-2 AT ROW 9.75 COL 41
     IMAGE-51 AT ROW 10.75 COL 33
     IMAGE-52 AT ROW 10.75 COL 41
     RECT-45 AT ROW 1.58 COL 2.86
     RECT-46 AT ROW 3.79 COL 3
     RECT-49 AT ROW 9.25 COL 3
     rt-buttom AT ROW 12.29 COL 2
     " Situaá∆o dos Itens" VIEW-AS TEXT
          SIZE 13.57 BY .54 AT ROW 3.5 COL 5.86
     " Seleá∆o" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 8.96 COL 5.86
     SPACE(57.99) SKIP(4.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Copia Itens de um Pedido"
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/d-dialog.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   L-To-R                                                               */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-cod-refer-fin IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-refer-ini IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-fin IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-ini IN FRAME D-Dialog
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-nome-abrev IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Copia Itens de um Pedido */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda D-Dialog
ON CHOOSE OF bt-ajuda IN FRAME D-Dialog /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
  FIND ped-venda WHERE ped-venda.nr-pedcli = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli NO-LOCK NO-ERROR.
  IF NOT AVAIL ped-venda THEN DO:
     MESSAGE "Pedido n∆o Cadastrado ! ! !" VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO fi-nr-pedcli IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
  ASSIGN c-nr-pedcli        = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli
         c-it-codigo-ini    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-ini
         c-it-codigo-fin    = INPUT FRAME {&FRAME-NAME} fi-it-codigo-fin
         c-cod-refer-ini    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-ini
         c-cod-refer-fin    = INPUT FRAME {&FRAME-NAME} fi-cod-refer-fin
         l-todos-itens      = INPUT FRAME {&FRAME-NAME} tg-todos-itens
         l-aberto           = INPUT FRAME {&FRAME-NAME} tg-aberto
         l-atendido-parcial = INPUT FRAME {&FRAME-NAME} tg-atendido-parcial
         l-atendido-total   = INPUT FRAME {&FRAME-NAME} tg-atendido-total
         l-pendente         = INPUT FRAME {&FRAME-NAME} tg-pendente
         l-suspenso         = INPUT FRAME {&FRAME-NAME} tg-suspenso
         l-cancelado        = INPUT FRAME {&FRAME-NAME} tg-cancelado
         l-fat-balcao       = INPUT FRAME {&FRAME-NAME} tg-fat-balcao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-pedcli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-pedcli D-Dialog
ON LEAVE OF fi-nr-pedcli IN FRAME D-Dialog /* Nß Pedido */
DO:
  ASSIGN fi-nome-abrev:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.
  FIND ped-venda WHERE ped-venda.nr-pedcli = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli NO-LOCK NO-ERROR.
  IF AVAIL ped-venda THEN
     ASSIGN fi-nome-abrev:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda.nome-abrev.
 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-todos-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-todos-itens D-Dialog
ON VALUE-CHANGED OF tg-todos-itens IN FRAME D-Dialog /* Copiar Todos os Itens */
DO:
  IF SELF:INPUT-VALUE = YES THEN DO.
     DISABLE {&list-1} WITH FRAME {&FRAME-NAME}.
     ASSIGN fi-it-codigo-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL(" ", 16)
            fi-it-codigo-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL("Z", 16)
            fi-cod-refer-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL(" ", 8)
            fi-cod-refer-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL("Z", 8).

  END.
  ELSE DO.
     ENABLE {&list-1} WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY fi-nr-pedcli fi-nome-abrev tg-todos-itens tg-aberto tg-suspenso 
          tg-atendido-parcial tg-cancelado tg-atendido-total tg-fat-balcao 
          tg-pendente fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini 
          fi-cod-refer-fin 
      WITH FRAME D-Dialog.
  ENABLE fi-nr-pedcli tg-todos-itens tg-aberto tg-suspenso tg-atendido-parcial 
         tg-cancelado tg-atendido-total tg-fat-balcao tg-pendente 
         fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin 
         bt-ok bt-cancela bt-ajuda IMAGE-1 IMAGE-2 IMAGE-51 IMAGE-52 RECT-45 
         RECT-46 RECT-49 rt-buttom 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy D-Dialog 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut9000.i "ESPD4000B" "2.04.00.000"}

  ASSIGN fi-nr-pedcli        = c-nr-pedcli
         tg-todos-itens      = l-todos-itens
         tg-aberto           = l-aberto 
         tg-atendido-parcial = l-atendido-parcial
         tg-atendido-total   = l-atendido-total
         tg-pendente         = l-pendente 
         tg-suspenso         = l-suspenso
         tg-cancelado        = l-cancelado
         tg-fat-balcao       = l-fat-balcao 
         fi-it-codigo-ini    = c-it-codigo-ini 
         fi-it-codigo-fin    = c-it-codigo-fin 
         fi-cod-refer-ini    = c-cod-refer-ini 
         fi-cod-refer-fin    = c-cod-refer-fin.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  APPLY "VALUE-CHANGED" TO  tg-todos-itens.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
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

