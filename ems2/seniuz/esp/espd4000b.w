&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-digita 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i XX9999 9.99.99.999}
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF OUTPUT PARAMETER c-nr-pedcli LIKE ped-item.nr-pedcli.
DEF OUTPUT PARAMETER l-todos-itens AS LOG.
DEF OUTPUT PARAMETER l-copia-reservas AS LOG.
DEF OUTPUT PARAMETER l-aberto AS LOG.
DEF OUTPUT PARAMETER l-atendido-parcial AS LOG.
DEF OUTPUT PARAMETER l-atendido-total AS LOG.
DEF OUTPUT PARAMETER l-pendente AS LOG.
DEF OUTPUT PARAMETER l-suspenso AS LOG.
DEF OUTPUT PARAMETER l-cancelado AS LOG.
DEF OUTPUT PARAMETER l-fat-balcao AS LOG.
DEF OUTPUT PARAMETER c-it-codigo-ini AS CHAR.                              
DEF OUTPUT PARAMETER c-it-codigo-fin AS CHAR.                              
DEF OUTPUT PARAMETER c-cod-refer-ini AS CHAR.                              
DEF OUTPUT PARAMETER c-cod-refer-fin AS CHAR.
DEF OUTPUT PARAMETER l-copia-observ AS LOG.
DEF OUTPUT PARAMETER l-copia-container AS LOG.
DEF OUTPUT PARAMETER l-subst-de-pedido AS LOG.
DEF OUTPUT PARAMETER l-ok AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 IMAGE-1 IMAGE-2 IMAGE-51 IMAGE-52 ~
RECT-45 RECT-46 fi-nr-pedcli tg-subst-pedido tg-todos-itens ~
tg-copia-reservas tg-copia-container tg-copia-observ fi-it-codigo-ini ~
fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin tg-aberto tg-suspenso ~
tg-atendido-parcial tg-cancelado tg-atendido-total tg-fat-balcao ~
tg-pendente bt-ok bt-cancelar bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-nr-pedcli fi-nome-abrev tg-subst-pedido ~
tg-todos-itens tg-copia-reservas tg-copia-container tg-copia-observ ~
fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin ~
tg-aberto tg-suspenso tg-atendido-parcial tg-cancelado tg-atendido-total ~
tg-fat-balcao tg-pendente 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini ~
fi-cod-refer-fin 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-digita AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

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
     SIZE 28.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli AS CHARACTER FORMAT "x(12)":U 
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

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 69 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-45
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 3.5.

DEFINE RECTANGLE RECT-46
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 68 BY 7.5.

DEFINE VARIABLE tg-aberto AS LOGICAL INITIAL yes 
     LABEL "Aberto" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .83 NO-UNDO.

DEFINE VARIABLE tg-atendido-parcial AS LOGICAL INITIAL no 
     LABEL "Atendido Parcial" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .83 NO-UNDO.

DEFINE VARIABLE tg-atendido-total AS LOGICAL INITIAL yes 
     LABEL "Atendido Total" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .83 NO-UNDO.

DEFINE VARIABLE tg-cancelado AS LOGICAL INITIAL no 
     LABEL "Cancelado" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .83 NO-UNDO.

DEFINE VARIABLE tg-copia-container AS LOGICAL INITIAL no 
     LABEL "Copiar Container" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .5 NO-UNDO.

DEFINE VARIABLE tg-copia-observ AS LOGICAL INITIAL no 
     LABEL "Copiar Observaá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .58 NO-UNDO.

DEFINE VARIABLE tg-copia-reservas AS LOGICAL INITIAL no 
     LABEL "Copiar Reservas" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .58 NO-UNDO.

DEFINE VARIABLE tg-fat-balcao AS LOGICAL INITIAL no 
     LABEL "Faturamento Balá∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE tg-pendente AS LOGICAL INITIAL yes 
     LABEL "Pendente" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .83 NO-UNDO.

DEFINE VARIABLE tg-subst-pedido AS LOGICAL INITIAL no 
     LABEL "Substiuir Pedido" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .58 NO-UNDO.

DEFINE VARIABLE tg-suspenso AS LOGICAL INITIAL no 
     LABEL "Suspendo" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .83 NO-UNDO.

DEFINE VARIABLE tg-todos-itens AS LOGICAL INITIAL yes 
     LABEL "Copiar Todos os Itens" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .54 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-nr-pedcli AT ROW 1.5 COL 14 COLON-ALIGNED WIDGET-ID 12
     fi-nome-abrev AT ROW 1.5 COL 23.57 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     tg-subst-pedido AT ROW 1.75 COL 55 WIDGET-ID 50
     tg-todos-itens AT ROW 2.75 COL 16 WIDGET-ID 46
     tg-copia-reservas AT ROW 2.75 COL 38 WIDGET-ID 38
     tg-copia-container AT ROW 3.75 COL 16 WIDGET-ID 48
     tg-copia-observ AT ROW 3.75 COL 38 WIDGET-ID 36
     fi-it-codigo-ini AT ROW 6.04 COL 13.57 COLON-ALIGNED WIDGET-ID 8
     fi-it-codigo-fin AT ROW 6.04 COL 42.57 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     fi-cod-refer-ini AT ROW 7.04 COL 13.57 COLON-ALIGNED WIDGET-ID 4
     fi-cod-refer-fin AT ROW 7.04 COL 42.57 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     tg-aberto AT ROW 8.5 COL 15.72 WIDGET-ID 28
     tg-suspenso AT ROW 8.5 COL 44.72 WIDGET-ID 44
     tg-atendido-parcial AT ROW 9.5 COL 15.72 WIDGET-ID 30
     tg-cancelado AT ROW 9.5 COL 44.72 WIDGET-ID 34
     tg-atendido-total AT ROW 10.5 COL 15.72 WIDGET-ID 32
     tg-fat-balcao AT ROW 10.5 COL 44.72 WIDGET-ID 40
     tg-pendente AT ROW 11.5 COL 15.72 WIDGET-ID 42
     bt-ok AT ROW 13.38 COL 2
     bt-cancelar AT ROW 13.38 COL 13
     bt-ajuda AT ROW 13.38 COL 59.14
     " Seleá∆o dos Itens" VIEW-AS TEXT
          SIZE 13.57 BY .54 AT ROW 5.25 COL 3 WIDGET-ID 26
     RECT-1 AT ROW 13.21 COL 1
     IMAGE-1 AT ROW 6.04 COL 32.57 WIDGET-ID 14
     IMAGE-2 AT ROW 6.04 COL 40.57 WIDGET-ID 16
     IMAGE-51 AT ROW 7.04 COL 32.57 WIDGET-ID 18
     IMAGE-52 AT ROW 7.04 COL 40.57 WIDGET-ID 20
     RECT-45 AT ROW 1.25 COL 2 WIDGET-ID 22
     RECT-46 AT ROW 5.5 COL 2 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 70 BY 14.08
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Digitacao
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-digita ASSIGN
         HIDDEN             = YES
         TITLE              = "Copia Itens de um Pedido"
         COLUMN             = 40.29
         ROW                = 6.54
         HEIGHT             = 13.83
         WIDTH              = 69.57
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-digita 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-digit.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-digita
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fi-cod-refer-fin IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-cod-refer-ini IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-fin IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-it-codigo-ini IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi-nome-abrev IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Copia Itens de um Pedido */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Copia Itens de um Pedido */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-digita
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-digita
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
   FIND ped-venda WHERE 
        ped-venda.nr-pedcli = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli NO-LOCK NO-ERROR.
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
          l-copia-reservas   = INPUT FRAME {&FRAME-NAME} tg-copia-reservas
          l-aberto           = INPUT FRAME {&FRAME-NAME} tg-aberto
          l-atendido-parcial = INPUT FRAME {&FRAME-NAME} tg-atendido-parcial
          l-atendido-total   = INPUT FRAME {&FRAME-NAME} tg-atendido-total
          l-pendente         = INPUT FRAME {&FRAME-NAME} tg-pendente
          l-suspenso         = INPUT FRAME {&FRAME-NAME} tg-suspenso
          l-cancelado        = INPUT FRAME {&FRAME-NAME} tg-cancelado
          l-fat-balcao       = INPUT FRAME {&FRAME-NAME} tg-fat-balcao
          l-copia-observ     = INPUT FRAME {&FRAME-NAME} tg-copia-observ
          l-copia-container  = INPUT FRAME {&FRAME-NAME} tg-copia-container
          l-subst-de-pedido  = INPUT FRAME {&FRAME-NAME} tg-subst-pedido
          l-ok               = YES.

   APPLY "close":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-pedcli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-pedcli w-digita
ON LEAVE OF fi-nr-pedcli IN FRAME F-Main /* Nß Pedido */
DO:
   FIND ped-venda WHERE 
        ped-venda.nr-pedcli = INPUT FRAME {&FRAME-NAME} fi-nr-pedcli NO-LOCK NO-ERROR.
   IF NOT AVAIL ped-venda THEN DO.
      MESSAGE 'Pedido de Venda n∆o Cadastrado...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
   END.

   ASSIGN tg-subst-pedido:SENSITIVE = YES.
   IF ped-venda.cod-sit-ped <> 1 THEN DO.
      ASSIGN tg-subst-pedido:SCREEN-VALUE = 'NO'.
      ASSIGN tg-subst-pedido:SENSITIVE = NO.
   END.

   ASSIGN fi-nome-abrev:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda.nome-abrev.
 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-pedcli w-digita
ON MOUSE-SELECT-DBLCLICK OF fi-nr-pedcli IN FRAME F-Main /* Nß Pedido */
DO:
  {include/zoomvar.i &prog-zoom = dizoom/z01di159.w
                     &campo     = fi-nr-pedcli
                     &campozoom = nr-pedcli}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-subst-pedido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-subst-pedido w-digita
ON VALUE-CHANGED OF tg-subst-pedido IN FRAME F-Main /* Substiuir Pedido */
DO:
   ASSIGN tg-todos-itens:SCREEN-VALUE = 'YES'
          tg-copia-observ:SCREEN-VALUE = 'NO'
          tg-copia-reservas:SCREEN-VALUE = 'NO'
          tg-copia-container:SCREEN-VALUE = 'NO'.

   ASSIGN tg-todos-itens:SENSITIVE = YES
          tg-copia-observ:SENSITIVE = YES
          tg-copia-reservas:SENSITIVE = YES
          tg-copia-container:SENSITIVE = YES.

   IF SELF:SCREEN-VALUE = 'YES' THEN DO.
      MESSAGE 'ATENÄ«O... Esse Pedido ser† Cancelado...' SKIP
              'Certifique se realmente deseja substitu°-lo...'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

      ASSIGN tg-todos-itens:SCREEN-VALUE = 'YES'
             tg-copia-observ:SCREEN-VALUE = 'YES'
             tg-copia-reservas:SCREEN-VALUE = 'YES'
             tg-copia-container:SCREEN-VALUE = 'YES'.

      ASSIGN tg-todos-itens:SENSITIVE = NO
             tg-copia-observ:SENSITIVE = NO
             tg-copia-reservas:SENSITIVE = NO
             tg-copia-container:SENSITIVE = NO.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-todos-itens
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-todos-itens w-digita
ON VALUE-CHANGED OF tg-todos-itens IN FRAME F-Main /* Copiar Todos os Itens */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */
fi-nr-pedcli:LOAD-MOUSE-POINTER("image/lupa.cur")    IN FRAME {&FRAME-NAME}.

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-digita  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-digita  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-digita  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
  THEN DELETE WIDGET w-digita.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-digita  _DEFAULT-ENABLE
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
  DISPLAY fi-nr-pedcli fi-nome-abrev tg-subst-pedido tg-todos-itens 
          tg-copia-reservas tg-copia-container tg-copia-observ fi-it-codigo-ini 
          fi-it-codigo-fin fi-cod-refer-ini fi-cod-refer-fin tg-aberto 
          tg-suspenso tg-atendido-parcial tg-cancelado tg-atendido-total 
          tg-fat-balcao tg-pendente 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE RECT-1 IMAGE-1 IMAGE-2 IMAGE-51 IMAGE-52 RECT-45 RECT-46 fi-nr-pedcli 
         tg-subst-pedido tg-todos-itens tg-copia-reservas tg-copia-container 
         tg-copia-observ fi-it-codigo-ini fi-it-codigo-fin fi-cod-refer-ini 
         fi-cod-refer-fin tg-aberto tg-suspenso tg-atendido-parcial 
         tg-cancelado tg-atendido-total tg-fat-balcao tg-pendente bt-ok 
         bt-cancelar bt-ajuda 
      WITH FRAME F-Main IN WINDOW w-digita.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-digita.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-digita 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-digita 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-digita 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut9000.i "ESPD4000B" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  APPLY "VALUE-CHANGED" TO  tg-todos-itens IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-digita  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this Digitacao, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-digita 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

