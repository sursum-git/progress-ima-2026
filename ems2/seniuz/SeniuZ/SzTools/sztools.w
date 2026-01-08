&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */


CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE NEW GLOBAL SHARED VAR adm-broker-hdl    AS HANDLE  NO-UNDO.

/* Temp Table Definitions ---                                           */
DEF TEMP-TABLE tt-menus 
    FIELD i-seq            AS INT  FORMAT "999" 
    FIELD c-tipo           AS CHAR FORMAT "x"
    FIELD c-nom-men        AS CHAR FORMAT "x(20)"
    FIELD c-nom-rot        AS CHAR FORMAT "x(20)"
    FIELD c-nom-sub        AS CHAR FORMAT "x(20)"
    FIELD c-nom-prog       AS CHAR FORMAT "x(10)"
    FIELD c-cod-prog       AS CHAR FORMAT "x(10)"
    FIELD c-icone          AS CHAR FORMAT "x" 
    FIELD c-image          AS CHAR FORMAT "x(20)"
    FIELD c-tooltip        AS CHAR FORMAT "x(50)".

DEF BUFFER b-rotinas   FOR tt-menus.
DEF BUFFER b-submenus  FOR tt-menus.
DEF BUFFER b-programas FOR tt-menus.

/* Local Variable Definitions ---                                       */
DEF VAR h-menu-bar  AS HANDLE.
DEF VAR h-menu-prin AS HANDLE.
DEF VAR h-menu      AS HANDLE.
DEF VAR h-rot       AS HANDLE.
DEF VAR h-sub       AS HANDLE.
DEF VAR h-prog      AS HANDLE.

DEF VAR wh-bt AS HANDLE EXTENT 100.
DEF VAR i-qt-bt AS INT.
DEF VAR i-bt AS INT.
DEF VAR i-col AS DEC.
DEF VAR i-row AS DEC.

DEF VAR c-label   AS CHAR.
DEF VAR c-tooltip AS CHAR INIT "Executa Programa....".
DEF VAR c-programa AS CHAR INIT "cdp/cd0204.w".

DEF VAR h-obj-men AS HANDLE.
DEF VAR h-obj-rot AS HANDLE.
DEF VAR h-obj-sub AS HANDLE.
DEF VAR h-obj-prog AS HANDLE.

/********************** Anderson 17-03-2010 ***********************/
DEFINE VARIABLE var-choose AS CHARACTER   NO-UNDO.


DEFINE VARIABLE i-handle  AS INTEGER NO-UNDO.
PROCEDURE FindWindowA EXTERNAL "USER32.DLL":
    DEFINE INPUT  PARAMETER intClassName AS LONG.
    DEFINE INPUT  PARAMETER chrCaption   AS CHARACTER.
    DEFINE RETURN PARAMETER intHandle    AS LONG.
END PROCEDURE.

RUN FindWindowA (0, "SzTools", OUTPUT i-handle).
IF i-handle <> 0 THEN 
   RETURN NO-APPLY.

/* Include para definir a hWenController */
/* Anderson Fagner 17/03/2010 */
{include\i-wendef.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-main
     RECT-1 AT ROW 1.13 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 27.43 BY 7.83.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "SzTools"
         HEIGHT             = 1.79
         WIDTH              = 19
         MAX-HEIGHT         = 30
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 30
         VIRTUAL-WIDTH      = 146.14
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("adeicon/trans%.ico":U) THEN
    MESSAGE "Unable to load icon: adeicon/trans%.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-main
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* SzTools */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* SzTools */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.


/*RUN btb/btb910za.r.*/


RUN pi-importa.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   RUN enable_UI.

   CREATE MENU h-menu-bar.

   CREATE SUB-MENU h-menu-prin
          ASSIGN PARENT = h-menu-bar
                 LABEL  = "Menus"
                 SENSITIVE = TRUE.

   CREATE MENU-ITEM h-prog
          ASSIGN PARENT = h-menu-bar
                 LABEL  = "Admin"
                 NAME   = "adm"
                 SENSITIVE = TRUE
          TRIGGERS:
              ON 'CHOOSE':U DO:
                  RUN seniuz\sztools\szt001.w.
                  RUN pi-atualiza-menus.
              END.
          END TRIGGERS.

   CREATE MENU-ITEM h-prog
          ASSIGN PARENT = h-menu-bar
                 NAME   = "sair"
                 LABEL  = "Sair"
                 SENSITIVE = TRUE
          TRIGGERS:
              ON 'CHOOSE':U DO:
                 APPLY 'CLOSE' TO THIS-PROCEDURE.
                 RETURN NO-APPLY.
              END.
          END TRIGGERS.

   RUN pi-menus.

   IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE RECT-1 
      WITH FRAME f-main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-f-main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-atualiza-menus C-Win 
PROCEDURE pi-atualiza-menus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      ASSIGN h-obj-men = h-menu-prin:FIRST-CHILD NO-ERROR.
      DO WHILE VALID-HANDLE(h-obj-men).
         IF h-obj-men:LABEL <> ? THEN DO.
            ASSIGN h-obj-rot = h-obj-men:FIRST-CHILD NO-ERROR.
            DO WHILE VALID-HANDLE(h-obj-rot).
                IF h-obj-rot:LABEL <> ? THEN DO.
                   ASSIGN h-obj-sub = h-obj-rot:FIRST-CHILD NO-ERROR.
                   DO WHILE VALID-HANDLE(h-obj-sub).
                       IF h-obj-sub:LABEL <> ? THEN DO.
                          ASSIGN h-obj-prog = h-obj-sub:FIRST-CHILD NO-ERROR.
                          DO WHILE VALID-HANDLE(h-obj-prog).
                             DELETE WIDGET h-obj-prog.
                             ASSIGN h-obj-prog = h-obj-sub:FIRST-CHILD NO-ERROR.
                          END.
                       END.
                       DELETE WIDGET h-obj-sub.
                       ASSIGN h-obj-sub = h-obj-rot:FIRST-CHILD NO-ERROR.
                   END.
                END.
                DELETE WIDGET h-obj-rot.
                ASSIGN h-obj-rot = h-obj-men:FIRST-CHILD NO-ERROR.
            END.
         END.
         IF h-obj-men:NAME = ? THEN DO.
            DELETE WIDGET h-obj-men.
            ASSIGN h-obj-men = h-menu-prin:FIRST-CHILD.
         END.
         ELSE
            ASSIGN h-obj-men = h-menu-prin:NEXT-SIBLING.
      END.
      DO i-bt = 1 TO 33.
         IF VALID-HANDLE(wh-bt[i-bt]) THEN
            DELETE WIDGET wh-bt[i-bt].
      END.

      RUN pi-importa.
      RUN pi-menus.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-importa C-Win 
PROCEDURE pi-importa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR c-arquivo AS CHAR FORMAT "x(30)".

    EMPTY TEMP-TABLE tt-menus.

    ASSIGN c-arquivo = "m:/ems206/especificos/seniuz/sztools/ini/" + TRIM(OS-GETENV("username")) + ".ini".
    IF SEARCH(c-arquivo) <> ? THEN DO.
        INPUT FROM VALUE(c-arquivo) NO-ECHO CONVERT SOURCE "iso8859-1".
        REPEAT WITH WIDTH 550.
            CREATE tt-menus.
            IMPORT DELIMITER "|" tt-menus.
        END.
        INPUT CLOSE.
    END.
    ASSIGN i-qt-bt = 0.
    FOR EACH tt-menus WHERE
             tt-menus.c-icone = 'S' NO-LOCK.
        ASSIGN i-qt-bt = i-qt-bt + 1.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-menus C-Win 
PROCEDURE pi-menus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH tt-menus WHERE
            tt-menus.c-tipo = 'M' NO-LOCK.
        
      IF tt-menus.c-cod-prog <> '' THEN DO.
          CREATE MENU-ITEM h-menu
                 ASSIGN PARENT = h-menu-prin
                        LABEL  = tt-menus.c-nom-men
                        SENSITIVE = TRUE
                 TRIGGERS:
                     ON "CHOOSE":U PERSISTENT RUN VALUE(tt-menus.c-cod-prog).
                 END TRIGGERS.
       END.
       ELSE DO.
           IF tt-menus.c-nom-men = "Regua" THEN
              CREATE MENU-ITEM h-menu
                     ASSIGN SUBTYPE = "Rule" 
                            PARENT = h-menu-prin.
           ELSE DO.
              CREATE SUB-MENU h-menu
                     ASSIGN PARENT = h-menu-prin
                            LABEL  = tt-menus.c-nom-men
                            SENSITIVE = TRUE.
    
              RUN pi-rotinas.
           END.
       END.
   END.

  /* calcular aqui qtde de butäes */
  ASSIGN C-Win:MENUBAR = h-menu-bar
         C-Win:HEIGHT = (TRUNC(i-qt-bt / 3.1,0) + 1 * 1.4) * 1.4
         FRAME f-main:HEIGHT = C-Win:HEIGHT * 2.

  ASSIGN i-bt = 1.
  FOR EACH tt-menus WHERE
           tt-menus.c-icone = 'S' NO-LOCK.

      IF i-bt = 33 THEN LEAVE.

      ASSIGN i-col = IF i-bt MODULO 3 = 0
                     THEN 14
                     ELSE ((i-bt MODULO 3) * 6) - 4 
             i-row = (TRUNC(i-bt / 3.1,0) + 1) * 1.4.

      CREATE BUTTON wh-bt[i-bt]
             ASSIGN FRAME = FRAME f-main:HANDLE
                    NO-FOCUS = YES
                    FLAT-BUTTON  = YES
                    WIDTH        = 4.86
                    HEIGHT       = 1.25
                    ROW          = i-row
                    COL          = i-col
                    VISIBLE      = YES
                    SENSITIVE    = YES
                    LABEL        = tt-menus.c-image
                    TOOLTIP      = tt-menus.c-tooltip
                    TRIGGERS:
                         ON "CHOOSE":U PERSISTENT RUN VALUE(tt-menus.c-cod-prog).
                    END TRIGGERS.
                    
    
      IF SEARCH(tt-menus.c-image) <> ? THEN
         wh-bt[i-bt]:LOAD-IMAGE-UP(tt-menus.c-image).

      ASSIGN i-bt = i-bt + 1.
  END.
  ASSIGN rect-1:HEIGHT = i-row + 0.3.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-procedimentos C-Win 
PROCEDURE pi-procedimentos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH b-programas WHERE
             b-programas.c-nom-men = b-submenus.c-nom-men AND
             b-programas.c-nom-rot = b-submenus.c-nom-rot AND
             b-programas.c-nom-sub = b-submenus.c-nom-sub AND
             b-programas.c-tipo = "P" NO-LOCK.
        IF b-programas.c-nom-prog = "Regua" THEN
           CREATE MENU-ITEM h-prog
                  ASSIGN SUBTYPE = "Rule" 
                         PARENT = h-sub.
        ELSE DO.
           CREATE MENU-ITEM h-prog
                  ASSIGN PARENT = h-sub
                         LABEL  = b-programas.c-nom-prog
                         SENSITIVE = TRUE
                  TRIGGERS:
                      ON "CHOOSE":U PERSISTENT RUN VALUE(b-programas.c-cod-prog).
                  END TRIGGERS.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-rotinas C-Win 
PROCEDURE pi-rotinas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH b-rotinas WHERE
             b-rotinas.c-nom-men = tt-menus.c-nom-men AND
             b-rotinas.c-tipo  = 'R' NO-LOCK.


       IF b-rotinas.c-cod-prog <> '' THEN DO.
          CREATE MENU-ITEM h-rot
                 ASSIGN PARENT = h-menu
                        LABEL  = b-rotinas.c-nom-rot
                        SENSITIVE = TRUE
                 TRIGGERS:
                     ON "CHOOSE":U PERSISTENT RUN VALUE(b-rotinas.c-cod-prog).
                 END TRIGGERS.
       END.
       ELSE DO.
            IF b-rotinas.c-nom-rot = "Regua" THEN
               CREATE MENU-ITEM h-rot
                      ASSIGN SUBTYPE = "Rule" 
                             PARENT = h-menu.
            ELSE DO.
               CREATE SUB-MENU h-rot
                      ASSIGN PARENT = h-menu
                             LABEL  = b-rotinas.c-nom-rot
                             SENSITIVE = TRUE.
    
               RUN pi-submenus.
            END.
       END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-submenus C-Win 
PROCEDURE pi-submenus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH b-submenus WHERE 
             b-submenus.c-nom-men = b-rotinas.c-nom-men AND
             b-submenus.c-nom-rot = b-rotinas.c-nom-rot AND
             b-submenus.c-tipo = "S" NO-LOCK.
        IF b-submenus.c-cod-prog <> '' THEN DO.
           CREATE MENU-ITEM h-sub
                  ASSIGN PARENT = h-rot
                         LABEL  = b-submenus.c-nom-sub
                         SENSITIVE = TRUE
                  TRIGGERS:
                      ON "CHOOSE":U PERSISTENT RUN VALUE(b-submenus.c-cod-prog).
                  END TRIGGERS.
        END.
        ELSE DO.
            IF b-submenus.c-nom-sub = "Regua" THEN
               CREATE MENU-ITEM h-sub
                      ASSIGN SUBTYPE = "Rule" 
                             PARENT = h-rot.
            ELSE DO.
               CREATE SUB-MENU h-sub
                      ASSIGN PARENT = h-rot
                             LABEL  = b-submenus.c-nom-sub
                             SENSITIVE = TRUE.
    
               RUN pi-procedimentos.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

