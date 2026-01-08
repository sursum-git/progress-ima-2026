&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF TEMP-TABLE tt-usr NO-UNDO
    FIELD banco       AS CHAR 
    FIELD cod-usuario AS CHAR
    FIELD usr         AS INT
    FIELD acesso      AS INT
    FIELD tempo       AS INT
    FIELD maquina     AS CHAR
    FIELD login       AS CHAR
    FIELD ultimo-acesso  AS CHAR
    FIELD marca       AS CHAR
    INDEX indice1 cod-usuario banco.

DEF TEMP-TABLE tt-usr-connect
    FIELD usr         AS INT
    FIELD cod-usuario AS CHAR
    FIELD maquina     AS CHAR
    FIELD marca       AS CHAR
    FIELD ultimo-acesso  AS CHAR
    FIELD login       AS CHAR
    INDEX indice1 cod-usuario.

DEF VAR c-super AS CHAR.
DEF VAR c-close AS CHAR.
DEF VAR c-time  AS CHAR.
DEF VAR c-marcas AS CHAR.

DEF VAR i-ct AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME br-usr

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-usr tt-usr-connect

/* Definitions for BROWSE br-usr                                        */
&Scoped-define FIELDS-IN-QUERY-br-usr tt-usr.banco tt-usr.usr tt-usr.tempo tt-usr.ultimo-acesso   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-usr   
&Scoped-define SELF-NAME br-usr
&Scoped-define QUERY-STRING-br-usr FOR EACH tt-usr WHERE                                  tt-usr.cod-usuario = tt-usr-connect.cod-usuario AND                                  tt-usr.maquina = tt-usr-connect.maquina                                  NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-usr OPEN QUERY {&SELF-NAME} FOR EACH tt-usr WHERE                                  tt-usr.cod-usuario = tt-usr-connect.cod-usuario AND                                  tt-usr.maquina = tt-usr-connect.maquina                                  NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-usr tt-usr
&Scoped-define FIRST-TABLE-IN-QUERY-br-usr tt-usr


/* Definitions for BROWSE br-usr-connect                                */
&Scoped-define FIELDS-IN-QUERY-br-usr-connect tt-usr-connect.cod-usuario tt-usr-connect.maquina tt-usr-connect.login   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-usr-connect   
&Scoped-define SELF-NAME br-usr-connect
&Scoped-define OPEN-QUERY-br-usr-connect RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-usr-connect WHERE                                  LOOKUP(tt-usr-connect.marca, ~
      c-marcas) > 0                                  NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-usr-connect tt-usr-connect
&Scoped-define FIRST-TABLE-IN-QUERY-br-usr-connect tt-usr-connect


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-br-usr}~
    ~{&OPEN-QUERY-br-usr-connect}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-14 RECT-15 RECT-16 RECT-17 RECT-18 ~
bt-exit tg-run tg-idle tg-out tg-sup br-usr-connect br-usr fi-exit bt-super ~
bt-mata-usr bt-atualiza sl-time 
&Scoped-Define DISPLAYED-OBJECTS tg-run tg-idle tg-out tg-sup fi-hora-ini ~
fi-hora-atu fi-exit sl-time fi-usr-all fi-usr-sup fi-usr-idle fi-usr-run ~
fi-usr-out 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-atualiza 
     IMAGE-UP FILE "image/im-thu.bmp":U
     LABEL "Button 1" 
     SIZE 4.14 BY 1.17 TOOLTIP "Atualiza Dados".

DEFINE BUTTON bt-exit AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "Button 2" 
     SIZE 7 BY 1.13.

DEFINE BUTTON bt-mata-usr 
     IMAGE-UP FILE "image/desconecta.bmp":U
     IMAGE-INSENSITIVE FILE "image/ii-desconecta.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.13 TOOLTIP "Desconectar Usu rio".

DEFINE BUTTON bt-super 
     IMAGE-UP FILE "image/super.jpg":U
     LABEL "Button 1" 
     SIZE 4 BY 1.13 TOOLTIP "Setar como Super Usuario".

DEFINE VARIABLE fi-exit AS CHARACTER FORMAT "99:99:99":U 
     LABEL "Finalizar Monitor" 
     VIEW-AS FILL-IN 
     SIZE 7.72 BY .88 NO-UNDO.

DEFINE VARIABLE fi-hora-atu AS CHARACTER FORMAT "X(256)":U 
     LABEL "Data/Hora Atual" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-hora-ini AS CHARACTER FORMAT "X(256)":U 
     LABEL "Monitor Inicializado em" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-usr-all AS INTEGER FORMAT ">>,>>9":U INITIAL 0 
     LABEL "Usuarios Conectados" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-usr-idle AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Parados" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     BGCOLOR 4 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-usr-out AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Desconectando" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     BGCOLOR 12 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-usr-run AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Processando" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     BGCOLOR 2 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE VARIABLE fi-usr-sup AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Super Usu rios" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     BGCOLOR 9 FGCOLOR 15 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 3.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 106 BY 1.5
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 57 BY 19.75.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 10.75.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48 BY 8.75.

DEFINE VARIABLE sl-time AS INTEGER INITIAL 10 
     VIEW-AS SLIDER MIN-VALUE 1 MAX-VALUE 60 HORIZONTAL 
     TIC-MARKS BOTTOM FREQUENCY 10
     SIZE 19.72 BY 2.5
     FONT 0 NO-UNDO.

DEFINE VARIABLE tg-idle AS LOGICAL INITIAL yes 
     LABEL "Parados" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.86 BY .83
     BGCOLOR 8 FGCOLOR 4 FONT 6 NO-UNDO.

DEFINE VARIABLE tg-out AS LOGICAL INITIAL yes 
     LABEL "Desconectando" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .83
     BGCOLOR 8 FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE tg-run AS LOGICAL INITIAL yes 
     LABEL "Processando" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.14 BY .83
     BGCOLOR 8 FGCOLOR 2 FONT 6 NO-UNDO.

DEFINE VARIABLE tg-sup AS LOGICAL INITIAL yes 
     LABEL "Super Usuarios" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .83
     BGCOLOR 8 FGCOLOR 9 FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-usr FOR 
      tt-usr SCROLLING.

DEFINE QUERY br-usr-connect FOR 
      tt-usr-connect SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-usr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-usr C-Win _FREEFORM
  QUERY br-usr NO-LOCK DISPLAY
      tt-usr.banco          FORMAT "x(15)"  COLUMN-LABEL "Banco"        WIDTH 15
      tt-usr.usr            FORMAT ">>9"    COLUMN-LABEL "ID"           WIDTH 5
      tt-usr.tempo          FORMAT ">>>9"   COLUMN-LABEL "Idle"         WIDTH 5
      tt-usr.ultimo-acesso  FORMAT "x(20)"  COLUMN-LABEL "Ult. Acesso"  WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 46 BY 10
         FONT 1.

DEFINE BROWSE br-usr-connect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-usr-connect C-Win _FREEFORM
  QUERY br-usr-connect NO-LOCK DISPLAY
      tt-usr-connect.cod-usuario    FORMAT "x(15)"  COLUMN-LABEL "Usuario"  WIDTH 13
      tt-usr-connect.maquina        FORMAT "x(20)"  COLUMN-LABEL "Device"   WIDTH 19
      tt-usr-connect.login          FORMAT "x(20)"  COLUMN-LABEL "Login"    WIDTH 18
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 55 BY 15.75
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     bt-exit AT ROW 1.21 COL 99.14 WIDGET-ID 58
     tg-run AT ROW 1.33 COL 2.86 WIDGET-ID 72
     tg-idle AT ROW 1.33 COL 22 WIDGET-ID 74
     tg-out AT ROW 1.33 COL 36.14 WIDGET-ID 76
     tg-sup AT ROW 1.33 COL 56.29 WIDGET-ID 78
     br-usr-connect AT ROW 3 COL 2 WIDGET-ID 300
     br-usr AT ROW 3 COL 60 WIDGET-ID 200
     fi-hora-ini AT ROW 14.13 COL 78.29 COLON-ALIGNED WIDGET-ID 40
     fi-hora-atu AT ROW 15.13 COL 78.29 COLON-ALIGNED
     fi-exit AT ROW 16.13 COL 78.29 COLON-ALIGNED WIDGET-ID 84
     bt-super AT ROW 19 COL 2 WIDGET-ID 42
     bt-mata-usr AT ROW 19 COL 6.29 WIDGET-ID 56
     bt-atualiza AT ROW 19 COL 10.72 WIDGET-ID 86
     sl-time AT ROW 19.17 COL 73.86 NO-LABEL WIDGET-ID 46
     fi-usr-all AT ROW 19.25 COL 28.86 COLON-ALIGNED WIDGET-ID 54
     fi-usr-sup AT ROW 20.25 COL 28.86 COLON-ALIGNED WIDGET-ID 64
     fi-usr-idle AT ROW 20.25 COL 48.29 COLON-ALIGNED WIDGET-ID 70
     fi-usr-run AT ROW 21.25 COL 28.86 COLON-ALIGNED WIDGET-ID 68
     fi-usr-out AT ROW 21.25 COL 48.29 COLON-ALIGNED WIDGET-ID 66
     " Intervalo para Desconectar Usuarios" VIEW-AS TEXT
          SIZE 32 BY .75 AT ROW 18.42 COL 66.57 WIDGET-ID 52
          FGCOLOR 1 FONT 6
     "min" VIEW-AS TEXT
          SIZE 4 BY .67 AT ROW 19.5 COL 76.86 WIDGET-ID 48
          FONT 6
     RECT-14 AT ROW 18.79 COL 64.57 WIDGET-ID 50
     RECT-15 AT ROW 1 COL 1 WIDGET-ID 60
     RECT-16 AT ROW 2.75 COL 1 WIDGET-ID 62
     RECT-17 AT ROW 2.75 COL 59 WIDGET-ID 80
     RECT-18 AT ROW 13.75 COL 59 WIDGET-ID 82
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 106.43 BY 21.63
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "SeniuZ - Database User Manager"
         HEIGHT             = 21.63
         WIDTH              = 106.43
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 30.04
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB br-usr-connect tg-sup DEFAULT-FRAME */
/* BROWSE-TAB br-usr br-usr-connect DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN fi-hora-atu IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-hora-ini IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-usr-all IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-usr-idle IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-usr-out IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-usr-run IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-usr-sup IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-usr
/* Query rebuild information for BROWSE br-usr
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-usr WHERE
                                 tt-usr.cod-usuario = tt-usr-connect.cod-usuario AND
                                 tt-usr.maquina = tt-usr-connect.maquina
                                 NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-usr */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-usr-connect
/* Query rebuild information for BROWSE br-usr-connect
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-usr-connect WHERE
                                 LOOKUP(tt-usr-connect.marca,c-marcas) > 0
                                 NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-usr-connect */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.25
       COLUMN          = 95.14
       HEIGHT          = 1
       WIDTH           = 4
       HIDDEN          = yes
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(bt-exit:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* SeniuZ - Database User Manager */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* SeniuZ - Database User Manager */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-usr-connect
&Scoped-define SELF-NAME br-usr-connect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-usr-connect C-Win
ON ROW-DISPLAY OF br-usr-connect IN FRAME DEFAULT-FRAME
DO:
   CASE tt-usr-connect.marca.
       WHEN 'o' THEN DO.
           tt-usr-connect.cod-usuario:FGCOLOR IN BROWSE br-usr-connect = 12.   
           tt-usr-connect.maquina:FGCOLOR IN BROWSE br-usr-connect = 12.       
           tt-usr-connect.login:FGCOLOR IN BROWSE br-usr-connect = 12.       

           tt-usr-connect.cod-usuario:FONT IN BROWSE br-usr-connect = 6.   
           tt-usr-connect.maquina:FONT IN BROWSE br-usr-connect = 6.       
           tt-usr-connect.login:FONT IN BROWSE br-usr-connect = 6.       
       END.
       WHEN 'r' THEN DO.
           tt-usr-connect.cod-usuario:FGCOLOR IN BROWSE br-usr-connect = 2.   
           tt-usr-connect.maquina:FGCOLOR IN BROWSE br-usr-connect = 2. 
           tt-usr-connect.login:FGCOLOR IN BROWSE br-usr-connect = 2.       

           tt-usr-connect.cod-usuario:FONT IN BROWSE br-usr-connect = 6.   
           tt-usr-connect.maquina:FONT IN BROWSE br-usr-connect = 6. 
           tt-usr-connect.login:FONT IN BROWSE br-usr-connect = 6.       
       END.
       WHEN 'i' THEN DO.
           tt-usr-connect.cod-usuario:FGCOLOR IN BROWSE br-usr-connect = 4.   
           tt-usr-connect.maquina:FGCOLOR IN BROWSE br-usr-connect = 4.
           tt-usr-connect.login:FGCOLOR IN BROWSE br-usr-connect = 4.       

           tt-usr-connect.cod-usuario:FONT IN BROWSE br-usr-connect = 6.   
           tt-usr-connect.maquina:FONT IN BROWSE br-usr-connect = 6. 
           tt-usr-connect.login:FONT IN BROWSE br-usr-connect = 6.       
       END.
       WHEN 's' THEN DO.
           tt-usr-connect.cod-usuario:FGCOLOR IN BROWSE br-usr-connect = 9.   
           tt-usr-connect.maquina:FGCOLOR IN BROWSE br-usr-connect = 9.
           tt-usr-connect.login:FGCOLOR IN BROWSE br-usr-connect = 9.       

           tt-usr-connect.cod-usuario:FONT IN BROWSE br-usr-connect = 6.   
           tt-usr-connect.maquina:FONT IN BROWSE br-usr-connect = 6.
           tt-usr-connect.login:FONT IN BROWSE br-usr-connect = 6.       
       END.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-usr-connect C-Win
ON VALUE-CHANGED OF br-usr-connect IN FRAME DEFAULT-FRAME
DO:
    {&OPEN-QUERY-br-usr}

    IF AVAIL tt-usr-connect THEN DO.
       ASSIGN bt-mata-usr:SENSITIVE = YES.
       IF tt-usr-connect.marca = 's' THEN  /* super usuario */
          ASSIGN bt-mata-usr:SENSITIVE = NO.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-atualiza
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-atualiza C-Win
ON CHOOSE OF bt-atualiza IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
    RUN pi-processa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-exit C-Win
ON CHOOSE OF bt-exit IN FRAME DEFAULT-FRAME /* Button 2 */
DO:
   APPLY 'CLOSE':U TO THIS-PROCEDURE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-mata-usr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-mata-usr C-Win
ON CHOOSE OF bt-mata-usr IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
  DO i-ct = 1 TO br-usr-connect:NUM-SELECTED-ROWS.
     IF br-usr-connect:FETCH-SELECTED-ROW(i-ct) THEN DO.
        CASE tt-usr-connect.marca.
             WHEN 'i' OR WHEN 'r' THEN DO.   /* Idle ou Run */
                 ASSIGN tt-usr-connect.marca = "o"
                        fi-usr-out = fi-usr-out + 1.

                 RUN pi-disconnect-usr (INPUT tt-usr-connect.marca,
                                        INPUT tt-usr-connect.cod-usuario,
                                        INPUT tt-usr-connect.maquina).
             END.
             WHEN 'o' THEN DO.
                 ASSIGN tt-usr-connect.marca = "r" 
                        fi-usr-out = fi-usr-out - 1.

                 RUN pi-disconnect-usr (INPUT tt-usr-connect.marca,
                                        INPUT tt-usr-connect.cod-usuario,
                                        INPUT tt-usr-connect.maquina).
             END.
        END CASE.
     END.
  END.


  br-usr-connect:REFRESH().

  DISP fi-usr-out
       WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-super
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-super C-Win
ON CHOOSE OF bt-super IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
    IF LOOKUP(tt-usr-connect.cod-usuario,c-super) > 0 THEN DO.
       IF LOOKUP(tt-usr-connect.cod-usuario,c-super) = 1 THEN
          ASSIGN c-super = IF NUM-ENTRIES(c-super) = 1
                           THEN ""
                           ELSE REPLACE(c-super,tt-usr-connect.cod-usuario + ',',"").
       ELSE
          ASSIGN c-super = REPLACE(c-super, "," + tt-usr-connect.cod-usuario,"").

       ASSIGN tt-usr.marca = 'r'
              tt-usr-connect.marca = 'r'.

       ASSIGN fi-usr-sup = fi-usr-sup - 1.
    END.
    ELSE DO.
       ASSIGN c-super = IF c-super = ""
                        THEN tt-usr-connect.cod-usuario
                        ELSE c-super + ',' +  tt-usr-connect.cod-usuario.

       ASSIGN tt-usr.marca = 's'
              tt-usr-connect.marca = 's'.

       ASSIGN fi-usr-sup = fi-usr-sup + 1.
    END.

    PUT-KEY-VALUE SECTION "Startup":U KEY "UserOut":U VALUE c-super.

    br-usr-connect:REFRESH().
    APPLY 'VALUE-CHANGED' TO br-usr-connect.

    DISP fi-usr-sup
         WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-exit C-Win
ON LEAVE OF fi-exit IN FRAME DEFAULT-FRAME /* Finalizar Monitor */
DO:
    PUT-KEY-VALUE SECTION "Startup":U KEY "CloseMonitor":U VALUE fi-exit:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sl-time
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sl-time C-Win
ON VALUE-CHANGED OF sl-time IN FRAME DEFAULT-FRAME
DO:
   PUT-KEY-VALUE SECTION "Startup":U KEY "TimeOut":U VALUE sl-time:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-idle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-idle C-Win
ON VALUE-CHANGED OF tg-idle IN FRAME DEFAULT-FRAME /* Parados */
DO:
  RUN pi-popula-browse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-out
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-out C-Win
ON VALUE-CHANGED OF tg-out IN FRAME DEFAULT-FRAME /* Desconectando */
DO:
  RUN pi-popula-browse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-run
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-run C-Win
ON VALUE-CHANGED OF tg-run IN FRAME DEFAULT-FRAME /* Processando */
DO:
   RUN pi-popula-browse. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-sup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-sup C-Win
ON VALUE-CHANGED OF tg-sup IN FRAME DEFAULT-FRAME /* Super Usuarios */
DO:
  RUN pi-popula-browse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-usr
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

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  GET-KEY-VALUE SECTION "Startup":U KEY "UserOut":U VALUE c-super.
  IF c-super = ? THEN
     ASSIGN c-super = ''.

  GET-KEY-VALUE SECTION "Startup":U KEY "CloseMonitor":U VALUE c-close.
  GET-KEY-VALUE SECTION "Startup":U KEY "TimeOut":U VALUE c-time.

  ASSIGN fi-hora-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY,"99/99/9999") + "  " +
                                                           STRING(TIME,"HH:MM:SS").
  ASSIGN fi-exit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-close
         sl-time:SCREEN-VALUE IN FRAME {&FRAME-NAME} = c-time.

  RUN pi-processa.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "sz-dbu.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "sz-dbu.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CtrlFrame.Pstimer.tick C-Win 
PROCEDURE CtrlFrame.Pstimer.tick :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-hora-atu:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY,"99/99/9999") + "  " +
                                                             STRING(TIME,"HH:MM:SS").

    IF STRING(TIME,"HH:MM:SS") = fi-exit:SCREEN-VALUE THEN DO.
       APPLY 'CHOOSE' TO bt-exit.
       RETURN NO-APPLY.
    END.

    IF INT(ENTRY(3,STRING(TIME,"HH:MM:SS"),":")) = 0 THEN
       RUN pi-processa.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  RUN control_load.
  DISPLAY tg-run tg-idle tg-out tg-sup fi-hora-ini fi-hora-atu fi-exit sl-time 
          fi-usr-all fi-usr-sup fi-usr-idle fi-usr-run fi-usr-out 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-14 RECT-15 RECT-16 RECT-17 RECT-18 bt-exit tg-run tg-idle tg-out 
         tg-sup br-usr-connect br-usr fi-exit bt-super bt-mata-usr bt-atualiza 
         sl-time 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-disconnect-usr C-Win 
PROCEDURE pi-disconnect-usr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-marca AS CHAR.
    DEF INPUT PARAMETER p-cod-usuario AS CHAR.
    DEF INPUT PARAMETER p-maquina AS CHAR.

    FOR EACH tt-usr WHERE
             tt-usr.cod-usuario = p-cod-usuario AND
             tt-usr.maquina = p-maquina NO-LOCK.
        RUN spool/ton/t70c.p tt-usr.banco p-marca STRING(tt-usr.usr).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse C-Win 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN INPUT FRAME {&FRAME-NAME} tg-run tg-idle tg-out tg-sup.
    
    ASSIGN c-marcas = ''.

    ASSIGN c-marcas = c-marcas + IF tg-run = YES THEN "r," ELSE ",".
    ASSIGN c-marcas = c-marcas + IF tg-idle = YES THEN "i," ELSE ",".
    ASSIGN c-marcas = c-marcas + IF tg-out = YES THEN "o," ELSE ",".
    ASSIGN c-marcas = c-marcas + IF tg-sup = YES THEN "s," ELSE ",".

    {&OPEN-QUERY-br-usr-connect}
    APPLY 'VALUE-CHANGED' TO br-usr-connect.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa C-Win 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR i-ct AS INTEGER.
    DEFINE VAR l-mata AS LOG.
    DEFINE VAR l-idle AS LOG.

    REPEAT i-ct = 1 TO NUM-DBS:
        RUN spool/ton/t70a.p (INPUT-OUTPUT TABLE tt-usr) LDBNAME(i-ct).
    END. 
  
    EMPTY TEMP-TABLE tt-usr-connect.

    FOR EACH tt-usr BREAK BY tt-usr.cod-usuario + tt-usr.maquina.

        IF FIRST-OF(tt-usr.cod-usuario + tt-usr.maquina) THEN
           ASSIGN l-mata = YES
                  l-idle = YES.

        FIND tt-usr-connect WHERE
             tt-usr-connect.cod-usuario = tt-usr.cod-usuario AND
             tt-usr-connect.maquina = tt-usr.maquina NO-ERROR.

        IF NOT AVAIL tt-usr-connect THEN DO.
           CREATE tt-usr-connect.
           ASSIGN tt-usr-connect.usr = tt-usr.usr
                  tt-usr-connect.cod-usuario = tt-usr.cod-usuario
                  tt-usr-connect.maquina = tt-usr.maquina
                  tt-usr-connect.marca = tt-usr.marca
                  tt-usr-connect.login = tt-usr.login.
        END.

        IF tt-usr.marca <> 'o' AND 
           tt-usr.tempo < INT(sl-time:INPUT-VALUE IN FRAME {&FRAME-NAME}) THEN
           ASSIGN l-mata = NO.

        IF tt-usr.tempo = 0 THEN
           l-idle = NO.

        IF LAST-OF(tt-usr.cod-usuario + tt-usr.maquina) THEN DO.
           IF tt-usr-connect.marca <> 's' THEN DO.
              ASSIGN tt-usr-connect.marca = IF l-idle 
                                            THEN 'i'
                                            ELSE 'r'.

              IF l-mata THEN
                 ASSIGN tt-usr-connect.marca = "o".
           END.
        END.
    END.

    FOR EACH tt-usr-connect WHERE
             tt-usr-connect.marca = 'o'.  /* Out */

        RUN pi-disconnect-usr (INPUT tt-usr-connect.marca,
                               INPUT tt-usr-connect.cod-usuario,
                               INPUT tt-usr-connect.maquina).
    END.

    RUN pi-popula-browse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais C-Win 
PROCEDURE pi-totais :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-usr-all = 0
           fi-usr-sup = 0
           fi-usr-idle = 0
           fi-usr-run = 0
           fi-usr-out = 0.

    FOR EACH tt-usr-connect.
        ASSIGN fi-usr-all = fi-usr-all + 1.

        CASE tt-usr-connect.marca.
            WHEN 's' THEN
                ASSIGN fi-usr-sup = fi-usr-sup + 1.
            WHEN "o" THEN
                ASSIGN fi-usr-out = fi-usr-out + 1.
            WHEN "r" THEN
                ASSIGN fi-usr-run = fi-usr-run + 1.
            WHEN "i" THEN
                ASSIGN fi-usr-idle = fi-usr-idle + 1.
        END CASE.
    END.

    DISP fi-usr-all 
         fi-usr-sup         
         fi-usr-idle
         fi-usr-run 
         fi-usr-out         
         WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

