&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
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
{include/i-prgvrs.i D99XX999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF TEMP-TABLE tt-cond-ped LIKE cond-ped
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE wt-cond-ped LIKE tt-cond-ped.

DEF BUFFER b-cond-ped FOR tt-cond-ped.

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tt-cond-ped.  
DEFINE INPUT        PARAMETER i-cond-pagto AS INT. 
DEFINE INPUT        PARAMETER p-acao AS CHAR.
  
DEF VAR i-dias-venc AS INT.
DEF VAR i-qtd-seq AS INT.
DEF VAR i-ult-seq AS INT.
DEF VAR i-ind AS INT.
DEF VAR i-ct AS INT.
DEF VAR de-perc-acum AS DEC.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-cond-esp

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-cond-ped

/* Definitions for BROWSE br-cond-esp                                   */
&Scoped-define FIELDS-IN-QUERY-br-cond-esp tt-cond-ped.nr-sequencia fn-vencto() tt-cond-ped.nr-dias-venc tt-cond-ped.data-pagto tt-cond-ped.perc-pagto   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-cond-esp tt-cond-ped.perc-pagto   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-cond-esp tt-cond-ped
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-cond-esp tt-cond-ped
&Scoped-define SELF-NAME br-cond-esp
&Scoped-define QUERY-STRING-br-cond-esp FOR EACH tt-cond-ped NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-cond-esp OPEN QUERY {&SELF-NAME} FOR EACH tt-cond-ped NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-cond-esp tt-cond-ped
&Scoped-define FIRST-TABLE-IN-QUERY-br-cond-esp tt-cond-ped


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-cond-esp}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-10 RECT-6 RECT-7 RECT-8 RECT-9 ~
rt-buttom br-cond-esp cb-tp-vencto tg-add-dias fi-dias fi-dt-vencto bt-add ~
bt-modifica bt-del tg-dias-fixos tg-intervalo tg-parc-venc bt-ok bt-cancela ~
bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS cb-tp-vencto tg-add-dias fi-dias ~
fi-add-dias fi-dt-vencto tg-dias-fixos tg-intervalo tg-parc-venc fi-dia-f1 ~
fi-dia-f2 fi-dia-f3 fi-dia-f4 fi-dia-f5 fi-dia-f6 fi-intervalo fi-qt-parc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi-dia-f1 fi-dia-f2 fi-dia-f3 fi-dia-f4 fi-dia-f5 ~
fi-dia-f6 fi-qt-parc 
&Scoped-define List-4 cb-tp-vencto tg-add-dias fi-dias fi-add-dias ~
fi-dt-vencto bt-add bt-del tg-dias-fixos tg-intervalo tg-parc-venc ~
fi-dia-f1 fi-dia-f2 fi-dia-f3 fi-dia-f4 fi-dia-f5 fi-dia-f6 fi-intervalo ~
fi-qt-parc bt-ok 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-vencto D-Dialog 
FUNCTION fn-vencto RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-add 
     IMAGE-UP FILE "image/im-add.bmp":U
     LABEL "Button 1" 
     SIZE 5.72 BY 1.67.

DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-del 
     IMAGE-UP FILE "image/im-era.bmp":U
     LABEL "Button 2" 
     SIZE 5.72 BY 1.67.

DEFINE BUTTON bt-modifica 
     IMAGE-UP FILE "image/im-mod.bmp":U
     LABEL "&Sair" 
     SIZE 5.72 BY 1.67 TOOLTIP "Modificar Pedido"
     BGCOLOR 8 FONT 10.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE cb-tp-vencto AS INTEGER FORMAT ">9":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Dias da Data",1,
                     "Data Fixa",2
     DROP-DOWN-LIST
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi-add-dias AS INTEGER FORMAT "->>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dia-f1 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dia-f2 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dia-f3 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dia-f4 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dia-f5 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dia-f6 AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dias AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-vencto AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-intervalo AS INTEGER FORMAT ">>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qt-parc AS INTEGER FORMAT ">>":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 16 BY 2.25
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64 BY 11.25.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 20 BY 8.25
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 33 BY 2.25
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 11 BY 2.25
     BGCOLOR 8 .

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 64 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE tg-add-dias AS LOGICAL INITIAL no 
     LABEL "(+)  Dias" 
     VIEW-AS TOGGLE-BOX
     SIZE 8.57 BY .83
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE tg-dias-fixos AS LOGICAL INITIAL no 
     LABEL "Dias Vencto Fixo" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.86 BY .83
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE tg-intervalo AS LOGICAL INITIAL no 
     LABEL "Intervalo" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .83
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE tg-parc-venc AS LOGICAL INITIAL no 
     LABEL "Parc. por Vencto" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .83
     BGCOLOR 8  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-cond-esp FOR 
      tt-cond-ped SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-cond-esp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-cond-esp D-Dialog _FREEFORM
  QUERY br-cond-esp NO-LOCK DISPLAY
      tt-cond-ped.nr-sequencia FORMAT ">>9":U WIDTH 4 COLUMN-LABEL "Seq" LABEL-FONT 6 LABEL-FGCOLOR 3
      fn-vencto() FORMAT "x(15)":U WIDTH 10 COLUMN-LABEL "Tipo Vencto" LABEL-FONT 6 LABEL-FGCOLOR 3
      tt-cond-ped.nr-dias-venc FORMAT ">>>":U WIDTH 4 COLUMN-LABEL "Dias" LABEL-FONT 6 LABEL-FGCOLOR 3
      tt-cond-ped.data-pagto FORMAT "99/99/9999":U WIDTH 10 COLUMN-LABEL "Data" LABEL-FONT 6 LABEL-FGCOLOR 3
      tt-cond-ped.perc-pagto FORMAT ">>9.99":U WIDTH 7 COLUMN-LABEL "% Pagto" LABEL-FONT 6 LABEL-FGCOLOR 3
ENABLE
     tt-cond-ped.perc-pagto
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 41 BY 8.25
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     br-cond-esp AT ROW 1.5 COL 3
     cb-tp-vencto AT ROW 2.5 COL 45 COLON-ALIGNED NO-LABEL
     tg-add-dias AT ROW 3.75 COL 55.29
     fi-dias AT ROW 4.38 COL 45 COLON-ALIGNED NO-LABEL
     fi-add-dias AT ROW 4.54 COL 53.29 COLON-ALIGNED NO-LABEL
     fi-dt-vencto AT ROW 6.33 COL 45 COLON-ALIGNED NO-LABEL
     bt-add AT ROW 7.75 COL 46
     bt-modifica AT ROW 7.75 COL 52
     bt-del AT ROW 7.75 COL 58
     tg-dias-fixos AT ROW 10.21 COL 4.57
     tg-intervalo AT ROW 10.25 COL 38
     tg-parc-venc AT ROW 10.25 COL 50
     fi-dia-f1 AT ROW 11 COL 2.57 COLON-ALIGNED NO-LABEL
     fi-dia-f2 AT ROW 11 COL 7.57 COLON-ALIGNED NO-LABEL
     fi-dia-f3 AT ROW 11 COL 12.57 COLON-ALIGNED NO-LABEL
     fi-dia-f4 AT ROW 11 COL 17.57 COLON-ALIGNED NO-LABEL
     fi-dia-f5 AT ROW 11 COL 22.57 COLON-ALIGNED NO-LABEL
     fi-dia-f6 AT ROW 11 COL 27.57 COLON-ALIGNED NO-LABEL
     fi-intervalo AT ROW 11 COL 39 COLON-ALIGNED NO-LABEL
     fi-qt-parc AT ROW 11 COL 51 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 13 COL 3
     bt-cancela AT ROW 13 COL 14
     bt-ajuda AT ROW 13 COL 55.14
     "Tipo Vencto" VIEW-AS TEXT
          SIZE 11 BY .5 AT ROW 1.92 COL 46.86
          BGCOLOR 8 FGCOLOR 3 FONT 6
     "Dias" VIEW-AS TEXT
          SIZE 5 BY .5 AT ROW 3.79 COL 47
          BGCOLOR 8 FGCOLOR 3 FONT 6
     "Data" VIEW-AS TEXT
          SIZE 5 BY .5 AT ROW 5.75 COL 47
          BGCOLOR 8 FGCOLOR 3 FONT 6
     RECT-10 AT ROW 10 COL 49
     RECT-6 AT ROW 1.25 COL 2
     RECT-7 AT ROW 1.5 COL 45
     RECT-8 AT ROW 10 COL 3
     RECT-9 AT ROW 10 COL 37
     rt-buttom AT ROW 12.75 COL 2
     SPACE(0.56) SKIP(0.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Condi‡äes Especiais de Pagto - ESPD4000A"
         DEFAULT-BUTTON bt-cancela CANCEL-BUTTON bt-cancela.


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
   FRAME-NAME L-To-R                                                    */
/* BROWSE-TAB br-cond-esp rt-buttom D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-add IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR BUTTON bt-del IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR BUTTON bt-ok IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR COMBO-BOX cb-tp-vencto IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-add-dias IN FRAME D-Dialog
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-dia-f1 IN FRAME D-Dialog
   NO-ENABLE 1 4                                                        */
/* SETTINGS FOR FILL-IN fi-dia-f2 IN FRAME D-Dialog
   NO-ENABLE 1 4                                                        */
/* SETTINGS FOR FILL-IN fi-dia-f3 IN FRAME D-Dialog
   NO-ENABLE 1 4                                                        */
/* SETTINGS FOR FILL-IN fi-dia-f4 IN FRAME D-Dialog
   NO-ENABLE 1 4                                                        */
/* SETTINGS FOR FILL-IN fi-dia-f5 IN FRAME D-Dialog
   NO-ENABLE 1 4                                                        */
/* SETTINGS FOR FILL-IN fi-dia-f6 IN FRAME D-Dialog
   NO-ENABLE 1 4                                                        */
/* SETTINGS FOR FILL-IN fi-dias IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-dt-vencto IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-intervalo IN FRAME D-Dialog
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qt-parc IN FRAME D-Dialog
   NO-ENABLE 1 4                                                        */
/* SETTINGS FOR TOGGLE-BOX tg-add-dias IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR TOGGLE-BOX tg-dias-fixos IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR TOGGLE-BOX tg-intervalo IN FRAME D-Dialog
   4                                                                    */
/* SETTINGS FOR TOGGLE-BOX tg-parc-venc IN FRAME D-Dialog
   4                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-cond-esp
/* Query rebuild information for BROWSE br-cond-esp
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-cond-ped NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-cond-esp */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Condi‡äes Especiais de Pagto - ESPD4000A */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-cond-esp
&Scoped-define SELF-NAME br-cond-esp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-cond-esp D-Dialog
ON VALUE-CHANGED OF br-cond-esp IN FRAME D-Dialog
DO:
   IF AVAIL tt-cond-ped THEN DO.
      IF p-acao <> 'Consultar' THEN
         ASSIGN cb-tp-vencto:SENSITIVE = YES.
     
      ASSIGN fi-dias:SCREEN-VALUE = STRING(tt-cond-ped.nr-dias-venc)
             fi-dt-vencto:SCREEN-VALUE = STRING(tt-cond-ped.data-pagto)
             cb-tp-vencto:SCREEN-VALUE = STRING(tt-cond-ped.cod-vencto).
    
      IF tt-cond-ped.log-2 THEN
         cb-tp-vencto:SCREEN-VALUE = '2'.
    
      APPLY 'value-changed' TO cb-tp-vencto.
   END.
   ELSE DO.
      ASSIGN fi-dias:SCREEN-VALUE = ''
             fi-dt-vencto:SCREEN-VALUE = ''
             cb-tp-vencto:SCREEN-VALUE = ''.

      ASSIGN cb-tp-vencto:SENSITIVE = NO
             fi-dt-vencto:SENSITIVE = NO 
             fi-dias:SENSITIVE = NO.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-add D-Dialog
ON CHOOSE OF bt-add IN FRAME D-Dialog /* Button 1 */
DO:
  RUN pi-busca-qtd-seq.

  FIND LAST tt-cond-ped NO-ERROR.
  IF AVAIL tt-cond-ped THEN DO.
     CREATE b-cond-ped.
     BUFFER-COPY tt-cond-ped TO b-cond-ped
                 ASSIGN b-cond-ped.nr-sequencia = i-ult-seq + 10
                        b-cond-ped.data-pagto = ?
                        b-cond-ped.nr-dias-venc = tt-cond-ped.nr-dias-venc + fi-intervalo:INPUT-VALUE.
  END.
  ELSE DO.
     CREATE tt-cond-ped.
     ASSIGN tt-cond-ped.nr-pedido = 9999
            tt-cond-ped.nr-sequencia = 10
            tt-cond-ped.nr-dias-venc = 30
            tt-cond-ped.perc-pagto = 100.
  END.

  RUN pi-perc-pagto.

  ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = YES
         tg-add-dias:SENSITIVE IN FRAME {&FRAME-NAME} = YES
         tg-dias-fixos:SENSITIVE IN FRAME {&FRAME-NAME} = YES
         tg-parc-venc:SENSITIVE IN FRAME {&FRAME-NAME} = YES
         tg-intervalo:SENSITIVE IN FRAME {&FRAME-NAME} = YES.

  IF fi-intervalo:SCREEN-VALUE = '' THEN
     ASSIGN fi-intervalo:SCREEN-VALUE = '30'.

 {&OPEN-QUERY-br-cond-esp}

  FIND LAST tt-cond-ped NO-ERROR.
  br-cond-esp:QUERY:REPOSITION-TO-ROWID(ROWID(tt-cond-ped)) NO-ERROR. 

  APPLY 'VALUE-CHANGED' TO br-cond-esp.
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


&Scoped-define SELF-NAME bt-cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancela D-Dialog
ON CHOOSE OF bt-cancela IN FRAME D-Dialog /* Cancelar */
DO:
  IF adm-new-record THEN DO.
     FOR EACH tt-cond-ped.
         DELETE tt-cond-ped.
     END.
  END.
  ELSE DO.
     FOR EACH tt-cond-ped.
         DELETE tt-cond-ped.
     END.
     FOR EACH wt-cond-ped.
         CREATE tt-cond-ped.
         BUFFER-COPY wt-cond-ped TO tt-cond-ped.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-del D-Dialog
ON CHOOSE OF bt-del IN FRAME D-Dialog /* Button 2 */
DO:
    FIND CURRENT tt-cond-ped NO-ERROR.
    DELETE tt-cond-ped.

    IF NOT CAN-FIND(FIRST tt-cond-ped) THEN
       ASSIGN bt-del:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              tg-add-dias:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              tg-dias-fixos:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              tg-parc-venc:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              tg-intervalo:SENSITIVE IN FRAME {&FRAME-NAME} = NO
              fi-intervalo:SCREEN-VALUE = '0'.

    RUN pi-acerta-seq.
    RUN pi-perc-pagto.

    {&OPEN-QUERY-br-cond-esp}
    APPLY 'VALUE-CHANGED' TO br-cond-esp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modifica D-Dialog
ON CHOOSE OF bt-modifica IN FRAME D-Dialog /* Sair */
DO:
    APPLY 'entry':U TO tt-cond-ped.perc-pagto IN BROWSE br-cond-esp. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
   ASSIGN de-perc-acum = 0.
   FOR EACH tt-cond-ped.
       ASSIGN de-perc-acum = de-perc-acum + tt-cond-ped.perc-pagto.
   END.

   IF de-perc-acum <> 100 THEN DO.
      MESSAGE 'Percentual de Pagamento' de-perc-acum 'deve ser igual … 100%'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'entry' TO tt-cond-ped.perc-pagto IN BROWSE br-cond-esp.
      RETURN NO-APPLY.
   END.

   FIND FIRST tt-cond-ped NO-ERROR.
   IF AVAIL tt-cond-ped AND tg-dias-fixos:SCREEN-VALUE = 'YES' THEN DO.
      IF fi-dia-f1:SCREEN-VALUE <> '' THEN
         ASSIGN tt-cond-ped.char-2 = fi-dia-f1:SCREEN-VALUE.
      IF fi-dia-f2:SCREEN-VALUE <> '' THEN
         ASSIGN tt-cond-ped.char-2 = tt-cond-ped.char-2 + ";" + fi-dia-f2:SCREEN-VALUE.
      IF fi-dia-f3:SCREEN-VALUE <> '' THEN
         ASSIGN tt-cond-ped.char-2 = tt-cond-ped.char-2 + ";" + fi-dia-f3:SCREEN-VALUE.
      IF fi-dia-f4:SCREEN-VALUE <> '' THEN
         ASSIGN tt-cond-ped.char-2 = tt-cond-ped.char-2 + ";" + fi-dia-f4:SCREEN-VALUE.
      IF fi-dia-f5:SCREEN-VALUE <> '' THEN
         ASSIGN tt-cond-ped.char-2 = tt-cond-ped.char-2 + ";" + fi-dia-f5:SCREEN-VALUE.
      IF fi-dia-f6:SCREEN-VALUE <> '' THEN
         ASSIGN tt-cond-ped.char-2 = tt-cond-ped.char-2 + ";" + fi-dia-f6:SCREEN-VALUE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cb-tp-vencto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cb-tp-vencto D-Dialog
ON VALUE-CHANGED OF cb-tp-vencto IN FRAME D-Dialog
DO:
   ASSIGN fi-dias:SENSITIVE = NO
          fi-dt-vencto:SENSITIVE = NO.

   IF SELF:SCREEN-VALUE = '1' THEN DO.
      ASSIGN fi-dt-vencto:SCREEN-VALUE  = ?
             fi-dias:SCREEN-VALUE = STRING(tt-cond-ped.nr-dias-venc)
             fi-dias:SENSITIVE = IF p-acao = 'Consultar'    
                                 THEN NO ELSE YES.          
   END.
   ELSE DO.
      ASSIGN fi-dias:SCREEN-VALUE = '0'
             fi-dt-vencto:SCREEN-VALUE = STRING(tt-cond-ped.data-pagto)
             fi-dt-vencto:SENSITIVE = IF p-acao = 'Consultar' 
                                      THEN NO ELSE YES.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-add-dias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-add-dias D-Dialog
ON LEAVE OF fi-add-dias IN FRAME D-Dialog
DO:
   FIND FIRST tt-cond-ped NO-LOCK NO-ERROR.
   IF tt-cond-ped.nr-dias-venc + fi-add-dias:INPUT-VALUE < 1 THEN DO.
      MESSAGE 'Opera‡Æo Inv lida!!!' SKIP
              'Prazo Solicitado menor que Permitido...' SKIP
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'entry' TO SELF.
      RETURN NO-APPLY.
   END.

   FOR EACH tt-cond-ped.
       ASSIGN tt-cond-ped.nr-dias-venc = tt-cond-ped.nr-dias-venc + fi-add-dias:INPUT-VALUE.
   END.

   ASSIGN tg-add-dias:SCREEN-VALUE = 'no'.
   APPLY 'value-changed' TO tg-add-dias.

   {&OPEN-QUERY-br-cond-esp}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dia-f1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dia-f1 D-Dialog
ON LEAVE OF fi-dia-f1 IN FRAME D-Dialog
DO:
   RUN pi-valida.
   IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
      MESSAGE 'Dia do Mˆs Inv lido...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN INPUT FRAME {&FRAME-NAME} fi-dia-f1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dia-f2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dia-f2 D-Dialog
ON LEAVE OF fi-dia-f2 IN FRAME D-Dialog
DO:
  RUN pi-valida.
  IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
     MESSAGE 'Dia do Mˆs Inv lido...'
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'ENTRY' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-dia-f2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dia-f3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dia-f3 D-Dialog
ON LEAVE OF fi-dia-f3 IN FRAME D-Dialog
DO:
  RUN pi-valida.
  IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
     MESSAGE 'Dia do Mˆs Inv lido...'
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'ENTRY' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-dia-f3.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dia-f4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dia-f4 D-Dialog
ON LEAVE OF fi-dia-f4 IN FRAME D-Dialog
DO:
   RUN pi-valida.
   IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
      MESSAGE 'Dia do Mˆs Inv lido...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.
   ASSIGN INPUT FRAME {&FRAME-NAME} fi-dia-f4.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dia-f5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dia-f5 D-Dialog
ON LEAVE OF fi-dia-f5 IN FRAME D-Dialog
DO:
  RUN pi-valida.
  IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
     MESSAGE 'Dia do Mˆs Inv lido...'
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'ENTRY' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-dia-f5.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dia-f6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dia-f6 D-Dialog
ON LEAVE OF fi-dia-f6 IN FRAME D-Dialog
DO:
  RUN pi-valida.
  IF RETURN-VALUE = 'ADM-ERROR' THEN DO.
     MESSAGE 'Dia do Mˆs Inv lido...'
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'ENTRY' TO SELF.
     RETURN NO-APPLY.
  END.
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-dia-f6.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dias D-Dialog
ON LEAVE OF fi-dias IN FRAME D-Dialog
DO:
   FIND LAST b-cond-ped WHERE
             b-cond-ped.nr-sequencia < tt-cond-ped.nr-sequencia NO-ERROR.

   IF AVAIL b-cond-ped AND
      SELF:INPUT-VALUE <= b-cond-ped.nr-dias-venc THEN DO.
      MESSAGE 'Numero de Dias menor que da Sequˆncia anterior...'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   IF SELF:INPUT-VALUE <> 0 THEN DO.
      ASSIGN tt-cond-ped.nr-dias-venc = SELF:INPUT-VALUE
             tt-cond-ped.data-pagto = ?
             tt-cond-ped.log-2 = NO.

      br-cond-esp:REFRESH().
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-vencto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-vencto D-Dialog
ON LEAVE OF fi-dt-vencto IN FRAME D-Dialog
DO:
   IF (AVAIL ped-venda AND SELF:INPUT-VALUE < ped-venda.dt-implant) OR
       NOT AVAIL ped-venda AND SELF:INPUT-VALUE < TODAY THEN DO.
       MESSAGE 'Data de Vencimento menor que Data de Implanta‡Æo do Pedido...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'ENTRY' TO SELF.
       RETURN NO-APPLY.
   END.

   FIND LAST b-cond-ped WHERE
             b-cond-ped.nr-sequencia < tt-cond-ped.nr-sequencia NO-ERROR.

   IF AVAIL b-cond-ped AND
      SELF:INPUT-VALUE <= b-cond-ped.data-pagto THEN DO.
      MESSAGE 'Data de Vencimento menor que da Sequˆncia anterior...'
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'ENTRY' TO SELF.
      RETURN NO-APPLY.
   END.

   IF SELF:INPUT-VALUE <> ? THEN DO.
      ASSIGN tt-cond-ped.nr-dias-venc = 0
             tt-cond-ped.data-pagto = SELF:INPUT-VALUE
             tt-cond-ped.log-2 = YES.

      br-cond-esp:REFRESH().
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-intervalo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-intervalo D-Dialog
ON LEAVE OF fi-intervalo IN FRAME D-Dialog
DO:
   IF SELF:INPUT-VALUE <> 0 THEN DO.
      FIND FIRST tt-cond-ped NO-ERROR.
      ASSIGN i-dias-venc = tt-cond-ped.nr-dias-venc.

      FOR EACH tt-cond-ped WHERE 
               tt-cond-ped.nr-dias-venc <> i-dias-venc 
               BREAK BY tt-cond-ped.nr-dias-venc.
          IF FIRST-OF(tt-cond-ped.nr-dias-venc) THEN DO.
             FOR EACH b-cond-ped WHERE
                      b-cond-ped.nr-dias-venc = tt-cond-ped.nr-dias-venc.
                 ASSIGN b-cond-ped.nr-dias-venc = b-cond-ped.nr-dias-venc + SELF:INPUT-VALUE.
             END.
          END.
      END.
   END.

   ASSIGN tg-intervalo:SCREEN-VALUE = 'NO'.
   APPLY 'value-changed' TO tg-intervalo.

   {&OPEN-QUERY-br-cond-esp}
   APPLY 'VALUE-CHANGED' TO br-cond-esp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-qt-parc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-qt-parc D-Dialog
ON LEAVE OF fi-qt-parc IN FRAME D-Dialog
DO:
   IF SELF:INPUT-VALUE > 1 THEN DO.
      ASSIGN i-ind = 0.
      FOR EACH tt-cond-ped BREAK BY tt-cond-ped.nr-dias-venc
                                 BY tt-cond-ped.nr-sequencia.
          IF FIRST-OF(tt-cond-ped.nr-dias-venc) THEN 
             ASSIGN i-ind = i-ind + 1.
      END.
      ASSIGN i-ind = i-ind * fi-qt-parc:INPUT-VALUE.
      
      IF i-ind > 9 THEN DO.
         MESSAGE "Permitido apenas 9 Vencimentos por Nota Fiscal..."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         DISP fi-qt-parc WITH FRAME {&FRAME-NAME}.
         APPLY 'entry' TO SELF.
         RETURN NO-APPLY.
      END.
    
      FOR EACH tt-cond-ped BREAK BY tt-cond-ped.nr-dias-venc
                                 BY tt-cond-ped.nr-sequencia.
          IF NOT FIRST-OF(tt-cond-ped.nr-dias-venc) THEN 
             DELETE tt-cond-ped.
      END.
      
      DO i-ct = 1 TO (fi-qt-parc:INPUT-VALUE - 1).
         FOR EACH tt-cond-ped BREAK BY tt-cond-ped.nr-dias-venc BY tt-cond-ped.nr-sequencia.
             IF LAST-OF(tt-cond-ped.nr-dias-venc) THEN DO.
                CREATE b-cond-ped.
                BUFFER-COPY tt-cond-ped TO b-cond-ped
                            ASSIGN b-cond-ped.nr-sequencia = tt-cond-ped.nr-sequencia + 1.
             END.
         END.
      END.
    
      RUN pi-perc-pagto.
   END.
  
   ASSIGN tg-parc-venc:SCREEN-VALUE = 'NO'.
   APPLY 'value-changed' TO tg-parc-venc.

   {&OPEN-QUERY-br-cond-esp}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-add-dias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-add-dias D-Dialog
ON VALUE-CHANGED OF tg-add-dias IN FRAME D-Dialog /* (+)  Dias */
DO:
   IF SELF:INPUT-VALUE = YES THEN DO.
      ASSIGN fi-add-dias:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
      APPLY 'entry' TO fi-add-dias.
      RETURN NO-APPLY.
   END.
   ELSE
      ASSIGN fi-add-dias:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
             fi-add-dias:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-dias-fixos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-dias-fixos D-Dialog
ON VALUE-CHANGED OF tg-dias-fixos IN FRAME D-Dialog /* Dias Vencto Fixo */
DO:
  IF SELF:INPUT-VALUE = YES THEN DO.
     ENABLE {&list-1} WITH FRAME {&FRAME-NAME}.
     APPLY 'entry' TO fi-dia-f1.
     RETURN NO-APPLY.
  END.
  ELSE DO.
     ASSIGN fi-dia-f1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
            fi-dia-f2:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
            fi-dia-f3:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
            fi-dia-f4:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'
            fi-dia-f5:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '0'.

     DISABLE {&list-1} WITH FRAME {&FRAME-NAME}.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-intervalo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-intervalo D-Dialog
ON VALUE-CHANGED OF tg-intervalo IN FRAME D-Dialog /* Intervalo */
DO:
   IF SELF:INPUT-VALUE = YES THEN DO.
      ASSIGN fi-intervalo:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
      APPLY 'entry' TO fi-intervalo.
      RETURN NO-APPLY.
   END.
   ELSE
      ASSIGN fi-intervalo:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-parc-venc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-parc-venc D-Dialog
ON VALUE-CHANGED OF tg-parc-venc IN FRAME D-Dialog /* Parc. por Vencto */
DO:
  IF SELF:INPUT-VALUE = YES THEN DO.
     ASSIGN fi-qt-parc:SENSITIVE IN FRAME {&FRAME-NAME} = YES.
     APPLY 'entry' TO fi-qt-parc.
     RETURN NO-APPLY.
  END.
  ELSE
     ASSIGN fi-qt-parc:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
ON 'RETURN':U ANYWHERE DO:
    APPLY 'TAB' TO SELF.
    RETURN NO-APPLY.
END.
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
  DISPLAY cb-tp-vencto tg-add-dias fi-dias fi-add-dias fi-dt-vencto 
          tg-dias-fixos tg-intervalo tg-parc-venc fi-dia-f1 fi-dia-f2 fi-dia-f3 
          fi-dia-f4 fi-dia-f5 fi-dia-f6 fi-intervalo fi-qt-parc 
      WITH FRAME D-Dialog.
  ENABLE RECT-10 RECT-6 RECT-7 RECT-8 RECT-9 rt-buttom br-cond-esp cb-tp-vencto 
         tg-add-dias fi-dias fi-dt-vencto bt-add bt-modifica bt-del 
         tg-dias-fixos tg-intervalo tg-parc-venc bt-ok bt-cancela bt-ajuda 
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

  /*  {utp/ut9000.i "D99XX999" "9.99.99.999"} */

  FIND FIRST tt-cond-ped NO-LOCK NO-ERROR.
  IF NOT AVAIL tt-cond-ped THEN DO.
     ASSIGN adm-new-record = YES.
     FIND cond-pagto WHERE
          cond-pagto.cod-cond-pag = i-cond-pagto NO-LOCK NO-ERROR.
     
     IF AVAIL cond-pagto THEN DO.
        DO i-ct = 1 TO 10.
           IF cond-pagto.prazos[i-ct] <> 0 THEN DO.
              CREATE tt-cond-ped.
              ASSIGN tt-cond-ped.nr-pedido = 9999
                     tt-cond-ped.nr-sequencia = i-ct * 10
                     tt-cond-ped.cod-vencto    = cond-pagto.cod-vencto
                     tt-cond-ped.nr-dias-venc = cond-pagto.prazos[i-ct]
                     tt-cond-ped.perc-pagto = cond-pagto.per-pg-dup[i-ct].
           END.
        END.
        ASSIGN fi-intervalo = cond-pagto.prazos[1].
     END.
     ELSE DO.
        /*
        CREATE tt-cond-ped.
        ASSIGN tt-cond-ped.nr-pedido = 9999
               tt-cond-ped.nr-sequencia = 10
               tt-cond-ped.nr-dias-venc = 30
               tt-cond-ped.perc-pagto = 100
               fi-intervalo = 30.
         */      
     END.
  END.
  ELSE DO.
      FIND ped-venda OF tt-cond-ped NO-LOCK NO-ERROR.

      ASSIGN fi-dia-f1 = INT(ENTRY(1,tt-cond-ped.char-2,";")) 
             fi-dia-f2 = INT(ENTRY(2,tt-cond-ped.char-2,";"))
             fi-dia-f3 = INT(ENTRY(3,tt-cond-ped.char-2,";"))
             fi-dia-f4 = INT(ENTRY(4,tt-cond-ped.char-2,";"))
             fi-dia-f5 = INT(ENTRY(5,tt-cond-ped.char-2,";"))
             fi-dia-f6 = INT(ENTRY(6,tt-cond-ped.char-2,";")) NO-ERROR.

      IF fi-dia-f1 <> 0 THEN
         ASSIGN tg-dias-fixos = YES.

      FOR EACH tt-cond-ped.
          CREATE wt-cond-ped.
          BUFFER-COPY tt-cond-ped TO wt-cond-ped.
      END.
  END.
                             
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  {&OPEN-QUERY-br-cond-esp}
   APPLY 'value-changed' TO br-cond-esp IN FRAME {&FRAME-NAME}. 

   IF p-acao = 'Consultar' THEN 
      DISABLE {&list-4} WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-acerta-seq D-Dialog 
PROCEDURE pi-acerta-seq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN i-ct = 1.
    FOR EACH tt-cond-ped. 
        ASSIGN tt-cond-ped.nr-sequencia = i-ct * 10  /* Acerta Sequencia */
               i-ct = i-ct + 1.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-busca-qtd-seq D-Dialog 
PROCEDURE pi-busca-qtd-seq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN i-qtd-seq = 0
           i-ult-seq = 0.
    FOR EACH b-cond-ped.
        ASSIGN i-qtd-seq = i-qtd-seq + 1.
    END.

    FOR LAST b-cond-ped BY b-cond-ped.nr-sequencia.
        ASSIGN i-ult-seq = b-cond-ped.nr-sequencia.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-perc-pagto D-Dialog 
PROCEDURE pi-perc-pagto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN pi-busca-qtd-seq.

    ASSIGN i-ct = 1.
    FOR EACH tt-cond-ped. 
        ASSIGN tt-cond-ped.perc-pagto = 100 / i-qtd-seq
               i-ct = i-ct + 1.
    END.

    ASSIGN i-ct = i-ct - 1.
    FIND LAST tt-cond-ped NO-ERROR.
    IF AVAIL tt-cond-ped THEN
       IF i-ct * tt-cond-ped.perc-pagto <> 100 THEN
          ASSIGN tt-cond-ped.perc-pagto = tt-cond-ped.perc-pagto + 
                                           (100 - (i-ct * tt-cond-ped.perc-pagto)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida D-Dialog 
PROCEDURE pi-valida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF SELF:SCREEN-VALUE <> '' THEN DO.
       IF SELF:INPUT-VALUE < 1 OR SELF:INPUT-VALUE > 30 THEN
          RETURN 'ADM-ERROR'.

       IF (SELF:NAME <> 'fi-dia-f1' AND SELF:INPUT-VALUE = fi-dia-f1) OR
          (SELF:NAME <> 'fi-dia-f2' AND SELF:INPUT-VALUE = fi-dia-f2) OR
          (SELF:NAME <> 'fi-dia-f3' AND SELF:INPUT-VALUE = fi-dia-f3) OR
          (SELF:NAME <> 'fi-dia-f4' AND SELF:INPUT-VALUE = fi-dia-f4) OR
          (SELF:NAME <> 'fi-dia-f5' AND SELF:INPUT-VALUE = fi-dia-f5) OR
          (SELF:NAME <> 'fi-dia-f6' AND SELF:INPUT-VALUE = fi-dia-f6) THEN
          RETURN 'ADM-ERROR'.

       IF (SELF:NAME = 'fi-dia-f1' AND fi-dia-f2 <> 0 AND SELF:INPUT-VALUE >= fi-dia-f2) OR
          (SELF:NAME = 'fi-dia-f2' AND SELF:INPUT-VALUE <= fi-dia-f1) OR
          (SELF:NAME = 'fi-dia-f2' AND fi-dia-f3 <> 0 AND SELF:INPUT-VALUE >= fi-dia-f3) OR
          (SELF:NAME = 'fi-dia-f3' AND SELF:INPUT-VALUE <= fi-dia-f2) OR
          (SELF:NAME = 'fi-dia-f3' AND fi-dia-f4 <> 0 AND SELF:INPUT-VALUE >= fi-dia-f4) OR
          (SELF:NAME = 'fi-dia-f4' AND SELF:INPUT-VALUE <= fi-dia-f3) OR
          (SELF:NAME = 'fi-dia-f4' AND fi-dia-f5 <> 0 AND SELF:INPUT-VALUE >= fi-dia-f5) OR
          (SELF:NAME = 'fi-dia-f5' AND SELF:INPUT-VALUE <= fi-dia-f4) OR
          (SELF:NAME = 'fi-dia-f5' AND fi-dia-f6 <> 0 AND SELF:INPUT-VALUE >= fi-dia-f6) OR
          (SELF:NAME = 'fi-dia-f6' AND SELF:INPUT-VALUE <= fi-dia-f5) THEN
          RETURN 'ADM-ERROR'.
    END.
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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-cond-ped"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-vencto D-Dialog 
FUNCTION fn-vencto RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR c-tp-vencto AS CHAR.

  {esinc/i-dsrb.i tt-cond-ped.cod-vencto tt-cond-ped.cod-vencto c-tp-vencto}.

  RETURN c-tp-vencto.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

