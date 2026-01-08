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

/* Temp Table Definitions ----                                          */
DEF TEMP-TABLE tt-ped-reserva LIKE ped-reserva
    FIELD visualiza    AS   LOG INIT YES.

DEF TEMP-TABLE tt-etiquetas
    FIELD cod-estabel  AS   CHAR
    FIELD row-tt-movto AS ROWID
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
    FIELD visualiza   AS  LOG INIT NO
    INDEX indice1 IS PRIMARY row-tt-movto num-etiqueta.

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER TABLE FOR tt-etiquetas.
DEFINE INPUT  PARAMETER p-qtd-disp     AS DEC.
DEFINE INPUT  PARAMETER p-it-codigo    AS CHAR.
DEFINE INPUT  PARAMETER p-cod-refer    AS CHAR.
DEFINE INPUT  PARAMETER p-nr-lote      AS CHAR.
DEFINE INPUT  PARAMETER p-corte-comerc AS CHAR.
DEFINE INPUT  PARAMETER i-tp-calculo   AS INT.
DEFINE INPUT  PARAMETER p-cod-estabel  AS CHAR.
DEFINE OUTPUT PARAMETER l-recalc       AS LOG.

/* Shared Variable Definitions ---                                      */
DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta AS ROWID NO-UNDO.

DEF NEW SHARED TEMP-TABLE wt-etiquetas LIKE ob-etiqueta.
DEF NEW SHARED VAR i-num-reserva LIKE ped-reserva.num-reserva.
DEF NEW SHARED VAR c-tp-acao      AS CHAR.
DEF NEW SHARED VAR de-qtd-disp    AS DEC.
DEF NEW SHARED VAR c-it-codigo    AS CHAR.
DEF NEW SHARED VAR c-cod-refer    AS CHAR.
DEF NEW SHARED VAR c-nr-lote      AS CHAR.
DEF NEW SHARED VAR c-corte-comerc AS CHAR.
DEF NEW SHARED VAR l-alterou-res   AS  LOG.
DEF NEW SHARED VAR c-estab        AS CHAR.

/* Local Variable Definitions ---                                       */
DEF VAR l-ok       AS LOG.
DEF VAR de-tot-res AS DEC.
DEF VAR de-ger-res AS DEC.
DEF VAR i-lin      AS INT INITIAL 99.
DEF VAR c-situacao AS CHAR FORMAT "x(9)".
DEF VAR c-empresa  AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-reservas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-ped-reserva

/* Definitions for BROWSE br-reservas                                   */
&Scoped-define FIELDS-IN-QUERY-br-reservas tt-ped-reserva.num-reserva fn-emitente() fn-repres() tt-ped-reserva.dt-validade   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-reservas   
&Scoped-define SELF-NAME br-reservas
&Scoped-define QUERY-STRING-br-reservas FOR EACH tt-ped-reserva WHERE                                  tt-ped-reserva.visualiza = YES NO-LOCK                                  INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-reservas OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-reserva WHERE                                  tt-ped-reserva.visualiza = YES NO-LOCK                                  INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-reservas tt-ped-reserva
&Scoped-define FIRST-TABLE-IN-QUERY-br-reservas tt-ped-reserva


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-reservas}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 rt-buttom rs-sel-res ~
br-reservas rs-manut-res bt-inclui bt-ok bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS rs-sel-res rs-manut-res 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 bt-cancela bt-detalhe bt-imprime 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-emitente D-Dialog 
FUNCTION fn-emitente RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-repres D-Dialog 
FUNCTION fn-repres RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "&Ajuda" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-cancela 
     IMAGE-UP FILE "image/img-era.bmp":U
     LABEL "Cancela" 
     SIZE 11.43 BY 1.13.

DEFINE BUTTON bt-detalhe 
     IMAGE-UP FILE "image/img-det.bmp":U
     LABEL "Detalhe" 
     SIZE 11.43 BY 1.13.

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/img-pri.bmp":U
     LABEL "Impress∆o" 
     SIZE 11.43 BY 1.13.

DEFINE BUTTON bt-inclui 
     IMAGE-UP FILE "image/imt-add.bmp":U
     LABEL "Modifica" 
     SIZE 11.43 BY 1.13.

DEFINE BUTTON bt-modifica 
     IMAGE-UP FILE "image/img-mod.bmp":U
     LABEL "Modifica" 
     SIZE 11.43 BY 1.13.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "Sair" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE rs-manut-res AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Nova Reserva", 1,
"Reserva Selecionada", 2
     SIZE 45 BY .75
     BGCOLOR 8 FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE rs-sel-res AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Reservas do Item", 1,
"Todas as Reservas", 2
     SIZE 51 BY 1
     BGCOLOR 8 FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 3.25
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.25
     BGCOLOR 8 .

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-reservas FOR 
      tt-ped-reserva SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-reservas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-reservas D-Dialog _FREEFORM
  QUERY br-reservas NO-LOCK DISPLAY
      tt-ped-reserva.num-reserva
      fn-emitente()              WIDTH 33  FORMAT "x(33)"
      fn-repres()                WIDTH 25  FORMAT "x(25)"
      tt-ped-reserva.dt-validade
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 79 BY 9.25
         FONT 1 ROW-HEIGHT-CHARS .63.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     rs-sel-res AT ROW 1.29 COL 4 NO-LABEL
     br-reservas AT ROW 2.67 COL 2
     rs-manut-res AT ROW 12.88 COL 3 NO-LABEL
     bt-inclui AT ROW 14 COL 3
     bt-modifica AT ROW 14 COL 15
     bt-cancela AT ROW 14 COL 27
     bt-detalhe AT ROW 14 COL 39
     bt-imprime AT ROW 14 COL 51.14
     bt-ok AT ROW 16 COL 3
     bt-ajuda AT ROW 16 COL 70.14
     " Manutená∆o de Reservas" VIEW-AS TEXT
          SIZE 22.29 BY .54 AT ROW 12.04 COL 2.86
          BGCOLOR 8 FONT 6
     RECT-1 AT ROW 12.25 COL 2
     RECT-2 AT ROW 1.17 COL 2
     rt-buttom AT ROW 15.75 COL 2
     SPACE(0.85) SKIP(0.15)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Detalha Reserva de Pedidos - ESSP0150H"
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
   NOT-VISIBLE FRAME-NAME L-To-R                                        */
/* BROWSE-TAB br-reservas rs-sel-res D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-cancela IN FRAME D-Dialog
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON bt-detalhe IN FRAME D-Dialog
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON bt-imprime IN FRAME D-Dialog
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON bt-modifica IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-reservas
/* Query rebuild information for BROWSE br-reservas
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-ped-reserva WHERE
                                 tt-ped-reserva.visualiza = YES NO-LOCK
                                 INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-reservas */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Detalha Reserva de Pedidos - ESSP0150H */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-reservas
&Scoped-define SELF-NAME br-reservas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-reservas D-Dialog
ON VALUE-CHANGED OF br-reservas IN FRAME D-Dialog
DO:
   ASSIGN rs-manut-res:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '1'.

   IF AVAIL tt-ped-reserva THEN 
      ASSIGN rs-manut-res:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '2'.

   APPLY 'VALUE-CHANGED' TO rs-manut-res.
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
ON CHOOSE OF bt-cancela IN FRAME D-Dialog /* Cancela */
DO:
  MESSAGE "Tem Certeza que deseja Eliminar TODOS os itens da reserva, e a Reserva " + 
          STRING(tt-ped-reserva.num-reserva) + " ?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          TITLE "Eliminar Reserva" UPDATE choice AS LOGICAL.
  IF choice THEN DO.
     ASSIGN l-recalc = YES.

     FIND ped-venda-ext WHERE
          ped-venda-ext.num-reserva = tt-ped-reserva.num-reserva NO-LOCK NO-ERROR.

     IF AVAIL ped-venda-ext THEN DO.
        MESSAGE 'Reserva j† est† relacionada ao Pedido: ' ped-venda-ext.nr-pedido SKIP
                'Impossivel Eliminar'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        NEXT.
     END.

     FIND ped-reserva WHERE
          ped-reserva.num-reserva = tt-ped-reserva.num-reserva 
          EXCLUSIVE-LOCK NO-ERROR.
     IF AVAIL ped-reserva THEN DO:
        FOR EACH ped-reserva-it WHERE
                 ped-reserva-it.num-reserva = ped-reserva.num-reserva EXCLUSIVE-LOCK.
            FOR EACH ped-reserva-etq OF ped-reserva-it EXCLUSIVE-LOCK.
                FIND ob-etiqueta WHERE
                     ob-etiqueta.cod-estabel  = ped-reserva.cod-estabel AND
                     ob-etiqueta.num-etiqueta = ped-reserva-etq.num-etiqueta
                     EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL ob-etiqueta THEN
                   ASSIGN ob-etiqueta.situacao = 3.

                DELETE ped-reserva-etq.
            END.
            DELETE ped-reserva-it.
        END.
        DELETE ped-reserva.
     END.
     DELETE tt-ped-reserva.
  END.

  {&OPEN-QUERY-br-reservas}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-detalhe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-detalhe D-Dialog
ON CHOOSE OF bt-detalhe IN FRAME D-Dialog /* Detalhe */
DO:
  ASSIGN i-num-reserva = tt-ped-reserva.num-reserva
             c-tp-acao = ''.

  FOR EACH wt-etiquetas.
      ASSIGN wt-etiquetas.situacao = 9.
  END.

  ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
  RUN esp/essp0162.w "SHARED".
  ASSIGN FRAME {&FRAME-NAME}:VISIBLE = YES.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime D-Dialog
ON CHOOSE OF bt-imprime IN FRAME D-Dialog /* Impress∆o */
DO:
  RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-inclui
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-inclui D-Dialog
ON CHOOSE OF bt-inclui IN FRAME D-Dialog /* Modifica */
DO:
   IF rs-manut-res:INPUT-VALUE = 1 THEN 
      ASSIGN i-num-reserva = 0
                c-tp-acao = 'Inc'.
   ELSE
      ASSIGN i-num-reserva = tt-ped-reserva.num-reserva
                 c-tp-acao = 'IncItem'.
 
   FOR EACH wt-etiquetas.
       ASSIGN wt-etiquetas.situacao = 3.
   END.

   ASSIGN l-alterou-res = NO.

   ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
   RUN esp/essp0162.w "SHARED".
   
   ASSIGN l-recalc = l-alterou-res.
   APPLY 'choose' TO bt-ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modifica D-Dialog
ON CHOOSE OF bt-modifica IN FRAME D-Dialog /* Modifica */
DO:
    ASSIGN i-num-reserva = tt-ped-reserva.num-reserva
           c-tp-acao = 'Mod'.

    FOR EACH wt-etiquetas.
        ASSIGN wt-etiquetas.situacao = 3.
    END.

    ASSIGN l-alterou-res = NO.
    ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
    RUN esp/essp0162.w "SHARED".

    ASSIGN l-recalc = l-alterou-res.
    APPLY 'choose' TO bt-ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-manut-res
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-manut-res D-Dialog
ON VALUE-CHANGED OF rs-manut-res IN FRAME D-Dialog
DO:
   ASSIGN bt-inclui:SENSITIVE IN FRAME {&FRAME-NAME} = YES
          bt-modifica:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
   DISABLE {&list-6} WITH FRAME {&FRAME-NAME}.
      
   IF SELF:INPUT-VALUE = 2 THEN DO.
      ENABLE {&list-6} WITH FRAME {&FRAME-NAME}.
      IF CAN-FIND(ped-reserva-it OF tt-ped-reserva WHERE
                  ped-reserva-it.it-codigo = c-it-codigo AND 
                  ped-reserva-it.cod-refer = c-cod-refer AND 
                  ped-reserva-it.nr-lote = c-nr-lote AND 
                  ped-reserva-it.corte-comerc = c-corte-comerc NO-LOCK) THEN 
         ASSIGN bt-inclui:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                bt-modifica:SENSITIVE IN FRAME {&FRAME-NAME} = YES. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-sel-res
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-sel-res D-Dialog
ON VALUE-CHANGED OF rs-sel-res IN FRAME D-Dialog
DO:
   FOR EACH tt-ped-reserva.
       ASSIGN tt-ped-reserva.visualiza = NO.
   END.

   IF SELF:INPUT-VALUE = 1 THEN DO.
      FOR EACH tt-ped-reserva.
          FOR EACH ped-reserva-it OF tt-ped-reserva WHERE
                   ped-reserva-it.it-codigo = c-it-codigo AND
                   ped-reserva-it.cod-refer = c-cod-refer AND
                   ped-reserva-it.nr-lote = c-nr-lote AND
                   ped-reserva-it.corte-comerc = c-corte-comerc NO-LOCK.
              ASSIGN tt-ped-reserva.visualiza = YES.
          END.
      END.
      IF NOT CAN-FIND(FIRST tt-ped-reserva WHERE
                            tt-ped-reserva.visualiza = YES) THEN DO.
         ASSIGN rs-manut-res:SCREEN-VALUE = '1'.
         APPLY 'VALUE-CHANGED' TO rs-manut-res.
      END.
   END.
   ELSE DO.
      FOR EACH tt-ped-reserva.
          ASSIGN tt-ped-reserva.visualiza = YES.
      END.
      
      FIND FIRST tt-ped-reserva WHERE
                 tt-ped-reserva.visualiza = YES NO-ERROR.
      
      IF NOT AVAIL tt-ped-reserva THEN DO.
         ASSIGN rs-manut-res:SCREEN-VALUE = '1'.
         APPLY 'VALUE-CHANGED' TO rs-manut-res.
      END.
   END.
   {&OPEN-QUERY-br-reservas}

   IF AVAIL tt-ped-reserva THEN DO.
      br-reservas:QUERY:GET-FIRST().
      br-reservas:QUERY:REPOSITION-TO-ROWID(ROWID(tt-ped-reserva)) IN FRAME {&FRAME-NAME}.
      br-reservas:SELECT-FOCUSED-ROW().
      APPLY 'VALUE-CHANGED' TO br-reservas.
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
  DISPLAY rs-sel-res rs-manut-res 
      WITH FRAME D-Dialog.
  ENABLE RECT-1 RECT-2 rt-buttom rs-sel-res br-reservas rs-manut-res bt-inclui 
         bt-ok bt-ajuda 
      WITH FRAME D-Dialog.
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

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    /* Busca Nome da Empresa */
    FIND FIRST param-global NO-LOCK NO-ERROR.
    FIND FIRST empresa
         WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
    ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN de-qtd-disp    = p-qtd-disp
           c-it-codigo    = p-it-codigo
           c-cod-refer    = p-cod-refer
           c-nr-lote      = p-nr-lote
           c-estab        = p-cod-estabel
           c-corte-comerc = p-corte-comerc.

    EMPTY TEMP-TABLE wt-etiquetas.

    FOR EACH tt-etiquetas WHERE
             tt-etiquetas.visualiza = YES NO-LOCK.

        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estabel  = p-cod-estabel AND
             ob-etiqueta.num-etiqueta = tt-etiquetas.num-etiqueta
             NO-LOCK NO-ERROR.

        IF ob-etiqueta.situacao <> 3 THEN NEXT.

        CREATE wt-etiquetas.
        BUFFER-COPY ob-etiqueta TO wt-etiquetas.
    END.

    EMPTY TEMP-TABLE tt-ped-reserva.
    FOR EACH ped-reserva WHERE
             ped-reserva.cod-estabel = p-cod-estabel AND
             ped-reserva.situacao = 1 NO-LOCK.
        CREATE tt-ped-reserva.
        BUFFER-COPY ped-reserva TO tt-ped-reserva
                    ASSIGN tt-ped-reserva.visualiza = YES.
    END.
    ASSIGN rs-sel-res:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '2'.

    FOR EACH tt-ped-reserva NO-LOCK.
        IF CAN-FIND(ped-reserva-it OF tt-ped-reserva WHERE
                    ped-reserva-it.it-codigo = c-it-codigo AND 
                    ped-reserva-it.cod-refer = c-cod-refer AND 
                    ped-reserva-it.nr-lote = c-nr-lote AND 
                    ped-reserva-it.corte-comerc = c-corte-comerc NO-LOCK) THEN DO.
            ASSIGN rs-sel-res:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '1'.
            LEAVE.
        END.
    END.
    APPLY 'VALUE-CHANGED' TO rs-sel-res.

    FIND FIRST tt-ped-reserva NO-ERROR.
    IF AVAIL tt-ped-reserva THEN
       br-reservas:QUERY:REPOSITION-TO-ROWID(ROWID(tt-ped-reserva)) IN FRAME {&FRAME-NAME}.

    DISABLE {&list-6} WITH FRAME {&FRAME-NAME}.

    IF NUM-RESULTS('br-reservas') > 0 THEN DO.
       APPLY 'VALUE-CHANGED' TO br-reservas.
       br-reservas:SELECT-FOCUSED-ROW().
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cabec D-Dialog 
PROCEDURE pi-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 PUT c-empresa  FORMAT "X(40)"                 AT  1
     "DATA: "                                  AT 60
     STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT 66
     "HORA: "                                  AT 86
     STRING(TIME,"hh:mm:ss")                   AT 92
     SKIP(1).
    
 PUT "RELATORIO DE DETALHE DA RESERVA" AT 40 SKIP(1).

 
 PUT "Reserva  Situacao  Dt.Reserva Cliente      Representante Item   Referencia Corte    Lote Quantidade" AT 1.
 PUT "-------- --------- ---------- ------------ ------------- ------ ---------- -------- ---- ----------" AT 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imprime D-Dialog 
PROCEDURE pi-imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 SYSTEM-DIALOG PRINTER-SETUP UPDATE l-ok.
 IF l-ok THEN DO:

    OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 61.
    PUT CONTROL "~033E~033(s16H". 

    ASSIGN de-tot-res =  0
           de-ger-res =  0
           i-lin      = 99.

    FIND ped-reserva WHERE
         ped-reserva.num-reserva = tt-ped-reserva.num-reserva NO-LOCK  NO-ERROR.
    IF AVAIL ped-reserva THEN DO:
       FIND emitente WHERE
            emitente.cod-emitente = ped-reserva.cod-emitente NO-LOCK NO-ERROR.
       FIND repres WHERE
            repres.cod-rep = ped-reserva.cod-rep NO-LOCK NO-ERROR.
    END.

    FOR EACH ped-reserva-it where
             ped-reserva-it.num-reserva = tt-ped-reserva.num-reserva NO-LOCK
        BREAK BY ped-reserva-it.num-reserva.
        ASSIGN de-tot-res = 0.
        FOR EACH  ped-reserva-etq OF ped-reserva-it NO-LOCK.
            FIND ped-reserva OF ped-reserva-etq NO-LOCK NO-ERROR.
            IF AVAIL ped-reserva THEN DO.
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel  = ped-reserva.cod-estabel AND
                    ob-etiqueta.num-etiqueta = ped-reserva-etq.num-etiqueta NO-LOCK NO-ERROR.
               IF AVAIL ob-etiqueta THEN
                  ASSIGN de-tot-res = de-tot-res + ob-etiqueta.quantidade.
            END.
        END.
        
        IF i-lin > 74 THEN DO:
           RUN pi-cabec.
           ASSIGN i-lin = 7.
        END.

        {esinc/i-dsrb.i ped-reserva.situacao ped-reserva.situacao c-situacao}  

        FIND corte-comerc WHERE
             corte-comerc.codigo = ped-reserva-it.corte-comerc NO-LOCK NO-ERROR.

        IF FIRST-OF(ped-reserva-it.num-reserva) THEN
           PUT ped-reserva.num-reserva FORMAT ">>>>>>>9" AT  1 
               c-situacao                                AT 10
               ped-reserva.dt-reserva                    AT 20
               emitente.nome-abrev FORMAT "x(12)"        AT 31
               repres.nome-abrev   FORMAT "x(12)"        AT 44.

        PUT ped-reserva-it.it-codigo FORMAT "x(6)"    AT 58
            ped-reserva-it.cod-refer                  AT 65
            corte-comerc.descricao FORMAT "x(8)"      AT 76
            ped-reserva-it.nr-lote FORMAT "x(2)"      AT 85
            de-tot-res FORMAT ">>>,>>9.99"            AT 90.

        ASSIGN de-ger-res = de-ger-res + de-tot-res.

   END.
   IF de-ger-res <> 0 THEN DO:
       PUT "----------"   AT 90 SKIP.
       PUT "TOTAL . . ."  AT 76.
       PUT de-ger-res FORMAT ">>>,>>9.99" AT 90.
   END.
   OUTPUT CLOSE.
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
  {src/adm/template/snd-list.i "tt-ped-reserva"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-emitente D-Dialog 
FUNCTION fn-emitente RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    FIND emitente WHERE
         emitente.cod-emit = tt-ped-reserva.cod-emit
         NO-LOCK NO-ERROR.
    IF AVAIL emitente THEN
       RETURN emitente.nome-emit.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-repres D-Dialog 
FUNCTION fn-repres RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND repres WHERE
       repres.cod-rep = tt-ped-reserva.cod-rep
       NO-LOCK NO-ERROR.
  IF AVAIL repres THEN
     RETURN repres.nome.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

