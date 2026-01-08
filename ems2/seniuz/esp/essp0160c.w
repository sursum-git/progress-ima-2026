&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
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
{include/i-prgvrs.i ESSP0150D 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF TEMP-TABLE tt-etiquetas
    FIELD cod-estabel  LIKE ob-etiqueta.cod-estabel
    FIELD nr-pedcli    LIKE ped-item.nr-pedcli 
    FIELD nr-sequencia LIKE ped-item.nr-sequencia
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
    FIELD visualiza    AS   LOG
    INDEX indice1 IS PRIMARY nr-pedcli nr-sequencia num-etiqueta
    INDEX indice2 num-etiqueta.

DEFINE INPUT PARAMETER TABLE FOR tt-etiquetas.  

/* Local Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR gr-ob-etiqueta AS ROWID NO-UNDO.
DEF VAR c-qualid LIKE qualid-tecido.descricao.
DEF VAR c-tecelagem AS CHAR FORMAT "x(10)".
DEF VAR i-lin       AS INT.
DEF VAR de-tot-etq  AS DEC.
DEF VAR c-empresa   AS CHAR.

/* Variaveis da Rotina de Impress∆o */
DEFINE VAR l-ok                AS LOG.
DEFINE VAR i-saida             AS INT.
DEFINE VAR c-saida             AS CHAR.
DEFINE VAR i-num-copias        AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-etiquetas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-etiquetas ob-etiqueta

/* Definitions for BROWSE br-etiquetas                                  */
&Scoped-define FIELDS-IN-QUERY-br-etiquetas ob-etiqueta.localizacao tt-etiquetas.num-etiqueta ob-etiqueta.it-codigo ob-etiqueta.cod-refer ob-etiqueta.nr-lote ob-etiqueta.corte-comerc ob-etiqueta.quantidade ob-etiqueta.nuance fn-qualid() @ c-qualid fn-tecelagem() @ c-tecelagem   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etiquetas   
&Scoped-define SELF-NAME br-etiquetas
&Scoped-define OPEN-QUERY-br-etiquetas RUN pi-total. OPEN QUERY {&SELF-NAME} FOR EACH tt-etiquetas WHERE                                  tt-etiquetas.visualiza = YES NO-LOCK, ~
                                   FIRST ob-etiqueta WHERE                                   ob-etiqueta.cod-estabel = tt-etiquetas.cod-estabel AND                                   ob-etiqueta.num-etiqueta = tt-etiquetas.num-etiqueta NO-LOCK                                   BY ob-etiqueta.localiz                                   BY ob-etiqueta.num-etiqueta DESCENDING.
&Scoped-define TABLES-IN-QUERY-br-etiquetas tt-etiquetas ob-etiqueta
&Scoped-define FIRST-TABLE-IN-QUERY-br-etiquetas tt-etiquetas
&Scoped-define SECOND-TABLE-IN-QUERY-br-etiquetas ob-etiqueta


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-etiquetas}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom br-etiquetas bt-detalhe bt-imprime ~
bt-ok bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-etq 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-qualid D-Dialog 
FUNCTION fn-qualid RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-tecelagem D-Dialog 
FUNCTION fn-tecelagem RETURNS CHARACTER
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

DEFINE BUTTON bt-cancela AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-detalhe 
     IMAGE-UP FILE "image/img-det.bmp":U
     LABEL "Detalhe" 
     SIZE 13 BY 1.13.

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/img-pri.bmp":U
     LABEL "Impress∆o" 
     SIZE 13 BY 1.13.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-tot-etq AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 79 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-etiquetas FOR 
      tt-etiquetas, 
      ob-etiqueta SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etiquetas D-Dialog _FREEFORM
  QUERY br-etiquetas NO-LOCK DISPLAY
      ob-etiqueta.localizacao  FORMAT "999/999" WIDTH 8
      tt-etiquetas.num-etiqueta
      ob-etiqueta.it-codigo   FORMAT "x(8)" WIDTH 7
      ob-etiqueta.cod-refer
      ob-etiqueta.nr-lote     WIDTH 4
      ob-etiqueta.corte-comerc
      ob-etiqueta.quantidade
      ob-etiqueta.nuance
      fn-qualid() @ c-qualid   WIDTH 11    COLUMN-LABEL "Qualidade"
      fn-tecelagem() @ c-tecelagem WIDTH 11  COLUMN-LABEL "Tecelagem"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 79 BY 11.67
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     br-etiquetas AT ROW 1.25 COL 2
     bt-detalhe AT ROW 13.25 COL 2.57
     bt-imprime AT ROW 13.25 COL 16.14
     fi-tot-etq AT ROW 13.25 COL 30 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 15 COL 3
     bt-cancela AT ROW 15 COL 14
     bt-ajuda AT ROW 15 COL 70.14
     rt-buttom AT ROW 14.75 COL 2
     SPACE(0.56) SKIP(0.20)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Detalha Etiquetas - ESSP0160C"
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
/* BROWSE-TAB br-etiquetas rt-buttom D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-tot-etq IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etiquetas
/* Query rebuild information for BROWSE br-etiquetas
     _START_FREEFORM
RUN pi-total.
OPEN QUERY {&SELF-NAME} FOR EACH tt-etiquetas WHERE
                                 tt-etiquetas.visualiza = YES NO-LOCK,
                            FIRST ob-etiqueta WHERE
                                  ob-etiqueta.cod-estabel = tt-etiquetas.cod-estabel AND
                                  ob-etiqueta.num-etiqueta = tt-etiquetas.num-etiqueta NO-LOCK
                                  BY ob-etiqueta.localiz
                                  BY ob-etiqueta.num-etiqueta DESCENDING.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-etiquetas */
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
ON ENTRY OF FRAME D-Dialog /* Detalha Etiquetas - ESSP0160C */
DO:
  APPLY 'cursor-left' TO br-etiquetas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Detalha Etiquetas - ESSP0160C */
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


&Scoped-define SELF-NAME bt-detalhe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-detalhe D-Dialog
ON CHOOSE OF bt-detalhe IN FRAME D-Dialog /* Detalhe */
DO:
   ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
   ASSIGN gr-ob-etiqueta = ROWID(ob-etiqueta).
   RUN esp/essp0146.p.
   ASSIGN FRAME {&FRAME-NAME}:VISIBLE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime D-Dialog
ON CHOOSE OF bt-imprime IN FRAME D-Dialog /* Impress∆o */
DO:
   CLOSE QUERY br-etiquetas.
   ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
   RUN pi-imprime.
   {&OPEN-QUERY-br-etiquetas}
   ASSIGN FRAME {&FRAME-NAME}:VISIBLE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etiquetas
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
  DISPLAY fi-tot-etq 
      WITH FRAME D-Dialog.
  ENABLE rt-buttom br-etiquetas bt-detalhe bt-imprime bt-ok bt-cancela bt-ajuda 
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

  /* {utp/ut9000.i "ESSP0150D" "2.04.00.000"} */ 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa WHERE
             empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec D-Dialog 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 PUT c-empresa  FORMAT "X(40)"                 AT  1
     "DATA: "                                  AT 47
     STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT 53
     "HORA: "                                  AT 67
     STRING(TIME,"hh:mm:ss")                   AT 73
     SKIP(1).
    
 PUT "RELATORIO DE DETALHAMENTO DAS ETIQUETAS" AT 21 SKIP(1).

 
 PUT "ETIQUETA    ITEM  REFER. LOTE QUANTIDADE NUANCE LOCALIZ QUALIDADE     TECELAGEM" AT 1.
 PUT "--------- ------ ------- ---- ---------- ------ ------- ------------- ---------" AT 1.
                                                                            
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
 DEF VAR h-prog  AS HANDLE NO-UNDO.
 DEF VAR i-ct    AS INT.

 RUN utp/ut-utils.p PERSISTENT SET h-prog.


 RUN esapi/saida-imp.p (OUTPUT i-saida,
                        OUTPUT c-saida,
                        OUTPUT i-num-copias,
                        OUTPUT l-ok).

 CASE i-saida:
     WHEN 1 THEN DO.
         OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 74.
         PUT CONTROL "~033E~033(s18H".    
     END.
     WHEN 2 THEN
         OUTPUT TO VALUE(c-saida).
     WHEN 3 THEN DO.
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0160c.tmp".
         OUTPUT TO VALUE(c-saida).
     END.
 END CASE.

 DO i-ct = 1 TO i-num-copias.
    ASSIGN de-tot-etq =  0
           i-lin      = 99.
    FOR EACH tt-etiquetas WHERE
             tt-etiquetas.visualiza NO-LOCK,
        FIRST ob-etiqueta WHERE
              ob-etiqueta.cod-estabel = tt-etiquetas.cod-estabel AND
              ob-etiqueta.num-etiqueta = tt-etiquetas.num-etiqueta NO-LOCK
              BY ob-etiqueta.localiz
              BY ob-etiqueta.num-etiqueta DESCENDING.
        
        IF i-lin > 74 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.
        /*
        FIND ordem-benefic OF ob-etiqueta NO-LOCK NO-ERROR.
        ASSIGN c-tecelagem = "".
        IF AVAIL ordem-benefic THEN
           ASSIGN c-tecelagem = ordem-benefic.tipo-tear.
        */
        FIND qualid-tecido WHERE
             qualid-tecido.codigo = ob-etiqueta.cod-qualid NO-LOCK NO-ERROR.
        IF AVAIL qualid-tecido THEN
           ASSIGN c-qualid = qualid-tecido.descricao.
        PUT tt-etiquetas.num-etiqueta                   AT  1
            ob-etiqueta.it-codigo   FORMAT "x(6)"       AT 11
            ob-etiqueta.cod-refer                       AT 18
            ob-etiqueta.nr-lote     FORMAT  "x(2)"      AT 26
            ob-etiqueta.quantidade  FORMAT ">>>,>>9.99" AT 31
            ob-etiqueta.nuance                          AT 42
            ob-etiqueta.localizacao FORMAT "999/999"    AT 49
            c-qualid   FORMAT "x(13)"                   AT 57
            c-tecelagem                                 AT 71.
        ASSIGN i-lin = i-lin + 1
               de-tot-etq = de-tot-etq + ob-etiqueta.quantidade.
    END.
    IF de-tot-etq <> 0 THEN DO:
       PUT "TOTAL . . ."  AT 18
           "----------"   AT 31.
       PUT de-tot-etq FORMAT ">>>,>>9.99" AT 31.
    END.
    IF i-saida = 1 THEN DO:
       PAGE.
       PUT "" AT 1.
    END.
 END.
 IF i-saida = 3 THEN DO.
  RUN EXECUTE IN h-prog(INPUT "notepad.exe",
                        INPUT c-saida).
  DELETE PROCEDURE h-prog.
 END.
 OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-total D-Dialog 
PROCEDURE pi-total :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-tot-etq = 0.
    FOR EACH tt-etiquetas WHERE
             tt-etiquetas.visualiza NO-LOCK,
        FIRST ob-etiqueta WHERE
              ob-etiqueta.cod-estabel = tt-etiquetas.cod-estabel AND
              ob-etiqueta.num-etiqueta = tt-etiquetas.num-etiqueta NO-LOCK.
        ASSIGN fi-tot-etq = fi-tot-etq + ob-etiqueta.quantidade.
    END.
    DISP fi-tot-etq WITH FRAME {&FRAME-NAME}.

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
  {src/adm/template/snd-list.i "tt-etiquetas"}
  {src/adm/template/snd-list.i "ob-etiqueta"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-qualid D-Dialog 
FUNCTION fn-qualid RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND qualid-tecido WHERE
       qualid-tecido.codigo = ob-etiqueta.cod-qualid
       NO-LOCK NO-ERROR.
  IF AVAIL qualid-tecido THEN 
     RETURN qualid-tecido.descricao.   /* Function return value. */
  ELSE
     RETURN "N∆o Informada".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-tecelagem D-Dialog 
FUNCTION fn-tecelagem RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   /*
   FIND ordem-benefic OF ob-etiqueta
        NO-LOCK NO-ERROR.

   ASSIGN c-tecelagem = "?".
   IF AVAIL ordem-benefic THEN
      ASSIGN c-tecelagem = ordem-benefic.tipo-tear.
  */
  RETURN c-tecelagem.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

