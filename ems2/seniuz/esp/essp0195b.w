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
{include/i-prgvrs.i ESSP0195B 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEF TEMP-TABLE tt-historico
    FIELD dt-manut AS CHAR FORMAT "x(10)"
    FIELD usuario  AS CHAR FORMAT "x(12)"
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD cod-refer    LIKE ob-etiqueta.cod-refer
    FIELD lote         LIKE ob-etiqueta.nr-lote
    FIELD corte-comerc LIKE corte-comerc.codigo.

DEF TEMP-TABLE tt-det-hist
    FIELD dt-manut     AS CHAR FORMAT "x(10)" 
    FIELD usuario      AS CHAR FORMAT "x(12)"
    FIELD cod-estabel  AS CHAR
    FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
    FIELD localiz-ant  LIKE ob-etiqueta.localizacao
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD cod-refer    LIKE ob-etiqueta.cod-refer
    FIELD lote         LIKE ob-etiqueta.nr-lote
    FIELD corte-comerc LIKE corte-comerc.codigo.

DEFINE INPUT PARAMETER TABLE FOR tt-historico.
DEFINE INPUT PARAMETER TABLE FOR tt-det-hist.

DEFINE INPUT PARAMETER p-it-codigo LIKE ob-etiqueta.it-codigo.
DEFINE INPUT PARAMETER p-cod-refer LIKE ob-etiqueta.cod-refer.
DEFINE INPUT PARAMETER p-lote LIKE ob-etiqueta.nr-lote.
DEFINE INPUT PARAMETER p-corte-comerc LIKE corte-comerc.codigo.



/* Local Variable Definitions ---                                       */
DEF VAR h-acomp   AS HANDLE NO-UNDO.
DEF VAR c-empresa AS CHAR.
DEF VAR c-data    AS CHAR FORMAT "x(10)".
DEF VAR de-total  AS DEC.

 /* Variaveis da Rotina de Impress∆o */
DEFINE VAR l-ok         AS LOG.
DEFINE VAR i-saida      AS INT.
DEFINE VAR c-saida      AS CHAR.
DEFINE VAR i-num-copias AS INT.
DEFINE VAR i-ct         AS INT.
DEFINE VAR i-lin        AS INT.
DEFINE VAR i-pag        AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-historico

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-historico

/* Definitions for BROWSE br-historico                                  */
&Scoped-define FIELDS-IN-QUERY-br-historico tt-historico.dt-manut tt-historico.usuario   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-historico   
&Scoped-define SELF-NAME br-historico
&Scoped-define QUERY-STRING-br-historico FOR EACH tt-historico WHERE                                  tt-historico.it-codigo    = p-it-codigo    AND                                  tt-historico.cod-refer    = p-cod-refer    AND                                  tt-historico.lote         = p-lote         AND                                  tt-historico.corte-comerc = p-corte-comerc NO-LOCK
&Scoped-define OPEN-QUERY-br-historico OPEN QUERY {&SELF-NAME} FOR EACH tt-historico WHERE                                  tt-historico.it-codigo    = p-it-codigo    AND                                  tt-historico.cod-refer    = p-cod-refer    AND                                  tt-historico.lote         = p-lote         AND                                  tt-historico.corte-comerc = p-corte-comerc NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-historico tt-historico
&Scoped-define FIRST-TABLE-IN-QUERY-br-historico tt-historico


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-historico}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-historico rt-buttom bt-imprime ~
bt-detalhe-ped bt-ajuda bt-ok bt-cancela 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

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

DEFINE BUTTON bt-detalhe-ped 
     IMAGE-UP FILE "image/imt-det-etq.bmp":U
     LABEL "Detalhe" 
     SIZE 15 BY 1.25 TOOLTIP "Detalhe da Manutená∆o Localizaá∆o".

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/img-pri.bmp":U
     LABEL "Impress∆o" 
     SIZE 13 BY 1.25 TOOLTIP "Imprime o Relat¢rio".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 44.43 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-historico FOR 
      tt-historico SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-historico
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-historico D-Dialog _FREEFORM
  QUERY br-historico NO-LOCK DISPLAY
      tt-historico.dt-manut    COLUMN-LABEL "Data Manutená∆o"  WIDTH 12.5 
      tt-historico.usuario     COLUMN-LABEL "Usuario"          WIDTH 28
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 44.29 BY 11.92
         FONT 1
         TITLE "Historico das Localizaá‰es Manuais" ROW-HEIGHT-CHARS .65.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     br-historico AT ROW 1.04 COL 1.57
     bt-imprime AT ROW 13.13 COL 2
     bt-detalhe-ped AT ROW 13.13 COL 19
     bt-ajuda AT ROW 14.75 COL 26
     bt-ok AT ROW 14.79 COL 2.57
     bt-cancela AT ROW 14.79 COL 13.57
     rt-buttom AT ROW 14.54 COL 1.57
     SPACE(0.85) SKIP(0.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Historico Manutená∆o Localizaá∆o - ESSP0195B"
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
/* BROWSE-TAB br-historico 1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-historico
/* Query rebuild information for BROWSE br-historico
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-historico WHERE
                                 tt-historico.it-codigo    = p-it-codigo    AND
                                 tt-historico.cod-refer    = p-cod-refer    AND
                                 tt-historico.lote         = p-lote         AND
                                 tt-historico.corte-comerc = p-corte-comerc NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-historico */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Historico Manutená∆o Localizaá∆o - ESSP0195B */
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


&Scoped-define SELF-NAME bt-detalhe-ped
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-detalhe-ped D-Dialog
ON CHOOSE OF bt-detalhe-ped IN FRAME D-Dialog /* Detalhe */
DO:
  IF AVAIL tt-historico THEN DO:
     ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
     RUN esp\essp0195c.w (INPUT TABLE tt-det-hist,
                          INPUT tt-historico.dt-manut,
                          INPUT tt-historico.usuario).
     ASSIGN FRAME {&FRAME-NAME}:VISIBLE = YES.
  END.
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


&Scoped-define BROWSE-NAME br-historico
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
  ENABLE br-historico rt-buttom bt-imprime bt-detalhe-ped bt-ajuda bt-ok 
         bt-cancela 
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

  /* {utp/ut9000.i "ESSP0174A" "2.04.00.000"} */ 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa
       WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").
 
  {&OPEN-QUERY-br-historico}
  APPLY 'entry' TO br-historico IN FRAME {&FRAME-NAME}.


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
  PUT c-empresa  FORMAT "X(40)"                 AT   1
      "DATA: "                                  AT  71
      STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  77
      "HORA: "                                  AT 104
      STRING(TIME,"hh:mm:ss")                   AT 110
      "PAG:"                                    AT 144
      i-pag FORMAT ">>>"                        AT 149
      SKIP(1).

  PUT "RELATORIO DA LOCALIZAÄ«O ETIQUETAS MANUAL DATA: " + tt-historico.dt-manut FORMAT "X(60)" AT 36 SKIP(1).


  PUT "ITEM    DESCRICAO                                REFERENCIA  LOTE CORTE COMERCIAL  ETIQUETA LOCAL ATU LOCAL ANT QUANTIDADE" AT 1.
  PUT "-----   ---------------------------------------  ----------  ---- --------------- --------- --------- --------- ----------" AT 1.
  ASSIGN i-pag = i-pag + 1.                                                                              
                                                                            
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
 DEF VAR h-prog AS HANDLE NO-UNDO.
 DEF VAR i-ct   AS INT.

 DEF VAR i-col AS INT INITIAL 0.


 RUN utp/ut-utils.p PERSISTENT SET h-prog.

 RUN esapi/saida-imp.p (OUTPUT i-saida,
                        OUTPUT c-saida,
                        OUTPUT i-num-copias,
                        OUTPUT l-ok).

 CASE i-saida:
     WHEN 1 THEN DO.
         OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 62.
         PUT CONTROL "~033E~033(s18H".    
     END.
     WHEN 2 THEN
         OUTPUT TO VALUE(c-saida).
     WHEN 3 THEN DO.
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0190b.tmp".
         OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
     END.
 END CASE.

 DO i-ct = 1 TO i-num-copias.
    ASSIGN i-pag      =  1
           i-lin      = 99
           de-total   =  0.
    FOR EACH tt-det-hist WHERE 
             tt-det-hist.dt-manut = tt-historico.dt-manut AND 
             tt-det-hist.usuario  = tt-historico.usuario NO-LOCK
          BY tt-det-hist.localiz-ant.

        FIND ob-etiqueta WHERE
             ob-etiqueta.cod-estabel  = tt-det-hist.cod-estabel AND
             ob-etiqueta.num-etiqueta = tt-det-hist.num-etiqueta NO-LOCK NO-ERROR.
        IF NOT AVAIL ob-etiqueta THEN NEXT.

        FIND ITEM WHERE
             ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

        FIND  corte-comerc WHERE
              corte-comerc.codigo = ob-etiqueta.corte-comerc NO-LOCK.

        IF i-lin > 62 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.

        PUT ob-etiqueta.it-codigo             FORMAT "x(6)"       AT   1
            item.desc-item                    FORMAT "x(40)"      AT   9
            ob-etiqueta.cod-refer             FORMAT "x(7)"       AT  50 
            ob-etiqueta.nr-lote               FORMAT "x(2)"       AT  62 
            corte-comerc.descricao            FORMAT "x(15)"      AT  67 
            ob-etiqueta.num-etiqueta          FORMAT ">>>>>>>>9"  AT  83 
            ob-etiqueta.localizacao           FORMAT "999/999"    AT  93 
            SUBSTR(ob-etiqueta.char-1, 23, 6) FORMAT "999/999"    AT 103 
            ob-etiqueta.quantidade            FORMAT ">>>,>>9.99" AT 113. 

        ASSIGN i-lin = i-lin + 1
               de-total = de-total + ob-etiqueta.quantidade.

    END.
    IF i-lin <>  99 THEN DO:
       PUT "----------" AT 113.
       PUT "TOTAL GERAL......." AT 67.
       PUT de-total   FORMAT ">>>,>>9.99" AT 113.
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
  {src/adm/template/snd-list.i "tt-historico"}

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

