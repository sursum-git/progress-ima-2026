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
{include/i-prgvrs.i ESSP0123A 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE TEMP-TABLE tt-compras NO-UNDO 
       FIELD dt-receb     LIKE mp-entr-mat.dt-recebimento
       FIELD nro-docto    LIKE mp-entr-mat.nro-docto
       FIELD cod-emit     LIKE mp-entr-mat.cod-emit
       FIELD padrao       AS CHAR FORMAT "x(20)"
       FIELD peso-nf      AS DEC
       FIELD qtd-fd-nf    AS INT
       FIELD peso-fd      AS DEC
       FIELD qtd-fd       AS INT.

/* Local Variable Definitions ---                                       */
DEF VAR h-acomp     AS HANDLE NO-UNDO.
DEF VAR c-empresa AS CHAR.

/* Variaveis da Rotina de Impress∆o */
DEFINE VAR i-saida      AS INT.
DEFINE VAR c-saida      AS CHAR.
DEFINE VAR i-num-copias AS INT.
DEFINE VAR i-lin        AS INT.
DEFINE VAR i-pag        AS INT.
DEFINE VAR l-ok         AS LOG.

DEF BUFFER b-tt-compras FOR tt-compras.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-compras

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-compras

/* Definitions for BROWSE br-compras                                    */
&Scoped-define FIELDS-IN-QUERY-br-compras tt-compras.dt-receb tt-compras.nro-docto tt-compras.cod-emit tt-compras.padrao tt-compras.peso-nf tt-compras.qtd-fd-nf tt-compras.peso-fd tt-compras.qtd-fd   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-compras   
&Scoped-define SELF-NAME br-compras
&Scoped-define OPEN-QUERY-br-compras RUN pi-total. OPEN QUERY {&SELF-NAME} FOR EACH tt-compras NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-compras tt-compras
&Scoped-define FIRST-TABLE-IN-QUERY-br-compras tt-compras


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-compras}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom IMAGE-1 IMAGE-29 IMAGE-30 IMAGE-32 ~
RECT-29 fi-dt-receb-ini fi-dt-receb-fin fi-nro-docto-ini fi-nro-docto-fin ~
bt-vapra br-compras bt-imprime bt-ok bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-dt-receb-ini fi-dt-receb-fin ~
fi-nro-docto-ini fi-nro-docto-fin fi-peso fi-nr-fardo 

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

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/img-pri.bmp":U
     LABEL "Impress∆o" 
     SIZE 13 BY 1.25.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-vapra 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "" 
     SIZE 5 BY .88 TOOLTIP "Procura o Padr∆o".

DEFINE VARIABLE fi-dt-receb-fin AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-receb-ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "Data Recebimento" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-fardo AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Qtde Fardos" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nro-docto-fin AS INTEGER FORMAT ">>>>>9":U INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nro-docto-ini AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Nß Nota Fiscal" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-peso AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Peso Total Fardos" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-29
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-30
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-32
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 3.25.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 93 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-compras FOR 
      tt-compras SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-compras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-compras D-Dialog _FREEFORM
  QUERY br-compras NO-LOCK DISPLAY
      tt-compras.dt-receb  COLUMN-LABEL "Dt.Receb"      
      tt-compras.nro-docto COLUMN-LABEL "Nß Docto"
      tt-compras.cod-emit  COLUMN-LABEL "Fornecedor"
      tt-compras.padrao    COLUMN-LABEL "Padrao" WIDTH 15.5
      tt-compras.peso-nf   COLUMN-LABEL "Peso Liquido NF"
      tt-compras.qtd-fd-nf COLUMN-LABEL "Qtd Fardo NF"
      tt-compras.peso-fd   COLUMN-LABEL "Peso Fardos Estoque"
      tt-compras.qtd-fd    COLUMN-LABEL "Qtd Fardos Estoque"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 93.29 BY 11.75
         FONT 1
         TITLE "Notas Fiscais de Compras Algod∆o" ROW-HEIGHT-CHARS .55.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-dt-receb-ini AT ROW 1.75 COL 26.86 COLON-ALIGNED WIDGET-ID 4
     fi-dt-receb-fin AT ROW 1.75 COL 51.72 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     fi-nro-docto-ini AT ROW 2.75 COL 26.86 COLON-ALIGNED WIDGET-ID 8
     fi-nro-docto-fin AT ROW 2.75 COL 51.72 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     bt-vapra AT ROW 2.75 COL 84 WIDGET-ID 20
     br-compras AT ROW 4.33 COL 1
     bt-imprime AT ROW 16.25 COL 4 WIDGET-ID 26
     fi-peso AT ROW 16.5 COL 57 COLON-ALIGNED WIDGET-ID 24
     fi-nr-fardo AT ROW 16.5 COL 79 COLON-ALIGNED WIDGET-ID 22
     bt-ok AT ROW 17.88 COL 2.57
     bt-cancela AT ROW 17.88 COL 13.57
     bt-ajuda AT ROW 17.88 COL 82.14
     rt-buttom AT ROW 17.63 COL 1.57
     IMAGE-1 AT ROW 1.75 COL 40.86 WIDGET-ID 10
     IMAGE-29 AT ROW 1.75 COL 48.43 WIDGET-ID 12
     IMAGE-30 AT ROW 2.75 COL 40.86 WIDGET-ID 14
     IMAGE-32 AT ROW 2.75 COL 48.43 WIDGET-ID 16
     RECT-29 AT ROW 1 COL 1.14 WIDGET-ID 18
     SPACE(0.99) SKIP(14.82)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Consulta NFs  Entrada Fardos - ESSP0123A"
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
/* BROWSE-TAB br-compras bt-vapra D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-nr-fardo IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-peso IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-compras
/* Query rebuild information for BROWSE br-compras
     _START_FREEFORM
RUN pi-total.
OPEN QUERY {&SELF-NAME} FOR EACH tt-compras NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-compras */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Consulta NFs  Entrada Fardos - ESSP0123A */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-compras
&Scoped-define SELF-NAME br-compras
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-compras D-Dialog
ON ROW-DISPLAY OF br-compras IN FRAME D-Dialog /* Notas Fiscais de Compras Algod∆o */
DO:
  IF tt-compras.qtd-fd-nf <> tt-compras.qtd-fd OR
     tt-compras.peso-nf   <> tt-compras.peso-fd THEN
     RUN pi-cor (INPUT 12).
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


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime D-Dialog
ON CHOOSE OF bt-imprime IN FRAME D-Dialog /* Impress∆o */
DO:
  RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapra D-Dialog
ON CHOOSE OF bt-vapra IN FRAME D-Dialog
DO:
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-dt-receb-ini
         INPUT FRAME {&FRAME-NAME} fi-dt-receb-fin
         INPUT FRAME {&FRAME-NAME} fi-nro-docto-ini
         INPUT FRAME {&FRAME-NAME} fi-nro-docto-fin.

  RUN pi-popula-browse.
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
  DISPLAY fi-dt-receb-ini fi-dt-receb-fin fi-nro-docto-ini fi-nro-docto-fin 
          fi-peso fi-nr-fardo 
      WITH FRAME D-Dialog.
  ENABLE rt-buttom IMAGE-1 IMAGE-29 IMAGE-30 IMAGE-32 RECT-29 fi-dt-receb-ini 
         fi-dt-receb-fin fi-nro-docto-ini fi-nro-docto-fin bt-vapra br-compras 
         bt-imprime bt-ok bt-cancela bt-ajuda 
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
  FIND FIRST empresa WHERE
             empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

  ASSIGN fi-dt-receb-ini:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "01" + STRING(MONTH(TODAY)) + STRING(YEAR(TODAY))
         fi-dt-receb-fin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).

  APPLY 'entry' TO fi-dt-receb-ini IN FRAME {&FRAME-NAME}.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cor D-Dialog 
PROCEDURE pi-cor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER p-cor AS INT.

 tt-compras.dt-receb:FGCOLOR IN BROWSE {&browse-name} = p-cor.
 tt-compras.nro-docto:FGCOLOR IN BROWSE {&browse-name} = p-cor.
 tt-compras.cod-emit:FGCOLOR IN BROWSE {&browse-name} = p-cor.
 tt-compras.padrao:FGCOLOR IN BROWSE {&browse-name} = p-cor.
 tt-compras.peso-nf:FGCOLOR IN BROWSE {&browse-name} = p-cor.
 tt-compras.qtd-fd-nf:FGCOLOR IN BROWSE {&browse-name} = p-cor.
 tt-compras.peso-fd:FGCOLOR IN BROWSE {&browse-name} = p-cor.
 tt-compras.qtd-fd:FGCOLOR IN BROWSE {&browse-name} = p-cor.


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
      "DATA: "                                  AT  58
      STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  64
      "HORA: "                                  AT  85
      STRING(TIME,"hh:mm:ss")                   AT  91
      "PAG:"                                    AT 125
      i-pag FORMAT ">>"                         AT 130
      SKIP(1).

  PUT "RELAT‡RIO DAS NFs DE ENTRADA DE FARDOS NO ESTOQUE - PERIODO: " AT 21
      fi-dt-receb-ini AT 82
      "A"             AT 93
      fi-dt-receb-fin AT 95 SKIP(1).

  PUT "Dt.Receb   Nß Docto Fornecedor Padrao               Peso Liquido NF Qtd Fardo NF Peso Fardo Estoque Qtd Fardos Estoque" AT 1.
  PUT "---------- -------- ---------- -------------------- --------------- ------------ ------------------ ------------------" AT 1.

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

 DEF VAR h-prog    AS HANDLE NO-UNDO.
 DEF VAR i-ct      AS INT.
 DEF VAR de-totais AS DEC EXTENT 3.

 RUN utp/ut-utils.p PERSISTENT SET h-prog.


 RUN esapi/saida-imp.p (OUTPUT i-saida,
                        OUTPUT c-saida,
                        OUTPUT i-num-copias,
                        OUTPUT l-ok).

 CASE i-saida:
     WHEN 1 THEN DO.
         OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 64.
         PUT CONTROL "~033E~033(s18H".    
     END.
     WHEN 2 THEN
         OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
     WHEN 3 THEN DO.
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0123a.tmp".
         OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
     END.
 END CASE.

 ASSIGN i-lin     = 99
        i-pag     =  1
        de-totais =  0.

 DO i-ct = 1 TO i-num-copias.
    FOR EACH b-tt-compras 
             NO-LOCK
             BY b-tt-compras.dt-receb.
    
        IF i-lin > 64 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.
        PUT b-tt-compras.dt-receb                          AT    1
            b-tt-compras.nro-docto                         AT   12
            b-tt-compras.cod-emit                          AT   21
            b-tt-compras.padrao    FORMAT "X(20)"          AT   32
            b-tt-compras.peso-nf   FORMAT ">>>,>>>,>>9.99" AT   54
            b-tt-compras.qtd-fd-nf FORMAT ">>>>,>>9"       AT   73
            b-tt-compras.peso-fd   FORMAT ">>>,>>>,>>9.99" AT   86
            b-tt-compras.qtd-fd    FORMAT ">>>>,>>9"       AT  111.
        ASSIGN i-lin = i-lin + 1.
        ACCUMULATE b-tt-compras.peso-nf   (TOTAL).
        ACCUMULATE b-tt-compras.qtd-fd-nf (TOTAL).
        ACCUMULATE b-tt-compras.peso-fd   (TOTAL).
        ACCUMULATE b-tt-compras.qtd-fd    (TOTAL).
    
    END.
    IF (ACCUM TOTAL b-tt-compras.peso-fd) <> 0 THEN DO:
       PUT "--------------- ------------ ------------------ ------------------" AT 53 SKIP.
       PUT ACCUM TOTAL b-tt-compras.peso-nf    FORMAT ">>>,>>>,>>9.99" AT  54.
       PUT ACCUM TOTAL b-tt-compras.qtd-fd-nf  FORMAT ">>>>,>>9"       AT  73.
       PUT ACCUM TOTAL b-tt-compras.peso-fd    FORMAT ">>>,>>>,>>9.99" AT  86.
       PUT ACCUM TOTAL b-tt-compras.qtd-fd     FORMAT ">>>>,>>9"       AT 111.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse D-Dialog 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR i-qtd-fd   AS INT.
 DEF VAR de-peso-fd AS DEC.

 RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
 {utp/ut-liter.i Selecionando_Fardos *}
 RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

 FOR EACH tt-compras.
     DELETE tt-compras.
 END.

 FOR EACH mp-entr-mat WHERE
          mp-entr-mat.nro-docto      >= fi-nro-docto-ini AND
          mp-entr-mat.nro-docto      <= fi-nro-docto-fin AND
          mp-entr-mat.dt-recebimento >= fi-dt-receb-ini AND
          mp-entr-mat.dt-recebimento <= fi-dt-receb-fin NO-LOCK.

     ASSIGN i-qtd-fd   = 0
            de-peso-fd = 0.
     FOR EACH mp-fardo OF mp-entr-mat NO-LOCK.

         RUN pi-acompanhar IN h-acomp (INPUT "Nota Fiscal: " + STRING(mp-entr-mat.nro-docto)
                                              + " Fardo: " + TRIM(STRING(mp-fardo.nr-fardo,"9999,9999"))).

         ASSIGN i-qtd-fd = i-qtd-fd + 1
                de-peso-fd = de-peso-fd + mp-fardo.peso.
     END.
     
     FIND tt-compras WHERE
          tt-compras.cod-emit = mp-entr-mat.cod-emit AND
          tt-compras.nro-docto = mp-entr-mat.nro-docto NO-LOCK NO-ERROR.
     IF NOT AVAIL tt-compras THEN DO:
        CREATE tt-compras.
        ASSIGN tt-compras.dt-receb  = mp-entr-mat.dt-recebimento
               tt-compras.cod-emit  = mp-entr-mat.cod-emit
               tt-compras.nro-docto = mp-entr-mat.nro-docto
               tt-compras.padrao    = mp-entr-mat.padrao[1]
               tt-compras.peso-nf   = mp-entr-mat.peso-nf
               tt-compras.qtd-fd-nf = mp-entr-mat.qtd-fardos[1] + mp-entr-mat.qtd-fardos[2] + mp-entr-mat.qtd-fardos[3] +
                                      mp-entr-mat.qtd-fardos[4] + mp-entr-mat.qtd-fardos[5]
               tt-compras.peso-fd   = de-peso-fd
               tt-compras.qtd-fd    = i-qtd-fd.
     END.
 END.

 RUN pi-finalizar in h-acomp.

{&OPEN-QUERY-BR-COMPRAS}

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
 FOR EACH tt-compras NO-LOCK.
     ASSIGN fi-nr-fardo = fi-nr-fardo + tt-compras.qtd-fd
            fi-peso     = fi-peso + tt-compras.peso-fd.
 END.
 DISPLAY fi-nr-fardo
         fi-peso
         WITH FRAME {&FRAME-NAME}.

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
  {src/adm/template/snd-list.i "tt-compras"}

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

