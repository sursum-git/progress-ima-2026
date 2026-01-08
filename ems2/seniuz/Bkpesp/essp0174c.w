&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          mgcad            PROGRESS
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
{include/i-prgvrs.i ESSP0174C 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE TEMP-TABLE tt-fat  NO-UNDO 
       FIELD tipo         AS CHAR
       FIELD dia          AS CHAR
       FIELD nr-nota-fis  LIKE nota-fiscal.nr-nota-fis
       FIELD dt-emis-nota LIKE nota-fiscal.dt-emis-nota
       FIELD dt-saida     LIKE nota-fiscal.dt-saida
       FIELD nome-ab-cli  LIKE nota-fiscal.nome-ab-cli
       FIELD nr-pedcli    LIKE nota-fiscal.nr-pedcli
       FIELD no-ab-reppri LIKE nota-fiscal.no-ab-reppri
       FIELD qtde         AS DEC
       FIELD valor        AS DEC
       INDEX indice1 dia nr-nota-fis.

DEFINE INPUT PARAMETER TABLE FOR tt-fat.  
DEFINE INPUT PARAMETER p-dia      AS CHAR.
DEFINE INPUT PARAMETER p-periodo  AS CHAR.
DEFINE INPUT PARAMETER p-tp-relat AS INT.

/* Local Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR gr-nota-fiscal AS ROWID NO-UNDO.

DEF VAR i-lin AS INT.
DEF VAR i-pag AS INT.
DEF VAR c-data AS CHAR FORMAT "x(10)".
DEF VAR c-empresa AS CHAR.

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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-nfs

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-fat

/* Definitions for BROWSE br-nfs                                        */
&Scoped-define FIELDS-IN-QUERY-br-nfs tt-fat.tipo tt-fat.nr-nota-fis tt-fat.dt-emis-nota tt-fat.dt-saida tt-fat.nr-pedcli tt-fat.nome-ab-cli tt-fat.no-ab-reppri tt-fat.qtde tt-fat.valor   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-nfs   
&Scoped-define SELF-NAME br-nfs
&Scoped-define OPEN-QUERY-br-nfs RUN pi-total. OPEN QUERY {&SELF-NAME} FOR EACH tt-fat WHERE                                  tt-fat.dia = p-dia NO-LOCK                               BY tt-fat.nr-nota-fis.
&Scoped-define TABLES-IN-QUERY-br-nfs tt-fat
&Scoped-define FIRST-TABLE-IN-QUERY-br-nfs tt-fat


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-nfs}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-nfs bt-detalhe bt-imprime bt-ok ~
bt-cancela bt-ajuda rt-buttom 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-qtd-nor fi-tot-vlr-nor ~
fi-tot-qtd-ljs fi-tot-vlr-ljs fi-tot-qtd-ind fi-tot-vlr-ind fi-tot-qtd-out ~
fi-tot-vlr-out fi-tot-qtd-geral fi-tot-vlr-geral 

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

DEFINE BUTTON bt-detalhe 
     IMAGE-UP FILE "image/img-det.bmp":U
     LABEL "Detalhe" 
     SIZE 13 BY 1.25 TOOLTIP "Detalhe da Nota Fiscal".

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/img-pri.bmp":U
     LABEL "Impress∆o" 
     SIZE 13 BY 1.25.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-tot-qtd-geral AS DECIMAL FORMAT "  ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-qtd-ind AS DECIMAL FORMAT "  ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.57 BY .88
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-tot-qtd-ljs AS DECIMAL FORMAT "  ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.57 BY .88
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE fi-tot-qtd-nor AS DECIMAL FORMAT "  ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.57 BY .88
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tot-qtd-out AS DECIMAL FORMAT "  ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.57 BY .88
     FGCOLOR 13  NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-geral AS DECIMAL FORMAT "   ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-ind AS DECIMAL FORMAT "   ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-ljs AS DECIMAL FORMAT "   ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-nor AS DECIMAL FORMAT "   ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-out AS DECIMAL FORMAT "   ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FGCOLOR 13  NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 94 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-nfs FOR 
      tt-fat SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-nfs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-nfs D-Dialog _FREEFORM
  QUERY br-nfs NO-LOCK DISPLAY
      tt-fat.tipo         COLUMN-LABEL "Tipo"      FORMAT "x(8)"

      tt-fat.nr-nota-fis  COLUMN-LABEL "N.Fiscal"  FORMAT "x(10)"  COLUMN-BGCOLOR 8

      tt-fat.dt-emis-nota COLUMN-LABEL "Dt.Emissao"    WIDTH 9.15

      tt-fat.dt-saida     COLUMN-LABEL "Dt.Saida"      WIDTH 9.15

      tt-fat.nr-pedcli    COLUMN-LABEL "Pedido"        WIDTH 6.10

      tt-fat.nome-ab-cli  COLUMN-LABEL "Cliente"       WIDTH 12.60

      tt-fat.no-ab-reppri COLUMN-LABEL "Representante" WIDTH 12.15

      tt-fat.qtde         COLUMN-LABEL "Metros Faturados"

      tt-fat.valor    FORMAT ">>>>,>>>,>>9.99"    COLUMN-LABEL "Valores Faturados"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 94 BY 11.92
         FONT 1
         TITLE "" ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     br-nfs AT ROW 1 COL 2
     bt-detalhe AT ROW 13.08 COL 2.57
     bt-imprime AT ROW 13.08 COL 16.14
     fi-tot-qtd-nor AT ROW 13.08 COL 66.86 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fi-tot-vlr-nor AT ROW 13.08 COL 79.14 COLON-ALIGNED NO-LABEL
     fi-tot-qtd-ljs AT ROW 14.08 COL 66.86 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fi-tot-vlr-ljs AT ROW 14.08 COL 79.14 COLON-ALIGNED NO-LABEL
     fi-tot-qtd-ind AT ROW 15.08 COL 66.86 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fi-tot-vlr-ind AT ROW 15.08 COL 79.14 COLON-ALIGNED NO-LABEL
     fi-tot-qtd-out AT ROW 16.08 COL 66.86 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fi-tot-vlr-out AT ROW 16.08 COL 79.14 COLON-ALIGNED NO-LABEL
     fi-tot-qtd-geral AT ROW 17.08 COL 66.86 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fi-tot-vlr-geral AT ROW 17.08 COL 79.14 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 18.33 COL 2.72
     bt-cancela AT ROW 18.33 COL 13.72
     bt-ajuda AT ROW 18.33 COL 84.57
     rt-buttom AT ROW 18.08 COL 1.72
     "Normal:" VIEW-AS TEXT
          SIZE 6 BY .88 AT ROW 13.08 COL 62.57
     "Lojas:" VIEW-AS TEXT
          SIZE 4.57 BY .88 AT ROW 14.04 COL 63.86
     "Industrializaá∆o:" VIEW-AS TEXT
          SIZE 11 BY .88 AT ROW 15.04 COL 57
     "Total Geral:" VIEW-AS TEXT
          SIZE 8 BY .88 AT ROW 16.83 COL 60.14
     "Outros:" VIEW-AS TEXT
          SIZE 5.57 BY .88 AT ROW 16.04 COL 63.29
     SPACE(27.42) SKIP(2.78)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Detalha Faturamento - ESSP0174C"
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
   NOT-VISIBLE L-To-R                                                   */
/* BROWSE-TAB br-nfs 1 D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-tot-qtd-geral IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-qtd-ind IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-qtd-ljs IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-qtd-nor IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-qtd-out IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-vlr-geral IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-vlr-ind IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-vlr-ljs IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-vlr-nor IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-vlr-out IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-nfs
/* Query rebuild information for BROWSE br-nfs
     _START_FREEFORM
RUN pi-total.
OPEN QUERY {&SELF-NAME} FOR EACH tt-fat WHERE
                                 tt-fat.dia = p-dia NO-LOCK
                              BY tt-fat.nr-nota-fis.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-nfs */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Detalha Faturamento - ESSP0174C */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-nfs
&Scoped-define SELF-NAME br-nfs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-nfs D-Dialog
ON ROW-DISPLAY OF br-nfs IN FRAME D-Dialog
DO:
  CASE tt-fat.tipo:
      WHEN "Normal" THEN
         RUN pi-cor (INPUT 1).
      WHEN "Loja" THEN
         RUN pi-cor (INPUT 2).
      WHEN "Indl" THEN
         RUN pi-cor (INPUT 4).
      WHEN "Outros" THEN
         RUN pi-cor (INPUT 13).
  END CASE.
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
  IF AVAIL tt-fat THEN DO:
     FIND nota-fiscal WHERE
          nota-fiscal.cod-estabel = '2' AND
          nota-fiscal.serie       = '1' AND
          nota-fiscal.nr-nota-fis = tt-fat.nr-nota-fis NO-LOCK NO-ERROR.
     ASSIGN gr-nota-fiscal = ROWID(nota-fiscal).
     ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
     RUN ftp/ft0904.
     ASSIGN FRAME {&FRAME-NAME}:VISIBLE = YES.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime D-Dialog
ON CHOOSE OF bt-imprime IN FRAME D-Dialog /* Impress∆o */
DO:
   CLOSE QUERY br-nfs.
   RUN pi-imprime.
   {&OPEN-QUERY-br-nfs}
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
  DISPLAY fi-tot-qtd-nor fi-tot-vlr-nor fi-tot-qtd-ljs fi-tot-vlr-ljs 
          fi-tot-qtd-ind fi-tot-vlr-ind fi-tot-qtd-out fi-tot-vlr-out 
          fi-tot-qtd-geral fi-tot-vlr-geral 
      WITH FRAME D-Dialog.
  ENABLE br-nfs bt-detalhe bt-imprime bt-ok bt-cancela bt-ajuda rt-buttom 
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

  IF p-tp-relat = 1 THEN DO:
     ASSIGN c-data = p-dia + "/" + SUBSTR(p-periodo,1,2) + "/" + SUBSTR(p-periodo,3,4).
     ASSIGN  br-nfs:TITLE IN FRAME {&FRAME-NAME} = "DETALHAMENTO DO FATURAMENTO DO DIA: " + " " + c-data.
  END.
  ELSE
     ASSIGN  br-nfs:TITLE IN FRAME {&FRAME-NAME} = "DETALHAMENTO DO FATURAMENTO DO M“S: " + " " +
             p-dia + "/" + SUBSTR(p-periodo,3,4).

  APPLY 'entry' TO br-nfs IN FRAME {&FRAME-NAME}.



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

 tt-fat.tipo:FGCOLOR IN BROWSE         {&browse-name} = p-cor.
 tt-fat.nr-nota-fis:FGCOLOR IN BROWSE    {&browse-name} = p-cor.
 tt-fat.dt-emis-nota:FGCOLOR IN BROWSE    {&browse-name} = p-cor.
 tt-fat.dt-saida:FGCOLOR IN BROWSE   {&browse-name} = p-cor.
 tt-fat.nr-pedcli:FGCOLOR IN BROWSE {&browse-name} = p-cor.
 tt-fat.nome-ab-cli:FGCOLOR IN BROWSE   {&browse-name} = p-cor.
 tt-fat.no-ab-reppri:FGCOLOR IN BROWSE   {&browse-name} = p-cor.
 tt-fat.qtd:FGCOLOR IN BROWSE          {&browse-name} = p-cor.
 tt-fat.valor:FGCOLOR IN BROWSE        {&browse-name} = p-cor. 

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
        "DATA: "                                  AT  52
        STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  58
        "HORA: "                                  AT  73
        STRING(TIME,"hh:mm:ss")                   AT  79
        "PAG:"                                    AT 105
        i-pag FORMAT ">>>"                        AT 110
        SKIP(1).
    IF p-tp-relat = 1 THEN DO:
       PUT "RELATORIO DE FATURAMENTO DO DIA " AT 36.
       PUT c-data AT 68 SKIP(1).
    END.
    ELSE DO:
        PUT "RELATORIO DE FATURAMENTO DO MES " AT 36.
        PUT SUBSTR(p-periodo,1,2) + "/" + SUBSTR(p-periodo,3,4)  AT 68 SKIP(1).
    END.

    PUT "Tipo    N.Fiscal  Dt.Emissao  Data Saida  Cliente       Pedido  Representante Metros Faturados Valores Faturados" AT 1.   
    PUT "------  --------  ----------  ----------  ------------  ------  ------------- ---------------- -----------------" AT 1.
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

 DEF VAR de-nor-qtd AS DEC.
 DEF VAR de-nor-vlr AS DEC.
 DEF VAR de-ljs-qtd AS DEC.
 DEF VAR de-ljs-vlr AS DEC.
 DEF VAR de-ind-qtd AS DEC.
 DEF VAR de-ind-vlr AS DEC.
 DEF VAR de-out-qtd AS DEC.
 DEF VAR de-out-vlr AS DEC.


 RUN utp/ut-utils.p PERSISTENT SET h-prog.


 RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

 CASE i-saida:
     WHEN 1 THEN DO.
         OUTPUT TO PRINTER CONVERT TARGET "ibm850" PAGED PAGE-SIZE 62.
         PUT CONTROL "~033E~033(s16H".    
     END.
     WHEN 2 THEN
         OUTPUT TO VALUE(c-saida).
     WHEN 3 THEN DO.
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0174c.tmp".
         OUTPUT TO VALUE(c-saida).
     END.
 END CASE.

 DO i-ct = 1 TO i-num-copias.
    ASSIGN i-pag       =  1
           i-lin       = 99
           de-nor-qtd  =  0
           de-nor-vlr  =  0
           de-ljs-qtd  =  0
           de-ljs-vlr  =  0
           de-ind-qtd  =  0
           de-ind-vlr  =  0
           de-out-qtd  =  0
           de-out-vlr  =  0.
    FOR EACH tt-fat WHERE
             tt-fat.dia = p-dia NO-LOCK
          BY tt-fat.nr-nota-fis.
        
        IF i-lin > 62 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.
        PUT tt-fat.tipo FORMAT "x(6)"             AT  1
            tt-fat.nr-nota-fis  FORMAT "X(7)"     AT  9
            tt-fat.dt-emis-nota                   AT 19
            tt-fat.dt-saida                       AT 31
            tt-fat.nome-ab-cli                    AT 43
            tt-fat.nr-pedcli FORMAT "x(6)"        AT 57
            tt-fat.no-ab-reppri                   AT 65
            tt-fat.qtde  FORMAT ">>>,>>>,>>9.99"  AT 81
            tt-fat.valor FORMAT ">>>,>>>,>>9.99"  AT 99.
        ASSIGN i-lin = i-lin + 1.
        ACCUMULATE tt-fat.qtde (TOTAL).
        ACCUMULATE tt-fat.valor (TOTAL).
        CASE tt-fat.tipo:
            WHEN "Normal" THEN
                ASSIGN de-nor-qtd = de-nor-qtd +  tt-fat.qtde
                       de-nor-vlr = de-nor-vlr +  tt-fat.valor.
            WHEN "Loja" THEN
                ASSIGN de-ljs-qtd = de-ljs-qtd + tt-fat.qtde
                       de-ljs-vlr = de-ljs-vlr + tt-fat.valor.
            WHEN "Indl" THEN
                ASSIGN de-ind-qtd = de-ind-qtd + tt-fat.qtde
                       de-ind-vlr = de-ind-vlr + tt-fat.valor.
            WHEN "Outros" THEN
                ASSIGN de-out-qtd = de-out-qtd + tt-fat.qtde
                       de-out-vlr = de-out-vlr + tt-fat.valor.
        END CASE.
    END.
    IF (ACCUM TOTAL tt-fat.qtde)  <> 0 OR
       (ACCUM TOTAL tt-fat.valor) <> 0 THEN DO:
       IF i-lin > 62 THEN DO:
          RUN pi-imp-cabec.
          ASSIGN i-lin = 7.
       END.
       PUT "---------------- -----------------" AT 79 SKIP.
       PUT "Normal..............:"              AT 57.
       PUT de-nor-qtd   FORMAT ">>>,>>>,>>9.99" AT 81.
       PUT de-nor-vlr   FORMAT ">>>,>>>,>>9.99" AT 99.
       PUT "Loja................:"              AT 57.
       PUT de-ljs-qtd   FORMAT ">>>,>>>,>>9.99" AT 81.
       PUT de-ljs-vlr   FORMAT ">>>,>>>,>>9.99" AT 99.
       PUT "Industrializaá∆o....:"              AT 57.
       PUT de-ind-qtd   FORMAT ">>>,>>>,>>9.99" AT 81.
       PUT de-ind-vlr   FORMAT ">>>,>>>,>>9.99" AT 99.
       PUT "Outros..............:"              AT 57.
       PUT de-out-qtd   FORMAT ">>>,>>>,>>9.99" AT 81.
       PUT de-out-vlr   FORMAT ">>>,>>>,>>9.99" AT 99.
       PUT "Total Geral.........:" AT 57.
       PUT ACCUM TOTAL tt-fat.qtde  FORMAT ">>>,>>>,>>9.99" AT 81.
       PUT ACCUM TOTAL tt-fat.valor FORMAT ">>>,>>>,>>9.99" AT 99.
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
    ASSIGN fi-tot-qtd-nor = 0
           fi-tot-vlr-nor = 0
           fi-tot-qtd-ljs = 0 
           fi-tot-vlr-ljs = 0
           fi-tot-qtd-out = 0 
           fi-tot-vlr-out = 0
           fi-tot-qtd-ind = 0 
           fi-tot-vlr-ind = 0.
    FOR EACH tt-fat WHERE
             tt-fat.dia = p-dia NO-LOCK.
        CASE tt-fat.tipo:
            WHEN "Normal" THEN
                ASSIGN fi-tot-qtd-nor = fi-tot-qtd-nor + tt-fat.qtde
                       fi-tot-vlr-nor = fi-tot-vlr-nor + tt-fat.valor.
            WHEN "Loja" THEN
                ASSIGN fi-tot-qtd-ljs = fi-tot-qtd-ljs + tt-fat.qtde  
                       fi-tot-vlr-ljs = fi-tot-vlr-ljs + tt-fat.valor.
            WHEN "Indl" THEN
                ASSIGN fi-tot-qtd-ind = fi-tot-qtd-ind + tt-fat.qtde  
                       fi-tot-vlr-ind = fi-tot-vlr-ind + tt-fat.valor.
            WHEN "Outros" THEN
                ASSIGN fi-tot-qtd-out = fi-tot-qtd-out + tt-fat.qtde  
                       fi-tot-vlr-out = fi-tot-vlr-out + tt-fat.valor.
        END CASE.
    END.
    ASSIGN fi-tot-qtd-geral = fi-tot-qtd-nor + fi-tot-qtd-ljs + fi-tot-qtd-ind + fi-tot-qtd-out
           fi-tot-vlr-geral = fi-tot-vlr-nor + fi-tot-vlr-ljs + fi-tot-vlr-ind + fi-tot-vlr-out.


    DISP fi-tot-qtd-nor 
         fi-tot-vlr-nor 
         fi-tot-qtd-ljs 
         fi-tot-vlr-ljs 
         fi-tot-qtd-out 
         fi-tot-vlr-out 
         fi-tot-qtd-ind 
         fi-tot-vlr-ind 
         fi-tot-qtd-geral
         fi-tot-vlr-geral
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
  {src/adm/template/snd-list.i "tt-fat"}

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

