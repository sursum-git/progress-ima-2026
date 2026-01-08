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
{include/i-prgvrs.i ESSP0174D 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE TEMP-TABLE tt-vda  NO-UNDO 
       FIELD tipo         AS CHAR
       FIELD dia          AS CHAR
       FIELD nr-pedcli    LIKE ped-venda.nr-pedcli
       FIELD nr-pedrep    LIKE ped-venda.nr-pedrep
       FIELD nome-abrev   LIKE ped-venda.nome-abrev
       FIELD no-ab-reppri LIKE ped-venda.no-ab-reppri
       FIELD dt-implant   LIKE ped-venda.dt-implant
       FIELD dt-entrega   LIKE ped-venda.dt-entrega
       FIELD Sit          AS CHAR
       FIELD qtde         AS DEC
       FIELD valor        AS DEC
       INDEX indice1 tipo dia nr-pedcli.

DEFINE INPUT PARAMETER TABLE FOR tt-vda.  
DEFINE INPUT PARAMETER p-periodo  AS CHAR.
DEFINE INPUT PARAMETER p-tp-relat AS INT.


/* Local Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR gr-ped-venda AS ROWID NO-UNDO.

DEF VAR i-lin AS INT.
DEF VAR i-pag AS INT.
DEF VAR c-data AS CHAR FORMAT "x(10)".
DEF VAR c-nr-pedcli LIKE ped-venda.nr-pedcli.
DEF VAR c-empresa AS CHAR.
DEF VAR h-query           AS HANDLE.


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
&Scoped-define BROWSE-NAME br-pedidos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-vda

/* Definitions for BROWSE br-pedidos                                    */
&Scoped-define FIELDS-IN-QUERY-br-pedidos tt-vda.tipo tt-vda.nr-pedcli tt-vda.nr-pedrep tt-vda.nome-abrev tt-vda.no-ab-reppri tt-vda.dt-implant tt-vda.dt-entrega tt-vda.sit tt-vda.qtde tt-vda.valor   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-pedidos   
&Scoped-define SELF-NAME br-pedidos
&Scoped-define OPEN-QUERY-br-pedidos RUN pi-total. OPEN QUERY {&SELF-NAME} FOR EACH tt-vda  NO-LOCK                               BY tt-vda.dt-implant                               BY tt-vda.nr-pedcli.
&Scoped-define TABLES-IN-QUERY-br-pedidos tt-vda
&Scoped-define FIRST-TABLE-IN-QUERY-br-pedidos tt-vda


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-pedidos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-pedidos bt-detalhe bt-imprime bt-vapara ~
bt-ok bt-cancela bt-ajuda rt-buttom 
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
     SIZE 13 BY 1.25 TOOLTIP "Detalhe do Pedido".

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/img-pri.bmp":U
     LABEL "Impress∆o" 
     SIZE 13 BY 1.25 TOOLTIP "Imprime os Pedidos Faturados".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-vapara 
     IMAGE-UP FILE "image/img-vapra.bmp":U
     LABEL "Impress∆o" 
     SIZE 13 BY 1.25 TOOLTIP "Va Para o Pedido".

DEFINE VARIABLE fi-tot-qtd-geral AS DECIMAL FORMAT "   ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-qtd-ind AS DECIMAL FORMAT "  ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-tot-qtd-ljs AS DECIMAL FORMAT "  ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE fi-tot-qtd-nor AS DECIMAL FORMAT "  ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tot-qtd-out AS DECIMAL FORMAT "  ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FGCOLOR 13  NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-geral AS DECIMAL FORMAT "ZZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-ind AS DECIMAL FORMAT " ZZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-ljs AS DECIMAL FORMAT " ZZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-nor AS DECIMAL FORMAT " ZZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tot-vlr-out AS DECIMAL FORMAT " ZZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88
     FGCOLOR 13  NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 93 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-pedidos FOR 
      tt-vda SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-pedidos D-Dialog _FREEFORM
  QUERY br-pedidos NO-LOCK DISPLAY
      tt-vda.tipo         COLUMN-LABEL "Tipo"      
      tt-vda.nr-pedcli    COLUMN-LABEL "Ped Cli"
tt-vda.nr-pedrep    COLUMN-LABEL "Ped Rep"
tt-vda.nome-abrev   COLUMN-LABEL "Cliente"
tt-vda.no-ab-reppri COLUMN-LABEL "Representante"
tt-vda.dt-implant   COLUMN-LABEL "Dt.Implant"   COLUMN-BGCOLOR 8
tt-vda.dt-entrega   COLUMN-LABEL "Dt.Entrega"
tt-vda.sit          COLUMN-LABEL "Sit"
tt-vda.qtde         COLUMN-LABEL "Metros Vendidos"
tt-vda.valor        COLUMN-LABEL "Valor Vendido"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 93.29 BY 12
         FONT 1
         TITLE "" ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     br-pedidos AT ROW 1 COL 1.72
     fi-tot-qtd-nor AT ROW 13.08 COL 64.14 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fi-tot-vlr-nor AT ROW 13.13 COL 76.86 COLON-ALIGNED NO-LABEL
     bt-detalhe AT ROW 13.21 COL 2.57
     bt-imprime AT ROW 13.21 COL 16.14
     bt-vapara AT ROW 13.21 COL 29.57
     fi-tot-qtd-ljs AT ROW 14.08 COL 64.14 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fi-tot-vlr-ljs AT ROW 14.08 COL 76.86 COLON-ALIGNED NO-LABEL
     fi-tot-qtd-ind AT ROW 15.08 COL 64.14 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fi-tot-vlr-ind AT ROW 15.08 COL 76.86 COLON-ALIGNED NO-LABEL
     fi-tot-qtd-out AT ROW 16.08 COL 64.14 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fi-tot-vlr-out AT ROW 16.08 COL 76.86 COLON-ALIGNED NO-LABEL
     fi-tot-qtd-geral AT ROW 17.08 COL 64.14 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fi-tot-vlr-geral AT ROW 17.08 COL 76.86 COLON-ALIGNED NO-LABEL
     bt-ok AT ROW 18.33 COL 2.57
     bt-cancela AT ROW 18.33 COL 13.57
     bt-ajuda AT ROW 18.33 COL 82.14
     rt-buttom AT ROW 18.08 COL 1.57
     "Normal:" VIEW-AS TEXT
          SIZE 6 BY .88 AT ROW 13 COL 60
     "Lojas:" VIEW-AS TEXT
          SIZE 4.57 BY .88 AT ROW 14 COL 61.14
     "Industrializaá∆o:" VIEW-AS TEXT
          SIZE 11 BY .88 AT ROW 15 COL 54.43
     "Total Geral:" VIEW-AS TEXT
          SIZE 8 BY .88 AT ROW 17 COL 57.43
     "Outros:" VIEW-AS TEXT
          SIZE 5.57 BY .88 AT ROW 16 COL 60.72
     SPACE(28.99) SKIP(2.74)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Detalha Pedidos do Periodo - ESSP0174D"
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
/* BROWSE-TAB br-pedidos 1 D-Dialog */
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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-pedidos
/* Query rebuild information for BROWSE br-pedidos
     _START_FREEFORM
RUN pi-total.
OPEN QUERY {&SELF-NAME} FOR EACH tt-vda  NO-LOCK
                              BY tt-vda.dt-implant
                              BY tt-vda.nr-pedcli.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-pedidos */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Detalha Pedidos do Periodo - ESSP0174D */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-pedidos
&Scoped-define SELF-NAME br-pedidos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-pedidos D-Dialog
ON ROW-DISPLAY OF br-pedidos IN FRAME D-Dialog
DO:
  CASE tt-vda.tipo:
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
  IF AVAIL tt-vda THEN DO:
     FIND ped-venda WHERE
          ped-venda.nr-pedcli  = tt-vda.nr-pedcli AND
          ped-venda.nome-abrev = tt-vda.nome-abrev NO-LOCK NO-ERROR.
     ASSIGN gr-ped-venda = ROWID(ped-venda).
     ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
     RUN esp\espd4000.w (INPUT "Consultar").
     ASSIGN FRAME {&FRAME-NAME}:VISIBLE = YES.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime D-Dialog
ON CHOOSE OF bt-imprime IN FRAME D-Dialog /* Impress∆o */
DO:
    CLOSE QUERY br-pedidos.
    RUN pi-imprime.
    {&OPEN-QUERY-br-pedidos}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-vapara
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-vapara D-Dialog
ON CHOOSE OF bt-vapara IN FRAME D-Dialog /* Impress∆o */
DO:
    RUN esdlg/d03essp0174.w (OUTPUT c-nr-pedcli).

    IF c-nr-pedcli <> ? THEN DO:
       FIND tt-vda WHERE
            tt-vda.nr-pedcli = c-nr-pedcli NO-LOCK NO-ERROR. 

       IF NOT AVAIL tt-vda THEN DO.
          MESSAGE "Pedido n∆o est† contido na seleá∆o!"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
       END.

       h-query:REPOSITION-TO-ROWID(ROWID(tt-vda)) NO-ERROR.
       APPLY 'value-changed' TO br-pedidos IN FRAME {&FRAME-NAME}.
       APPLY 'entry' TO br-pedidos IN FRAME {&FRAME-NAME}.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */
ASSIGN h-query = br-pedidos:QUERY.

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
  ENABLE br-pedidos bt-detalhe bt-imprime bt-vapara bt-ok bt-cancela bt-ajuda 
         rt-buttom 
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

  /* {utp/ut9000.i "ESSP0174D" "2.04.00.000"} */ 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa WHERE
             empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

  IF p-tp-relat = 1 THEN
     ASSIGN  br-pedidos:TITLE IN FRAME {&FRAME-NAME} = "DETALHAMENTO DE PEDIDOS DE VENDAS DO PERIODO: " + 
                                                       SUBSTR(p-periodo,1,2) + "/" + SUBSTR(p-periodo,3,4).
  ELSE
      ASSIGN  br-pedidos:TITLE IN FRAME {&FRAME-NAME} = "DETALHAMENTO DE PEDIDOS DE VENDAS DO EXERCICIO: " + 
                                                         SUBSTR(p-periodo,3,4).

  APPLY 'entry' TO br-pedidos IN FRAME {&FRAME-NAME}.

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

 tt-vda.tipo:FGCOLOR IN BROWSE         {&browse-name} = p-cor.
 tt-vda.nr-pedcli:FGCOLOR IN BROWSE    {&browse-name} = p-cor.
 tt-vda.nr-pedrep:FGCOLOR IN BROWSE    {&browse-name} = p-cor.
 tt-vda.nome-abrev:FGCOLOR IN BROWSE   {&browse-name} = p-cor.
 tt-vda.no-ab-reppri:FGCOLOR IN BROWSE {&browse-name} = p-cor.
 tt-vda.dt-implant:FGCOLOR IN BROWSE   {&browse-name} = p-cor.
 tt-vda.dt-entrega:FGCOLOR IN BROWSE   {&browse-name} = p-cor.
 tt-vda.sit:FGCOLOR IN BROWSE          {&browse-name} = p-cor.
 tt-vda.qtd:FGCOLOR IN BROWSE          {&browse-name} = p-cor.
 tt-vda.valor:FGCOLOR IN BROWSE        {&browse-name} = p-cor. 

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
    IF p-tp-relat = 1 THEN DO:
       PUT "RELATORIO DE DETALHAMENTO DE PEDIDOS DE VENDA DO PERIODO " AT 31.
       PUT SUBSTR(p-periodo,1,2) + "/" + substr(p-periodo,3,4) AT 88 SKIP(1).
    END.
    ELSE DO:
       PUT "RELATORIO DE DETALHAMENTO DE PEDIDOS DE VENDA DO EXERCICIO " AT 31.
       PUT SUBSTR(p-periodo,3,4) AT 90 SKIP(1).
    END.
 
    PUT "Tipo   Ped Cli Ped. Repres. Cliente      Representante Sit Dt.Implant Dt.Entrega   Qtde Vendida  Valor Vendido" AT 1.
    PUT "------ ------- ------------ ------------ ------------- --- ---------- ---------- -------------- --------------" AT 1.

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
 DEF VAR h-prog     AS HANDLE NO-UNDO.
 DEF VAR i-ct       AS INT.

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
         PUT CONTROL "~033E~033(s18H".    
     END.
     WHEN 2 THEN
         OUTPUT TO VALUE(c-saida).
     WHEN 3 THEN DO.
         ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0174d.tmp".
         OUTPUT TO VALUE(c-saida).
     END.
 END CASE.

 DO i-ct = 1 TO i-num-copias.
    ASSIGN i-pag      =  1
           i-lin      = 99
           de-nor-qtd =  0
           de-nor-vlr =  0
           de-ljs-qtd =  0
           de-ljs-vlr =  0
           de-ind-qtd =  0
           de-ind-vlr =  0
           de-out-qtd =  0
           de-out-vlr =  0.
    FOR EACH tt-vda WHERE NO-LOCK
          BY tt-vda.dt-implant
          BY tt-vda.nr-pedcli. 
        
        IF i-lin > 62 THEN DO:
           RUN pi-imp-cabec.
           ASSIGN i-lin = 7.
        END.
        PUT tt-vda.tipo       FORMAT "x(6)"      AT  1
            tt-vda.nr-pedcli  FORMAT "x(6)"      AT  8
            tt-vda.nr-pedrep                     AT 16
            tt-vda.nome-abrev                    AT 29
            tt-vda.no-ab-reppri                  AT 42
            tt-vda.sit  FORMAT "x(3)"            AT 56
            tt-vda.dt-implant                    AT 60
            tt-vda.dt-entrega                    AT 71
            tt-vda.qtde  FORMAT ">>>,>>>,>>9.99" AT 82
            tt-vda.valor FORMAT ">>>,>>>,>>9.99" AT 97.
        ASSIGN i-lin = i-lin + 1.
        ACCUMULATE tt-vda.qtde (TOTAL).
        ACCUMULATE tt-vda.valor (TOTAL).
        CASE tt-vda.tipo:
            WHEN "Normal" THEN
                ASSIGN de-nor-qtd = de-nor-qtd +  tt-vda.qtde
                       de-nor-vlr = de-nor-vlr +  tt-vda.valor.
            WHEN "Loja" THEN
                ASSIGN de-ljs-qtd = de-ljs-qtd + tt-vda.qtde
                       de-ljs-vlr = de-ljs-vlr + tt-vda.valor.
            WHEN "Indl" THEN
                ASSIGN de-ind-qtd = de-ind-qtd + tt-vda.qtde
                       de-ind-vlr = de-ind-vlr + tt-vda.valor.
            WHEN "Outros" THEN
                ASSIGN de-out-qtd = de-out-qtd + tt-vda.qtde
                       de-out-vlr = de-out-vlr + tt-vda.valor.
        END CASE.
   END.
   IF (ACCUM TOTAL tt-vda.qtde)  <> 0 OR
      (ACCUM TOTAL tt-vda.valor) <> 0 THEN DO:
      IF i-lin > 60 THEN DO:
         RUN pi-imp-cabec.
         ASSIGN i-lin = 7.
      END.
      PUT "-------------- --------------" AT 82 SKIP.
      PUT "Normal..............:"              AT 60.
      PUT de-nor-qtd   FORMAT ">>>,>>>,>>9.99" AT 82.
      PUT de-nor-vlr   FORMAT ">>>,>>>,>>9.99" AT 97.
      PUT "Loja................:"              AT 60.
      PUT de-ljs-qtd   FORMAT ">>>,>>>,>>9.99" AT 82.
      PUT de-ljs-vlr   FORMAT ">>>,>>>,>>9.99" AT 97.
      PUT "Industrializaá∆o....:"              AT 60.
      PUT de-ind-qtd   FORMAT ">>>,>>>,>>9.99" AT 82.
      PUT de-ind-vlr   FORMAT ">>>,>>>,>>9.99" AT 97.
      PUT "Outros..............:"              AT 60.
      PUT de-out-qtd   FORMAT ">>>,>>>,>>9.99" AT 82.
      PUT de-out-vlr   FORMAT ">>>,>>>,>>9.99" AT 97.
      PUT "Total Geral.........:"              AT 60.
      PUT ACCUM TOTAL tt-vda.qtde  FORMAT ">>>,>>>,>>9.99" AT 82.
      PUT ACCUM TOTAL tt-vda.valor FORMAT ">>>,>>>,>>9.99" AT 97.
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
    FOR EACH tt-vda NO-LOCK.
        CASE tt-vda.tipo:
            WHEN "Normal" THEN
                ASSIGN fi-tot-qtd-nor = fi-tot-qtd-nor + tt-vda.qtde
                       fi-tot-vlr-nor = fi-tot-vlr-nor + tt-vda.valor.
            WHEN "Loja" THEN
                ASSIGN fi-tot-qtd-ljs = fi-tot-qtd-ljs + tt-vda.qtde  
                       fi-tot-vlr-ljs = fi-tot-vlr-ljs + tt-vda.valor.
            WHEN "Indl" THEN
                ASSIGN fi-tot-qtd-ind = fi-tot-qtd-ind + tt-vda.qtde  
                       fi-tot-vlr-ind = fi-tot-vlr-ind + tt-vda.valor.
            WHEN "Outros" THEN
                ASSIGN fi-tot-qtd-out = fi-tot-qtd-out + tt-vda.qtde  
                       fi-tot-vlr-out = fi-tot-vlr-out + tt-vda.valor.
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
  {src/adm/template/snd-list.i "tt-vda"}

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

