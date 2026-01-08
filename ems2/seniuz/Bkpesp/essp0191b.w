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
{include/i-prgvrs.i ESSP0191B 2.04.00.000}

{utp/utapi011.i} /* Geraá∆o de Graficos */

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */


&GLOBAL-DEFINE xx FOR EACH tt-nfiscal WHERE tt-nfiscal.it-codigo = p-it-codigo. 
&GLOBAL-DEFINE yy FOR EACH tt-nfiscal. 

DEF TEMP-TABLE tt-nfiscal 
    FIELD cod-estabel  LIKE movto-estoq.cod-estabel
    FIELD serie        LIKE movto-estoq.serie-docto
    FIELD nr-nota-fis  LIKE movto-estoq.nro-docto
    FIELD it-codigo    LIKE movto-estoq.it-codigo
    FIELD cod-rep      LIKE nota-fiscal.cod-rep.

DEFINE TEMP-TABLE tt-work NO-UNDO 
       FIELD nr-nota-fis  LIKE nota-fiscal.nr-nota-fis
       FIELD cod-estabel  LIKE nota-fiscal.cod-estabel
       FIELD serie        LIKE nota-fiscal.serie
       FIELD dt-emis-nota LIKE nota-fiscal.dt-emis-nota
       FIELD dt-saida     LIKE nota-fiscal.dt-saida
       FIELD nome-abrev   LIKE ped-venda.nome-abrev
       FIELD qtde         AS DEC
       FIELD valor        AS DEC.

DEFINE BUFFER b-tt-work FOR tt-work.

DEFINE INPUT PARAMETER TABLE FOR tt-nfiscal.  
DEFINE INPUT PARAMETER p-it-codigo LIKE movto-estoq.it-codigo.
DEFINE INPUT PARAMETER p-dt-ini AS DATE.
DEFINE INPUT PARAMETER p-dt-fin AS DATE.


/* Local Variable Definitions ---                                       */
DEF NEW GLOBAL SHARED VAR gr-ped-venda   AS ROWID NO-UNDO.
DEF NEW GLOBAL SHARED VAR gr-nota-fiscal AS ROWID NO-UNDO.

DEF VAR h-acomp      AS HANDLE NO-UNDO.
DEF VAR opt-sort     AS INT.
DEF VAR c-data       AS CHAR FORMAT "x(10)".
DEF VAR de-tot-qtd   AS DEC.
DEF VAR de-tot-vlr   AS DEC.
DEF VAR de-ger-res   AS DEC.
DEF VAR c-sit-ped    AS CHAR.
DEF VAR c-regiao     AS CHAR.
DEF VAR c-selecao    AS CHAR.
DEF VAR i-prz        AS INT.
DEF VAR c-cond-pagto AS CHAR.
DEF VAR c-empresa    AS CHAR.

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
&Scoped-define BROWSE-NAME br-work

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-work

/* Definitions for BROWSE br-work                                       */
&Scoped-define FIELDS-IN-QUERY-br-work tt-work.nr-nota-fis tt-work.serie tt-work.nome-abrev tt-work.dt-emis-nota tt-work.dt-saida tt-work.qtde tt-work.valor (tt-work.valor / tt-work.qtde)   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-work   
&Scoped-define SELF-NAME br-work
&Scoped-define OPEN-QUERY-br-work RUN pi-total. OPEN QUERY {&SELF-NAME} FOR EACH tt-work NO-LOCK                               BY tt-work.dt-emis-nota                               BY tt-work.nr-nota-fis.
&Scoped-define TABLES-IN-QUERY-br-work tt-work
&Scoped-define FIRST-TABLE-IN-QUERY-br-work tt-work


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-work}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom br-work bt-detalhe-nf bt-imprime ~
bt-Grafico-vlr bt-ok bt-cancela bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-tot-qtd fi-tot-vlr fi-pco-medio 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 bt-Grafico-vlr 

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

DEFINE BUTTON bt-detalhe-nf 
     IMAGE-UP FILE "image/img-detnf.bmp":U
     LABEL "Detalhe" 
     SIZE 18 BY 1.25 TOOLTIP "Detalhe da Nota Fiscal".

DEFINE BUTTON bt-Grafico-vlr 
     IMAGE-UP FILE "image/im-grfcp.bmp":U
     LABEL "Grafico" 
     SIZE 5.72 BY 1.25 TOOLTIP "Visualizar o Grafico de Valores".

DEFINE BUTTON bt-imprime 
     IMAGE-UP FILE "image/img-pri.bmp":U
     LABEL "Impress∆o" 
     SIZE 13 BY 1.25 TOOLTIP "Imprime o Relat¢rio".

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-pco-medio AS DECIMAL FORMAT "   ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.72 BY .88
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tot-qtd AS DECIMAL FORMAT "ZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi-tot-vlr AS DECIMAL FORMAT "ZZZ,ZZZ,ZZ9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .88
     FGCOLOR 1  NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80.43 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-work FOR 
      tt-work SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-work
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-work D-Dialog _FREEFORM
  QUERY br-work NO-LOCK DISPLAY
      tt-work.nr-nota-fis  COLUMN-LABEL "N.Fiscal"      WIDTH  6
      tt-work.serie        COLUMN-LABEL "Serie"         WIDTH  4
      tt-work.nome-abrev   COLUMN-LABEL "Cliente"       WIDTH 12
      tt-work.dt-emis-nota COLUMN-LABEL "Dt.Emiss∆o"    WIDTH 8.5
      tt-work.dt-saida     COLUMN-LABEL "Data Saida"    WIDTH 8.5
      tt-work.qtde         COLUMN-LABEL "Quantidade" FORMAT "->>,>>>,>>9.99"    WIDTH 10
      tt-work.valor        COLUMN-LABEL "Valor"      FORMAT "->>,>>>,>>9.99"    WIDTH 10
      (tt-work.valor /
       tt-work.qtde)       COLUMN-LABEL "Preáo MÇdio"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 80.14 BY 11.92
         FONT 1
         TITLE "" ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     br-work AT ROW 1.13 COL 1.86
     fi-tot-qtd AT ROW 13.08 COL 49.57 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fi-tot-vlr AT ROW 13.13 COL 60.14 COLON-ALIGNED NO-LABEL
     fi-pco-medio AT ROW 13.13 COL 71 COLON-ALIGNED NO-LABEL
     bt-detalhe-nf AT ROW 13.17 COL 2
     bt-imprime AT ROW 13.17 COL 22.29
     bt-Grafico-vlr AT ROW 13.17 COL 37 WIDGET-ID 2
     bt-ok AT ROW 14.79 COL 2.57
     bt-cancela AT ROW 14.79 COL 13.57
     bt-ajuda AT ROW 14.79 COL 70.72
     "Totais:" VIEW-AS TEXT
          SIZE 5 BY .88 AT ROW 13.04 COL 46
     rt-buttom AT ROW 14.54 COL 1.57
     SPACE(0.13) SKIP(0.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Detalha Nota Fiscal - ESSP0191B"
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
/* BROWSE-TAB br-work rt-buttom D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-Grafico-vlr IN FRAME D-Dialog
   6                                                                    */
/* SETTINGS FOR FILL-IN fi-pco-medio IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-qtd IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-vlr IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-work
/* Query rebuild information for BROWSE br-work
     _START_FREEFORM
RUN pi-total.
OPEN QUERY {&SELF-NAME} FOR EACH tt-work NO-LOCK
                              BY tt-work.dt-emis-nota
                              BY tt-work.nr-nota-fis.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-work */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Detalha Nota Fiscal - ESSP0191B */
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


&Scoped-define SELF-NAME bt-detalhe-nf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-detalhe-nf D-Dialog
ON CHOOSE OF bt-detalhe-nf IN FRAME D-Dialog /* Detalhe */
DO:
  IF AVAIL tt-work THEN DO:
      FIND nota-fiscal WHERE
           nota-fiscal.cod-estabel = tt-work.cod-estabel AND
           nota-fiscal.serie       = tt-work.serie       AND
           nota-fiscal.nr-nota-fis = tt-work.nr-nota-fis NO-LOCK NO-ERROR.
     IF AVAIL nota-fiscal THEN DO:
        ASSIGN gr-nota-fiscal = ROWID(nota-fiscal).
        ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
        RUN ftp/ft0904.
        ASSIGN FRAME {&FRAME-NAME}:VISIBLE = YES.
     END.  
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-Grafico-vlr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-Grafico-vlr D-Dialog
ON CHOOSE OF bt-Grafico-vlr IN FRAME D-Dialog /* Grafico */
DO:
    
   RUN pi-grafico-vlr.
 
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


&Scoped-define BROWSE-NAME br-work
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
  DISPLAY fi-tot-qtd fi-tot-vlr fi-pco-medio 
      WITH FRAME D-Dialog.
  ENABLE rt-buttom br-work bt-detalhe-nf bt-imprime bt-Grafico-vlr bt-ok 
         bt-cancela bt-ajuda 
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

  /* {utp/ut9000.i "ESSP0191B" "2.04.00.000"} */ 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa WHERE
             empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

  RUN pi-processa.

  {&OPEN-QUERY-br-work}
  APPLY 'entry' TO br-work IN FRAME {&FRAME-NAME}.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grafico-vlr D-Dialog 
PROCEDURE pi-grafico-vlr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR i-point AS INT INITIAL 1.

 DEF VAR i-numsets AS INT INITIAL 0.
 DEF VAR c-titulo-grafico AS CHAR.


       ASSIGN c-titulo-grafico = "Valores Faturadosn NF".


 ASSIGN c-titulo-grafico = c-titulo-grafico + "Em ".
 EMPTY TEMP-TABLE tt-atributos.
 EMPTY TEMP-TABLE tt-sets.
 EMPTY TEMP-TABLE tt-points-2.
 EMPTY TEMP-TABLE tt-dados.
 EMPTY TEMP-TABLE tt-erros.

 /* Configuraá∆o Geral do Grafico */
 /*                               */
 CREATE tt-atributos.

    ASSIGN tt-atributos.cod-versao-integracao = 3
           tt-atributos.graphtype             = 4
           tt-atributos.graphtitle            = c-titulo-grafico + " " +
                                                string(p-dt-ini) + "/" + string(p-dt-fin) + '.'
           tt-atributos.lefttitle             = 'Valores em REAL.'
           tt-atributos.lefttitlestyle        = 2
           tt-atributos.bottomtitle           = 'D I A S'
           tt-atributos.numgraph              = 1.



 /* Configuraá∆o das Variantes do Grafico (Linhas ou  Barras */
 /*                                                          */
 ASSIGN i-numsets = 1.


    CREATE tt-sets.
    ASSIGN tt-sets.NumSet   = i-numsets 
           tt-sets.NumGraph = 1
           tt-sets.ColorSet = 2
           tt-sets.legendText = "Valores Faturadosv NF".


 FOR EACH tt-work WHERE NO-LOCK.

    /* Valores do EIXO X (DIAS) */
    CREATE tt-points-2.

       ASSIGN tt-points-2.NumPoint  = i-point
              tt-points-2.NumGraph  = 1
              tt-points-2.labeltext = tt-work.nr-nota-fis.

    ASSIGN i-numsets = 1.

       /* Valores do EIXI Y (Metros Faturados) */
       CREATE tt-dados.
       ASSIGN tt-dados.NumPoint   = i-point
              tt-dados.NumSet     = i-numsets
              tt-dados.NumGraph   = 1
              tt-dados.graphdata  = tt-work.valor.

    ASSIGN i-point = i-point + 1.

 END.

 DEF VAR h-utapi011 AS HANDLE NO-UNDO.

 RUN utp/utapi011.p PERSISTENT SET h-utapi011.

 RUN pi-execute IN h-utapi011 ( INPUT  TABLE tt-atributos,
                                INPUT  TABLE tt-points-2,
                                INPUT  TABLE tt-sets,
                                INPUT  TABLE tt-dados,
                                INPUT  TABLE tt-ylabels,
                                OUTPUT TABLE tt-erros).

 IF RETURN-VALUE = "NOK" THEN DO: 
    FOR EACH tt-erros: 
        DISP cod-erro desc-erro FORMAT "x(100)" WITH 1 COL WIDTH 500. 
    END.
 END.                  

 DELETE PROCEDURE h-utapi011.



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
  PUT c-empresa FORMAT "x(40)"                  AT   1
      "DATA: "                                  AT  50
      STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  56
      "PAG:"                                    AT  86
      i-pag FORMAT ">>>"                        AT  91
      SKIP(1).

  IF p-it-codigo <> "" THEN
     PUT "NOTAS FISCAIS DO ITEM " + p-it-codigo + " NO PERIODO " + 
         STRING(p-dt-ini, "99/99/9999") + " A " +  STRING(p-dt-fin, "99/99/9999")
         FORMAT "X(100)" AT 14 SKIP(1).
  ELSE
      PUT "NOTAS FISCAIS DO PERIODO " + STRING(p-dt-ini, "99/99/9999") + " A " + 
          STRING(p-dt-fin, "99/99/9999") FORMAT "X(100)" AT 23 SKIP(1).

  PUT "NOTAS FISCAIS SERIE CLIENTE      DT.EMISSAO   DT-SAIDA    QUANTIDADE           VALOR  P.MEDIO" AT 1.
  PUT "------------- ----- ------------ ---------- ---------- -------------  -------------- --------" AT 1.
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

  RUN utp/ut-utils.p PERSISTENT SET h-prog.


  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).

  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO VALUE(c-saida) CONVERT TARGET "ibm850" PAGED PAGE-SIZE 62.
           PUT CONTROL "~033E~033(s15H". /* ORIENTAÄ«O RETRATO & COMPACTA */
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0191b.tmp".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      END.
  END CASE.

  DO i-ct = 1 TO i-num-copias.
     ASSIGN i-pag      =  1
            i-lin      = 99.

     FOR EACH b-tt-work NO-LOCK        
           BY b-tt-work.dt-emis-nota      
           BY b-tt-work.nr-nota-fis.  

         IF i-lin > 62 THEN DO:
            RUN pi-imp-cabec.
            ASSIGN i-lin = 7.
         END.
    
         PUT b-tt-work.nr-nota-fis  FORMAT "x(10)"          AT  1     
             b-tt-work.serie        FORMAT "x(5)"           AT 15
             b-tt-work.nome-abrev                           AT 21
             b-tt-work.dt-emis-nota                         AT 34
             b-tt-work.dt-saida                             AT 45
             b-tt-work.qtd          FORMAT ">>,>>>,>>9.99"  AT 56 
             b-tt-work.valor        FORMAT ">>>,>>>,>>9.99" AT 71 
             b-tt-work.valor /
             b-tt-work.qtd          FORMAT ">,>>9.99"       AT 86.
          
         ASSIGN i-lin = i-lin + 1.

         ACCUMULATE b-tt-work.qtd   (TOTAL).
         ACCUMULATE b-tt-work.valor (TOTAL).

     END.

     IF i-lin > 62 THEN DO:
        RUN pi-imp-cabec.
        ASSIGN i-lin = 7.
     END.
     PUT "-------------  -------------- --------" AT 56.
     PUT "TOTAL GERAL.......:" AT 10.

     PUT ACCUM TOTAL b-tt-work.qtd   FORMAT  ">>,>>>,>>9.99" AT  56.
     PUT ACCUM TOTAL b-tt-work.valor FORMAT ">>>,>>>,>>9.99" AT  71.
     PUT (ACCUM TOTAL b-tt-work.valor) /
         (ACCUM TOTAL b-tt-work.qtd)  FORMAT ">,>>9.99" AT  86.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-nota-fiscal D-Dialog 
PROCEDURE pi-nota-fiscal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR de-qtd AS DEC.
  DEF VAR de-vlr AS DEC.

  EMPTY TEMP-TABLE tt-work.

  IF p-it-codigo <> "" THEN DO:
     br-work:TITLE IN FRAME {&FRAME-NAME} = "Notas Fiscais do Item " + p-it-codigo.
     FOR EACH tt-nfiscal WHERE
              tt-nfiscal.it-codigo = p-it-codigo. 
         FIND nota-fiscal WHERE
              nota-fiscal.cod-estabel = tt-nfiscal.cod-estabel AND
              nota-fiscal.serie       = tt-nfiscal.serie       AND
              nota-fiscal.nr-nota-fis = tt-nfiscal.nr-nota-fis NO-LOCK NO-ERROR.
         IF NOT AVAIL nota-fiscal THEN NEXT.

         IF nota-fiscal.nat-operacao = "599FAB" THEN NEXT.

         RUN pi-acompanhar IN h-acomp (INPUT "Data: "    + STRING(nota-fiscal.dt-emis-nota) +
                                             " Nota Fiscal: " + nota-fiscal.nr-nota-fis).
             
         ASSIGN de-qtd = 0
                de-vlr = 0.
         FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
             IF p-it-codigo <> "" AND p-it-codigo <> it-nota-fisc.it-codigo THEN NEXT.

              

             ASSIGN de-qtd = de-qtd + it-nota-fisc.qt-faturada[1]
                    de-vlr = de-vlr + it-nota-fisc.vl-tot-item. 

         END.
         IF de-qtd + de-vlr = 0 THEN NEXT.

         FIND tt-work WHERE
              tt-work.nr-nota-fis = nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.
         IF NOT AVAIL tt-work THEN DO:
            CREATE tt-work.
            ASSIGN tt-work.nr-nota-fis  = nota-fiscal.nr-nota-fis
                   tt-work.cod-estabel  = tt-nfiscal.cod-estabel
                   tt-work.serie        = tt-nfiscal.serie
                   tt-work.nome-abrev   = nota-fiscal.nome-ab-cli
                   tt-work.dt-emis-nota = nota-fiscal.dt-emis-nota
                   tt-work.dt-saida     = nota-fiscal.dt-saida
                   tt-work.qtde         = de-qtd  
                   tt-work.valor        = de-vlr.  
         END.
     END.
  END.
  ELSE DO:
      br-work:TITLE IN FRAME {&FRAME-NAME} = "Notas Fiscais do Periodo " + STRING(p-dt-ini, "99/99/9999") + " a " +
                                                                           STRING(p-dt-fin, "99/99/9999").
      FOR EACH tt-nfiscal NO-LOCK 
            BY tt-nfiscal.nr-nota-fis.
          FIND nota-fiscal WHERE
               nota-fiscal.cod-estabel = tt-nfiscal.cod-estabel AND
               nota-fiscal.serie       = tt-nfiscal.serie       AND
               nota-fiscal.nr-nota-fis = tt-nfiscal.nr-nota-fis NO-LOCK NO-ERROR.
          IF NOT AVAIL nota-fiscal THEN NEXT.

          RUN pi-acompanhar IN h-acomp (INPUT "Data: "    + STRING(nota-fiscal.dt-emis-nota) +
                                              " Nota Fiscal: " + nota-fiscal.nr-nota-fis).

          ASSIGN de-qtd = 0
                 de-vlr = 0.
          FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.

              ASSIGN de-qtd = de-qtd + it-nota-fisc.qt-faturada[1]
                     de-vlr = de-vlr + it-nota-fisc.vl-tot-item. 

          END.
          IF de-qtd + de-vlr = 0 THEN NEXT.

          FIND tt-work WHERE
               tt-work.nr-nota-fis = nota-fiscal.nr-nota-fis NO-LOCK NO-ERROR.
          IF NOT AVAIL tt-work THEN DO:
             CREATE tt-work.
             ASSIGN tt-work.nr-nota-fis  = nota-fiscal.nr-nota-fis
                    tt-work.cod-estabel  = tt-nfiscal.cod-estabel
                    tt-work.serie        = tt-nfiscal.serie
                    tt-work.nome-abrev   = nota-fiscal.nome-ab-cli
                    tt-work.dt-emis-nota = nota-fiscal.dt-emis-nota
                    tt-work.dt-saida     = nota-fiscal.dt-saida
                    tt-work.qtde         = de-qtd  
                    tt-work.valor        = de-vlr.  
          END.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-processa D-Dialog 
PROCEDURE pi-processa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 ASSIGN FRAME {&FRAME-NAME}:VISIBLE = NO.
 RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
 {utp/ut-liter.i Verificando_Notas_Fiscais *}
 RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

 RUN pi-nota-fiscal.

 RUN pi-finalizar in h-acomp.
 ASSIGN FRAME {&FRAME-NAME}:VISIBLE = YES.

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
 ASSIGN fi-tot-qtd = 0
        fi-tot-vlr = 0.
 FOR EACH tt-work NO-LOCK.
     ASSIGN fi-tot-qtd = fi-tot-qtd + tt-work.qtde  
            fi-tot-vlr = fi-tot-vlr + tt-work.valor.
 END.
 ASSIGN fi-pco-medio = fi-tot-vlr / fi-tot-qtd.
 DISP fi-tot-qtd 
      fi-tot-vlr
      fi-pco-medio
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
  {src/adm/template/snd-list.i "tt-work"}

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

