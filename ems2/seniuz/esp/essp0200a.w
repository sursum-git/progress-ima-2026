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
{include/i-prgvrs.i ESSP0204A 2.06.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

{include/tt-edit.i}
{include/pi-edit.i}

/* ***************************  Definitions  ************************** */
define buffer empresa for mgcad.empresa.

DEF TEMP-TABLE tt-itens-icstcs
    FIELD nr-seq      AS INT
    FIELD it-codigo   LIKE it-nota-fisc.it-codigo
    FIELD cod-refer   LIKE ITEM.cod-refer
    FIELD nr-lote     LIKE ob-etiqueta.nr-lote
    FIELD qtde-item   AS DECIMAL FORMAT ">,>>>,>>>,>>9.99"
    FIELD vlr-unit    AS DECIMAL FORMAT ">,>>>,>>>,>>9.99"
    FIELD vlr-total   AS DECIMAL FORMAT ">,>>>,>>>,>>9.99"
    FIELD narrativa   AS CHAR.

DEF TEMP-TABLE tt-itens-sel
    FIELD marca       AS CHAR
    FIELD nr-nota-fis LIKE it-nota-fisc.nr-nota-fis
    FIELD nome-abrev   LIKE nota-fiscal.nome-ab-cli
    FIELD dt-emis     LIKE it-nota-fisc.dt-emis-nota
    FIELD it-codigo   LIKE it-nota-fisc.it-codigo
    FIELD cod-refer   LIKE ITEM.cod-refer
    FIELD nr-lote     LIKE ob-etiqueta.nr-lote
    FIELD qtde-item   AS DECIMAL FORMAT ">,>>>,>>>,>>9.99"
    FIELD vlr-unit    AS DECIMAL FORMAT ">,>>>,>>>,>>9.99"
    FIELD vlr-total   AS DECIMAL FORMAT ">,>>>,>>>,>>9.99"
    FIELD nr-pedcli   LIKE it-nota-fisc.nr-pedcli
    FIELD descricao   AS CHAR FORMAT "x(30)"
    FIELD nr-seq-ped  AS INT
    FIELD manual      AS CHAR
    INDEX indice1 it-codigo cod-refer nr-lote.


/* Parameters Definitions ---                                           */
    
DEF INPUT PARAMETER TABLE FOR tt-itens-icstcs.
DEF INPUT PARAMETER TABLE FOR tt-itens-sel.
DEF OUTPUT PARAMETER p-autorizacao   AS CHAR. 
DEF INPUT  PARAMETER c-cod-emit      LIKE emitente.cod-emit.
DEF INPUT  PARAMETER c-nr-nota-fis   LIKE nota-fiscal.nr-nota-fis.
DEF INPUT  PARAMETER da-dt-emis-nota LIKE nota-fiscal.nr-nota-fis.
DEF INPUT  PARAMETER de-qtd          AS DEC.
DEF INPUT  PARAMETER de-vlr          AS DEC.
DEF INPUT  PARAMETER c-nome-transp   AS CHAR.
DEF INPUT  PARAMETER i-frete         AS INT.
DEF INPUT  PARAMETER c-motivo        AS CHAR FORMAT "x(3000)".


/* Local Variable Definitions ---                                       */
DEF VAR h-acomp     AS HANDLE.
DEF VAR c-empresa   AS CHAR.
DEF VAR c-arq-email AS CHAR.
DEF VAR de-total-qtd AS DEC.
DEF VAR de-total-vlr AS DEC.


/* Variaveis da Rotina de ImpressÆo */
DEFINE VAR l-ok           AS LOG.
DEFINE VAR l-enviou-email AS LOG INIT NO.
DEFINE VAR c-saida        AS CHAR.
DEFINE VAR i-saida        AS INT.
DEFINE VAR i-num-copias   AS INT.
DEFINE VAR i-lin          AS INT.
DEFINE VAR i-ct           AS INT.
DEFINE VAR i-pag          AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog
&Scoped-define BROWSE-NAME br-icstcs

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-itens-icstcs

/* Definitions for BROWSE br-icstcs                                     */
&Scoped-define FIELDS-IN-QUERY-br-icstcs tt-itens-icstcs.nr-seq tt-itens-icstcs.it-codigo tt-itens-icstcs.cod-refer tt-itens-icstcs.nr-lote tt-itens-icstcs.qtde-item tt-itens-icstcs.vlr-unit tt-itens-icstcs.vlr-total   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-icstcs   
&Scoped-define SELF-NAME br-icstcs
&Scoped-define QUERY-STRING-br-icstcs FOR EACH tt-itens-icstcs                             WHERE ~{&KEY-PHRASE} NO-LOCK
&Scoped-define OPEN-QUERY-br-icstcs OPEN QUERY {&SELF-NAME} FOR EACH tt-itens-icstcs                             WHERE ~{&KEY-PHRASE} NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-icstcs tt-itens-icstcs
&Scoped-define FIRST-TABLE-IN-QUERY-br-icstcs tt-itens-icstcs


/* Definitions for DIALOG-BOX D-Dialog                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-D-Dialog ~
    ~{&OPEN-QUERY-br-icstcs}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-buttom br-icstcs ed-autorizacao ~
bt-cancela bt-imprime bt-email bt-ok bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS ed-narrativa ed-autorizacao 

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
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON bt-email 
     IMAGE-UP FILE "image/im-email.bmp":U
     LABEL "Button 6" 
     SIZE 8 BY 1.

DEFINE BUTTON bt-imprime 
     LABEL "Imprimir" 
     SIZE 12 BY 1 TOOLTIP "Imprimir a Programa‡Æo da Produ‡Æo"
     FGCOLOR 12 .

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&Receber com Inconsistˆncias" 
     SIZE 35 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE ed-autorizacao AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 87 BY 2.25 NO-UNDO.

DEFINE VARIABLE ed-narrativa AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 86.72 BY 3.21 NO-UNDO.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 87 BY 1.42
     BGCOLOR 7 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-icstcs FOR 
      tt-itens-icstcs SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-icstcs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-icstcs D-Dialog _FREEFORM
  QUERY br-icstcs NO-LOCK DISPLAY
      tt-itens-icstcs.nr-seq    COLUMN-LABEL "Seq"        WIDTH 5
      tt-itens-icstcs.it-codigo COLUMN-LABEL "Item"       FORMAT "x(10)":U WIDTH 14
      tt-itens-icstcs.cod-refer COLUMN-LABEL "Refer."     FORMAT "x(10)":U WIDTH 8
      tt-itens-icstcs.nr-lote   COLUMN-LABEL "Lote"       FORMAT "x(3)":U WIDTH 8
      tt-itens-icstcs.qtde-item COLUMN-LABEL "Qtde"       FORMAT ">,>>>,>>>,>>9.99":U WIDTH 15
      tt-itens-icstcs.vlr-unit  COLUMN-LABEL "Vlr. Unit." FORMAT ">,>>>,>>>,>>9.99":U WIDTH 15
      tt-itens-icstcs.vlr-total COLUMN-LABEL "Vlr. Total" FORMAT ">,>>>,>>>,>>9.99":U WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 87 BY 7.54
         TITLE "I N C O N S I S T Ò N C I A S" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     br-icstcs AT ROW 1.21 COL 2 WIDGET-ID 200
     ed-narrativa AT ROW 9.5 COL 2.14 NO-LABEL WIDGET-ID 2
     ed-autorizacao AT ROW 13.54 COL 2 NO-LABEL WIDGET-ID 74 NO-TAB-STOP 
     bt-cancela AT ROW 16.25 COL 2.86
     bt-imprime AT ROW 16.25 COL 16 WIDGET-ID 72
     bt-email AT ROW 16.25 COL 30.14 WIDGET-ID 22
     bt-ok AT ROW 16.25 COL 40.29
     bt-ajuda AT ROW 16.25 COL 78
     "Autoriza‡Æo de Recebimento" VIEW-AS TEXT
          SIZE 29 BY .67 AT ROW 12.92 COL 2.14 WIDGET-ID 78
          FGCOLOR 12 
     "Motivo da Inconsistˆncia" VIEW-AS TEXT
          SIZE 23 BY .67 AT ROW 8.83 COL 2.14 WIDGET-ID 80
          FGCOLOR 12 
     rt-buttom AT ROW 16 COL 2
     SPACE(0.85) SKIP(0.36)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Detalha Inconsistˆncias - ESSP0200a"
         DEFAULT-BUTTON bt-ok WIDGET-ID 100.


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
/* BROWSE-TAB br-icstcs rt-buttom D-Dialog */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR ed-narrativa IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-icstcs
/* Query rebuild information for BROWSE br-icstcs
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-itens-icstcs
                            WHERE ~{&KEY-PHRASE} NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE br-icstcs */
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Detalha Inconsistˆncias - ESSP0200a */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-icstcs
&Scoped-define SELF-NAME br-icstcs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-icstcs D-Dialog
ON VALUE-CHANGED OF br-icstcs IN FRAME D-Dialog /* I N C O N S I S T Ò N C I A S */
DO:
  IF AVAIL tt-itens-icstcs THEN
     ASSIGN ed-narrativa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tt-itens-icstcs.narrativa.
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
  ASSIGN p-autorizacao = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-email
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-email D-Dialog
ON CHOOSE OF bt-email IN FRAME D-Dialog /* Button 6 */
DO:
   DEF VAR c-mensagem   AS CHAR.
   DEF VAR c-tipo-frete AS CHAR.

   MESSAGE "Deseja Comunicar a Devolu‡Æo Recebida Via E-MAIL ?"
         VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE choice AS LOGICAL.

   FIND emitente WHERE 
        emitente.cod-emit = c-cod-emit NO-LOCK NO-ERROR.
   FIND repres WHERE
        repres.cod-rep = emitente.cod-rep NO-LOCK NO-ERROR.
     
   CASE choice:
      WHEN FALSE THEN 
         RETURN NO-APPLY.
      WHEN TRUE THEN DO:
           RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
           {utp/ut-liter.i Enviando_Emails *}
           RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

           CASE i-frete:
               WHEN 1 THEN
                   ASSIGN c-tipo-frete = 'CIF'.
               WHEN 2 THEN
                   ASSIGN c-tipo-frete = 'FOB'.
               WHEN 3 THEN
                   ASSIGN c-tipo-frete = 'OUTROS'.
           END CASE.

           ASSIGN c-mensagem = "Prezados(as) Senhores(as)," + CHR(13) + CHR(13) +
                  "Em anexo arquivo com dados da devolu‡Æo recebida do cliente " +
                   UPPER(TRIM(emitente.nome-emit)) + "." + CHR(13) + CHR(13) +
                  "Expedi‡Æo PARAOPEBA" + CHR(13) +
                  "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA".
        
            ASSIGN  i-lin = 99
                    i-pag =  1.
        
           ASSIGN c-arq-email = SESSION:TEMP-DIRECTORY + "Devolucao Cliente " + STRING(emitente.cod-emit, ">>>,>>9") + ".txt".
           OUTPUT TO VALUE(c-arq-email) CONVERT SOURCE "ibm850".

           FOR EACH tt-itens-sel NO-LOCK
               BREAK BY tt-itens-sel.nr-nota-fis.
        
               FIND FIRST ITEM WHERE
                          ITEM.it-codigo = tt-itens-sel.it-codigo NO-LOCK NO-ERROR.

               RUN pi-acompanhar IN h-acomp (INPUT "Nota Fiscal: " + tt-itens-sel.nr-nota-fis + "   " + 
                                                   "Itens: " + tt-itens-sel.it-codigo).
        
               IF i-lin > 61 THEN DO:
                  RUN pi-imp-cabec-email.
                  ASSIGN i-lin = 7.
               END.
        
               PUT STRING(tt-itens-sel.nr-nota-fis)                          AT   1 
                   tt-itens-sel.dt-emis     FORMAT "99/99/9999"              AT  10
                   tt-itens-sel.it-codigo   FORMAT "x(6)"                    AT  22
                   tt-itens-sel.descricao   FORMAT "x(48)"                   AT  29
                   STRING(tt-itens-sel.cod-refer, "99.9999-9") FORMAT "X(9)" AT  78
                   tt-itens-sel.nr-lote     FORMAT "x(4)"                    AT  89
                   tt-itens-sel.qtde-item   FORMAT ">>>,>>9.99"              AT  94
                   tt-itens-sel.vlr-unit    FORMAT ">>>,>>9.99"              AT 106
                   tt-itens-sel.vlr-total   FORMAT ">>>,>>9.99"              AT 118
                   tt-itens-sel.nr-pedcli   FORMAT "99999999"                AT 129.
                  
               ASSIGN i-lin = i-lin + 1.
               ACCUMULATE tt-itens-sel.qtde-item (TOTAL BY tt-itens-sel.nr-nota-fis).
               ACCUMULATE tt-itens-sel.vlr-total (TOTAL BY tt-itens-sel.nr-nota-fis).

               PUT SKIP.
               ASSIGN i-lin = i-lin + 1.
              
               IF LAST-OF(tt-itens-sel.nr-nota-fis) THEN DO:
                  IF i-lin > 61 THEN DO:
                     RUN pi-imp-cabec-email.
                     ASSIGN i-lin = 7.
                  END.

                  PUT "----------             -----------"  AT 94.
                  ASSIGN i-lin = i-lin + 1.
                  PUT "TOTAL DA NOTA FISCAL.............:" FORMAT "x(34)" AT 59.
                  PUT ACCUM TOTAL BY tt-itens-sel.nr-nota-fis tt-itens-sel.qtde-item FORMAT ">>>,>>9.99"  AT  94.
                  PUT ACCUM TOTAL BY tt-itens-sel.nr-nota-fis tt-itens-sel.vlr-total FORMAT ">>>,>>9.99"  AT 118.
                  ASSIGN de-total-qtd = de-total-qtd + (ACCUM TOTAL BY tt-itens-sel.nr-nota-fis tt-itens-sel.qtde-item)
                         de-total-vlr = de-total-vlr + (ACCUM TOTAL BY tt-itens-sel.nr-nota-fis tt-itens-sel.vlr-total)
                         i-lin = i-lin + 1.

                  PUT SKIP.
               END.
           END.
           PUT SKIP(1).
           ASSIGN i-lin = i-lin + 1.
           PUT "TOTAL GERAL DA DEVOLU€ÇO.........:" FORMAT "x(34)" AT 59.
           PUT de-total-qtd FORMAT ">>>,>>9.99"  AT  94.
           PUT de-total-vlr FORMAT ">>>,>>9.99"  AT 118.
           PUT SKIP(1).
           PUT "TRANSPORTADORA..: " + c-nome-transp FORMAT "x(50)" AT 1. 
           PUT "TIPO DO FRETE...: " + c-tipo-frete  FORMAT "x(25)" AT 1.
           PUT "MOTIVO DEVOLUCAO: " + c-motivo      FORMAT "x(17)" AT 1.
           RUN pi-print-editor(INPUT REPLACE(REPLACE(TRIM(c-motivo),CHR(13)," "),CHR(10)," "), INPUT 110). 
           FOR EACH tt-editor:
               PUT tt-editor.conteudo AT 19 SKIP.
               ASSIGN i-lin = i-lin + 1.
           END.
           PUT SKIP(1).
           PUT "AUTORIZA€ÇO DE RECEBIMENTO: " AT 1.
           RUN pi-print-editor(INPUT REPLACE(REPLACE(TRIM(ed-autorizacao:SCREEN-VALUE IN FRAME {&FRAME-NAME}),CHR(13)," "),CHR(10)," "), INPUT 110). 
           FOR EACH tt-editor:
               PUT tt-editor.conteudo AT 29 SKIP.
               ASSIGN i-lin = i-lin + 1.
           END.
           OUTPUT CLOSE.
           RUN esapi/esapi002.p (INPUT "teartextil@teartextil.com.br", /* e-mail remetente */ 
                                 INPUT "ti.seniuz@gmail.com",           /* e-mail destinat rio */ 
                                 INPUT "DEVOLUCAO DE CLIENTE" ,        /* Assunto  */
                                 INPUT c-mensagem,                     /* Mensagem */
                                 INPUT c-arq-email,                    /*arquivo anexo */
                                 INPUT YES).                           /* Mostra Erros */

           ASSIGN  i-lin          = 99
                   l-enviou-email = YES.
                   i-pag          =  1.

           RUN pi-finalizar in h-acomp.
           MESSAGE 'Email(s) Enviado(s) com Sucesso....'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           APPLY "CLOSE":U TO THIS-PROCEDURE.
      END.
   END CASE.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime D-Dialog
ON CHOOSE OF bt-imprime IN FRAME D-Dialog /* Imprimir */
DO:
  RUN pi-imprime.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* Receber com Inconsistˆncias */
DO:
   IF NOT l-enviou-email THEN DO.
      MESSAGE "E-mail NÆo Foi Enviado, Deseja Processar a Devolu‡Æo ?"
             VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE l-ok AS LOGICAL.
      IF NOT l-ok THEN 
         RETURN NO-APPLY.
   END.

   IF ed-autorizacao:SCREEN-VALUE = "" OR 
      LENGTH(ed-autorizacao:SCREEN-VALUE) < 20 THEN DO.

      MESSAGE 'Favor Informar autoriza‡Æo com no m¡nimo 20 caracteres...'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

      APPLY 'ENTRY' TO ed-autorizacao.
      RETURN NO-APPLY.
   END.
   ASSIGN p-autorizacao = ed-autorizacao:SCREEN-VALUE.
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
  DISPLAY ed-narrativa ed-autorizacao 
      WITH FRAME D-Dialog.
  ENABLE rt-buttom br-icstcs ed-autorizacao bt-cancela bt-imprime bt-email 
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

  {utp/ut9000.i "ESSP0204A" "2.06.00.000"}
                              
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Busca Nome da Empresa */
  FIND FIRST param-global NO-LOCK NO-ERROR.
  FIND FIRST empresa
       WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 
  ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

  FIND FIRST param-dis NO-LOCK NO-ERROR.

  APPLY "value-changed" TO br-icstcs.


  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec-email D-Dialog 
PROCEDURE pi-imp-cabec-email :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 PUT c-empresa  FORMAT "x(40)"                 AT   1
     "DATA: "                                  AT  71
     STRING(TODAY,"99/99/9999") FORMAT "x(10)" AT  77
     "HORA: "                                  AT 104
     STRING(TIME,"hh:mm:ss")                   AT 110
     "PAG:"                                    AT 150
     i-pag FORMAT ">>>"                        AT 155
     SKIP(1).

 PUT "DEVOLU€ÇO DE CLIENTE" FORMAT "x(25)" AT 44  SKIP(1).

 PUT "CLIENTE: " + STRING(emitente.cod-emit, ">>>>>>>9") + " - " + emitente.nome-emit FORMAT "x(36)" AT 1
     "N§ DOCTO: "              FORMAT "x(10)"  AT 47
      c-nr-nota-fis            FORMAT "x(11)"  AT 57 
      "DATA ENTRADA: "         FORMAT "x(14)"  AT 71
      TRIM(STRING(TODAY, "99/99/9999"))   FORMAT "x(10)" AT  85
      "QTDE DEV.: "                       FORMAT "x(11)" AT  97
      TRIM(STRING(de-qtd, ">>>>,>>9.99")) FORMAT "x(11)" AT 108
      "VALOR: "                           FORMAT "X(7)"  AT 128
      TRIM(STRING(de-vlr, ">>>>,>>9.99")) FORMAT "x(11)" AT 135.
 PUT "REPRES.: "  FORMAT "x(9)"   AT  1
      repres.nome FORMAT "x(40)" AT 10
      SKIP(1).

 PUT "N.FISCAL DATA DA NF ITEM   DESCRI€ÇO                                         REFERENCIA LOTE QUANTIDADE VL.UNITARIO VALOR TOTAL PEDIDO" AT 1.
 PUT "-------- ---------- ------ ------------------------------------------------- ---------- ---- ---------- ----------- ----------- ------" AT 1.
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

&SCOPED-DEFINE BREAKBY2 tt-itens-icstcs.nr-seq 

  DEF VAR tot-qtde-item AS DECIMAL NO-UNDO.
  DEF VAR tot-vlr-unit  AS DECIMAL NO-UNDO.
  DEF VAR tot-vlr-tot   AS DECIMAL NO-UNDO.
  DEF VAR i-lin-aux     AS INT NO-UNDO.
  DEF VAR c-autorizacao AS CHAR FORMAT "x(3000)".

  RUN utp/ut-utils.p PERSISTENT SET h-prog.

  RUN esapi/saida-imp.p (OUTPUT i-saida,
                         OUTPUT c-saida,
                         OUTPUT i-num-copias,
                         OUTPUT l-ok).
  CASE i-saida:
      WHEN 1 THEN DO.
          OUTPUT TO VALUE(c-saida) CONVERT TARGET "ibm850" PAGED PAGE-SIZE 39.
          PUT CONTROL "~033&l1O~033(s16H". /* ORIENTA€ÇO PAISAGEM & COMPACTA */ 
      END.
      WHEN 2 THEN
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      WHEN 3 THEN DO.
          ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0204a.tmp".
          OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
      END.
  END CASE.

  ASSIGN c-autorizacao = ed-autorizacao:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

  DO i-ct = 1 TO i-num-copias.
     ASSIGN i-pag      =  1
            i-lin      = 99.

     ASSIGN tot-qtde-item  = 0 
            tot-vlr-unit = 0
            tot-vlr-tot = 0.
  
     FOR EACH tt-itens-icstcs NO-LOCK  
         BREAK BY {&BREAKBY2}.

         IF i-lin > 39 THEN DO:
            PUT c-empresa FORMAT "X(40)"                   AT   1
                "DATA: "                                   AT  60
                STRING(TODAY,"99/99/9999") FORMAT "X(10)"  AT  66
                "HORA: "                                   AT 102
                STRING(TIME,"hh:mm:ss")                    AT 108
                "PAG:"                                     AT 142
                i-pag FORMAT ">>>"                         AT 147 SKIP(1).
            PUT "RELATORIO INCONSISTENCIAS DA DEVOLU€ÇO CLIENTE" AT 57 SKIP(1).
            PUT "SEQ ITEM   REFER.    LOTE QUANTIDADE V.UNITARIO VALOR TOTAL MOTIVO DA INCONSISTENCIA" AT 1.
            PUT "--- ------ --------- ---- ---------- ---------- ----------- ------------------------------------------------------------------------------------------" AT 1.
            ASSIGN i-pag = i-pag + 1.  
            ASSIGN i-lin = 7.
         END.
            
         PUT tt-itens-icstcs.nr-seq     FORMAT "999":U                      AT  1
             tt-itens-icstcs.it-codigo  FORMAT "X(6)":U                     AT  5
             STRING(tt-itens-icstcs.cod-refer, "99.9999-9") FORMAT "X(9)":U AT 12
             tt-itens-icstcs.nr-lote    FORMAT "X(4)":U                     AT 22
             tt-itens-icstcs.qtde-item  FORMAT ">>>,>>9.99":U               AT 27
             tt-itens-icstcs.vlr-unit   FORMAT ">>>,>>9.99":U               AT 38
             tt-itens-icstcs.vlr-total  FORMAT ">>>>,>>9.99":U              AT 49
             tt-itens-icstcs.narrativa  FORMAT "x(89)":U                    AT 61.
          ASSIGN i-lin = i-lin + 1.

             ASSIGN tot-qtde-item  = tot-qtde-item + tt-itens-icstcs.qtde-item.
                
     END.
     PUT SKIP(1).
     ASSIGN i-lin = i-lin + 2.
     PUT "AUTORIZA€ÇO DE RECEBIMENTO: " AT 1.
     RUN pi-print-editor(INPUT REPLACE(REPLACE(TRIM(c-autorizacao),CHR(13)," "),CHR(10)," "), INPUT 110). 
     FOR EACH tt-editor:
         PUT tt-editor.conteudo AT 29 SKIP.
         ASSIGN i-lin = i-lin + 1.
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
  {src/adm/template/snd-list.i "tt-itens-icstcs"}

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

