&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-digita 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i ESSP0133 2.04.00.000}
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---  
*/

DEFINE TEMP-TABLE tt-mp-fardo 
       FIELD nr-fardo   LIKE mp-fardo.nr-fardo
       FIELD padrao     LIKE mp-fardo.padrao
       FIELD nr-mistura LIKE mp-fardo.nr-mistura
       FIELD letra          AS CHAR FORMAT "x(3)"
       FIELD desc-coloracao AS CHAR FORMAT "x(20)" 
       FIELD desc-tipo      AS CHAR FORMAT "x(15)"
       FIELD sl2            AS CHAR FORMAT "x(11)" 
       FIELD peso       LIKE mp-fardo.peso
       FIELD cod-depos  LIKE mp-fardo.cod-depos
       INDEX indice1 nr-fardo.

DEF VAR i-ct              AS INT.
DEF VAR l-opc             AS LOG.
DEF VAR de-peso-paraopeba AS DEC.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-fardos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-mp-fardo

/* Definitions for BROWSE br-fardos                                     */
&Scoped-define FIELDS-IN-QUERY-br-fardos tt-mp-fardo.nr-fardo tt-mp-fardo.padrao tt-mp-fardo.letra tt-mp-fardo.nr-mistura tt-mp-fardo.desc-coloracao tt-mp-fardo.desc-tipo tt-mp-fardo.sl2 tt-mp-fardo.peso   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-fardos   
&Scoped-define SELF-NAME br-fardos
&Scoped-define QUERY-STRING-br-fardos FOR EACH tt-mp-fardo NO-LOCK
&Scoped-define OPEN-QUERY-br-fardos OPEN QUERY {&SELF-NAME} FOR EACH tt-mp-fardo NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-fardos tt-mp-fardo
&Scoped-define FIRST-TABLE-IN-QUERY-br-fardos tt-mp-fardo


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-fardos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-dt-baixa fi-nr-mistura fi-nr-fardo ~
bt-processa br-fardos bt-ajuda bt-ok bt-cancelar bt-elimina RECT-1 RECT-44 
&Scoped-Define DISPLAYED-OBJECTS fi-dt-baixa fi-nr-mistura fi-nr-fardo ~
fi-qt-fardo fi-peso-fardo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 fi-nr-fardo 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-digita AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-elimina 
     LABEL "Eliminar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "OK" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-processa 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "Button 1" 
     SIZE 4 BY 1.

DEFINE VARIABLE fi-dt-baixa AS DATE FORMAT "99/99/9999":U 
     LABEL "Data da Baixa" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data da Baixa do Estoque" NO-UNDO.

DEFINE VARIABLE fi-nr-fardo AS INTEGER FORMAT "->>>>>>>>9":U INITIAL 0 
     LABEL "Nß Fardo" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-mistura AS INTEGER FORMAT ">>>,>>>,>>9":U INITIAL 5024 
     LABEL "Nß Mistura" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-peso-fardo AS DECIMAL FORMAT ">>,>>>,>>9.99":U INITIAL 0 
     LABEL "Peso Fardos" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qt-fardo AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Qtd Fardos" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 80 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 1.75.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-fardos FOR 
      tt-mp-fardo SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-fardos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-fardos w-digita _FREEFORM
  QUERY br-fardos DISPLAY
      tt-mp-fardo.nr-fardo  COLUMN-LABEL "Fardo" COLUMN-FONT 9
      tt-mp-fardo.padrao    
      tt-mp-fardo.letra           COLUMN-LABEL "letra"
      tt-mp-fardo.nr-mistura      COLUMN-LABEL "Mistura" WIDTH 6
      tt-mp-fardo.desc-coloracao  COLUMN-LABEL "Tonalidade"
      tt-mp-fardo.desc-tipo       COLUMN-LABEL "Tipo" WIDTH 8.95
      tt-mp-fardo.sl2             COLUMN-LABEL "Comprimento"
      tt-mp-fardo.peso            COLUMN-LABEL "Peso"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 79 BY 15
         FONT 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-dt-baixa AT ROW 1.5 COL 13 COLON-ALIGNED
     fi-nr-mistura AT ROW 1.5 COL 36.86 COLON-ALIGNED
     fi-nr-fardo AT ROW 1.5 COL 61.57 COLON-ALIGNED
     bt-processa AT ROW 1.42 COL 73.14
     br-fardos AT ROW 3 COL 2
     fi-qt-fardo AT ROW 18.25 COL 11 COLON-ALIGNED
     fi-peso-fardo AT ROW 18.25 COL 67.43 COLON-ALIGNED
     bt-ajuda AT ROW 19.71 COL 70.14
     bt-ok AT ROW 19.75 COL 2.14
     bt-cancelar AT ROW 19.75 COL 13.14
     bt-elimina AT ROW 19.75 COL 45.43
     RECT-1 AT ROW 19.5 COL 1
     RECT-44 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 19.88
         FONT 1
         DEFAULT-BUTTON bt-ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Digitacao
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-digita ASSIGN
         HIDDEN             = YES
         TITLE              = "Baixa de Fardos de Algod∆o"
         HEIGHT             = 19.88
         WIDTH              = 80
         MAX-HEIGHT         = 29
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 29
         VIRTUAL-WIDTH      = 146.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-digita 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-digit.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-digita
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB br-fardos bt-processa F-Main */
/* SETTINGS FOR FILL-IN fi-nr-fardo IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN fi-peso-fardo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qt-fardo IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-fardos
/* Query rebuild information for BROWSE br-fardos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-mp-fardo NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-fardos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Baixa de Fardos de Algod∆o */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Baixa de Fardos de Algod∆o */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-fardos
&Scoped-define SELF-NAME br-fardos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-fardos w-digita
ON DELETE-CHARACTER OF br-fardos IN FRAME F-Main
DO:
  APPLY 'choose' TO bt-elimina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-digita
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-cancelar w-digita
ON CHOOSE OF bt-cancelar IN FRAME F-Main /* Cancelar */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-elimina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-elimina w-digita
ON CHOOSE OF bt-elimina IN FRAME F-Main /* Eliminar */
DO:
   IF AVAIL tt-mp-fardo  THEN DO:
      ASSIGN fi-qt-fardo = fi-qt-fardo - 1
             fi-peso-fardo = fi-peso-fardo - tt-mp-fardo.peso
             fi-qt-fardo:SCREEN-VALUE   = STRING(fi-qt-fardo)
             fi-peso-fardo:SCREEN-VALUE = STRING(fi-peso-fardo).
      DELETE tt-mp-fardo.
      {&OPEN-QUERY-br-fardos}
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* OK */
DO:
   IF SEARCH("n:\interfac\mail\blat.exe") = ? THEN DO:
      MESSAGE "N∆o consigo conectar com o programa de transmiss∆o de email ! BAIXA N«O FOI EFETUADA"
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'entry' TO bt-ok IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   FOR EACH tt-mp-fardo.
       FIND mp-fardo OF tt-mp-fardo NO-ERROR.
       IF AVAIL mp-fardo THEN DO:
          ASSIGN mp-fardo.situacao   = 4
                 mp-fardo.dt-baixa   = INPUT FRAME {&FRAME-NAME} fi-dt-baixa
                 mp-fardo.nr-mistura = tt-mp-fardo.nr-mistura.
       END.
   END.
   RUN envia-email.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-processa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-processa w-digita
ON CHOOSE OF bt-processa IN FRAME F-Main /* Button 1 */
DO:
    /*
  FIND mp-mistura WHERE
       mp-mistura.nr-mistura = fi-nr-mistura:INPUT-VALUE NO-LOCK NO-ERROR.

  IF NOT AVAIL mp-mistura THEN DO.
     MESSAGE "Favor informar Mistura Valida ! ! !" 
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' TO fi-nr-mistura IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.
  */
  IF fi-nr-fardo:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> '' THEN DO.
     FIND mp-fardo WHERE
          mp-fardo.nr-fardo = fi-nr-fardo:INPUT-VALUE IN FRAME {&FRAME-NAME}
          NO-LOCK NO-ERROR.
    
     IF AVAIL mp-fardo THEN DO.
         IF mp-fardo.situacao = 4 THEN DO: /* Fardo Baixado */
             MESSAGE "Fardo j† foi Baixado ! ! !" VIEW-AS ALERT-BOX.             
             APPLY 'entry' TO fi-nr-fardo.
             RETURN NO-APPLY.                                                    
         END.
        FIND mp-coloracao  WHERE
             mp-coloracao.codigo = mp-fardo.cd-coloracao NO-LOCK NO-ERROR.
    
        FIND mp-tipo  WHERE
             mp-tipo.codigo = mp-fardo.cd-tipo NO-LOCK NO-ERROR.
    
        FIND tt-mp-fardo WHERE
             tt-mp-fardo.nr-fardo = mp-fardo.nr-fardo 
             NO-LOCK NO-ERROR.     
        IF NOT AVAIL tt-mp-fardo THEN DO:
           CREATE tt-mp-fardo.
           ASSIGN tt-mp-fardo.nr-fardo       = mp-fardo.nr-fardo       
                  tt-mp-fardo.padrao         = mp-fardo.padrao
                  tt-mp-fardo.nr-mistura     = fi-nr-mistura:INPUT-VALUE
                  tt-mp-fardo.letra          = mp-fardo.letra + STRING(mp-fardo.cd-coloracao) + 
                                                                STRING(mp-fardo.cd-tipo)
                  tt-mp-fardo.desc-coloracao = IF AVAIL mp-coloracao 
                                               THEN mp-coloracao.tonalidade ELSE ''
                  tt-mp-fardo.desc-tipo      = IF AVAIL mp-tipo
                                               THEN mp-tipo.tipo ELSE ''
                  tt-mp-fardo.sl2            = STRING(mp-fardo.sl2, ">>9.99")
                  tt-mp-fardo.cod-depos      = mp-fardo.cod-depos
                  tt-mp-fardo.peso           = mp-fardo.peso. 

            ASSIGN fi-qt-fardo = fi-qt-fardo + 1
                   fi-peso-fardo = fi-peso-fardo + mp-fardo.peso
                   fi-qt-fardo:SCREEN-VALUE   = STRING(fi-qt-fardo)
                   fi-peso-fardo:SCREEN-VALUE = STRING(fi-peso-fardo).

            fi-dt-baixa:SENSITIVE IN FRAME {&FRAME-NAME}   = NO.
            fi-nr-mistura:SENSITIVE IN FRAME {&FRAME-NAME} = NO.


        END.
        {&OPEN-QUERY-br-fardos}
     END.
     ASSIGN fi-nr-fardo:SCREEN-VALUE = ''.
  END.
  APPLY 'entry' TO fi-nr-fardo.
  return 'ADM-ERROR':U.                                                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-baixa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-baixa w-digita
ON LEAVE OF fi-dt-baixa IN FRAME F-Main /* Data da Baixa */
DO:
  IF fi-dt-baixa:INPUT-VALUE IN FRAME {&FRAME-NAME} > TODAY THEN DO.
     MESSAGE "Data da Baixa " + fi-dt-baixa:INPUT-VALUE IN FRAME {&FRAME-NAME} +
             " n∆o pode ser maior que data Atual " + STRING(TODAY, "99/99/9999") 
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' TO fi-dt-baixa IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
  END.

  IF fi-dt-baixa:INPUT-VALUE IN FRAME {&FRAME-NAME} <  TODAY - 3 THEN DO.
     MESSAGE "Data da Baixa " + fi-dt-baixa:INPUT-VALUE IN FRAME {&FRAME-NAME} +
             " muito atrasada  " SKIP
             "Confirma a data mesmo assim ? "
             VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE l-opc.
     IF l-opc = NO THEN DO:
        APPLY 'entry' TO fi-dt-baixa IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-fardo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-fardo w-digita
ON RETURN OF fi-nr-fardo IN FRAME F-Main /* Nß Fardo */
DO:
  APPLY 'choose' TO bt-processa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */
/* Include custom  Main Block code for SmartWindows. */

{src/adm/template/windowmn.i}


ASSIGN fi-dt-baixa:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(TODAY,"99/99/9999").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-digita  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-digita  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-digita  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
  THEN DELETE WIDGET w-digita.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-digita  _DEFAULT-ENABLE
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
  DISPLAY fi-dt-baixa fi-nr-mistura fi-nr-fardo fi-qt-fardo fi-peso-fardo 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE fi-dt-baixa fi-nr-mistura fi-nr-fardo bt-processa br-fardos bt-ajuda 
         bt-ok bt-cancelar bt-elimina RECT-1 RECT-44 
      WITH FRAME F-Main IN WINDOW w-digita.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-digita.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE envia-email w-digita 
PROCEDURE envia-email :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR c-mensagem AS CHAR.

ASSIGN c-mensagem = "Favor Transferir para o Deposito PRE " + fi-qt-fardo:SCREEN-VALUE IN FRAME {&FRAME-NAME} +
                    " Fardos com o peso de " +
                      fi-peso-fardo:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " Kg no EMS" + CHR(13) +
                    "Com a data de " +  fi-dt-baixa:SCREEN-VALUE IN FRAME {&FRAME-NAME} + CHR(13) + CHR(13) +
                    "Obrigada, " + CHR(13) +
                    "Descarga de Algod∆o" + CHR(13) +
                    "TEAR TEXTIL INDUSTRIA COMERCIO LTDA".

RUN esapi/esapi002.p (INPUT "teartextil@teartextil.com.br", /* e-mail remetente*/
                      INPUT "qualidade@teartextil.com.br", /* e-mail destino */ 
                      INPUT "Baixa de Fardos de Algod∆o", /* Assunto */
                      INPUT c-mensagem, /* Mensagem */
                      INPUT "",  /*arquivo anexo*/
                      INPUT YES). /* Mostra Erros */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-digita 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-digita 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-digita 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  {utp/ut9000.i "ESSP0133" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

 /* APPLY 'entry' TO fi-nr-fardo IN FRAME {&FRAME-NAME}. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-digita  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-mp-fardo"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-digita 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

