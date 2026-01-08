&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-window 
/*:T *******************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i XX9999 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE JanelaDetalhe
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 fi-nr-ob fi-cod-estabel ~
fi-num-etiqueta fi-localiz fi-qtde fi-lote fi-corte bt-ok bt-salva bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-nr-ob fi-cod-estabel fi-num-etiqueta ~
fi-localiz fi-qtde fi-lote fi-corte 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 fi-nr-ob 
&Scoped-define List-2 fi-nr-ob 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-window AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "Sair" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-salva AUTO-GO 
     LABEL "Salvar" 
     SIZE 10 BY 1
     FONT 0.

DEFINE VARIABLE fi-cod-estabel AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-corte AS CHARACTER FORMAT "X":U 
     LABEL "Corte Comerc" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-localiz AS CHARACTER FORMAT "X(256)":U 
     LABEL "Localizaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-lote AS CHARACTER FORMAT "XX":U 
     LABEL "Lote" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-ob AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     LABEL "N£mero da OB" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE fi-num-etiqueta AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Etiqueta" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-qtde AS DECIMAL FORMAT ">>9.99":U INITIAL 0 
     LABEL "Quantidade (m)" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 77.86 BY 1.38
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi-nr-ob AT ROW 1.25 COL 25 COLON-ALIGNED
     fi-cod-estabel AT ROW 2.25 COL 25 COLON-ALIGNED WIDGET-ID 2
     fi-num-etiqueta AT ROW 3.25 COL 25 COLON-ALIGNED
     fi-localiz AT ROW 4.42 COL 25 COLON-ALIGNED
     fi-qtde AT ROW 5.42 COL 25 COLON-ALIGNED
     fi-lote AT ROW 6.42 COL 25 COLON-ALIGNED
     fi-corte AT ROW 7.42 COL 25 COLON-ALIGNED
     bt-ok AT ROW 8.92 COL 3
     bt-salva AT ROW 8.92 COL 13.72
     bt-ajuda AT ROW 8.92 COL 69
     RECT-1 AT ROW 8.71 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 9.38
         FONT 1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: JanelaDetalhe
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-window ASSIGN
         HIDDEN             = YES
         TITLE              = "Cria Etiquetas"
         HEIGHT             = 9.21
         WIDTH              = 80
         MAX-HEIGHT         = 28.92
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.92
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-window 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-window.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fi-nr-ob IN FRAME F-Main
   1 2                                                                  */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
THEN w-window:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON END-ERROR OF w-window /* Cria Etiquetas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-window w-window
ON WINDOW-CLOSE OF w-window /* Cria Etiquetas */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ajuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ajuda w-window
ON CHOOSE OF bt-ajuda IN FRAME F-Main /* Ajuda */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  {include/ajuda.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-window
ON CHOOSE OF bt-ok IN FRAME F-Main /* Sair */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-salva
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-salva w-window
ON CHOOSE OF bt-salva IN FRAME F-Main /* Salvar */
DO:
   FIND LAST ordem-benefic WHERE
             ordem-benefic.nr-ob = INPUT FRAME {&FRAME-NAME} fi-nr-ob
             NO-ERROR.
   IF NOT AVAIL ordem-benefic THEN DO.
      MESSAGE 'ob n∆o cadastrada...'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO fi-nr-ob.
      RETURN NO-APPLY.
   END.

  FIND ob-etiqueta WHERE
       ob-etiqueta.cod-estabel  = INPUT FRAME {&FRAME-NAME} fi-cod-estabel AND
       ob-etiqueta.num-etiqueta = INPUT FRAME {&FRAME-NAME} fi-num-etiqueta
       NO-LOCK NO-ERROR.
  IF AVAIL ob-etiqueta THEN DO.
     MESSAGE 'Etiqueta j† Cadastrada...'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO fi-num-etiqueta.
     RETURN NO-APPLY.
  END.

  FIND corte-comerc WHERE
       corte-comerc.codigo =  INPUT FRAME {&FRAME-NAME} fi-corte
       NO-LOCK NO-ERROR.
  IF NOT AVAIL corte-comerc THEN DO.
     MESSAGE 'Corte Comercial n∆o Cadastrado...'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO fi-corte.
     RETURN NO-APPLY.
  END.

  RUN pi-grava-etiq.
  /*RUN pi-grava-invent.*/

  ASSIGN fi-nr-ob:SCREEN-VALUE = ''
         fi-num-etiqueta:SCREEN-VALUE = ''
         fi-localiz:SCREEN-VALUE = ''
         fi-qtde:SCREEN-VALUE = ''
         fi-lote:SCREEN-VALUE = ''
         fi-corte:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-window 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-window  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-window  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-window  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-window)
  THEN DELETE WIDGET w-window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-window  _DEFAULT-ENABLE
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
  DISPLAY fi-nr-ob fi-cod-estabel fi-num-etiqueta fi-localiz fi-qtde fi-lote 
          fi-corte 
      WITH FRAME F-Main IN WINDOW w-window.
  ENABLE RECT-1 fi-nr-ob fi-cod-estabel fi-num-etiqueta fi-localiz fi-qtde 
         fi-lote fi-corte bt-ok bt-salva bt-ajuda 
      WITH FRAME F-Main IN WINDOW w-window.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW w-window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-window 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-window 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {include/win-size.i}
  
  {utp/ut9000.i "XX9999" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-etiq w-window 
PROCEDURE pi-grava-etiq :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR i-nr-seq LIKE ob-etiqueta.nr-sequencia.

    FIND LAST ob-etiqueta WHERE
              ob-etiqueta.nr-ob = ordem-benefic.nr-ob AND
              ob-etiqueta.nr-carro = ordem-benefic.nr-carro USE-INDEX indice2
              NO-LOCK NO-ERROR.
    
    IF AVAIL ob-etiqueta THEN
       ASSIGN i-nr-seq = ob-etiqueta.nr-sequencia.
    ELSE
       ASSIGN i-nr-seq = (ordem-benefic.ind-ob * ob-param.max-etq-carro) - ob-param.max-etq-carro.

    ASSIGN i-nr-seq = i-nr-seq + 1.

    CREATE ob-etiqueta.
    ASSIGN ordem-benefic.qtd-planejada = ordem-benefic.qtd-planejada + 1
           ob-etiqueta.nr-ob           = ordem-benefic.nr-ob
           ob-etiqueta.dt-ob           = ordem-benefic.dt-ob
           ob-etiqueta.dt-emissao      = ordem-benefic.dt-ob
           ob-etiqueta.hr-emissao      = STRING(TIME,"HH:MM")
           ob-etiqueta.nr-carro        = ordem-benefic.nr-carro
           ob-etiqueta.tipo-ordem      = ordem-benefic.tipo-ordem
           ob-etiqueta.it-codigo       = ordem-benefic.it-codigo
           ob-etiqueta.cod-refer       = ordem-benefic.cod-refer
           ob-etiqueta.cod-qualid      = 'D'
           ob-etiqueta.situacao        = 3
           ob-etiqueta.acondic         = corte-comerc.descricao
           ob-etiqueta.cod-estabel     = ordem-benefic.cod-estabel
           ob-etiqueta.num-etiqueta    = INPUT FRAME {&FRAME-NAME} fi-num-etiqueta
           ob-etiqueta.nr-sequencia    = i-nr-seq
           ob-etiqueta.corte-comerc    = INPUT FRAME {&FRAME-NAME} fi-corte
           ob-etiqueta.nr-lote         = INPUT FRAME {&FRAME-NAME} fi-lote
           ob-etiqueta.quantidade      = INPUT FRAME {&FRAME-NAME} fi-qtde 
           ob-etiqueta.localiz         = INPUT FRAME {&FRAME-NAME} fi-localiz.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-grava-invent w-window 
PROCEDURE pi-grava-invent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR i-nr-seq LIKE inv-acab.seq.
    DEF VAR c-localiz LIKE ob-etiqueta.localizacao.
    DEF VAR c-etiqueta LIKE ob-etiqueta.num-etiqueta.
    
    FIND LAST inv-acab WHERE
              inv-acab.data-invent = 01.05.2009  AND
              inv-acab.docto = 1459801
              USE-INDEX indice1 NO-LOCK NO-ERROR.
    
    ASSIGN i-nr-seq = IF AVAIL inv-acab
                      THEN inv-acab.seq + 1
                      ELSE 0.
    
    ASSIGN i-nr-seq = i-nr-seq + 1.
    
    CREATE inv-acab.
    ASSIGN inv-acab.it-codigo    = ob-etiqueta.it-codigo
           inv-acab.cod-refer    = ob-etiqueta.cod-refer
           inv-acab.data-invent  = 01.05.2009
           inv-acab.docto        = 1459801
           inv-acab.seq          = i-nr-seq
           inv-acab.qtd-inv      = ob-etiqueta.quantidade
           inv-acab.lote         = ob-etiqueta.nr-lote + ob-etiqueta.cod-refer
           inv-acab.num-etiqueta = ob-etiqueta.num-etiqueta.
           inv-acab.situacao     = 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-window  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this JanelaDetalhe, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-window 
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

