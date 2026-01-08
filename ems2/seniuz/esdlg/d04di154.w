&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i D04DI154 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE NEW GLOBAL SHARED VAR fi-it-codigo      LIKE ped-item-ext.it-codigo.
DEFINE NEW GLOBAL SHARED VAR fi-ini-cod-refer  LIKE ped-item-ext.cod-refer.
DEFINE NEW GLOBAL SHARED VAR fi-fin-cod-refer  LIKE ped-item-ext.cod-refer.
DEFINE NEW GLOBAL SHARED VAR fi-ini-dt-entrega LIKE ped-item.dt-entrega.
DEFINE NEW GLOBAL SHARED VAR fi-fin-dt-entrega LIKE ped-item.dt-entrega.
DEFINE NEW GLOBAL SHARED VAR fi-ini-dt-implant LIKE ped-venda.dt-implant.
DEFINE NEW GLOBAL SHARED VAR fi-fin-dt-implant LIKE ped-venda.dt-implant.
DEFINE NEW GLOBAL SHARED VAR fi-ini-acond      LIKE ped-item-ext.acondicionamento.
DEFINE NEW GLOBAL SHARED VAR fi-fin-acond      LIKE ped-item-ext.acondicionamento.
DEFINE NEW GLOBAL SHARED VAR fi-serie-entr     LIKE movto-estoq.serie.
DEFINE NEW GLOBAL SHARED VAR fi-dias-res       AS INT.
DEFINE NEW GLOBAL SHARED VAR fi-perc-var-res   AS INT.
DEFINE NEW GLOBAL SHARED VAR to-abe            AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-atp            AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-att            AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-pen            AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-sus            AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-can            AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-bal            AS LOG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi-it-codigo1 fi-ini-cod-refer1 ~
fi-fin-cod-refer1 fi-ini-dt-entrega1 fi-fin-dt-entrega1 fi-ini-dt-implant1 ~
fi-fin-dt-implant1 fi-ini-acond1 fi-fin-acond1 to-abe1 to-sus1 ~
fi-serie-entr1 to-atp1 to-can1 fi-dias-res1 to-att1 to-bal1 ~
fi-perc-var-res1 to-pen1 bt-ok bt-cancela bt-ajuda IMAGE-10 IMAGE-37 ~
IMAGE-38 IMAGE-39 IMAGE-40 IMAGE-7 IMAGE-8 IMAGE-9 RECT-1 RECT-28 rt-buttom 
&Scoped-Define DISPLAYED-OBJECTS fi-it-codigo1 fi-desc-item ~
fi-ini-cod-refer1 fi-fin-cod-refer1 fi-ini-dt-entrega1 fi-fin-dt-entrega1 ~
fi-ini-dt-implant1 fi-fin-dt-implant1 fi-ini-acond1 fi-fin-acond1 to-abe1 ~
to-sus1 fi-serie-entr1 to-atp1 to-can1 fi-dias-res1 to-att1 to-bal1 ~
fi-perc-var-res1 to-pen1 

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

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "&OK" 
     SIZE 10 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "x(60)" 
     VIEW-AS FILL-IN 
     SIZE 36.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dias-res1 AS INTEGER FORMAT ">9":U INITIAL 5 
     LABEL "Nß Dias Reserva" 
     VIEW-AS FILL-IN 
     SIZE 4.43 BY .88 TOOLTIP "Nß de dias a considerar para busca das Reservas." NO-UNDO.

DEFINE VARIABLE fi-fin-acond1 AS CHARACTER FORMAT "X(8)":U INITIAL "ZZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-fin-cod-refer1 AS CHARACTER FORMAT "x(8)" INITIAL "ZZZZZZZ" 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .88 TOOLTIP "Referància final" NO-UNDO.

DEFINE VARIABLE fi-fin-dt-entrega1 AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de entrega final" NO-UNDO.

DEFINE VARIABLE fi-fin-dt-implant1 AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de implantaá∆o final" NO-UNDO.

DEFINE VARIABLE fi-ini-acond1 AS CHARACTER FORMAT "X(8)":U 
     LABEL "Acondicionamento" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-ini-cod-refer1 AS CHARACTER FORMAT "x(8)" 
     LABEL "Referància":R12 
     VIEW-AS FILL-IN 
     SIZE 9.57 BY .88 TOOLTIP "Referància" NO-UNDO.

DEFINE VARIABLE fi-ini-dt-entrega1 AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data Entrega" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de entrega inicial" NO-UNDO.

DEFINE VARIABLE fi-ini-dt-implant1 AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 
     LABEL "Data Implantaá∆o" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 TOOLTIP "Data de implantaá∆o inicial" NO-UNDO.

DEFINE VARIABLE fi-it-codigo1 AS CHARACTER FORMAT "x(16)" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .88 TOOLTIP "Item" NO-UNDO.

DEFINE VARIABLE fi-perc-var-res1 AS INTEGER FORMAT ">9":U INITIAL 10 
     LABEL "% Variaá∆o Reserva" 
     VIEW-AS FILL-IN 
     SIZE 4.43 BY .88 TOOLTIP "% de variaá∆o toler†vel entre Quantidade Pedida x Reservada." NO-UNDO.

DEFINE VARIABLE fi-serie-entr1 AS CHARACTER FORMAT "X(3)":U INITIAL "leo" 
     LABEL "SÇrie Entrada" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .88 TOOLTIP "SÇrie padr∆o para busca da £ltima entrada no estoque." NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-37
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-38
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-39
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-40
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-7
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-8
     FILENAME "image\im-las":U
     SIZE 3 BY .88.

DEFINE IMAGE IMAGE-9
     FILENAME "image\im-fir":U
     SIZE 3 BY .88.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 37.72 BY 5 TOOLTIP "Seleá∆o das Situaá‰es de ÷tens de Pedido a serem mostradas.".

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 27 BY 5.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 68 BY 1.42
     BGCOLOR 7 .

DEFINE VARIABLE to-abe1 AS LOGICAL INITIAL no 
     LABEL "Aberto" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .88 NO-UNDO.

DEFINE VARIABLE to-atp1 AS LOGICAL INITIAL no 
     LABEL "Atendido Parcial" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE to-att1 AS LOGICAL INITIAL no 
     LABEL "Atendido Total" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE to-bal1 AS LOGICAL INITIAL no 
     LABEL "Faturamento Balc∆o" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.86 BY .88 NO-UNDO.

DEFINE VARIABLE to-can1 AS LOGICAL INITIAL no 
     LABEL "Cancelado" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.86 BY .88 NO-UNDO.

DEFINE VARIABLE to-pen1 AS LOGICAL INITIAL no 
     LABEL "Pendente" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE to-sus1 AS LOGICAL INITIAL no 
     LABEL "Suspenso" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .88 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-it-codigo1 AT ROW 1.58 COL 14 COLON-ALIGNED HELP
          "Codigo do ÷tem no EMS"
     fi-desc-item AT ROW 1.58 COL 31.29 COLON-ALIGNED NO-LABEL
     fi-ini-cod-refer1 AT ROW 2.58 COL 14 COLON-ALIGNED HELP
          "C¢digo de referància do item"
     fi-fin-cod-refer1 AT ROW 2.58 COL 42.72 COLON-ALIGNED HELP
          "C¢digo de referància do item" NO-LABEL
     fi-ini-dt-entrega1 AT ROW 3.58 COL 14 COLON-ALIGNED HELP
          "Data de Entrega inicial"
     fi-fin-dt-entrega1 AT ROW 3.58 COL 42.72 COLON-ALIGNED HELP
          "Data de Entrega final" NO-LABEL
     fi-ini-dt-implant1 AT ROW 4.58 COL 14 COLON-ALIGNED HELP
          "Data de Entrega inicial"
     fi-fin-dt-implant1 AT ROW 4.58 COL 42.86 COLON-ALIGNED HELP
          "Data de Entrega final" NO-LABEL
     fi-ini-acond1 AT ROW 5.58 COL 14 COLON-ALIGNED
     fi-fin-acond1 AT ROW 5.58 COL 42.86 COLON-ALIGNED NO-LABEL
     to-abe1 AT ROW 7.54 COL 5.14
     to-sus1 AT ROW 7.54 COL 22.43
     fi-serie-entr1 AT ROW 8.13 COL 55.57 COLON-ALIGNED
     to-atp1 AT ROW 8.54 COL 5.14
     to-can1 AT ROW 8.54 COL 22.43
     fi-dias-res1 AT ROW 9.13 COL 55.57 COLON-ALIGNED
     to-att1 AT ROW 9.54 COL 5.14
     to-bal1 AT ROW 9.54 COL 22.43
     fi-perc-var-res1 AT ROW 10.13 COL 55.57 COLON-ALIGNED
     to-pen1 AT ROW 10.54 COL 5.14
     bt-ok AT ROW 12.54 COL 3
     bt-cancela AT ROW 12.54 COL 14
     bt-ajuda AT ROW 12.54 COL 59
     IMAGE-10 AT ROW 4.58 COL 41.57
     IMAGE-37 AT ROW 5.58 COL 41.57
     IMAGE-38 AT ROW 5.58 COL 33.43
     IMAGE-39 AT ROW 2.58 COL 33.43
     IMAGE-40 AT ROW 2.58 COL 41.57
     IMAGE-7 AT ROW 3.58 COL 33.43
     IMAGE-8 AT ROW 3.58 COL 41.57
     IMAGE-9 AT ROW 4.58 COL 33.43
     RECT-1 AT ROW 7.04 COL 3
     RECT-28 AT ROW 7.04 COL 41.86
     rt-buttom AT ROW 12.29 COL 2
     "Situaá∆o dos ÷tens de Pedido" VIEW-AS TEXT
          SIZE 20.57 BY .54 AT ROW 6.75 COL 11.57
     "ParÉmetros" VIEW-AS TEXT
          SIZE 8.29 BY .54 AT ROW 6.75 COL 50.86
     SPACE(11.70) SKIP(6.49)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Seleá∆o de ÷tens de Pedidos"
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
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Seleá∆o de ÷tens de Pedidos */
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


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok D-Dialog
ON CHOOSE OF bt-ok IN FRAME D-Dialog /* OK */
DO:
  ASSIGN fi-it-codigo      = INPUT FRAME {&FRAME-NAME} fi-it-codigo1
         fi-ini-cod-refer  = INPUT FRAME {&FRAME-NAME} fi-ini-cod-refer1  
         fi-fin-cod-refer  = INPUT FRAME {&FRAME-NAME} fi-fin-cod-refer1  
         fi-ini-dt-entrega = INPUT FRAME {&FRAME-NAME} fi-ini-dt-entrega1
         fi-fin-dt-entrega = INPUT FRAME {&FRAME-NAME} fi-fin-dt-entrega1
         fi-ini-dt-implant = INPUT FRAME {&FRAME-NAME} fi-ini-dt-implant1
         fi-fin-dt-implant = INPUT FRAME {&FRAME-NAME} fi-fin-dt-implant1
         fi-ini-acond      = INPUT FRAME {&FRAME-NAME} fi-ini-acond1
         fi-fin-acond      = INPUT FRAME {&FRAME-NAME} fi-fin-acond1
         fi-serie-entr     = INPUT FRAME {&FRAME-NAME} fi-serie-entr1
         fi-dias-res       = INPUT FRAME {&FRAME-NAME} fi-dias-res1
         fi-perc-var-res   = INPUT FRAME {&FRAME-NAME} fi-perc-var-res1
         to-abe            = INPUT FRAME {&FRAME-NAME} to-abe1
         to-atp            = INPUT FRAME {&FRAME-NAME} to-atp1
         to-att            = INPUT FRAME {&FRAME-NAME} to-att1
         to-pen            = INPUT FRAME {&FRAME-NAME} to-pen1
         to-sus            = INPUT FRAME {&FRAME-NAME} to-sus1
         to-can            = INPUT FRAME {&FRAME-NAME} to-can1
         to-bal            = INPUT FRAME {&FRAME-NAME} to-bal1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-fin-cod-refer1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-fin-cod-refer1 D-Dialog
ON LEAVE OF fi-fin-cod-refer1 IN FRAME D-Dialog /* Referància */
DO:
  IF (SUBSTR(INPUT FRAME {&FRAME-NAME} fi-ini-cod-refer1,7,1) =  "0" AND
      SUBSTR(INPUT FRAME {&FRAME-NAME} fi-fin-cod-refer1,7,1) <> "0") OR
     (SUBSTR(INPUT FRAME {&FRAME-NAME} fi-ini-cod-refer1,7,1) <> "0" AND
      SUBSTR(INPUT FRAME {&FRAME-NAME} fi-fin-cod-refer1,7,1) =  "0") THEN DO:
        MESSAGE "N∆o Ç poss°vel misturar Referàncias Lisas e Estampadas." VIEW-AS ALERT-BOX.
        APPLY 'entry' TO fi-ini-cod-refer1 IN FRAME {&FRAME-NAME}.
        RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-acond1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-acond1 D-Dialog
ON LEAVE OF fi-ini-acond1 IN FRAME D-Dialog /* Acondicionamento */
DO:
  ASSIGN fi-fin-acond1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = INPUT FRAME {&FRAME-NAME}
                                                             fi-ini-acond1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-ini-cod-refer1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-ini-cod-refer1 D-Dialog
ON LEAVE OF fi-ini-cod-refer1 IN FRAME D-Dialog /* Referància */
DO:
  ASSIGN fi-fin-cod-refer1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = INPUT FRAME {&FRAME-NAME}
                                                             fi-ini-cod-refer1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-it-codigo1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-it-codigo1 D-Dialog
ON LEAVE OF fi-it-codigo1 IN FRAME D-Dialog /* Item */
DO:
  FIND ITEM WHERE ITEM.it-codigo = INPUT FRAME {&frame-name} fi-it-codigo1 NO-LOCK NO-ERROR.
  IF AVAIL ITEM THEN
     ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ITEM.desc-item.
  ELSE DO:
     ASSIGN fi-desc-item:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
     MESSAGE "Item n∆o cadastrado." VIEW-AS ALERT-BOX.
     APPLY 'entry' TO fi-it-codigo1 IN FRAME {&FRAME-NAME}.
     RETURN NO-APPLY.
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
  DISPLAY fi-it-codigo1 fi-desc-item fi-ini-cod-refer1 fi-fin-cod-refer1 
          fi-ini-dt-entrega1 fi-fin-dt-entrega1 fi-ini-dt-implant1 
          fi-fin-dt-implant1 fi-ini-acond1 fi-fin-acond1 to-abe1 to-sus1 
          fi-serie-entr1 to-atp1 to-can1 fi-dias-res1 to-att1 to-bal1 
          fi-perc-var-res1 to-pen1 
      WITH FRAME D-Dialog.
  ENABLE fi-it-codigo1 fi-ini-cod-refer1 fi-fin-cod-refer1 fi-ini-dt-entrega1 
         fi-fin-dt-entrega1 fi-ini-dt-implant1 fi-fin-dt-implant1 fi-ini-acond1 
         fi-fin-acond1 to-abe1 to-sus1 fi-serie-entr1 to-atp1 to-can1 
         fi-dias-res1 to-att1 to-bal1 fi-perc-var-res1 to-pen1 bt-ok bt-cancela 
         bt-ajuda IMAGE-10 IMAGE-37 IMAGE-38 IMAGE-39 IMAGE-40 IMAGE-7 IMAGE-8 
         IMAGE-9 RECT-1 RECT-28 rt-buttom 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
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

  {utp/ut9000.i "D04DI154" "2.04.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN fi-it-codigo1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi-it-codigo
         fi-ini-cod-refer1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi-ini-cod-refer
         fi-fin-cod-refer1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi-fin-cod-refer
         fi-ini-dt-entrega1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(fi-ini-dt-entrega)
         fi-fin-dt-entrega1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(fi-fin-dt-entrega)
         fi-ini-dt-implant1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(fi-ini-dt-implant)
         fi-fin-dt-implant1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(fi-fin-dt-implant)
         fi-ini-acond1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi-ini-acond
         fi-fin-acond1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi-fin-acond
         fi-serie-entr1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = fi-serie-entr
         fi-dias-res1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(fi-dias-res)
         fi-perc-var-res1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(fi-perc-var-res)
         to-abe1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(to-abe)
         to-atp1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(to-atp)
         to-att1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(to-att)
         to-pen1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(to-pen)
         to-sus1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(to-sus)
         to-can1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(to-can)
         to-bal1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = string(to-bal).
                                                                    
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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

