&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
{include/i-prgvrs.i ESSP0153C 2.04.00.000}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER p-nr-pedcli LIKE ped-venda.nr-pedcli.
DEFINE OUTPUT PARAMETER c-pedidos AS CHAR.


/* Temp Tables  Definitions ---         */
DEF TEMP-TABLE tt-ped-venda LIKE ped-venda
     FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-ped-item LIKE ped-item 
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE wt-ped-item NO-UNDO LIKE ped-item 
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-ped-repre LIKE ped-repre
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE wt-ped-repre NO-UNDO LIKE ped-repre
    FIELD r-rowid AS ROWID.

DEF TEMP-TABLE tt-cond-ped NO-UNDO LIKE cond-ped
    FIELD r-rowid AS ROWID.
DEF TEMP-TABLE wt-cond-ped NO-UNDO LIKE cond-ped
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF TEMP-TABLE tt-ped-item-ext LIKE ped-item-ext.
DEF TEMP-TABLE tt-ped-venda-ext LIKE ped-venda-ext.

/* Local Variable Definitions --- */
DEF VAR h-acomp      AS HANDLE NO-UNDO.
DEF VAR h-bodi018    AS HANDLE.
DEF VAR h-bodi157    AS HANDLE.

DEF VAR i-cont        AS INT.
DEF VAR de-qtd-div    AS DEC.
DEF VAR i-sit-ped-ori LIKE ped-venda.cod-sit-ped.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-5 rt-buttom fi-div-ped bt-ok bt-cancela ~
bt-ajuda 
&Scoped-Define DISPLAYED-OBJECTS fi-nr-pedcli fi-tp-pedido fi-dt-implant ~
fi-dt-entrega fi-cliente fi-nome-cli fi-cod-rep fi-nome-rep fi-div-ped 

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

DEFINE VARIABLE fi-cliente AS CHARACTER FORMAT "X(12)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-rep AS CHARACTER FORMAT "X(12)":U 
     LABEL "Representante" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE fi-div-ped AS INTEGER FORMAT ">>":U INITIAL 0 
     LABEL "Fator Divis∆o Pedido" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .88 TOOLTIP "N£mero que indica os desdobramentos do pedido" NO-UNDO.

DEFINE VARIABLE fi-dt-entrega AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Entrega" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-implant AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Implantacao" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-cli AS CHARACTER FORMAT "X(36)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nome-rep AS CHARACTER FORMAT "X(36)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-pedcli AS CHARACTER FORMAT "X(12)":U 
     LABEL "Nß Pedido" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88
     FONT 11 NO-UNDO.

DEFINE VARIABLE fi-tp-pedido AS CHARACTER FORMAT "X(15)":U 
     LABEL "Tipo do Pedido" 
     VIEW-AS FILL-IN 
     SIZE 15.86 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 70 BY 4.63.

DEFINE RECTANGLE rt-buttom
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 70.29 BY 1.42
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     fi-nr-pedcli AT ROW 1.5 COL 17 COLON-ALIGNED
     fi-tp-pedido AT ROW 1.5 COL 47 COLON-ALIGNED
     fi-dt-implant AT ROW 2.5 COL 17 COLON-ALIGNED
     fi-dt-entrega AT ROW 2.5 COL 47 COLON-ALIGNED
     fi-cliente AT ROW 3.5 COL 17 COLON-ALIGNED
     fi-nome-cli AT ROW 3.5 COL 30.43 COLON-ALIGNED NO-LABEL
     fi-cod-rep AT ROW 4.5 COL 17 COLON-ALIGNED
     fi-nome-rep AT ROW 4.5 COL 30.43 COLON-ALIGNED NO-LABEL
     fi-div-ped AT ROW 6.5 COL 17 COLON-ALIGNED
     bt-ok AT ROW 8.33 COL 2.14
     bt-cancela AT ROW 8.33 COL 12.72
     bt-ajuda AT ROW 8.33 COL 60.57
     RECT-5 AT ROW 1.13 COL 1
     rt-buttom AT ROW 8.08 COL 1
     SPACE(0.00) SKIP(0.12)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 1
         TITLE "Divis∆o de Pedido"
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
   FRAME-NAME L-To-R                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-cliente IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cod-rep IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-entrega IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-implant IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-cli IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-rep IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-pedcli IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tp-pedido IN FRAME D-Dialog
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Divis∆o de Pedido */
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
  ASSIGN INPUT FRAME {&FRAME-NAME} fi-div-ped.

  /* Verificando Divis∆o multipla do CORTE COMERCIAL */
  FOR EACH ped-item OF ped-venda WHERE
           INDEX("125",STRING(ped-item.cod-sit-item)) <> 0  NO-LOCK.
      FIND ped-item-ext OF ped-item NO-LOCK NO-ERROR.
      IF AVAIL ped-item-ext THEN DO:
         FIND corte-comerc WHERE
              corte-comerc.codigo = ped-item-ext.corte-comerc NO-LOCK NO-ERROR.
         ASSIGN de-qtd-div = TRUNCATE(ped-item.qt-pedida / fi-div-ped,0).
         IF de-qtd-div < corte-comerc.compr-min THEN DO:
             MESSAGE  "                      A T E N Ä « O !!!! " SKIP(1)
                      "Item: "        ped-item.it-codigo           
                      " Referencia: " ped-item.cod-refer           
                      " Sequencia: "  ped-item.nr-sequencia SKIP 
                      "Acondicionamento " corte-comerc.descricao SKIP
                      " Qtde Pedido: " ped-item.qt-pedida 
                      "     Calculado: " de-qtd-div  SKIP(1)
                      "O Valor calculado Ç menor que o min°mo do corte " SKIP
                      "comercial " corte-comerc.compr-min
                      " Metros"
                 VIEW-AS ALERT-BOX ERROR BUTTONS OK.
             APPLY 'entry' TO fi-div-ped.
             RETURN NO-APPLY.
         END.
         IF de-qtd-div MODULO corte-comerc.compr-med <> 0 THEN DO:
            MESSAGE  "                      A T E N Ä « O !!!! " SKIP(1)
                     "Item: "        ped-item.it-codigo           
                     " Referencia: " ped-item.cod-refer           
                     " Sequencia: "  ped-item.nr-sequencia SKIP   
                     " Qtde Pedido: " ped-item.qt-pedida 
                     "       Calculado: " de-qtd-div SKIP(1)
                     " O Valor calculado n∆o Ç multiplo do corte comercial " SKIP
                     corte-comerc.descricao
                 VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY 'entry' TO fi-div-ped.
            RETURN NO-APPLY.
         END.
      END.
      ELSE DO:
         MESSAGE "Item: "        ped-item.it-codigo
                 " Referencia: " ped-item.cod-refer
                 " Sequencia: "  ped-item.nr-sequencia SKIP
                 "N∆o possui informaá‰es Complementares na tabela 'ped-item-ext'"
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'entry' TO fi-div-ped.
         RETURN NO-APPLY.
      END.
  END.

  ASSIGN INPUT FRAME {&FRAME-NAME} fi-div-ped.

  IF NOT VALID-HANDLE(h-bodi018) OR  /* Tabela:COND-PED Condiá∆o de Pagamento */ 
     h-bodi018:TYPE      <> "PROCEDURE":U OR
     h-bodi018:FILE-NAME <> "dibo/bodi018.p":U THEN
     RUN dibo/bodi018.p PERSISTENT SET h-bodi018.

  IF NOT VALID-HANDLE(h-bodi157) OR /* Tabela: PED-REPRES Representantes */
     h-bodi157:TYPE      <> "PROCEDURE":U OR
     h-bodi157:FILE-NAME <> "dibo/bodi157.p":U THEN
     RUN dibo/bodi157.p PERSISTENT SET h-bodi157.

  RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
  {utp/ut-liter.i Gerando_Pedidos *}
  RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

  RUN pi-acompanhar IN h-acomp (INPUT "Criando Areas de Trabalho...").

  CREATE tt-ped-venda.
  BUFFER-COPY ped-venda TO tt-ped-venda.

  FOR EACH ped-item OF ped-venda WHERE
           INDEX("125", STRING(ped-item.cod-sit-item)) <> 0  NO-LOCK.
      CREATE tt-ped-item.
      BUFFER-COPY ped-item TO tt-ped-item
          ASSIGN tt-ped-item.qt-pedida = TRUNCATE(ped-item.qt-pedida / fi-div-ped,0).

      FIND ped-item-ext OF ped-item NO-LOCK NO-ERROR.
      IF AVAIL ped-item-ext THEN DO.
         CREATE tt-ped-item-ext.
         BUFFER-COPY ped-item-ext TO tt-ped-item-ext.
      END.
  END.

  FOR EACH cond-ped OF ped-venda NO-LOCK.
      CREATE tt-cond-ped.
      BUFFER-COPY cond-ped TO tt-cond-ped.
  END.

  FOR EACH ped-repre OF ped-venda NO-LOCK.
      CREATE tt-ped-repre.
      BUFFER-COPY ped-repre TO tt-ped-repre.
  END.

  FIND ped-venda-ext WHERE
       ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
       ped-venda-ext.nr-pedido = ped-venda.nr-pedido NO-LOCK NO-ERROR.
  IF AVAIL ped-venda-ext THEN DO.
     CREATE tt-ped-venda-ext.
     BUFFER-COPY ped-venda-ext TO tt-ped-venda-ext.
  END.

  DO i-cont = 1 TO fi-div-ped - 1.
     RUN pi-ped-venda.
  END.

  RUN pi-acompanhar IN h-acomp (INPUT "Acertando o Pedido Original...").

  RUN pi-acerta-pedori.  

  RUN pi-finalizar in h-acomp.

  MESSAGE "Foram gerados os pedidos " c-pedidos
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  IF VALID-HANDLE(h-bodi018) THEN
     DELETE PROCEDURE h-bodi018.

  IF VALID-HANDLE(h-bodi157) THEN
     DELETE PROCEDURE h-bodi157.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-div-ped
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-div-ped D-Dialog
ON LEAVE OF fi-div-ped IN FRAME D-Dialog /* Fator Divis∆o Pedido */
DO:
  IF SELF:INPUT-VALUE < 2 THEN DO:
     MESSAGE "Valor para Fator de Divis∆o Invalido !!!"
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' TO fi-div-ped.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-div-ped D-Dialog
ON RETURN OF fi-div-ped IN FRAME D-Dialog /* Fator Divis∆o Pedido */
DO:
   APPLY 'TAB':U TO SELF.
   RETURN NO-APPLY.
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
  DISPLAY fi-nr-pedcli fi-tp-pedido fi-dt-implant fi-dt-entrega fi-cliente 
          fi-nome-cli fi-cod-rep fi-nome-rep fi-div-ped 
      WITH FRAME D-Dialog.
  ENABLE RECT-5 rt-buttom fi-div-ped bt-ok bt-cancela bt-ajuda 
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

  {utp/ut9000.i "ESSP0155C" "2.04.00.001"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  FIND ped-venda WHERE
       ped-venda.nr-pedcli = p-nr-pedcli NO-LOCK NO-ERROR.

  IF AVAIL ped-venda THEN DO:
     ASSIGN i-sit-ped-ori = ped-venda.cod-sit-ped.

     ASSIGN fi-nr-pedcli:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = ped-venda.nr-pedcli
            fi-dt-implant:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda.dt-implant)
            fi-dt-entrega:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ped-venda.dt-entrega).

     FIND emitente WHERE
          emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.
     IF AVAIL emitente THEN
        ASSIGN fi-cliente:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = ped-venda.nome-abrev
               fi-nome-cli:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = emitente.nome-emit.

     FIND ped-venda-ext WHERE
          ped-venda-ext.nr-pedcli = ped-venda.nr-pedcli NO-LOCK NO-ERROR.
     IF AVAIL ped-venda-ext THEN
        ASSIGN fi-tp-pedido:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ped-venda-ext.tp-pedido.

     FIND repres WHERE
          repres.nome-abrev = ped-venda.no-ab-reppri NO-LOCK NO-ERROR.
     IF AVAIL repres THEN
        ASSIGN fi-cod-rep:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome-abrev
               fi-nome-rep:SCREEN-VALUE IN FRAME {&FRAME-NAME} = repres.nome.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-acerta-pedori D-Dialog 
PROCEDURE pi-acerta-pedori :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND ped-venda WHERE
         ped-venda.nr-pedcli = p-nr-pedcli NO-LOCK NO-ERROR.

    FOR EACH ped-item OF ped-venda WHERE
             INDEX("125",STRING(ped-item.cod-sit-item)) <> 0 NO-LOCK.
        FIND tt-ped-item WHERE
             tt-ped-item.nr-sequencia = ped-item.nr-sequencia NO-ERROR.

        ASSIGN tt-ped-item.nr-pedcli = ped-item.nr-pedcli
               tt-ped-item.qt-pedida = ped-item.qt-pedida - (tt-ped-item.qt-pedida * (fi-div-ped - 1)).

        RUN esapi/altera-peditem.p (INPUT tt-ped-item.nr-pedcli,
                                    INPUT tt-ped-item.nr-sequencia,
                                    INPUT tt-ped-item.qt-pedida,
                                    INPUT ped-item.vl-preori).
    END.

    RUN esapi/completa-pedvenda.p (INPUT ped-venda.nr-pedido).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-cria-log D-Dialog 
PROCEDURE pi-cria-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER p-nr-pedcli LIKE ped-venda.nr-pedcli.
    DEF INPUT PARAMETER p-nome-abrev LIKE ped-venda.nome-abrev.
    DEF INPUT PARAMETER p-ocorrencia AS CHAR.

    CREATE his-ped-venda-ext.
    ASSIGN his-ped-venda-ext.nr-pedcli  = p-nr-pedcli
           his-ped-venda-ext.nome-abrev = p-nome-abrev
           his-ped-venda-ext.dt-trans   = TODAY
           his-ped-venda-ext.hr-trans   = TIME
           his-ped-venda-ext.usuario    = c-seg-usuario
           his-ped-venda-ext.ocorrencia = p-ocorrencia.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-ped-venda D-Dialog 
PROCEDURE pi-ped-venda :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* Cria o Pedido */
   ASSIGN tt-ped-venda.nr-pedido = NEXT-VALUE(seq-nr-pedido)
          tt-ped-venda.nr-pedcli = STRING(tt-ped-venda.nr-pedido).

   RUN pi-acompanhar IN h-acomp (INPUT "Criando o Pedido de Venda...").

   FIND ped-venda WHERE
        ped-venda.nome-abrev = tt-ped-venda.nome-abrev AND
        ped-venda.nr-pedcli  = tt-ped-venda.nr-pedcli NO-LOCK NO-ERROR.

   IF NOT AVAIL ped-venda THEN DO.
      RUN esapi/cria-pedvenda.p (INPUT TABLE tt-ped-venda).
      IF RETURN-VALUE = 'ADM-ERROR' THEN
         RETURN 'ADM-ERROR'.

      RUN pi-cria-log ( INPUT tt-ped-venda.nr-pedcli,
                        INPUT tt-ped-venda.nome-abrev,
                        INPUT "Implantado por Desmembramento o Pedido: " + tt-ped-venda.nr-pedcli + "   Cliente:" + tt-ped-venda.nome-abrev).
   END.

   /* Cria Itens do Pedido */
   RUN pi-acompanhar IN h-acomp (INPUT "Criando Itens do Pedido de Venda...").

   FOR EACH tt-ped-item.
       ASSIGN tt-ped-item.nr-pedcli = tt-ped-venda.nr-pedcli.
   END.

   FOR EACH tt-ped-item.
       RUN esapi/cria-peditem.p (INPUT tt-ped-item.nr-pedcli,
                                 INPUT tt-ped-item.nr-sequencia,
                                 INPUT tt-ped-item.it-codigo,
                                 INPUT tt-ped-item.cod-refer,
                                 INPUT tt-ped-item.qt-pedida,
                                 INPUT tt-ped-item.vl-preuni).

       IF RETURN-VALUE = 'NOK' THEN NEXT.

       /* Cria Dados Complementares do Item */
       FIND tt-ped-item-ext WHERE
            tt-ped-item-ext.nr-sequencia = tt-ped-item.nr-sequencia
            NO-LOCK NO-ERROR.

       IF AVAIL tt-ped-item-ext THEN DO.
          CREATE ped-item-ext.
          BUFFER-COPY tt-ped-item-ext TO ped-item-ext
              ASSIGN ped-item-ext.nr-pedcli = tt-ped-item.nr-pedcli.
       END.
   END.


   /* Cria Condiá∆o de Pagamento Especial */
   RUN pi-acompanhar IN h-acomp (INPUT "Criando a Condiá∆o Pagamento...").

   RUN openQueryStatic IN h-bodi018 (INPUT "Main":U).
   RUN emptyRowErrors IN h-bodi018.
   FOR EACH tt-cond-ped NO-LOCK.
       EMPTY TEMP-TABLE wt-cond-ped.
       CREATE wt-cond-ped.
       BUFFER-COPY tt-cond-ped TO wt-cond-ped
              ASSIGN wt-cond-ped.nr-pedido = tt-ped-venda.nr-pedido.

       RUN setRecord IN h-bodi018 (INPUT TABLE wt-cond-ped).
       RUN createRecord IN h-bodi018.
   END.
   RUN getRowErrors IN h-bodi018 (OUTPUT TABLE RowErrors).
   IF CAN-FIND(FIRST RowErrors 
             WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
      FOR EACH rowerrors WHERE
               RowErrors.ErrorSubType = "ERROR":U:
          MESSAGE "Erro ao Condiá∆o de Pagamento Especial" SKIP
                  rowerrors.errordescription 
                  VIEW-AS ALERT-BOX.
      END.
      RETURN 'ADM-ERROR'.
   END.


   /* Cria Representantes do Pedido */
   RUN pi-acompanhar IN h-acomp (INPUT "Criando o Representante do Pedido de Venda...").

   RUN openQueryStatic IN h-bodi157 (INPUT "Main":U).
   RUN emptyRowErrors IN h-bodi157.
   FOR EACH tt-ped-repre NO-LOCK.
       EMPTY TEMP-TABLE wt-ped-repre.
       CREATE wt-ped-repre.
       BUFFER-COPY tt-ped-repre TO wt-ped-repre
           ASSIGN wt-ped-repre.nr-pedido = tt-ped-venda.nr-pedido.
           
       RUN setRecord IN h-bodi157 (INPUT TABLE wt-ped-repre).
       RUN createRecord IN h-bodi157.
   END.
   RUN getRowErrors IN h-bodi157 (OUTPUT TABLE RowErrors).
   IF CAN-FIND(FIRST RowErrors 
               WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
      FOR EACH rowerrors WHERE
               RowErrors.ErrorSubType = "ERROR":U:
          MESSAGE "Erro ao Gravar Representantes do Pedido" SKIP
                  rowerrors.errordescription 
                  VIEW-AS ALERT-BOX.
      END.
      RETURN 'ADM-ERROR'.
   END.
   

   /* Completa o Pedido */
   RUN pi-acompanhar IN h-acomp (INPUT "Completando o Pedido de Venda...").

   FIND ped-venda WHERE
        ped-venda.nr-pedcli = tt-ped-venda.nr-pedcli AND
        ped-venda.nome-abrev = tt-ped-venda.nome-abrev
        NO-ERROR.

   RUN esapi/completa-pedvenda.p (INPUT tt-ped-venda.nr-pedido).

   ASSIGN ped-venda.cod-sit-ped = i-sit-ped-ori. /* Novos pedidos recebem a mesma situacao do pedido original */

   FOR EACH ped-ent OF ped-venda.
       ASSIGN ped-ent.tipo-atend = 2.
   END.

   /* Cria Dados Complementares do Pedido */
   RUN pi-acompanhar IN h-acomp (INPUT "Criando os Dados Complementares do Pedido de Venda...").

   FIND FIRST tt-ped-venda-ext NO-ERROR.
   IF AVAIL tt-ped-venda-ext THEN DO.
      ASSIGN tt-ped-venda-ext.nr-pedido = tt-ped-venda.nr-pedido.
      CREATE ped-venda-ext.
      BUFFER-COPY tt-ped-venda-ext TO ped-venda-ext.
   END.

   ASSIGN c-pedidos = IF c-pedidos = ''
                      THEN ped-venda.nr-pedcli
                      ELSE c-pedidos + "," +  ped-venda.nr-pedcli.
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

