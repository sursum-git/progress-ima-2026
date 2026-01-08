&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS w-livre 
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

DEFINE TEMP-TABLE tt
    FIELD origem            AS CHAR FORMAT 'x(100)'
    FIELD cod_empresa       AS CHAR
    FIELD cod_estab         AS CHAR
    FIELD cod_emitente      AS CHAR FORMAT 'x(50)'
    FIELD desc_emitente     AS CHAR FORMAT 'x(200)'
    FIELD data              AS DATE
    /*FIELD cod_classif       AS INT
    FIELD desc_classif      AS CHAR FORMAT 'x(50)'*/
    FIELD conta_contabil    AS CHAR FORMAT 'x(12)'
    FIELD valor             AS DECIMAL  
    FIELD conta_corrente    AS CHAR FORMAT 'x(20)'
    FIELD base              AS CHAR 
    FIELD DESC_conta        AS CHAR FORMAT 'x(150)'
    FIELD tipo              AS CHAR
    FIELD cc                AS CHAR 
    FIELD DESC_cc           AS CHAR FORMAT 'x(50)'.
/*FIELD agencia           AS CHAR FORMAT 'x(10)'
    FIELD banco             AS CHAR FORMAT 'x(10)'
    FIELD tipo_registro     AS CHAR . /*fluxo, conta*/*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE w-livre
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-cad

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rt-button fiDtIni fiDtFim btExecutar 
&Scoped-Define DISPLAYED-OBJECTS fiDtIni fiDtFim 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-livre AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU mi-programa 
       MENU-ITEM mi-consultas   LABEL "Co&nsultas"     ACCELERATOR "CTRL-L"
       MENU-ITEM mi-imprimir    LABEL "&Relat¢rios"    ACCELERATOR "CTRL-P"
       RULE
       MENU-ITEM mi-sair        LABEL "&Sair"          ACCELERATOR "CTRL-X".

DEFINE SUB-MENU m_Ajuda 
       MENU-ITEM mi-conteudo    LABEL "&Conteudo"     
       MENU-ITEM mi-sobre       LABEL "&Sobre..."     .

DEFINE MENU m-livre MENUBAR
       SUB-MENU  mi-programa    LABEL "&Nome-do-Programa"
       SUB-MENU  m_Ajuda        LABEL "&Ajuda"        .


/* Definitions of the field level widgets                               */
DEFINE BUTTON btExecutar 
     LABEL "Executar" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE fiDtFim AS DATE FORMAT "99/99/9999":U 
     LABEL "At‚" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiDtIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Data De" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fiDtIni AT ROW 2.75 COL 15.57 COLON-ALIGNED WIDGET-ID 4
     fiDtFim AT ROW 2.79 COL 40.72 COLON-ALIGNED WIDGET-ID 6
     btExecutar AT ROW 2.79 COL 63.86 WIDGET-ID 2
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90 BY 14.04
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: w-livre
   Allow: Basic,Browse,DB-Fields,Smart,Window,Query
   Container Links: 
   Add Fields to: Neither
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW w-livre ASSIGN
         HIDDEN             = YES
         TITLE              = "Template Livre <Insira complemento>"
         HEIGHT             = 3.75
         WIDTH              = 89.86
         MAX-HEIGHT         = 27.5
         MAX-WIDTH          = 195.14
         VIRTUAL-HEIGHT     = 27.5
         VIRTUAL-WIDTH      = 195.14
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU m-livre:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB w-livre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}
{include/w-livre.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW w-livre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME f-cad
   FRAME-NAME L-To-R                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
THEN w-livre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-livre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON END-ERROR OF w-livre /* Template Livre <Insira complemento> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* Template Livre <Insira complemento> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btExecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExecutar w-livre
ON CHOOSE OF btExecutar IN FRAME f-cad /* Executar */
DO:
  ASSIGN btExecutar:LABEL = "Executando.."
         btExecutar:SENSITIVE = NO.
  RUN pibuscarDAdos.
  OUTPUT TO VALUE(SESSION:TEMP-DIRECTORY + "desembolso.txt").
  FOR EACH tt:

      EXPORT DELIMITER "|" tt.
  END.
  OUTPUT CLOSE.
   ASSIGN btExecutar:LABEL = "Executar"
          btExecutar:SENSITIVE = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-consultas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-consultas w-livre
ON CHOOSE OF MENU-ITEM mi-consultas /* Consultas */
DO:
  RUN pi-consulta IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-conteudo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-conteudo w-livre
ON CHOOSE OF MENU-ITEM mi-conteudo /* Conteudo */
OR HELP OF FRAME {&FRAME-NAME}
DO:
  RUN pi-ajuda IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-imprimir w-livre
ON CHOOSE OF MENU-ITEM mi-imprimir /* Relat¢rios */
DO:
  RUN pi-imprimir IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-programa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-programa w-livre
ON MENU-DROP OF MENU mi-programa /* Nome-do-Programa */
DO:
  run pi-disable-menu.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sair w-livre
ON CHOOSE OF MENU-ITEM mi-sair /* Sair */
DO:
  RUN pi-sair IN h_p-exihel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mi-sobre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mi-sobre w-livre
ON CHOOSE OF MENU-ITEM mi-sobre /* Sobre... */
DO:
  {include/sobre.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-livre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects w-livre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available w-livre  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI w-livre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-livre)
  THEN DELETE WIDGET w-livre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI w-livre  _DEFAULT-ENABLE
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
  DISPLAY fiDtIni fiDtFim 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button fiDtIni fiDtFim btExecutar 
      WITH FRAME f-cad IN WINDOW w-livre.
  {&OPEN-BROWSERS-IN-QUERY-f-cad}
  VIEW w-livre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit w-livre 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize w-livre 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  run pi-before-initialize.

  {include/win-size.i}

  {utp/ut9000.i "XX9999" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarDados w-livre 
PROCEDURE piBuscarDados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE valor AS DECIMAL     NO-UNDO.
FOR EACH movto_cta_corren 
    WHERE movto_cta_corren.dat_transacao >= INPUT FRAME {&frame-name} fiDtIni
    AND   movto_cta_corren.dat_transacao <= INPUT FRAME {&frame-name} fiDtFim 
    AND   ind_tip_movto_cta_corren = 're' /*apenas movimentos realizados*/  NO-LOCK:
   
    CASE movto_cta_corren.cod_modul_dtsul:
        WHEN 'acr'  THEN DO:
           FOR EACH movto_tit_acr OF movto_cta_corren
               NO-LOCK:
               FIND FIRST tit_acr OF movto_tit_acr NO-LOCK NO-ERROR.                                                                                                               
               FIND FIRST emitente WHERE                                                                                                                                           
                    tit_acr.cdn_cliente =  emitente.cod-emitente NO-LOCK NO-ERROR.                                                                                                 
               FOR EACH aprop_ctbl_acr OF movto_tit_acr                                                                                                                            
                   WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_acr.ind_natur_lancto_ctbl = 'cr')                                                    
                    OR  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'sai'   AND aprop_ctbl_acr.ind_natur_lancto_ctbl = 'db') NO-LOCK :                                          
                   CREATE tt.                                                                                                                                                      
                   ASSIGN                                                                                                                                                          
                   tt.cod_empresa       = aprop_ctbl_acr.cod_empresa                                                                                                               
                   tt.cod_estab         = aprop_ctbl_acr.cod_estab                                                                                                                 
                   tt.cod_emitente      =  IF AVAIL tit_acr THEN string(tit_acr.cdn_cliente) ELSE  '0'                                                                             
                   tt.desc_emitente     =  IF AVAIL emitente THEN emitente.nome-emit ELSE ''                                                                                       
                   tt.data              =  aprop_ctbl_acr.dat_transacao                                                                                                            
                   tt.conta_contabil    =  aprop_ctbl_acr.cod_cta_ctbl                                                                                                             
                   tt.valor             =  IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN  aprop_ctbl_acr.val_aprop_ctbl  ELSE  aprop_ctbl_acr.val_aprop_ctbl * -1    
                   tt.conta_corrente    =  movto_cta_corren.cod_cta_corren                                                                                                         
                   tt.base              =  '10'                                                                                                                                    
                   tt.origem            = 'Contas a Receber'                                                                                                                       
                   tt.tipo              = movto_cta_corren.ind_fluxo_movto_cta_corren                                                                                              
                   tt.cc                = aprop_ctbl_acr.cod_ccusto.                                                                                                               
               END.
           END.
        END.
        WHEN 'apb'  THEN DO:
           FOR EACH movto_tit_ap OF movto_cta_corren
               NO-LOCK:
               FIND FIRST tit_ap OF movto_tit_ap NO-LOCK NO-ERROR.   
               FIND FIRST emitente                                                                                                                                              
                   WHERE tit_ap.cdn_fornecedor = emitente.cod-emitente NO-LOCK NO-ERROR.                                                                                        
               FOR EACH aprop_ctbl_ap OF movto_tit_ap NO-LOCK                                                                                                                   
                   WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'cr')                                                  
                    OR  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'sai'   AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'db')                                                  
                   /*AND aprop_ctbl_ap.cod_indic_econ = 'real'*/ :     
                   IF aprop_ctbl_ap.cod_indic_econ = 'dolar' THEN DO:
                      FIND FIRST val_aprop_ctbl_ap OF aprop_ctbl_ap
                            WHERE val_aprop_ctbl_ap.cod_finalid_econ = 'corrente' NO-LOCK NO-ERROR.
                      IF AVAIL val_aprop_ctbl_ap THEN 
                         ASSIGN valor = val_aprop_ctbl_ap.val_aprop_ctbl.
                      ELSE
                         ASSIGN valor = aprop_ctbl_ap.val_aprop_ctbl.
                   END.

                   CREATE tt.                                                                                                                                                   
                   ASSIGN                                                                                                                                                       
                   tt.cod_empresa       = aprop_ctbl_ap.cod_empresa                                                                                                             
                   tt.cod_estab         = aprop_ctbl_ap.cod_estab                                                                                                               
                   tt.cod_emitente      =  IF AVAIL tit_ap THEN string(tit_ap.cdn_fornecedor) ELSE '0'                                                                          
                   tt.desc_emitente     =  IF AVAIL emitente THEN emitente.nome-emit  ELSE ''                                                                                   
                   tt.data              =  aprop_ctbl_ap.dat_transacao                                                                                                          
                   tt.conta_contabil    =  aprop_ctbl_ap.cod_cta_ctbl                                                                                                           
                   tt.valor             = IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN valor  ELSE  valor * -1     
                   tt.conta_corrente    =  movto_cta_corren.cod_cta_corren                                                                                                      
                   tt.base              =  '10'                                                                                                                                 
                   tt.origem            =  'Contas a Pagar'                                                                                                                     
                   tt.tipo              = movto_cta_corren.ind_fluxo_movto_cta_corren                                                                                           
                   tt.cc                = aprop_ctbl_ap.cod_ccusto.                                                                                                             
                                                                                                                                                                                
               END.  
           END.
        END.
        WHEN 'cmg'  THEN DO:
            FIND FIRST tip_trans_cx OF movto_cta_corren NO-LOCK NO-ERROR.
            FOR EACH aprop_ctbl_cmg OF movto_cta_corren
                WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_cmg.ind_natur_lancto_ctbl = 'cr')
                OR  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'sai' AND aprop_ctbl_cmg.ind_natur_lancto_ctbl = 'db') NO-LOCK :
               CREATE tt.
               ASSIGN 
               tt.cod_empresa       = aprop_ctbl_cmg.cod_empresa 
               tt.cod_estab         = aprop_ctbl_cmg.cod_estab
               tt.cod_emitente      = movto_cta_corren.cod_tip_trans_cx
               tt.desc_emitente     = IF AVAIL tip_trans_cx THEN tip_trans_cx.des_tip_trans_cx  ELSE ''
               tt.data              = aprop_ctbl_cmg.dat_transacao
               tt.conta_contabil    = aprop_ctbl_cmg.cod_cta_ctbl
               tt.valor             = IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' THEN aprop_ctbl_cmg.val_movto_cta_corren ELSE aprop_ctbl_cmg.val_movto_cta_corren * -1
               tt.conta_corrente    = movto_cta_corren.cod_cta_corren
               tt.base              = '10'
               tt.origem            = 'Caixa e Bancos'
               tt.tipo              = movto_cta_corren.ind_fluxo_movto_cta_corren
               tt.cc                = aprop_ctbl_cmg.cod_ccusto.    
            END.
        END.
    END CASE.
END.
FOR EACH tt BREAK BY tt.conta_contabil:
    IF FIRST-OF(tt.conta_contabil) THEN DO:
       FIND FIRST cta_ctbl
           WHERE cta_ctbl.cod_cta_ctbl = tt.conta_contabil NO-LOCK NO-ERROR.
    END.
    ASSIGN tt.DESC_conta = cta_ctbl.DES_tit_ctbl.
END.
FOR EACH tt 
    WHERE tt.cc <> '' BREAK BY tt.cc:
    IF FIRST-OF(tt.cc) THEN DO:
       FIND FIRST ems5.ccusto
           WHERE ccusto.cod_ccusto = tt.cc NO-LOCK NO-ERROR.
    END.
    ASSIGN tt.DESC_cc = cCusto.Des_tit_ctbl.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piBuscarNotasRateio w-livre 
PROCEDURE piBuscarNotasRateio :
/*------------------------------------------------------------------------------
 serie-docto -> cod_ser_docto
 nro-docto -> cod_tit_ap
 cod_emitente -> cdn_fornecedor
 nat_operacao -> n/d
 parcela -> cod_parcela
------------------------------------------------------------------------------
*/
DEFINE INPUT  PARAMETER pSerie       AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pDocumento   AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pCodEmitente AS INT         NO-UNDO.
DEFINE INPUT  PARAMETER pParcela     AS CHARACTER   NO-UNDO.

/*DEFINE INPUT  PARAMETER pNatureza   AS CHARACTER   NO-UNDO.*/




FIND FIRST dupli-apagar 
     WHERE dupli-aPagar.serie-docto     = pSerie
     AND   dupli-aPagar.nro-docto       = pDocumento
     AND   dupli-aPagar.cod-emitente    = pCodEmitente
     AND   dupli-aPagar.parcel          = pParcela
    NO-LOCK NO-ERROR.
IF AVAIL dupli-apagar THEN DO:
   FIND FIRST docum-est OF dupli-apagar NO-LOCK NO-ERROR.
   FOR EACH item-doc-est OF docum-est NO-LOCK.
       DISP ct-codigo sc-codigo .
   END.
       



END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records w-livre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this w-livre, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed w-livre 
PROCEDURE state-changed :
/*:T -----------------------------------------------------------
  Purpose:     Manuseia trocas de estado dos SmartObjects
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

  run pi-trata-state (p-issuer-hdl, p-state).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

