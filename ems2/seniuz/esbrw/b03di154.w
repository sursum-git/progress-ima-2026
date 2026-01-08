&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/********************************************************************************
** Copyright DATASUL S.A. (1997)
** Todos os Direitos Reservados.
**
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so podera ser feita mediante
** autorizacao expressa.
*******************************************************************************/
{include/i-prgvrs.i B03DI154 2.04.00.000}

/* Create an unnamed pool to store all the widgets created by this procedure.
   This is a good default which assures that this procedure's triggers and 
   internal procedures will execute in this procedure's storage, and that 
   proper cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEFINE NEW GLOBAL SHARED VAR fi-ini-it-codigo  LIKE ped-item-ext.it-codigo.
DEFINE NEW GLOBAL SHARED VAR fi-fin-it-codigo  LIKE ped-item-ext.it-codigo.
DEFINE NEW GLOBAL SHARED VAR to-abe AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-atp AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-att AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-pen AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-sus AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-can AS LOG.
DEFINE NEW GLOBAL SHARED VAR to-bal AS LOG.

def temp-table tt-ped-item
    field line as int /* Este campo ‚ obrigat¢rio */
    field nr-sequencia LIKE ped-item.nr-sequencia
    field it-codigo    LIKE ped-item.it-codigo
    FIELD desc-item    AS CHAR FORMAT "x(19)"
    FIELD cod-refer    LIKE ped-item.cod-refer
    FIELD sit-item     AS CHAR FORMAT "x(3)"
    FIELD vl-unitario  AS DEC FORMAT ">>,>>9.99"
    FIELD perc-icms    AS DEC FORMAT ">9.99"
    FIELD qt-faturada  AS DEC FORMAT ">>>,>>9.99"
    FIELD qt-aberta    AS DEC FORMAT ">>>,>>9.99"
    FIELD qt-reserva   AS DEC FORMAT ">>>,>>9.99"
    FIELD qt-estoque   AS DEC FORMAT "->,>>>,>>9"
    FIELD qt-disponiv  AS DEC FORMAT "->,>>>,>>9"
    FIELD prg-prc-prt  AS CHAR FORMAT "x(6)"
    INDEX ch-seq nr-sequencia.

def var c-cod-lista-obj as char no-undo.

DEF VAR de-sld-estoque AS dec.
DEF VAR de-res-item    AS DEC.
DEF VAR de-res-tot     AS DEC.
DEF VAR c-sit-prog     AS CHAR.
DEF VAR c-sit-item     AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE BrowseDigitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-digita

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES ped-venda
&Scoped-define FIRST-EXTERNAL-TABLE ped-venda


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ped-venda.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-ped-item

/* Definitions for BROWSE br-digita                                     */
&Scoped-define FIELDS-IN-QUERY-br-digita tt-ped-item.nr-sequencia tt-ped-item.it-codigo tt-ped-item.desc-item tt-ped-item.cod-refer tt-ped-item.sit-item tt-ped-item.vl-unitario tt-ped-item.perc-icms tt-ped-item.qt-faturada tt-ped-item.qt-aberta tt-ped-item.qt-reserva tt-ped-item.qt-estoque tt-ped-item.qt-disponiv tt-ped-item.prg-prc-prt   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-digita   
&Scoped-define SELF-NAME br-digita
&Scoped-define QUERY-STRING-br-digita FOR EACH tt-ped-item NO-LOCK
&Scoped-define OPEN-QUERY-br-digita OPEN QUERY {&self-name} FOR EACH tt-ped-item NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-digita tt-ped-item
&Scoped-define FIRST-TABLE-IN-QUERY-br-digita tt-ped-item


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br-digita 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-name
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS
><EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-name
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
************************
* Initialize Filter Attributes */
RUN set-attribute-list IN THIS-PROCEDURE ('
  Filter-Value=':U).
/************************
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-digita FOR 
      tt-ped-item SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-digita B-table-Win _FREEFORM
  QUERY br-digita NO-LOCK DISPLAY
      tt-ped-item.nr-sequencia COLUMN-LABEL "Seq."      FORMAT "9999"           
      tt-ped-item.it-codigo    COLUMN-LABEL "Ötem"      FORMAT "x(6)" 
      tt-ped-item.desc-item    COLUMN-LABEL "Descri‡Æo"
      tt-ped-item.cod-refer    COLUMN-LABEL "Refer"     FORMAT "x(7)"
      tt-ped-item.sit-item     COLUMN-LABEL "Sit"
      tt-ped-item.vl-unitario  COLUMN-LABEL "Pre‡oUn"
      tt-ped-item.perc-icms    COLUMN-LABEL "%ICMS"
      tt-ped-item.qt-faturada  COLUMN-LABEL "Faturado"
      tt-ped-item.qt-aberta    COLUMN-LABEL "Aberto"
      tt-ped-item.qt-reserva   COLUMN-LABEL "Reservado"
      tt-ped-item.qt-estoque   COLUMN-LABEL "Estoque"
      tt-ped-item.qt-disponiv  COLUMN-LABEL "Dispon¡v."
      tt-ped-item.prg-prc-prt  COLUMN-LABEL "PrPcPt"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 89.57 BY 8.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br-digita AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: BrowseDigitacao
   External Tables: movdis.ped-venda
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 8.08
         WIDTH              = 89.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{utp/ut-glob.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br-digita 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-digita
/* Query rebuild information for BROWSE br-digita
     _START_FREEFORM
OPEN QUERY {&self-name} FOR EACH tt-ped-item NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* BROWSE br-digita */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br-digita
&Scoped-define SELF-NAME br-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-digita B-table-Win
ON VALUE-CHANGED OF br-digita IN FRAME F-Main
DO:
    /* This ADM trigger code must be preserved in order to notify other
       objects when the browser's current row changes. */
    {src/adm/template/brschnge.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
br-digita:NUM-LOCKED-COLUMNS = 2.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "ped-venda"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ped-venda"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available B-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN fi-ini-it-codigo = ""
         fi-fin-it-codigo = "ZZZZZZZZZZZZZZZZ".

  RUN pi-popula-browse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-popula-browse B-table-Win 
PROCEDURE pi-popula-browse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH tt-ped-item.
      DELETE tt-ped-item.
  END.

  FOR EACH ped-item OF ped-venda WHERE
           ped-item.it-codigo >= fi-ini-it-codigo AND
           ped-item.it-codigo <= fi-fin-it-codigo AND
           (ped-item.cod-sit-item = 1 AND to-abe = YES OR
            ped-item.cod-sit-item = 2 AND to-atp = YES OR 
            ped-item.cod-sit-item = 3 AND to-att = YES OR 
            ped-item.cod-sit-item = 4 AND to-pen = YES OR 
            ped-item.cod-sit-item = 5 AND to-sus = YES OR 
            ped-item.cod-sit-item = 6 AND to-can = YES OR 
            ped-item.cod-sit-item = 7 AND to-bal = yes) NO-LOCK.

      FIND ped-item-ext OF ped-item NO-LOCK NO-ERROR.

      CREATE tt-ped-item.

      FIND ITEM OF ped-item NO-LOCK NO-ERROR.
       
      /* Situa‡Æo do Item de pedido */
      ASSIGN c-sit-item = SUBSTR("AbeAtpAttPenSusCanBal",ped-item.cod-sit-item * 3 - 2,3).

      /* Verifica Programacao */
      find ref-item-ext where 
           ref-item-ext.it-codigo = ped-item.it-codigo AND
           ref-item-ext.cod-refer = ped-item.cod-refer no-lock no-error.

      if not avail ref-item-ext then
         assign c-sit-prog = "N N N".
      else do:
         if ref-item-ext.qtd-prog <> 0 then
            assign c-sit-prog = "S ".
         else
            assign c-sit-prog = "N ".
         if ref-item-ext.qtd-proc <> 0 then
            assign c-sit-prog = c-sit-prog + "S ".
         else
            assign c-sit-prog = c-sit-prog + "N ".
         if ref-item-ext.qtd-pron <> 0 then
            assign c-sit-prog = c-sit-prog + "S".
         else
            assign c-sit-prog = c-sit-prog + "N".
      end.

      /* Verifica Estoque */
      ASSIGN de-sld-estoque = 0.
      FOR EACH saldo-estoq where 
               saldo-estoq.cod-estabel = ped-venda.cod-estabel AND
               saldo-estoq.it-codigo   = ped-item.it-codigo AND
               saldo-estoq.cod-depos   = "exp" AND
               saldo-estoq.cod-refer   = ped-item.cod-refer AND
               saldo-estoq.lote        = ped-item-ext.lote NO-LOCK.
          ASSIGN de-sld-estoque = de-sld-estoque + saldo-estoq.qtidade-atu.
      END.

      /* Verifica Reservas */
      ASSIGN de-res-item = 0.
      FOR EACH ped-item-res 
         WHERE ped-item-res.nome-abrev = ped-item.nome-abrev
           AND ped-item-res.nr-pedcli  = ped-item.nr-pedcli
           AND ped-item-res.nr-sequencia = ped-item.nr-sequencia
           AND ped-item-res.it-codigo  = ped-item.it-codigo 
           AND ped-item-res.cod-refer  = ped-item.cod-refer
           NO-LOCK:

         IF ped-item-res.faturado = yes then do:
            FIND nota-fiscal WHERE
                 nota-fiscal.cod-estabel = ped-item-res.cod-estabel AND
                 nota-fiscal.serie       = ped-item-res.serie AND
                 nota-fiscal.nr-nota-fis = string(ped-item-res.nr-nota-fis,"9999999")
                 NO-LOCK NO-ERROR.

            IF AVAIL nota-fiscal then
               IF  nota-fiscal.ind-sit-nota < 6
               AND nota-fiscal.dt-confirma  = ?
               AND nota-fiscal.dt-cancela   = ? THEN
                   ASSIGN de-res-item = de-res-item + ped-item-res.qt-pedida.
         END.
         ELSE
            ASSIGN de-res-item = de-res-item + ped-item-res.qt-pedida.
      END. 

      ASSIGN de-res-tot = 0.
      FOR each ped-item-res 
         where ped-item-res.it-codigo = item.it-codigo 
           and ped-item-res.cod-refer = ped-item.cod-refer
           AND ped-item-res.lote      = ped-item-ext.lote NO-LOCK:
         IF ped-item-res.faturado = yes then do:
            find nota-fiscal
                 where nota-fiscal.cod-estabel = ped-item-res.cod-estabel
                   and nota-fiscal.serie       = ped-item-res.serie
                   and nota-fiscal.nr-nota-fis = string(ped-item-res.nr-nota-fis,"9999999")
                 no-lock no-error.

            if avail nota-fiscal then
               if  nota-fiscal.ind-sit-nota < 6
               AND nota-fiscal.dt-confirma  = ?
               and nota-fiscal.dt-cancela   = ? then
                   assign de-res-tot = de-res-tot + ped-item-res.qt-pedida.
         END.
         ELSE
            ASSIGN de-res-tot = de-res-tot + ped-item-res.qt-pedida.
      END. 
      
      FIND natur-oper OF ped-item NO-LOCK.
      
      ASSIGN tt-ped-item.nr-sequencia = ped-item.nr-sequencia
             tt-ped-item.it-codigo    = ped-item.it-codigo
             tt-ped-item.desc-item    = ITEM.desc-item
             tt-ped-item.cod-refer    = ped-item.cod-refer
             tt-ped-item.sit-item     = c-sit-item
             tt-ped-item.vl-unitario  = ped-item.vl-preori
             tt-ped-item.perc-icms    = natur-oper.aliquota-icm
             tt-ped-item.qt-faturada  = ped-item.qt-atendida
             tt-ped-item.qt-aberta    = ped-item.qt-pedida -
                                        ped-item.qt-atendida +
                                        ped-item.qt-devolvida
             tt-ped-item.qt-reserva   = de-res-item
             tt-ped-item.qt-estoque   = de-sld-estoque
             tt-ped-item.qt-disponiv  = de-sld-estoque - de-res-tot
             tt-ped-item.prg-prc-prt  = c-sit-prog.
  END.

  {&OPEN-QUERY-br-digita}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "ped-venda"}
  {src/adm/template/snd-list.i "tt-ped-item"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

