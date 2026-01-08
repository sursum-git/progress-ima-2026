&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          espec            PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEF TEMP-TABLE tt-arquivo
    FIELD num-etiqueta AS CHAR.

/* Parameters Definitions ---                                           */
DEF BUFFER b-etiqueta FOR ob-etiqueta.
DEF BUFFER b-nota-fiscal FOR nota-fiscal.

/* Local Variable Definitions ---                                       */

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.
DEF NEW GLOBAL SHARED VAR g-tp-dados AS CHAR.

DEF VAR c-cod-estabel AS CHAR NO-UNDO.
DEF VAR i-ct AS INT.
DEF VAR c-desc-item AS CHAR FORMAT "x(36)".
DEF VAR v-defeito AS CHAR EXTENT 3.
DEF VAR i-lote AS INT.
DEF VAR c-comando AS CHAR.
DEF VAR c-code-ant AS CHAR.
DEF VAR i-sit-ant AS INT.
DEF VAR c-desc-situacao AS CHAR FORMAT "x(20)".
DEF VAR i-num-bar AS INT.
DEF VAR c-lote AS CHAR.
DEF VAR i-tp-embal AS INT.
DEF VAR c-corte-comerc AS CHAR.
DEF VAR de-quantidade AS DEC.

DEF VAR h-prog AS HANDLE.

DEF VAR i-tempo-ini    AS INT.
DEF VAR c-prog-balanca AS CHAR.
DEF VAR c-peso-balanca AS CHAR.
DEF VAR c-peso AS CHAR FORMAT "x(10)".
DEF VAR de-peso-lido   AS DEC FORMAT ">>>,>>9.9".

DEF VAR de-peso-calc   AS DEC FORMAT ">>>,>>9.9".
DEF VAR de-media-peso  AS DEC.

DEF VAR c-form-epl    AS CHAR FORMAT "x(30)".
DEF VAR c-prog-epl    AS CHAR FORMAT "x(50)".
DEF VAR i-tot-etq     AS INT.
DEF VAR i-etq-imp     AS INT.
DEF VAR c-seq         AS CHAR.
DEF VAR c-origem      AS CHAR.
DEF VAR c-emissao     AS CHAR.
DEF VAR c-pedido      AS CHAR.
DEF VAR c-localiz     AS CHAR. 
DEF VAR c-cod-esta    LIKE nota-fiscal.cod-estabel INIT '2'.
DEF VAR c-arquivo     AS CHAR.
DEF VAR c-impressora  AS CHAR.
DEF VAR c-coletor     AS CHAR.
DEF VAR l-erro        AS LOG.
DEF VAR l-valida      AS LOG.
DEF VAR i-digito      AS INT NO-UNDO.

{esinc/sz-pcl.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ob-etiqueta

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH ob-etiqueta SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH ob-etiqueta SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME ob-etiqueta
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME ob-etiqueta


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn-importa fi-num-etiqueta bt-imprime ~
bt-sair RECT-51 RECT-52 RECT-54 
&Scoped-Define DISPLAYED-OBJECTS fi-num-etiqueta fi-quantidade fi-cod-estab ~
fi-nome-estab fi-nr-ob fi-dt-emissao fi-hora fi-it-codigo fi-desc-item ~
fi-cod-refer fi-desc-refer fi-peso-bruto fi-tot-ob fi-corte-comerc ~
fi-qt-volumes fi-nr-lote 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 fi-cod-estab 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-imprime AUTO-GO 
     IMAGE-UP FILE "image/im-f-dv.bmp":U
     LABEL "Btn 2" 
     SIZE 6 BY 1.5 TOOLTIP "Imprime Etiqueta".

DEFINE BUTTON bt-sair AUTO-END-KEY 
     IMAGE-UP FILE "image/im-exi.bmp":U
     LABEL "Btn 2" 
     SIZE 6 BY 1.5 TOOLTIP "Sair".

DEFINE BUTTON btn-importa 
     IMAGE-UP FILE "image/emstec.ico":U NO-FOCUS
     LABEL "Button 1" 
     SIZE 6 BY 1.5 TOOLTIP "Importar Dados do Coletor para Localizaá∆o de Etiqueta na Doca.".

DEFINE VARIABLE fi-cod-estab AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estabelecimento" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-cod-refer AS CHARACTER FORMAT "X(8)" 
     LABEL "Referància" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE fi-corte-comerc AS CHARACTER FORMAT "!" 
     LABEL "Corte-Comercial" 
     VIEW-AS FILL-IN 
     SIZE 3 BY .88.

DEFINE VARIABLE fi-desc-item AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-desc-refer AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32.57 BY .88 NO-UNDO.

DEFINE VARIABLE fi-dt-emissao AS DATE FORMAT "99/99/9999" 
     LABEL "Dt Emiss∆o Etq" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 DROP-TARGET.

DEFINE VARIABLE fi-hora AS CHARACTER FORMAT "99:99":U 
     LABEL "Hora" 
     VIEW-AS FILL-IN 
     SIZE 5.86 BY .88 NO-UNDO.

DEFINE VARIABLE fi-it-codigo AS CHARACTER FORMAT "X(16)" 
     LABEL "Item" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .88.

DEFINE VARIABLE fi-nome-estab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-lote AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lote" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .88 NO-UNDO.

DEFINE VARIABLE fi-nr-ob AS INTEGER FORMAT ">>>,>>9" INITIAL 0 
     LABEL "N£mero da OB" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .88.

DEFINE VARIABLE fi-num-etiqueta AS CHARACTER FORMAT "x(9)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.75
     FONT 20 NO-UNDO.

DEFINE VARIABLE fi-peso-bruto AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Peso Bruto (KG)" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qt-volumes AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Volumes" 
     VIEW-AS FILL-IN 
     SIZE 5.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-quantidade AS DECIMAL FORMAT ">>>,>>9.99" INITIAL 0 
     LABEL "Quantidade" 
     VIEW-AS FILL-IN 
     SIZE 8.29 BY .88.

DEFINE VARIABLE fi-tot-ob AS DECIMAL FORMAT ">>>,>>9.99" INITIAL 0 
     LABEL "Quantidade" 
     VIEW-AS FILL-IN 
     SIZE 8.29 BY .88.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 12.75.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32 BY 3.25.

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 29 BY 1.75
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      ob-etiqueta SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btn-importa AT ROW 1.67 COL 58
     fi-num-etiqueta AT ROW 1.5 COL 22 COLON-ALIGNED NO-LABEL
     fi-quantidade AT ROW 9.38 COL 22 COLON-ALIGNED WIDGET-ID 28
     bt-imprime AT ROW 1.67 COL 51.72
     bt-sair AT ROW 1.67 COL 73.43
     fi-cod-estab AT ROW 4.38 COL 22 COLON-ALIGNED WIDGET-ID 4
     fi-nome-estab AT ROW 4.38 COL 26.57 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     fi-nr-ob AT ROW 5.38 COL 22 COLON-ALIGNED WIDGET-ID 26
     fi-dt-emissao AT ROW 6.38 COL 22 COLON-ALIGNED WIDGET-ID 12
     fi-hora AT ROW 6.38 COL 64.14 COLON-ALIGNED WIDGET-ID 18
     fi-it-codigo AT ROW 7.38 COL 22 COLON-ALIGNED WIDGET-ID 24
     fi-desc-item AT ROW 7.38 COL 33.43 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     fi-cod-refer AT ROW 8.38 COL 22 COLON-ALIGNED WIDGET-ID 10
     fi-desc-refer AT ROW 8.38 COL 30.57 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     fi-peso-bruto AT ROW 10.38 COL 22 COLON-ALIGNED WIDGET-ID 20
     fi-tot-ob AT ROW 10.75 COL 52.43 COLON-ALIGNED WIDGET-ID 40
     fi-corte-comerc AT ROW 11.38 COL 22 COLON-ALIGNED WIDGET-ID 14
     fi-qt-volumes AT ROW 11.75 COL 52.43 COLON-ALIGNED WIDGET-ID 44
     fi-nr-lote AT ROW 12.38 COL 22 COLON-ALIGNED WIDGET-ID 30
     "Etiqueta:" VIEW-AS TEXT
          SIZE 17 BY 1.5 AT ROW 1.58 COL 6.72
          FGCOLOR 9 FONT 20
     " Entradas da OB" VIEW-AS TEXT
          SIZE 14.72 BY .54 AT ROW 9.79 COL 41.29 WIDGET-ID 34
          FGCOLOR 9 FONT 6
     RECT-51 AT ROW 1 COL 1 WIDGET-ID 2
     RECT-52 AT ROW 10 COL 40 WIDGET-ID 32
     RECT-54 AT ROW 1.5 COL 51 WIDGET-ID 52
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 12.88
         FONT 1
         CANCEL-BUTTON bt-sair.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Emiss∆o de Etiqueta Final - ESSP0134"
         HEIGHT             = 12.88
         WIDTH              = 80
         MAX-HEIGHT         = 17.29
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 17.29
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN fi-cod-estab IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi-cod-refer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-corte-comerc IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-item IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-desc-refer IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-emissao IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-hora IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-it-codigo IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nome-estab IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-lote IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-nr-ob IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-peso-bruto IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-qt-volumes IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-quantidade IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-tot-ob IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "espec.ob-etiqueta"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Emiss∆o de Etiqueta Final - ESSP0134 */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Emiss∆o de Etiqueta Final - ESSP0134 */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-imprime
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-imprime C-Win
ON CHOOSE OF bt-imprime IN FRAME DEFAULT-FRAME /* Btn 2 */
DO:
    IF NOT AVAIL ob-etiqueta THEN DO.
       APPLY 'ENTRY' TO fi-num-etiqueta.
       RETURN NO-APPLY.
    END.

    /*
    IF c-seg-usuario <> 'super' THEN DO.
       RUN pi-valida. 
       IF RETURN-VALUE = 'NOK' THEN DO.
          APPLY 'entry' TO fi-num-etiqueta.
          RETURN NO-APPLY.
       END.
    END.
    */

    FIND corte-comerc WHERE
         corte-comerc.codigo = INPUT FRAME {&FRAME-NAME} fi-corte-comerc
         NO-LOCK NO-ERROR.

    ASSIGN de-media-peso = 0.
    /*IF item-ext.indigo THEN
       ASSIGN de-media-peso = corte-comerc.peso-emb-indigo.
    ELSE */
       ASSIGN de-media-peso = corte-comerc.peso-emb-outros.

    IF ob-etiqueta.situacao = 2 OR
       (ob-etiqueta.situacao = 1 AND ob-etiqueta.tipo-ordem = 4) THEN DO.

       /* Regra de Validade de Peso pela Metragem */
       /*
       IF SUBSTR(ob-etiqueta.nr-lote,1,2) <> "SC" THEN DO:
          ASSIGN de-peso-calc = (ITEM.peso-liquido * INPUT FRAME {&FRAME-NAME} fi-quantidade) + de-media-peso.
          IF ABS(de-peso-lido - de-peso-calc) / ITEM.peso-liquido > ob-param.mt-tecido-per THEN DO.
             MESSAGE "Peso da Balanáa: " de-peso-lido SKIP
                     "Peso Calculado: " de-peso-calc SKIP
                     "Metragem: " ob-etiqueta.quantidade SKIP
                     "O Peso da Balanca, esta  esta fora dos limites esperados...."
                     VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    
             ASSIGN ob-etiqueta.erro-peso = YES
                    ob-etiqueta.peso-bruto = de-peso-lido.
    
             APPLY 'entry' TO fi-num-etiqueta.
             RETURN NO-APPLY.
          END.
       END.
       */

       IF ob-etiqueta.situacao = 1 AND ob-etiqueta.tipo-ordem = 4 THEN DO.
          ASSIGN fi-quantidade:SENSITIVE = NO
                 fi-nr-lote:SENSITIVE = NO.

          ASSIGN ob-etiqueta.dt-emissao = INPUT FRAME {&FRAME-NAME} fi-dt-emissao
                 ob-etiqueta.hr-emissao = INPUT FRAME {&FRAME-NAME} fi-hora
                 ob-etiqueta.quantidade = INPUT FRAME {&FRAME-NAME} fi-quantidade
                 ob-etiqueta.corte-comerc = INPUT FRAME {&FRAME-NAME} fi-corte-comerc
                 ob-etiqueta.nr-lote = INPUT FRAME {&FRAME-NAME} fi-nr-lote
                 ob-etiqueta.cod-qualid = IF ob-etiqueta.nr-lote = "RP"
                                          THEN 'B' ELSE 'D'.
       END.

       IF SUBSTR(ob-etiqueta.nr-lote,1,2) = "SC" THEN
          ASSIGN ob-etiqueta.quantidade = de-peso-lido.
    END.
    ASSIGN ob-etiqueta.erro-peso = NO.

    RUN esapi/imp-etq-estoque.p (INPUT ob-etiqueta.cod-estabel,
                                 INPUT ob-etiqueta.num-etiqueta,
                                 INPUT NO).
    
    FIND CURRENT ob-etiqueta SHARE-LOCK NO-ERROR.
    ASSIGN i-sit-ant = ob-etiqueta.situacao.
    IF ob-etiqueta.situacao <= 2 THEN   
       ASSIGN ob-etiqueta.situacao = 3
              ob-etiqueta.peso-bruto = de-peso-lido.  

    FIND CURRENT ob-etiqueta NO-LOCK NO-ERROR.

    ASSIGN fi-num-etiqueta:SCREEN-VALUE = ''
           fi-cod-estab:SCREEN-VALUE = ''
           fi-nome-estab:SCREEN-VALUE = ''.

    ASSIGN fi-nr-ob:SCREEN-VALUE = ''
           fi-dt-emissao:SCREEN-VALUE = ''
           fi-hora:SCREEN-VALUE = ''
           fi-it-codigo:SCREEN-VALUE = ''
           fi-desc-item:SCREEN-VALUE = ''
           fi-cod-refer:SCREEN-VALUE = ''
           fi-desc-refer:SCREEN-VALUE = ''
           fi-quantidade:SCREEN-VALUE = ''
           fi-peso-bruto:SCREEN-VALUE = ''
           fi-corte-comerc:SCREEN-VALUE = ''
           fi-nr-lote:SCREEN-VALUE = ''.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-sair
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-sair C-Win
ON CHOOSE OF bt-sair IN FRAME DEFAULT-FRAME /* Btn 2 */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-importa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-importa C-Win
ON CHOOSE OF btn-importa IN FRAME DEFAULT-FRAME /* Button 1 */
DO:
  /* ASSIGN g-tp-dados = "LOCALIZACAO"
          btn-importa:SENSITIVE IN FRAME {&FRAME-NAME} = NO
          bt-sair:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
  
   RUN esp/essp0105.w "NEW GLOBAL SHARED".
  
   ASSIGN btn-importa:SENSITIVE IN FRAME {&FRAME-NAME} = YES
          bt-sair:SENSITIVE IN FRAME {&FRAME-NAME} = YES.  */

   RUN esp\essp0139a.w (OUTPUT c-coletor).

   ASSIGN l-valida = NO.

   FOR EACH tt-arquivo NO-LOCK.
       DELETE tt-arquivo.
   END.  

   IF c-coletor = "1" THEN DO.
      IF SEARCH("c:\temp\form.dat") = ? THEN
         MESSAGE 'Arquivo n∆o foi descarregado...' 
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ELSE
         RUN pi-imp-unitech.
   END.
    
   IF c-coletor = "2" THEN DO:
      ASSIGN c-arquivo = "c:\temp\data.txt".
      OS-DELETE SILENT VALUE(c-arquivo). 

      IF SEARCH ("C:\coletor\NetO32.exe") <> ? THEN DO:
    /*   ASSIGN c-comando = "C:\coletor\IMA.nsf".   */
    
         ASSIGN c-comando = '"C:\coletor\NetO32.exe" -c:7 -b:19200 -dr:c:\temp -dt:1'.   
         OS-COMMAND SILENT VALUE(c-comando).

         IF SEARCH("c:\temp\data.txt") = ? THEN
            MESSAGE 'Arquivo n∆o foi descarregado...' 
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
         ELSE
            RUN pi-imp-opticon.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-corte-comerc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-corte-comerc C-Win
ON VALUE-CHANGED OF fi-corte-comerc IN FRAME DEFAULT-FRAME /* Corte-Comercial */
DO:
   IF INPUT FRAME {&FRAME-NAME} fi-corte-comerc <> ob-etiqueta.corte-comerc THEN DO:
      FIND corte-comerc WHERE corte-comerc.codigo = INPUT FRAME {&FRAME-NAME} fi-corte-comerc
                        NO-LOCK NO-ERROR.
      IF NOT AVAIL corte-comerc THEN
         MESSAGE "Corte comercial n∆o encontrado."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-nr-lote
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-nr-lote C-Win
ON LEAVE OF fi-nr-lote IN FRAME DEFAULT-FRAME /* Lote */
DO:
    IF LOOKUP(SELF:SCREEN-VALUE,"RP,RD,CA,SC") = 0 THEN DO.
       MESSAGE "Lote Inv†lido, deve ser RP,RD,CA ou SC"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY 'ENTRY' TO SELF.
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-num-etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-etiqueta C-Win
ON LEAVE OF fi-num-etiqueta IN FRAME DEFAULT-FRAME
DO:
   IF SELF:SCREEN-VALUE = '' THEN 
      RETURN NO-APPLY.

   FIND ob-etiqueta WHERE
        ob-etiqueta.cod-estabel = c-cod-estabel AND 
        ob-etiqueta.num-etiqueta = INTEGER(INPUT FRAME {&FRAME-NAME} fi-num-etiqueta)
        USE-INDEX indice4 SHARE-LOCK NO-ERROR.

   IF NOT AVAIL ob-etiqueta THEN DO.
      MESSAGE 'Etiqueta n∆o Cadastrada no Sistema...'
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'entry' TO fi-num-etiqueta.
      RETURN NO-APPLY.
   END.

   IF ob-etiqueta.tipo-ordem <> 4 AND
      ob-etiqueta.quantidade = 0 AND SUBSTR(ob-etiqueta.nr-lote,1,2) <> "SC" THEN DO.
      MESSAGE 'Etiqueta n∆o foi Revisada, Confirme com setor Respons†vel'
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'entry' TO fi-num-etiqueta.
      RETURN NO-APPLY.
   END.
   /*
   IF SUBSTR(ob-etiqueta.char-1,1330,1) <> "T" THEN DO. /* N∆o Ç Etq de Transferencia */
      FIND ordem-benefic WHERE
           ordem-benefic.cod-estabel = ob-etiqueta.cod-estabel AND
           ordem-benefic.nr-ob = ob-etiqueta.nr-ob AND
           ordem-benefic.dt-ob = ob-etiqueta.dt-ob AND
           ordem-benefic.nr-carro = ob-etiqueta.nr-carro NO-LOCK NO-ERROR.
      
      IF NOT AVAIL ordem-benefic THEN DO.
         MESSAGE 'OB n∆o encontrada para essa Sequencia'
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    
         APPLY 'entry' TO fi-num-etiqueta.
         RETURN NO-APPLY.
      END.
      
   END.
   */
   FIND ITEM WHERE
        ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

   FIND item-ext WHERE
        item-ext.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

   FIND referencia WHERE
        referencia.cod-refer = ob-etiqueta.cod-refer NO-LOCK NO-ERROR.

   /*
   IF AVAIL ordem-benefic AND 
      ob-etiqueta.it-codigo <> ordem-benefic.it-codigo AND
      ob-etiqueta.nr-lote <> 'SC' THEN DO.
      MESSAGE 'Item da Etiqueta est† Diferente do Item da OB '
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY 'entry' TO fi-num-etiqueta.
      RETURN NO-APPLY.
   END.
   */

   ASSIGN de-peso-lido = ob-etiqueta.peso.
   IF ob-etiqueta.situacao = 2 OR 
      (ob-etiqueta.situacao = 1 AND ob-etiqueta.tipo-ordem = 4) THEN DO.
      RUN pi-captura-peso.
      IF RETURN-VALUE = 'ADM-ERROR' THEN 
         RETURN NO-APPLY.    
   END.

   /*
   IF ob-etiqueta.acondic BEGINS "ROLO" THEN
      ASSIGN c-lote = 'RP'
             i-tp-embal = 1.
   ELSE
   IF ob-etiqueta.acondic BEGINS "PECA" THEN
      ASSIGN c-lote = 'PP'
             i-tp-embal = 2.
   ELSE
   IF ob-etiqueta.acondic BEGINS "CORTE" THEN
      ASSIGN c-lote = 'CA'
             i-tp-embal = 4.
   ELSE
      ASSIGN c-lote = 'SC'
             i-tp-embal = 3.
   */

   ASSIGN de-quantidade = ob-etiqueta.quantidade
          c-corte-comerc = 'Z'.

   /*
   IF ob-etiqueta.corte-comerc = '' THEN DO.
      FIND ob-acondic OF ob-etiqueta NO-LOCK NO-ERROR.

      FIND corte-comerc WHERE
           corte-comerc.codigo = ob-acondic.corte-comerc NO-LOCK NO-ERROR.

      ASSIGN c-corte-comerc = corte-comerc.codigo
             de-quantidade = corte-comerc.compr-med.
   END.
   */
   FIND estabelec WHERE
        estabelec.cod-estab = ob-etiqueta.cod-estab
        NO-LOCK NO-ERROR.

   ASSIGN fi-cod-estab:SCREEN-VALUE = estabelec.cod-estab
          fi-nome-estab:SCREEN-VALUE = estabelec.nome.

   ASSIGN fi-nr-ob:SCREEN-VALUE = STRING(ob-etiqueta.nr-ob)
          fi-dt-emissao:SCREEN-VALUE = STRING(ob-etiqueta.dt-emissao)
          fi-hora:SCREEN-VALUE = STRING(ob-etiqueta.hr-emissao)
          fi-it-codigo:SCREEN-VALUE = ob-etiqueta.it-codigo
          fi-desc-item:SCREEN-VALUE = ITEM.desc-item
          fi-cod-refer:SCREEN-VALUE = ob-etiqueta.cod-refer
          fi-desc-refer:SCREEN-VALUE = IF AVAIL referencia
                                       THEN referencia.descricao
                                       ELSE ''
          fi-quantidade:SCREEN-VALUE = STRING(de-quantidade)
          fi-peso-bruto:SCREEN-VALUE = STRING(de-peso-lido)
          fi-corte-comerc:SCREEN-VALUE = c-corte-comerc
          fi-nr-lote:SCREEN-VALUE = ob-etiqueta.nr-lote.
   /*
   ASSIGN fi-tot-ob = 0
          fi-qt-volumes = 0.
   FOR EACH b-etiqueta WHERE
            b-etiqueta.cod-estab = ob-etiqueta.cod-estab AND
            b-etiqueta.situacao >= 3 AND
            b-etiqueta.nr-ob = ob-etiqueta.nr-ob AND
            b-etiqueta.nr-carro = b-etiqueta.nr-carro NO-LOCK.
       ASSIGN fi-tot-ob = fi-tot-ob + b-etiqueta.quantidade
              fi-qt-volumes = fi-qt-volumes + 1.
   END.
   */
   DISP fi-tot-ob
        fi-qt-volumes 
        WITH FRAME {&FRAME-NAME}.

   IF (ob-etiqueta.situacao = 1 AND ob-etiqueta.tipo-ordem = 4) THEN DO.
      ASSIGN fi-dt-emissao:SCREEN-VALUE = STRING(TODAY)
             fi-hora:SCREEN-VALUE = STRING(TIME,"HH:MM")
             fi-nr-lote:SCREEN-VALUE = c-lote.

      ASSIGN fi-quantidade:SENSITIVE = YES
             fi-nr-lote:SENSITIVE = YES.

      APPLY 'ENTRY' TO fi-quantidade.
      RETURN NO-APPLY.
   END.
   ELSE DO.
      APPLY 'CHOOSE' TO bt-imprime. 

      APPLY 'entry' TO fi-num-etiqueta.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-etiqueta C-Win
ON VALUE-CHANGED OF fi-num-etiqueta IN FRAME DEFAULT-FRAME
DO:
  IF SELF:SCREEN-VALUE <> '' AND
     (SUBSTR(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) < '0' OR
      SUBSTR(SELF:SCREEN-VALUE,LENGTH(SELF:SCREEN-VALUE),1) > '9') THEN DO.
      BELL.
      APPLY 'backspace' TO SELF.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-quantidade
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-quantidade C-Win
ON LEAVE OF fi-quantidade IN FRAME DEFAULT-FRAME /* Quantidade */
DO:
   FIND corte-comerc WHERE
        corte-comerc.compr-min <= INPUT FRAME {&FRAME-NAME} fi-quantidade AND
        corte-comerc.compr-max >= INPUT FRAME {&FRAME-NAME} fi-quantidade AND
        corte-comerc.tp-embalag = i-tp-embal NO-LOCK NO-ERROR.

   IF NOT AVAIL corte-comerc THEN DO.
      MESSAGE "Metragem informada n∆o pertece a nenhum Corte Cadastrado..." VIEW-AS ALERT-BOX.
      APPLY 'entry' TO fi-quantidade.
      RETURN NO-APPLY.
   END.

   ASSIGN fi-corte-comerc:SCREEN-VALUE = corte-comerc.codigo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

ON 'return':U ANYWHERE DO.
   APPLY 'TAB' TO SELF.
   RETURN NO-APPLY.
END.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  IF c-seg-usuario = "" THEN
     ASSIGN c-seg-usuario = TRIM(OS-GETENV("username")).

  FIND FIRST ped-venda NO-LOCK NO-ERROR.
  ASSIGN c-cod-estabel = ped-venda.cod-estabel.

  FIND FIRST ob-param NO-LOCK NO-ERROR.

  APPLY 'ENTRY' TO fi-num-etiqueta.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  DISPLAY fi-num-etiqueta fi-quantidade fi-cod-estab fi-nome-estab fi-nr-ob 
          fi-dt-emissao fi-hora fi-it-codigo fi-desc-item fi-cod-refer 
          fi-desc-refer fi-peso-bruto fi-tot-ob fi-corte-comerc fi-qt-volumes 
          fi-nr-lote 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btn-importa fi-num-etiqueta bt-imprime bt-sair RECT-51 RECT-52 RECT-54 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-captura-peso C-Win 
PROCEDURE pi-captura-peso :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF SEARCH(c-prog-balanca) = ? THEN DO:
     MESSAGE "Programa que Captura o peso da Balanáa, n∆o foi encontrado..."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' TO fi-num-etiqueta IN FRAME {&FRAME-NAME}.
     RETURN 'ADM-ERROR'.
  END.

  ASSIGN c-comando = c-prog-balanca + " " + c-peso-balanca.
  OS-DELETE SILENT VALUE(c-peso-balanca).

  ASSIGN i-tempo-ini = TIME.
  DO WHILE (TIME - i-tempo-ini) < 5 AND SEARCH(c-peso-balanca) = ?:
     OS-COMMAND SILENT VALUE(c-comando).
  END.

  IF SEARCH(c-peso-balanca) = ? THEN DO.
     MESSAGE "Falha na Comunicaá∆o, n∆o foi poss°vel capturar o peso da Balanáa...."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' TO fi-num-etiqueta.
     RETURN 'ADM-ERROR'.
  END.

  INPUT FROM VALUE(c-peso-balanca) NO-ECHO.
  REPEAT:
     SET c-peso.
  END.
  INPUT CLOSE.
  
  IF INT(SUBSTR(c-peso,6,5)) = 0 THEN DO.
     MESSAGE "Peso Capturado est† Zerado, favor verificar, ...."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
     APPLY 'entry' TO fi-num-etiqueta.
     RETURN 'ADM-ERROR'.
  END.

  ASSIGN de-peso-lido = DEC(SUBSTR(c-peso,6,5)) / 10.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-opticon C-Win 
PROCEDURE pi-imp-opticon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    INPUT FROM "c:\temp\data.txt".
       REPEAT.   
           CREATE tt-arquivo.
           IMPORT DELIMITER ";" tt-arquivo.
       END.
    INPUT CLOSE.
    
    FOR EACH tt-arquivo NO-LOCK.
        IF tt-arquivo.num-etiqueta = "" THEN NEXT.
        ASSIGN fi-num-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(tt-arquivo.num-etiqueta,1,9).
        APPLY "LEAVE" TO fi-num-etiqueta.

        PAUSE 2 NO-MESSAGE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-unitech C-Win 
PROCEDURE pi-imp-unitech :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    INPUT FROM "c:\temp\form.dat".
        REPEAT.
            CREATE tt-arquivo.
            IMPORT DELIMITER ";" tt-arquivo.
        END.
    INPUT CLOSE.
    
    FOR EACH tt-arquivo NO-LOCK.
        IF tt-arquivo.num-etiqueta = "" THEN NEXT.
        ASSIGN fi-num-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTRING(tt-arquivo.num-etiqueta,7,9).
        APPLY "LEAVE" TO fi-num-etiqueta.

        PAUSE 2 NO-MESSAGE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-valida C-Win 
PROCEDURE pi-valida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR c-ok AS CHAR.
    DEF VAR c-usuarios AS CHAR.
    
    IF (ob-etiqueta.tipo-ordem = 4 AND ob-etiqueta.situacao <> 1) OR
       (ob-etiqueta.tipo-ordem <> 4 AND ob-etiqueta.situacao <> 2) THEN DO.
       /*{esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-desc-situacao}.*/
       MESSAGE "Situaá∆o da Etiqueta: " c-desc-situacao SKIP
               "Usu†rio " c-seg-usuario " sem Permiss∆o para Re-Imprimir Etiquetas" SKIP(1)
               "Favor autenticar outro usuario...."
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.

       FOR EACH usuar_grp_usuar WHERE 
                usuar_grp_usuar.cod_grp_usuar = "EP0" NO-LOCK.
           ASSIGN c-usuarios = c-usuarios + usuar_grp_usuar.cod_usuar + ','.
       END.

       RUN btb/btb910zc.p (INPUT c-usuarios, INPUT YES, INPUT YES, OUTPUT c-ok).
       IF c-ok = ? THEN 
          RETURN 'NOK'.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

