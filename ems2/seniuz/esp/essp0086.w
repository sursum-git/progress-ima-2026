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
{include/i-prgvrs.i ESSP0244 2.06.00.000}
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

DEFINE TEMP-TABLE tt-ob-etiqueta LIKE ob-etiqueta
       FIELD nova-localizacao AS CHAR
       FIELD narrativa AS CHAR FORMAT "x(80)"
       INDEX indice1 nr-sequencia DESCENDING.

DEF VAR c-situacao    AS CHAR.
DEF VAR c-etiqueta    AS CHAR.
DEF VAR c-cod-estabel AS CHAR.
DEF VAR c-arq-anexo   AS CHAR.
DEF VAR c-mensagem    AS CHAR.
DEF VAR c-doca        AS CHAR.
DEF VAR i-pag         AS INT.
DEF VAR i-lin         AS INT.
DEF VAR i-hr-inicio   AS INT.
DEF VAR de-total      AS DEC.

DEF VAR l-erro        AS LOG.
DEF VAR i-nr-seq      AS INT.
DEF VAR i-seq-leitura AS INT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Digitacao
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br-etiquetas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-ob-etiqueta

/* Definitions for BROWSE br-etiquetas                                  */
&Scoped-define FIELDS-IN-QUERY-br-etiquetas tt-ob-etiqueta.nova-localizacao tt-ob-etiqueta.num-etiqueta tt-ob-etiqueta.it-codigo tt-ob-etiqueta.cod-refer tt-ob-etiqueta.quantidade tt-ob-etiqueta.dt-emiss fn-sit-etq() tt-ob-etiqueta.localizacao tt-ob-etiqueta.narrativa   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-etiquetas tt-ob-etiqueta.nova-localizacao   
&Scoped-define ENABLED-TABLES-IN-QUERY-br-etiquetas tt-ob-etiqueta
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br-etiquetas tt-ob-etiqueta
&Scoped-define SELF-NAME br-etiquetas
&Scoped-define OPEN-QUERY-br-etiquetas RUN pi-totais. OPEN QUERY {&SELF-NAME} FOR EACH tt-ob-etiqueta NO-LOCK.
&Scoped-define TABLES-IN-QUERY-br-etiquetas tt-ob-etiqueta
&Scoped-define FIRST-TABLE-IN-QUERY-br-etiquetas tt-ob-etiqueta


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br-etiquetas}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rs-tp-leitura br-etiquetas bt-ajuda bt-ok ~
bt-cancelar bt-elimina fi-num-etiqueta bt-modifica RECT-1 RECT-44 RECT-55 ~
RECT-56 
&Scoped-Define DISPLAYED-OBJECTS tg-inv-doca fi-dt-invent rs-tp-leitura ~
fi-rua fi-operador fi-qt-etiqueta fi-docas fi-num-etiqueta 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-4 fi-operador 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-sit-etq w-digita 
FUNCTION fn-sit-etq RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR w-digita AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-ajuda 
     LABEL "Ajuda" 
     SIZE 12 BY 1.

DEFINE BUTTON bt-cancelar AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY 1.

DEFINE BUTTON bt-elimina 
     LABEL "Eliminar Leitura" 
     SIZE 12 BY 1.

DEFINE BUTTON bt-modifica 
     LABEL "Modificar Leitura" 
     SIZE 13 BY 1.

DEFINE BUTTON bt-ok AUTO-GO 
     LABEL "Confirma Localizaá∆o" 
     SIZE 21.86 BY 1.

DEFINE BUTTON bt-processa 
     IMAGE-UP FILE "image/im-vapra.bmp":U
     LABEL "Button 1" 
     SIZE 7.72 BY 1.13.

DEFINE BUTTON fi-botao 
     LABEL "/" 
     SIZE 1.29 BY .96
     FONT 6.

DEFINE VARIABLE fi-docas AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 5.72 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE fi-dt-invent AS DATE FORMAT "99/99/9999":U 
     LABEL "Data Invent†rio" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .88 NO-UNDO.

DEFINE VARIABLE fi-num-etiqueta AS CHARACTER FORMAT "x(9)":U 
     LABEL "Etiqueta" 
     VIEW-AS FILL-IN 
     SIZE 14.14 BY .88
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-operador AS CHARACTER FORMAT "X(12)":U 
     LABEL "Usu†rio" 
     VIEW-AS FILL-IN 
     SIZE 14.14 BY .88 NO-UNDO.

DEFINE VARIABLE fi-qt-etiqueta AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Qt TOTAL Etiquetas" 
     VIEW-AS FILL-IN 
     SIZE 7.43 BY .88 NO-UNDO.

DEFINE VARIABLE fi-rua AS CHARACTER FORMAT "X(3)":U 
     LABEL "Rua/Doca:" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .88
     FONT 1 NO-UNDO.

DEFINE VARIABLE rs-tp-leitura AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Localizaá∆o", 1,
"Conferància de Docas", 2,
"Invent†rio", 3
     SIZE 18 BY 2.5 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 111 BY 1.38
     BGCOLOR 7 .

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 111 BY 3.92.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50.72 BY 3.25.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56.57 BY 3.25.

DEFINE VARIABLE tg-inv-doca AS LOGICAL INITIAL no 
     LABEL "Inventariar TODAS Etiquetas da Doca" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .83 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br-etiquetas FOR 
      tt-ob-etiqueta SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-etiquetas w-digita _FREEFORM
  QUERY br-etiquetas DISPLAY
      tt-ob-etiqueta.nova-localizacao COLUMN-LABEL "Localiz LIDA"  FORMAT "999/999"
      tt-ob-etiqueta.num-etiqueta     COLUMN-LABEL "Etiqueta"
      tt-ob-etiqueta.it-codigo        COLUMN-LABEL "Item"          WIDTH 8
      tt-ob-etiqueta.cod-refer        COLUMN-LABEL "Ref"           WIDTH 5
      tt-ob-etiqueta.quantidade       COLUMN-LABEL "Quantidade"
      tt-ob-etiqueta.dt-emiss         COLUMN-LABEL "Dt.Estoque"
      fn-sit-etq()                    COLUMN-LABEL "Situacao"      FORMAT "x(12)"
      tt-ob-etiqueta.localizacao      COLUMN-LABEL "Loaliz Atual"  FORMAT "999/999"
      tt-ob-etiqueta.narrativa        COLUMN-LABEL "Mensagens"     WIDTH 40
ENABLE
      tt-ob-etiqueta.nova-localizacao
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 110 BY 12.79
         FONT 1
         TITLE "Etiquetas Lidas".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     tg-inv-doca AT ROW 1.79 COL 79.29 WIDGET-ID 36
     fi-dt-invent AT ROW 3.58 COL 38 COLON-ALIGNED WIDGET-ID 34
     rs-tp-leitura AT ROW 1.96 COL 10.43 NO-LABEL WIDGET-ID 20
     fi-botao AT ROW 2.63 COL 71.14 WIDGET-ID 14
     fi-rua AT ROW 2.63 COL 62.57 COLON-ALIGNED WIDGET-ID 8
     fi-operador AT ROW 3.63 COL 62.57 COLON-ALIGNED
     bt-processa AT ROW 3.38 COL 102
     br-etiquetas AT ROW 5.25 COL 1.57
     fi-qt-etiqueta AT ROW 18.25 COL 14.57 COLON-ALIGNED
     bt-ajuda AT ROW 19.71 COL 99.29
     bt-ok AT ROW 19.71 COL 2.14
     bt-cancelar AT ROW 19.71 COL 24.43
     bt-elimina AT ROW 18.25 COL 99.57
     fi-docas AT ROW 2.67 COL 71 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     fi-num-etiqueta AT ROW 1.63 COL 62.57 COLON-ALIGNED WIDGET-ID 16
     bt-modifica AT ROW 18.25 COL 86 WIDGET-ID 32
     " Tipo Leitura" VIEW-AS TEXT
          SIZE 9 BY .83 AT ROW 1 COL 4.72 WIDGET-ID 24
     RECT-1 AT ROW 19.5 COL 1
     RECT-44 AT ROW 1.08 COL 1
     RECT-55 AT ROW 1.42 COL 2.29 WIDGET-ID 28
     RECT-56 AT ROW 1.42 COL 54 WIDGET-ID 30
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 111.29 BY 19.88
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
         TITLE              = "Localizaá∆o de Estoque de Rolos"
         HEIGHT             = 19.88
         WIDTH              = 111.29
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
/* BROWSE-TAB br-etiquetas bt-processa F-Main */
/* SETTINGS FOR BUTTON bt-processa IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       bt-processa:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON fi-botao IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-docas IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-dt-invent IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-operador IN FRAME F-Main
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN fi-qt-etiqueta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-rua IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tg-inv-doca IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(w-digita)
THEN w-digita:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-etiquetas
/* Query rebuild information for BROWSE br-etiquetas
     _START_FREEFORM
RUN pi-totais.
OPEN QUERY {&SELF-NAME} FOR EACH tt-ob-etiqueta NO-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE br-etiquetas */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME w-digita
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON END-ERROR OF w-digita /* Localizaá∆o de Estoque de Rolos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-digita w-digita
ON WINDOW-CLOSE OF w-digita /* Localizaá∆o de Estoque de Rolos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-etiquetas
&Scoped-define SELF-NAME br-etiquetas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etiquetas w-digita
ON DELETE-CHARACTER OF br-etiquetas IN FRAME F-Main /* Etiquetas Lidas */
DO:
  APPLY 'choose' TO bt-elimina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-etiquetas w-digita
ON ROW-LEAVE OF br-etiquetas IN FRAME F-Main /* Etiquetas Lidas */
DO:
  ASSIGN tt-ob-etiqueta.nova-localizacao:READ-ONLY IN BROWSE br-etiquetas = YES.

  FIND ob-localiz WHERE
       ob-localiz.cod-localiz = tt-ob-etiqueta.nova-localizacao NO-LOCK NO-ERROR.
  IF AVAIL ob-localiz THEN
     ASSIGN tt-ob-etiqueta.narrativa = "".

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
ON CHOOSE OF bt-elimina IN FRAME F-Main /* Eliminar Leitura */
DO:
   IF AVAIL tt-ob-etiqueta THEN DO:
      DELETE tt-ob-etiqueta.
      {&OPEN-QUERY-br-etiquetas}
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-modifica
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-modifica w-digita
ON CHOOSE OF bt-modifica IN FRAME F-Main /* Modificar Leitura */
DO:
    ASSIGN tt-ob-etiqueta.nova-localizacao:READ-ONLY IN BROWSE br-etiquetas = NO.
    APPLY 'ENTRY' TO tt-ob-etiqueta.nova-localizacao.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok w-digita
ON CHOOSE OF bt-ok IN FRAME F-Main /* Confirma Localizaá∆o */
DO:
   ASSIGN INPUT FRAME {&FRAME-NAME} rs-tp-leitura.

   FIND FIRST tt-ob-etiqueta WHERE
              tt-ob-etiqueta.narrativa <> '' NO-LOCK NO-ERROR.
   IF AVAIL tt-ob-etiqueta THEN DO.
      MESSAGE 'Existem Etiquetas com Mensagem de Erro, Favor Verificar...'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
   END.

   CASE rs-tp-leitura.
       WHEN 1 THEN DO.
          FOR EACH tt-ob-etiqueta NO-LOCK.
              FIND ob-etiqueta WHERE 
                   ob-etiqueta.cod-estabel = tt-ob-etiqueta.cod-estabel AND
                   ob-etiqueta.num-etiqueta = tt-ob-etiqueta.num-etiqueta SHARE-LOCK NO-ERROR.
              IF AVAIL ob-etiqueta THEN 
                 ASSIGN ob-etiqueta.localizacao = tt-ob-etiqueta.nova-localizacao
                        ob-etiqueta.situacao = IF ob-etiqueta.situacao <> 4
                                               THEN 3 ELSE 4. 
          END.
       END.
       WHEN 2 THEN DO.
          FIND FIRST tt-ob-etiqueta NO-LOCK NO-ERROR.
          IF NOT AVAIL tt-ob-etiqueta THEN DO.
             MESSAGE 'Deseja Realmente LIMPAR a Localizaá∆o ?'
                 VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE l-limpa AS LOGICAL.

             IF l-limpa THEN DO.
                FOR EACH ob-etiqueta WHERE
                         ob-etiqueta.cod-estabel = c-cod-estabel AND
                         ob-etiqueta.localiz = c-doca SHARE-LOCK.

                    IF ob-etiqueta.situacao <> 3 AND
                       ob-etiqueta.situacao <> 4 THEN NEXT. 

                    ASSIGN ob-etiqueta.localiz = ''.
                END.
             END.
          END.
          ELSE DO.
              /* Limpa Localizaá∆o das Docas Lidas */
              FOR EACH tt-ob-etiqueta BREAK BY tt-ob-etiqueta.nova-localizacao.
                  IF FIRST-OF(tt-ob-etiqueta.nova-localizacao) THEN DO.
                     FOR EACH ob-etiqueta WHERE
                              ob-etiqueta.cod-estabel = c-cod-estabel AND
                              ob-etiqueta.localiz = tt-ob-etiqueta.nova-localizacao SHARE-LOCK.
    
                          IF ob-etiqueta.situacao <> 3 AND
                             ob-etiqueta.situacao <> 4 THEN NEXT. 
    
                          ASSIGN ob-etiqueta.localiz = ''.
                      END.
                  END.
              END.

              /* Localiza Etiquetas Lidas */
              FOR EACH tt-ob-etiqueta NO-LOCK.
                  FIND ob-etiqueta WHERE 
                       ob-etiqueta.cod-estabel = tt-ob-etiqueta.cod-estabel AND
                       ob-etiqueta.num-etiqueta = tt-ob-etiqueta.num-etiqueta SHARE-LOCK NO-ERROR.
                  IF AVAIL ob-etiqueta THEN DO.
                     ASSIGN ob-etiqueta.localizacao = tt-ob-etiqueta.nova-localizacao.
    
                     ASSIGN SUBSTR(ob-etiqueta.char-1,50,200) = "ROLO LOCALIZAÄ«O DOCA :" + ob-etiqueta.localizacao + "USUARIO :" + fi-operador:SCREEN-VALUE + "DATA :" + STRING(TODAY) + "HORA :" + STRING(TIME,"HH:MM:SS"). 
    
                     /* Volta Etiqueta para Estoque */
                     IF ob-etiqueta.situacao <> 3 AND
                        ob-etiqueta.situacao <> 4 THEN
                        ASSIGN ob-etiqueta.situacao = 3.  
                  END.
              END.
          END.
       END.
       WHEN 3 THEN DO.   /* Invent†rio */ 
          FOR EACH tt-ob-etiqueta NO-LOCK.
              FIND ob-etiqueta WHERE 
                   ob-etiqueta.cod-estabel = tt-ob-etiqueta.cod-estabel AND
                   ob-etiqueta.num-etiqueta = tt-ob-etiqueta.num-etiqueta SHARE-LOCK NO-ERROR.
              IF NOT AVAIL ob-etiqueta THEN NEXT.

              FIND ITEM WHERE
                   ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

              FIND LAST inv-acab WHERE
                        inv-acab.cod-estabel = c-cod-estabel AND
                        inv-acab.data-invent = INPUT FRAME {&FRAME-NAME} fi-dt-invent 
                        USE-INDEX indice1 NO-LOCK NO-ERROR.
        
              ASSIGN i-nr-seq = IF AVAIL inv-acab
                                THEN inv-acab.seq + 1
                                ELSE 0.
    
              ASSIGN i-nr-seq = i-nr-seq + 1.
              CREATE inv-acab.
              ASSIGN inv-acab.cod-estabel  = ob-etiqueta.cod-estabel
                     inv-acab.it-codigo    = ob-etiqueta.it-codigo
                     inv-acab.cod-refer    = ob-etiqueta.cod-refer
                     inv-acab.data-invent  = INPUT FRAME {&FRAME-NAME} fi-dt-invent
                     inv-acab.data-trans   = TODAY
                     inv-acab.hora-trans   = STRING(TIME,"HH:MM")
                     inv-acab.localiz      = tt-ob-etiqueta.nova-localizacao
                     inv-acab.seq          = i-nr-seq
                     inv-acab.qtd-inv      = ob-etiqueta.quantidade
                     inv-acab.lote         = ob-etiqueta.cod-refer
                     inv-acab.num-etiqueta = ob-etiqueta.num-etiqueta
                     inv-acab.usuario      = fi-operador:SCREEN-VALUE
                     inv-acab.origem       = 2
                     inv-acab.un           = item.un
                     inv-acab.situacao     = 1.
        
              FIND CURRENT ob-etiqueta SHARE-LOCK NO-ERROR.
              ASSIGN ob-etiqueta.localizacao = tt-ob-etiqueta.nova-localizacao. 
              FIND CURRENT ob-etiqueta NO-LOCK NO-ERROR.
          END.
          MESSAGE 'Inventario da Doca ' c-doca ' Efetuado...'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
       END.
   END CASE.

   /*
   MESSAGE 'Etiquetas Processadas'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

   FOR EACH tt-ob-etiqueta.
       DELETE tt-ob-etiqueta.
   END.
   ASSIGN c-doca = "".

   ASSIGN fi-rua:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
          fi-docas:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
          fi-num-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

   APPLY 'ENTRY' TO fi-num-etiqueta.
   RETURN NO-APPLY.
   */

   APPLY 'CLOSE':U TO THIS-PROCEDURE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-processa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-processa w-digita
ON CHOOSE OF bt-processa IN FRAME F-Main /* Button 1 */
DO:
  IF LENGTH(fi-num-etiqueta:INPUT-VALUE IN FRAME {&FRAME-NAME}) = 6 THEN DO: /* Etiqueta Ç RUA/DOCA */
     ASSIGN c-doca = fi-num-etiqueta:INPUT-VALUE IN FRAME {&FRAME-NAME}.

     ASSIGN fi-rua:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTR(c-doca,1,3)
            fi-docas:SCREEN-VALUE IN FRAME {&FRAME-NAME} = SUBSTR(c-doca,4,3)
            fi-num-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

     APPLY 'entry' TO fi-num-etiqueta.
     RETURN 'ADM-ERROR':U.
  END.
  
  IF c-doca = "" THEN DO.
     MESSAGE 'Favor Ler Primeiro a Etiqueta de Rua / Doca'
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY 'entry' TO fi-num-etiqueta.
     RETURN 'ADM-ERROR':U.
  END.

  FIND FIRST inv-acab WHERE
             inv-acab.cod-estabel  = c-cod-estabel AND
             inv-acab.data-invent  = INPUT FRAME {&FRAME-NAME} fi-dt-invent  AND 
             inv-acab.num-etiqueta = fi-num-etiqueta:INPUT-VALUE IN FRAME {&FRAME-NAME}
             NO-LOCK NO-ERROR.

  IF AVAIL inv-acab THEN DO.
     CREATE tt-ob-etiqueta.
     ASSIGN tt-ob-etiqueta.cod-estabel = c-cod-estabel
            tt-ob-etiqueta.num-etiqueta = fi-num-etiqueta:INPUT-VALUE IN FRAME {&FRAME-NAME}
            tt-ob-etiqueta.nova-localizacao = c-doca
            tt-ob-etiqueta.narrativa = "Etiqueta j† Inventariada na Doca: " + inv-acab.localizacao. 
  END.
  ELSE DO.
      FIND FIRST tt-ob-etiqueta WHERE
                 tt-ob-etiqueta.cod-estabel  = c-cod-estabel AND
                 tt-ob-etiqueta.num-etiqueta = INTEGER(INPUT FRAME {&FRAME-NAME} fi-num-etiqueta)
                 NO-LOCK NO-ERROR.
    
      IF NOT AVAIL tt-ob-etiqueta THEN DO:
         FIND ob-etiqueta WHERE
              ob-etiqueta.cod-estabel  = c-cod-estabel AND
              ob-etiqueta.num-etiqueta = fi-num-etiqueta:INPUT-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
         IF AVAIL ob-etiqueta THEN DO.
            CREATE tt-ob-etiqueta.
            BUFFER-COPY ob-etiqueta TO tt-ob-etiqueta.
            ASSIGN tt-ob-etiqueta.nova-localizacao = c-doca.
         END.
         ELSE DO.
            CREATE tt-ob-etiqueta.
            ASSIGN tt-ob-etiqueta.cod-estabel = c-cod-estabel
                   tt-ob-etiqueta.num-etiqueta = fi-num-etiqueta:INPUT-VALUE IN FRAME {&FRAME-NAME}
                   tt-ob-etiqueta.nova-localizacao = c-doca
                   tt-ob-etiqueta.narrativa = "ETIQUETA N«O Cadastrada".
         END.
      END.
      ELSE DO.
         CREATE tt-ob-etiqueta.
         ASSIGN tt-ob-etiqueta.cod-estabel = c-cod-estabel
                tt-ob-etiqueta.num-etiqueta = fi-num-etiqueta:INPUT-VALUE IN FRAME {&FRAME-NAME}
                tt-ob-etiqueta.nova-localizacao = c-doca
                tt-ob-etiqueta.narrativa = "Etiqueta FOI Lida duas ou mais Vezes".
      END.
  END.

  IF INT(c-doca) <= 0 THEN 
     ASSIGN tt-ob-etiqueta.narrativa = tt-ob-etiqueta.narrativa + " // " + "RUA/DOCA n∆o foi Lida".
  ELSE DO.
     FIND ob-localiz WHERE
          ob-localiz.cod-localiz = c-doca NO-LOCK NO-ERROR.
     IF NOT AVAIL ob-localiz THEN
        ASSIGN tt-ob-etiqueta.narrativa = tt-ob-etiqueta.narrativa + " // " + "RUA/DOCA, N∆o Est† Cadastrado na Tabela de LOCALIZAÄ«O".
  END.
  
  ASSIGN i-seq-leitura = i-seq-leitura + 1.
  ASSIGN tt-ob-etiqueta.nr-sequencia = i-seq-leitura.
  
  ASSIGN fi-num-etiqueta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
  {&OPEN-QUERY-br-etiquetas}

  APPLY 'entry' TO fi-num-etiqueta.
  RETURN 'ADM-ERROR':U.                                                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-dt-invent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-dt-invent w-digita
ON LEAVE OF fi-dt-invent IN FRAME F-Main /* Data Invent†rio */
DO:
   APPLY 'ENTRY' TO fi-num-etiqueta.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi-num-etiqueta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-etiqueta w-digita
ON LEAVE OF fi-num-etiqueta IN FRAME F-Main /* Etiqueta */
DO:
   IF SELF:SCREEN-VALUE = '' THEN DO.
       APPLY 'entry' TO fi-num-etiqueta.
       RETURN 'ADM-ERROR':U.
   END.

   APPLY 'CHOOSE' TO bt-processa. 

   APPLY 'entry' TO fi-num-etiqueta.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-etiqueta w-digita
ON RETURN OF fi-num-etiqueta IN FRAME F-Main /* Etiqueta */
DO:
   APPLY 'TAB' TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-num-etiqueta w-digita
ON VALUE-CHANGED OF fi-num-etiqueta IN FRAME F-Main /* Etiqueta */
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


&Scoped-define SELF-NAME fi-operador
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi-operador w-digita
ON RETURN OF fi-operador IN FRAME F-Main /* Usu†rio */
DO:
  APPLY 'choose' TO bt-processa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rs-tp-leitura
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rs-tp-leitura w-digita
ON VALUE-CHANGED OF rs-tp-leitura IN FRAME F-Main
DO:
   ASSIGN tg-inv-doca:SENSITIVE = NO
          fi-dt-invent:SENSITIVE = NO
          fi-dt-invent:SCREEN-VALUE = ''.
          
   CASE SELF:SCREEN-VALUE.
       WHEN '1' THEN DO.
            ASSIGN bt-ok:LABEL = "Confirma LOCALIZAÄ«O".
            APPLY 'ENTRY' TO fi-num-etiqueta.
       END.
       WHEN '2' THEN DO.
            ASSIGN bt-ok:LABEL = "Confirma CONFER“NCIA".
            APPLY 'ENTRY' TO fi-num-etiqueta.
       END.
       WHEN '3' THEN DO.
            ASSIGN bt-ok:LABEL = "Confirma INVENTµRIO"
                   fi-dt-invent:SENSITIVE = YES
                   tg-inv-doca:SENSITIVE = YES.

            ASSIGN fi-dt-invent:SCREEN-VALUE = '19/12/2015'.
            APPLY 'ENTRY' TO fi-dt-invent.
       END.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tg-inv-doca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tg-inv-doca w-digita
ON VALUE-CHANGED OF tg-inv-doca IN FRAME F-Main /* Inventariar TODAS Etiquetas da Doca */
DO:
   IF c-doca = "" THEN DO.
      MESSAGE 'Favor Ler Primeiro a Etiqueta de Rua / Doca'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'entry' TO fi-num-etiqueta.
      RETURN 'ADM-ERROR':U.
   END.
 
   EMPTY TEMP-TABLE tt-ob-etiqueta.
   IF SELF:SCREEN-VALUE = 'YES' THEN DO.
      FOR EACH ob-etiqueta WHERE
               ob-etiqueta.cod-estabel = c-cod-estabel AND
               ob-etiqueta.situacao >= 3 AND
               ob-etiqueta.situacao <= 4 AND
               ob-etiqueta.localiz = c-doca NO-LOCK BY ob-etiqueta.num-etiqueta.
     
          FIND FIRST inv-acab WHERE
                     inv-acab.cod-estabel  = c-cod-estabel AND
                     inv-acab.data-invent  = INPUT FRAME {&FRAME-NAME} fi-dt-invent  AND 
                     inv-acab.num-etiqueta = ob-etiqueta.num-etiqueta
                     NO-LOCK NO-ERROR.

          IF AVAIL inv-acab THEN DO.
             CREATE tt-ob-etiqueta.
             ASSIGN tt-ob-etiqueta.cod-estabel = c-cod-estabel
                    tt-ob-etiqueta.num-etiqueta = ob-etiqueta.num-etiqueta
                    tt-ob-etiqueta.nova-localizacao = c-doca
                    tt-ob-etiqueta.narrativa = "Etiqueta j† Inventariada na Doca: " + inv-acab.localizacao. 
          END.

          FIND FIRST tt-ob-etiqueta WHERE
                     tt-ob-etiqueta.cod-estabel  = ob-etiqueta.cod-estabel AND
                     tt-ob-etiqueta.num-etiqueta = ob-etiqueta.num-etiqueta
                     NO-LOCK NO-ERROR.
     
          IF NOT AVAIL tt-ob-etiqueta THEN DO:
             CREATE tt-ob-etiqueta.
             BUFFER-COPY ob-etiqueta TO tt-ob-etiqueta.
             ASSIGN tt-ob-etiqueta.nova-localizacao = c-doca.
          END.
          ELSE DO.
             CREATE tt-ob-etiqueta.
             ASSIGN tt-ob-etiqueta.cod-estabel = c-cod-estabel
                    tt-ob-etiqueta.num-etiqueta = ob-etiqueta.num-etiqueta
                    tt-ob-etiqueta.nova-localizacao = c-doca
                    tt-ob-etiqueta.narrativa = "Etiqueta DUPLICADA na DOCA".
          END.
      END.
   END.
   {&OPEN-QUERY-br-etiquetas}
 
   APPLY 'entry' TO fi-num-etiqueta.
   RETURN 'ADM-ERROR':U.                                                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK w-digita 


/* ***************************  Main Block  *************************** */
ASSIGN tt-ob-etiqueta.nova-localizacao:READ-ONLY IN BROWSE br-etiquetas = YES.

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

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
  DISPLAY tg-inv-doca fi-dt-invent rs-tp-leitura fi-rua fi-operador 
          fi-qt-etiqueta fi-docas fi-num-etiqueta 
      WITH FRAME F-Main IN WINDOW w-digita.
  ENABLE rs-tp-leitura br-etiquetas bt-ajuda bt-ok bt-cancelar bt-elimina 
         fi-num-etiqueta bt-modifica RECT-1 RECT-44 RECT-55 RECT-56 
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
 ASSIGN c-arq-anexo = SESSION:TEMP-DIRECTORY +  fi-operador:SCREEN-VALUE IN FRAME {&FRAME-NAME} + ".TXT".
 OUTPUT TO VALUE(c-arq-anexo) CONVERT SOURCE "ibm850".
 ASSIGN i-lin    = 99
        de-total =  0
        i-pag    =  1.

 FOR EACH tt-ob-etiqueta WHERE
          tt-ob-etiqueta.quantidade > 0 NO-LOCK.
     IF i-lin > 62 THEN DO:
        RUN pi-imp-cabec.
        ASSIGN i-lin = 7.
     END.
     PUT tt-ob-etiqueta.num-etiqueta     FORMAT "999999999"   AT  1
         tt-ob-etiqueta.it-codigo        FORMAT "x(6)"        AT 11
         tt-ob-etiqueta.cod-refer        FORMAT "99-9999-9"   AT 18
         tt-ob-etiqueta.quantidade       FORMAT ">>>,>>9.99"  AT 29
         tt-ob-etiqueta.nr-lote          FORMAT "x(2)"        AT 41
         tt-ob-etiqueta.situacao         FORMAT "9"           AT 46
         tt-ob-etiqueta.peso-bruto       FORMAT ">>>,>>9.99"  AT 49
         tt-ob-etiqueta.dt-emis       FORMAT "99/99/9999"  AT 60
         tt-ob-etiqueta.nova-localizacao FORMAT "999/999"     AT 73
         tt-ob-etiqueta.localizacao      FORMAT "999/999"     AT 86
         tt-ob-etiqueta.resp-revisao     FORMAT "x(12)"       AT 96.
     ASSIGN i-lin    = i-lin + 1
            de-total = de-total + tt-ob-etiqueta.peso-bruto.
 END.
 IF i-lin <> 99 THEN DO:
    PUT   "----------"                  AT 49 SKIP
          "TOTAL . . ."                 AT 29
          de-total FORMAT ">>>,>>9.99"  AT 49
          "TOTAL ETIQUETAS LIDAS: "     AT 70
          fi-qt-etiqueta FORMAT ">>>9"  AT 93.
 END.
 OUTPUT CLOSE.

 ASSIGN c-mensagem = "Em anexo, arquivo com as localizac‰es efetuada por " + fi-operador:SCREEN-VALUE IN FRAME {&FRAME-NAME} +
                     "." + CHR(13) + CHR(13) +
                     "        Expediá∆o Paraopeba" + CHR(13) +
                     "TEAR TEXTIL INDUSTRIA COMERCIO LTDA".

 RUN esapi/esapi002.p (INPUT "teartextil@teartextil.com.br", /* e-mail remetente*/
                       INPUT "fabio.lanza@teartextil.com.br", /* e-mail destino */ 
                       INPUT "LOCALIZACAO ETIQUETAS EM " + STRING(TODAY) ,           /* Assunto        */
                       INPUT c-mensagem,   /* Mensagem      */
                       INPUT c-arq-anexo,  /* arquivo anexo */
                       INPUT YES).         /* Mostra Erros  */

 OS-DELETE VALUE(c-arq-anexo). 

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

  {utp/ut9000.i "ESSP0086" "2.06.00.000"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST ped-venda NO-LOCK NO-ERROR.
  ASSIGN c-cod-estabel = ped-venda.cod-estabel.

  ASSIGN fi-operador:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = c-seg-usuario.

  APPLY 'entry' TO fi-num-etiqueta IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-imp-cabec w-digita 
PROCEDURE pi-imp-cabec :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  PUT "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA."  AT   1
      "DATA: "                                  AT  68
      STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  74
      "HORA: "                                  AT 114
      STRING(TIME,"hh:mm:ss")                   AT 120
      SKIP(1).



  PUT "RELAT‡RIO DA LOCALIZAÄ«O EFETUADA EM: " + STRING(TODAY) +
      " POR " + fi-OPERADOR:SCREEN-VALUE IN FRAME {&FRAME-NAME}  FORMAT "x(60)" AT 40 
      SKIP.


  PUT " Etiqueta item   Referància Quantidade Lote Sit Peso Bruto Dt.Estoque Localizacao Localiz. Ant Revisor" AT 1.
  PUT "--------- ------ ---------- ---------- ---- --- ---------- ---------- ----------- ------------ ------------" AT 1.
  ASSIGN i-pag = i-pag + 1.                                                                              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pi-totais w-digita 
PROCEDURE pi-totais :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN fi-qt-etiqueta = 0.
    FOR EACH tt-ob-etiqueta NO-LOCK.
        ASSIGN fi-qt-etiqueta = fi-qt-etiqueta + 1.
    END.
    DISP fi-qt-etiqueta
         WITH FRAME {&FRAME-NAME}.

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
  {src/adm/template/snd-list.i "tt-ob-etiqueta"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-sit-etq w-digita 
FUNCTION fn-sit-etq RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  {esinc/i-dsrb.i tt-ob-etiqueta.situacao tt-ob-etiqueta.situacao c-situacao}  

  ASSIGN c-situacao = 'Estoque'.
  RETURN c-situacao.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

