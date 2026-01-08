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
{include/i-prgvrs.i ESFT099 9.99.99.999}

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cCaminhoXML             AS CHARACTER FORMAT 'X(200)'  NO-UNDO.
DEFINE VARIABLE lSemParam               AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lXmlExiste              AS LOGICAL     NO-UNDO.
DEFINE VARIABLE vlIcms                  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE vlIcmsOu                AS DECIMAL     NO-UNDO.
DEFINE VARIABLE vlIcmsNt                AS DECIMAL     NO-UNDO.
DEFINE VARIABLE vlDespesas              AS DECIMAL     NO-UNDO.
DEFINE VARIABLE vlPIs                   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE vlCofins                AS DECIMAL     NO-UNDO.
DEFINE VARIABLE vlPIsNota               AS DECIMAL     NO-UNDO.
DEFINE VARIABLE vlCofinsNota            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iPosicaoPIS             AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPosicaoCOFINS          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iQtPosicaoPIS           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iQtPosicaoCOFINS        AS INTEGER     NO-UNDO.
DEFINE VARIABLE dPercPIS                AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dPercCOFINS             AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dBaseCalc               AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dTotBaseCalc            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iSinal                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE cArquivoXml             AS CHARACTER FORMAT 'x(200)'   NO-UNDO.
DEFINE VARIABLE cNotaRef                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE daDataRef               AS DATE  NO-UNDO.
DEFINE VARIABLE cChaveRef               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSerieRef               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iEmitenteRef            AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDescEmitRef            AS CHARACTER   NO-UNDO FORMAT 'x(30)'.
DEFINE VARIABLE cNatOPeracao            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cListaNatOPeracao       AS CHARACTER FORMAT 'x(200)'  NO-UNDO.
DEFINE VARIABLE daliqCOFINS             AS DECIMAL     NO-UNDO.
DEFINE TEMP-TABLE tt
      FIELD cod-estabel                 LIKE nota-fiscal.cod-estabel
      FIELD nr-nota-fis                 LIKE nota-fiscal.nr-nota-fis
      FIELD serie                       LIKE nota-fiscal.serie
      FIELD especie                     AS CHAR
      FIELD cod-emitente                LIKE nota-fiscal.cod-emitente
      FIELD nome-abrev                  LIKE emitente.nome-abrev
      FIELD dt-emis-nota                LIKE nota-fiscal.dt-emis-nota FORMAT '99/99/9999'
      FIELD nat-operacao                LIKE nota-fiscal.nat-operacao
      FIELD cod-cfop                    LIKE natur-oper.cod-cfop
      FIELD nr-pedcli                   LIKE nota-fiscal.nr-pedcli
      FIELD cgc                         LIKE emitente.cgc
      FIELD cidade                      LIKE emitente.cidade
      FIELD estado                      LIKE emitente.estado
      FIELD log-nf-cancel               AS CHAR
      FIELD des-idi-sit-nf-eletro        AS CHAR
      FIELD cod-chave-aces-nf-eletro     LIKE nota-fiscal.cod-chave-aces-nf-eletro
      FIELD des-idi-forma-emis-nf-eletro AS CHAR
      FIELD vl-tot-nota                 LIKE nota-fiscal.vl-tot-nota
      FIELD vl-mercadoria               LIKE nota-fiscal.vl-mercad
      FIELD vl-seguro                   LIKE nota-fiscal.vl-seguro
      FIELD vl-embalagem                LIKE nota-fiscal.vl-embalagem
      FIELD vl-frete                    LIKE nota-fiscal.vl-frete
      FIELD vl-icms                     AS DECIMAL
      FIELD vl-icms-ou                  AS DECIMAL
      FIELD vl-icms-nt                  AS DECIMAL
      /*FIELD vl-despesas                 AS DECIMAL*/
      FIELD vl-base-pis-cofins          AS DECIMAL
      /*FIELD perc-pis                    AS DECIMAL*/
      FIELD vl-pis                      AS DECIMAL
      /*FIELD perc-cofins                 AS DECIMAL*/
      FIELD vl-cofins                   AS DECIMAL
      FIELD log-xml-existe              AS CHAR
      FIELD log-gera-dp                 AS CHAR
      FIELD notaref                     AS CHAR
      FIELD serieRef                    AS CHAR
      FIELD emitenteRef                 AS INT
      FIELD descEmitRef                 AS CHAR
      FIELD dataref                     AS CHAR FORMAT 'x(12)'
      FIELD chaveREF                    AS CHAR 
      FIELD rRowid                      AS ROWID
      .

DEFINE TEMP-TABLE ttEstab
        FIELD codEstabel AS CHAR.

DEFINE TEMP-TABLE ttItens
    FIELD rRowidNota    AS ROWID
    FIELD sequencia     AS INT
    FIELD itCodigo      AS CHAR FORMAT 'x(20)'
    FIELD vlBasePis     AS DECIMAL
    FIELD vlBaseCofins  AS DECIMAL
    FIELD percPis       AS DECIMAL
    FIELD percCofins    AS DECIMAL
    FIELD vlPis         AS DECIMAL
    FIELD vlCofins      AS DECIMAL
    FIELD origem        AS CHAR
    FIELD descItem                    AS CHAR FORMAT 'X(100)'
    FIELD codRefer      AS CHAR
    FIELD percPisExt    AS DECIMAL
    FIELD percCofinsExt AS DECIMAL
    INDEX unico AS UNIQUE rRowidnota sequencia origem .





{utp/ut-glob.i}

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
&Scoped-Define ENABLED-OBJECTS rt-button fi_estab fi_serie_ini fi_serie_fim ~
fi_nf_ini fi_nf_fim fi_cliente_ini fi_cliente_fim dt_emissao_ini ~
dt_emissao_fim tg_of tg_re bt_executar 
&Scoped-Define DISPLAYED-OBJECTS fi_estab fi_serie_ini fi_serie_fim ~
fi_nf_ini fi_nf_fim fi_cliente_ini fi_cliente_fim dt_emissao_ini ~
dt_emissao_fim tg_of tg_re 

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


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-exihel AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt_executar 
     LABEL "Executar" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE dt_emissao_fim AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999 
     LABEL "At‚" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE dt_emissao_ini AS DATE FORMAT "99/99/9999":U INITIAL 01/01/01 
     LABEL "Dt.Emissao De" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fi_cliente_fim AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 999999 
     LABEL "At‚" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fi_cliente_ini AS INTEGER FORMAT ">,>>>,>>9":U INITIAL 0 
     LABEL "Cliente De" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fi_estab AS CHARACTER FORMAT "X(3)":U 
     LABEL "Estab" 
     VIEW-AS FILL-IN 
     SIZE 7.86 BY 1 NO-UNDO.

DEFINE VARIABLE fi_nf_fim AS CHARACTER FORMAT "X(12)":U INITIAL "9999999" 
     LABEL "At‚" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fi_nf_ini AS CHARACTER FORMAT "X(12)":U 
     LABEL "NF De" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fi_serie_fim AS CHARACTER FORMAT "X(5)":U INITIAL "zzz" 
     LABEL "At‚" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE fi_serie_ini AS CHARACTER FORMAT "X(5)":U 
     LABEL "S‚rie" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE RECTANGLE rt-button
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 89.72 BY 1.46
     BGCOLOR 7 .

DEFINE VARIABLE tg_of AS LOGICAL INITIAL no 
     LABEL "Buscar Registros OF" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE tg_re AS LOGICAL INITIAL yes 
     LABEL "Buscar Registros RE" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-cad
     fi_estab AT ROW 3.75 COL 18.14 COLON-ALIGNED WIDGET-ID 2
     fi_serie_ini AT ROW 5.04 COL 18.14 COLON-ALIGNED WIDGET-ID 6
     fi_serie_fim AT ROW 5.04 COL 54.14 COLON-ALIGNED WIDGET-ID 8
     fi_nf_ini AT ROW 6.38 COL 18.14 COLON-ALIGNED WIDGET-ID 10
     fi_nf_fim AT ROW 6.38 COL 54.14 COLON-ALIGNED WIDGET-ID 12
     fi_cliente_ini AT ROW 7.71 COL 18.29 COLON-ALIGNED WIDGET-ID 18
     fi_cliente_fim AT ROW 7.71 COL 54.29 COLON-ALIGNED WIDGET-ID 20
     dt_emissao_ini AT ROW 9.08 COL 18 COLON-ALIGNED WIDGET-ID 22
     dt_emissao_fim AT ROW 9.08 COL 54 COLON-ALIGNED WIDGET-ID 24
     tg_of AT ROW 10.33 COL 20 WIDGET-ID 38
     tg_re AT ROW 10.33 COL 39.43 WIDGET-ID 40
     bt_executar AT ROW 11.38 COL 20 WIDGET-ID 36
     rt-button AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.72 BY 12.08
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
         TITLE              = "PIS/COFINS Notas de Importa‡Æo"
         HEIGHT             = 12.08
         WIDTH              = 89.72
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
ON END-ERROR OF w-livre /* PIS/COFINS Notas de Importa‡Æo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-livre w-livre
ON WINDOW-CLOSE OF w-livre /* PIS/COFINS Notas de Importa‡Æo */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt_executar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt_executar w-livre
ON CHOOSE OF bt_executar IN FRAME f-cad /* Executar */
DO:
  ASSIGN bt_executar:LABEL = 'Executando...'
         bt_executar:SENSITIVE = NO.
  
  EMPTY TEMP-TABLE tt.
  EMPTY TEMP-TABLE ttItens.
  OUTPUT TO c:\temp\logesft999.txt.
  FIND FIRST im-param
     WHERE im-param.cod-param = 'nat-operacao-imp'
     NO-LOCK NO-ERROR.
  IF AVAIL im-param THEN DO:
      ASSIGN cListaNatOperacao = im-param.val-param.
  END.
 /* MESSAGE cListaNatOperacao SKIP
          LOOKUP('31201m',cListaNatOperacao,",")
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
 /* MESSAGE
      fi_estab:SCREEN-VALUE IN FRAME {&FRAME-NAME}          SKIP
      fi_serie_ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}      SKIP
      fi_serie_fim:SCREEN-VALUE IN FRAME {&FRAME-NAME}      SKIP
      fi_nf_ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}         SKIP
      fi_nf_fim:SCREEN-VALUE IN FRAME {&FRAME-NAME}         SKIP
      INPUT FRAME {&frame-name} fi_cliente_ini              SKIP
      INPUT FRAME  {&frame-name} fi_cliente_fim             SKIP
      INPUT FRAME  {&FRAME-NAME} dt_emissao_ini             SKIP
      INPUT FRAME {&FRAME-NAME}  dt_emissao_fim             SKIP
      VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    



  FOR EACH nota-fiscal NO-LOCK
      WHERE nota-fiscal.cod-estabel      = fi_estab:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      AND   nota-fiscal.serie           >= fi_serie_ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      AND   nota-fiscal.serie           <= fi_serie_fim:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      AND   nota-fiscal.nr-nota-fis     >= fi_nf_ini:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      AND   nota-fiscal.nr-nota-fis     <= fi_nf_fim:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      AND   nota-fiscal.cod-emitente    >= INPUT FRAME {&frame-name} fi_cliente_ini
      AND   nota-fiscal.cod-emitente    <= INPUT FRAME  {&frame-name} fi_cliente_fim
      AND   nota-fiscal.dt-emis-nota    >= INPUT FRAME  {&FRAME-NAME} dt_emissao_ini
      AND   nota-fiscal.dt-emis-nota    <= INPUT FRAME {&FRAME-NAME}  dt_emissao_fim
      AND   nota-fiscal.dt-cancel = ?
      AND   LOOKUP(nota-fiscal.nat-operacao,cListaNatOperacao,",")  > 0.
      
      /*MESSAGE 'entrei'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      FIND FIRST doc-fiscal 
          WHERE doc-fiscal.cod-estabel  = nota-fiscal.cod-estabel
          AND   doc-fiscal.serie        = nota-fiscal.serie
          AND   doc-fiscal.nr-doc-fis   = nota-fiscal.nr-nota-fis
          AND   doc-fiscal.nat-operacao = nota-fiscal.nat-operacao
          AND   doc-fiscal.cod-emitente = nota-fiscal.cod-emitente
          NO-LOCK NO-ERROR.

            
      PUT "nota fiscal:" nota-fiscal.nr-nota-fis nota-fiscal.dt-emis-nota SKIP .
      
      
      
      FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.
          FIND FIRST ITEM OF it-nota-fisc 
              NO-LOCK NO-ERROR.
          FIND FIRST ext-it-nota-fisc OF it-nota-fisc
              /*WHERE ext-it-nota-fisc.cod-param = 'inf-PIS/COFINS'*/
              NO-LOCK NO-ERROR.
          

          FIND FIRST it-doc-fisc OF doc-fiscal 
              WHERE it-doc-fisc.it-codigo = it-nota-fisc.it-codigo
              AND   it-doc-fisc.nr-seq-doc = it-nota-fisc.nr-seq-fat
              NO-LOCK NO-ERROR.
          
           
          PUT "existe item OF:"  AVAIL it-doc-fisc SKIP.
          FIND FIRST docum-est 
               WHERE docum-est.serie-docto  = nota-fiscal.serie
               AND   docum-est.nro-docto    = nota-fiscal.nr-nota-fis
               AND   docum-est.cod-emitente = nota-fiscal.cod-emitente
               AND   docum-est.nat-operacao = nota-fiscal.nat-operacao
               NO-LOCK NO-ERROR.

          FIND FIRST item-doc-est OF docum-est 
               WHERE item-doc-est.it-codigo = it-nota-fisc.it-codigo 
               AND   item-doc-est.cod-refer = it-nota-fisc.cod-refer
               NO-LOCK NO-ERROR.
           
          
          //registro com base nos itens da nota fiscal
          CREATE ttItens.
          ASSIGN ttItens.origem         = "FAT"
                 ttItens.sequencia      = it-nota-fisc.nr-seq-fat
                 ttItens.itCodigo       = it-nota-fisc.it-codigo
                 ttItens.codRefer       = it-nota-fisc.cod-refer
                 ttItens.vlPis          = IF AVAIL ext-it-nota-fisc THEN ext-it-nota-fisc.val-livre-3 ELSE 0
                 ttItens.vlCofins       = IF AVAIL ext-it-nota-fisc THEN ext-it-nota-fisc.val-livre-6 ELSE 0
                 ttItens.percPis        = IF AVAIL ext-it-nota-fisc THEN ext-it-nota-fisc.val-livre-1 ELSE 0
                 ttItens.percCofins     = IF AVAIL ext-it-nota-fisc THEN ext-it-nota-fisc.val-livre-4 ELSE 0
                 ttItens.vlBasePis      = IF AVAIL ext-it-nota-fisc THEN ext-it-nota-fisc.val-livre-2 ELSE 0
                 ttItens.vlBaseCofins   = IF AVAIL ext-it-nota-fisc THEN ext-it-nota-fisc.val-livre-5 ELSE 0 
                 ttItens.rRowid         = ROWID(nota-fiscal)
                 ttItens.descItem     = IF AVAIL ITEM THEN ITEM.desc-item ELSE ''.
          
         //registro com base nos itens de obriga‡äes fiscais
         PUT "existe item OF:"  AVAIL it-doc-fisc SKIP.
         IF AVAIL it-doc-fisc AND INPUT FRAME {&FRAME-NAME} tg_of = YES THEN DO:
             PUT 'entrei na cria‡Æo dos itens do of' SKIP.
            CREATE ttItens.
            ASSIGN ttItens.origem       = "OF"
                   ttItens.sequencia    = it-doc-fisc.nr-seq-doc
                   ttItens.itCodigo     = it-doc-fisc.it-codigo
                   ttItens.codRefer     = ''
                   ttItens.vlPis        = it-doc-fisc.val-pis * -1
                   ttItens.vlCofins     = it-doc-fisc.val-cofins * -1
                   ttItens.percPis      = dec(substr(it-doc-fisc.char-2,22,8))
                   ttItens.percCofins   = dec(substr(it-doc-fisc.char-2,30,8))
                   ttItens.vlBasePis    = it-doc-fisc.val-base-calc-pis * -1
                   ttItens.vlBaseCofins = it-doc-fisc.val-base-calc-cofins  * -1
                   ttItens.rrowid       = ROWID(nota-fiscal)
                   ttItens.descItem     = IF AVAIL ITEM THEN ITEM.desc-item ELSE ''.
            
         END.
         //registro com base nos itens do recebimento
          IF AVAIL item-doc-est AND INPUT FRAME {&FRAME-NAME} tg_re = YES THEN DO:
             EXPORT DELIMITER "|" item-doc-est.
             //EXPORT item-doc-est.char-1 .
             //EXPORT  .
             ASSIGN dAliqCOFINS = dec(substr(item-doc-est.char-2,609,4)).
             CREATE ttItens.
             ASSIGN ttItens.origem       = "RE"
                   ttItens.sequencia    = item-doc-est.sequencia
                   ttItens.itCodigo     = item-doc-est.it-codigo
                   ttItens.vlPis        = item-doc-est.valor-pis  *  -1
                   ttItens.vlCofins     = item-doc-est.val-cofins / item-doc-est.val-aliq-cofins * (dAliqCofins)  *  -1
                   ttItens.percPis      = item-doc-est.val-aliq-pis
                   ttItens.percCofins   = item-doc-est.val-aliq-cofins
                   ttItens.vlBasePis    = item-doc-est.base-pis * -1
                   ttItens.vlBaseCofins = item-doc-est.val-base-calc-cofins * -1 
                   ttItens.rrowid       = ROWID(nota-fiscal)
                   ttItens.descItem     = IF AVAIL ITEM THEN ITEM.desc-item ELSE ''
                   ttItens.percPisExt   = item-doc-est.val-aliq-ext-pis    
                   ttItens.percCofinsExt   = item-doc-est.val-aliq-ext-cofins   .
    
    
          END.
      END.
      
      FIND emitente OF nota-fiscal NO-LOCK NO-ERROR.
      FIND natur-oper OF nota-fiscal NO-LOCK NO-ERROR.
      CREATE tt.
      ASSIGN  tt.vl-base-pis-cofins             =   dTotBaseCalc * iSinal
              tt.cod-estabel                    =   nota-fiscal.cod-estabel             
              tt.nr-nota-fis                    =   nota-fiscal.nr-nota-fis
              tt.serie                          =   nota-fiscal.serie
              tt.especie                        =   {ininc/i03in218.i 4 nota-fiscal.esp-docto} 
              tt.cod-emitente                   =   nota-fiscal.cod-emitente
              tt.nome-abrev                     =   IF AVAIL emitente THEN emitente.nome-abrev ELSE ''
              tt.dt-emis-nota                   =   nota-fiscal.dt-emis-nota
              tt.nat-operacao                   =   nota-fiscal.nat-operacao
              tt.cod-cfop                       =   IF AVAIL natur-oper THEN  natur-oper.cod-cfop ELSE ''
              tt.nr-pedcli                      =   nota-fiscal.nr-pedcli
              tt.cgc                            =   IF AVAIL emitente THEN emitente.cgc ELSE ''
              tt.cgc                            =  "'" + tt.cgc
              tt.cidade                         =   IF AVAIL emitente THEN emitente.cidade ELSE ''
              tt.estado                         =   IF AVAIL emitente THEN emitente.estado ELSE ''
              tt.log-nf-cancel                  =  IF nota-fiscal.dt-cancel <> ?  THEN "Sim" ELSE "NÆo"
              tt.des-idi-sit-nf-eletro          =  IF nota-fiscal.idi-sit-nf-eletro <> 0 THEN {diinc/i01di135.i 4 nota-fiscal.idi-sit-nf-eletro} ELSE "NÆo Informado"
              tt.cod-chave-aces-nf-eletro       =   "'" + nota-fiscal.cod-chave-aces-nf-eletro 
              tt.des-idi-forma-emis-nf-eletro   =   IF nota-fiscal.idi-forma-emis-nf-eletro <> 0 THEN {diinc/i02di135.i 4 nota-fiscal.idi-forma-emis-nf-eletro} 
                                                    ELSE "NÆo Informado"
              tt.log-xml-existe                 =  IF  lXMLExiste THEN "Sim" ELSE "NÆo"
              tt.vl-tot-nota                    = nota-fiscal.vl-tot-nota       * iSinal             
              tt.vl-mercadoria                  = nota-fiscal.vl-mercad         * iSinal 
              tt.vl-seguro                      = nota-fiscal.vl-seguro         * iSinal 
              tt.vl-embalagem                   = nota-fiscal.vl-embalagem      * iSinal 
              tt.vl-frete                       = nota-fiscal.vl-frete          * iSinal 
              tt.vl-icms                        = vlIcms   * iSinal 
              tt.vl-icms-ou                     = vlIcmsOu * iSinal 
              tt.vl-icms-nt                     = vlIcmsNt * iSinal 
              /*tt.vl-despesas                    = vlDespesas*/
              /*tt.perc-Pis                       = dPercPis*/
              /*tt.perc-Cofins                    = dPercCofins*/
              tt.vl-pis                         = vlPis * iSinal 
              tt.vl-cofins                      = vlCofins * iSinal 
              tt.log-gera-dp                    = IF AVAIL fat-duplic THEN "Sim" ELSE "NÆo"
              tt.emitenteRef                    = iEmitenteRef
              tt.descEmitRef                    = cDescEmitRef
              tt.notaref                        = cNotaref
              tt.serieRef                       = cSerieRef
              tt.dataRef                        = IF daDataref <> ? THEN string(daDataref,'99/99/9999') ELSE ''
              tt.chaveref                       = "'" + cChaveRef
              tt.rRowid                         = ROWID(nota-fiscal). 
     
  END.
  OUTPUT CLOSE.

  OUTPUT TO value('c:\temp\esft999.txt').
  FOR EACH tt BY tt.dt-Emis-nota:
      FOR EACH ttItens 
          WHERE ttItens.rRowid =  tt.rRowid :
          EXPORT DELIMITER "|" 
          tt.cod-estabel
          tt.nr-nota-fis                   
          tt.serie                         
          tt.especie                       
          tt.cod-emitente                  
          tt.nome-abrev                    
          tt.dt-emis-nota                  
          tt.nat-operacao                  
          tt.cod-cfop
          ttItens.origem       
          ttItens.sequencia   
          ttItens.itCodigo    
          ttItens.vlBasePis   
          ttItens.vlBaseCofins
          ttItens.percPis     
          ttItens.percCofins  
          ttItens.vlPis       
          ttItens.vlCofins 
          ttItens.descItem
          ttItens.percPisExt     
          ttItens.percCofinsExt    . 
      END.
      //EXPORT DELIMITER ";" tt EXCEPT tt.rRowid.
  END.
  OUTPUT CLOSE.
  OS-COMMAND SILENT VALUE('start excel /t t:\especificos\excel\esft999.xls').
  ASSIGN bt_executar:LABEL = 'Executar'
         bt_executar:SENSITIVE = YES.

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
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'panel/p-exihel.w':U ,
             INPUT  FRAME f-cad:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-exihel ).
       RUN set-position IN h_p-exihel ( 1.17 , 74.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.25 , 16.00 ) */

       /* Links to SmartPanel h_p-exihel. */
       RUN add-link IN adm-broker-hdl ( h_p-exihel , 'State':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-exihel ,
             fi_estab:HANDLE IN FRAME f-cad , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

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
  DISPLAY fi_estab fi_serie_ini fi_serie_fim fi_nf_ini fi_nf_fim fi_cliente_ini 
          fi_cliente_fim dt_emissao_ini dt_emissao_fim tg_of tg_re 
      WITH FRAME f-cad IN WINDOW w-livre.
  ENABLE rt-button fi_estab fi_serie_ini fi_serie_fim fi_nf_ini fi_nf_fim 
         fi_cliente_ini fi_cliente_fim dt_emissao_ini dt_emissao_fim tg_of 
         tg_re bt_executar 
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

  {utp/ut9000.i "ESFT999" "9.99.99.999"}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  run pi-after-initialize.
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

