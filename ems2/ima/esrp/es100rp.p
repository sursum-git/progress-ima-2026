&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/* ***************************  Definitions  ************************** */

  DEFINE INPUT PARAMETER dt-ini                AS DATE.                                    
  DEFINE INPUT PARAMETER dt-fim                AS DATE.          
  DEFINE INPUT PARAMETER p-cod-estabel         AS CHAR.     
  DEFINE INPUT PARAMETER p-cod-estabel-fim     AS CHAR. 
  DEFINE INPUT PARAMETER p-cod-emitente        AS CHAR.       
  DEFINE INPUT PARAMETER p-cod-emitente-fim    AS CHAR.   
  DEFINE INPUT PARAMETER p-nome-ab-rep         AS CHAR.    
  DEFINE INPUT PARAMETER p-nome-ab-rep-fim     AS CHAR.
  DEFINE INPUT PARAMETER p-motivo              AS CHAR.           
  DEFINE INPUT PARAMETER p-motivo-fim          AS CHAR.  
  DEFINE INPUT PARAMETER p-it-codigo           AS CHAR.
  DEFINE INPUT PARAMETER p-it-codigo-fim       AS CHAR.
  
  DEF TEMP-TABLE tt-itens-devol 
      FIELD cod-estabel LIKE docum-est.cod-estabel
      FIELD nome-ab-rep LIKE repres.nome-abrev
      FIELD cod-cli     AS CHAR
      FIELD nome-abrev  LIKE emitente.nome-abrev
      FIELD data        LIKE docum-est.dt-trans
      FIELD it-codigo   LIKE ITEM.it-codigo
      FIELD cod-refer   AS CHAR
      FIELD valor       AS DEC
      FIELD quantidade  AS DEC
      FIELD motivo      AS CHAR
      FIELD descricao   AS CHAR
      FIELD nota-fiscal AS CHAR
      FIELD nota-devol  AS CHAR
      FIELD serie-docto AS CHAR
      FIELD num-ped     AS CHAR
      FIELD nro-docto   AS CHAR FORMAT 'x(20)'
      INDEX rep nome-ab-rep data.

 DEF TEMP-TABLE tt-nf-devol
     FIELD nota-fiscal LIKE tt-itens-devol.nota-fiscal. 

 DEF TEMP-TABLE tt-excel LIKE tt-itens-devol.

 /* Vari veis para Excel */
 DEFINE VARIABLE chExcelApp  AS COM-HANDLE NO-UNDO.
 DEFINE VARIABLE chWorkBook  AS COM-HANDLE NO-UNDO.
 DEFINE VARIABLE chWorkSheet AS COM-HANDLE NO-UNDO.
 DEFINE VARIABLE i-linha     AS INTEGER    NO-UNDO.

  DEF VAR c-range AS CHAR NO-UNDO.
  DEF VAR coluna    AS INT.
  DEF VAR de-tot-vlr   AS DEC INIT 0.
  DEF VAR de-tot-qtd   AS DEC INIT 0.
  DEF VAR col-total AS INT.
  DEF VAR tot-vr-item AS DEC INIT 0.
  DEF VAR tot-qt-item AS DEC INIT 0.
  DEF VAR de-vl-desconto AS DEC.
  DEF VAR de-fator AS DEC.
  DEF VAR h-acomp AS HANDLE  NO-UNDO.


{include/tt-edit.i}
{include/pi-edit.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fn-letra) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-letra Procedure 
FUNCTION fn-letra RETURNS CHARACTER
  ( coluna AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15.63
         WIDTH              = 45.86.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Processando..").

FOR EACH docum-est WHERE
         docum-est.cod-estabel >= p-cod-estabel AND
         docum-est.cod-estabel <= p-cod-estabel-fim AND
         docum-est.dt-trans >= dt-ini     AND
         docum-est.dt-trans <= dt-fim  NO-LOCK.

    FIND natur-oper WHERE
         natur-oper.nat-operacao = docum-est.nat-operacao NO-LOCK NO-ERROR.
    IF NOT AVAIL natur-oper THEN NEXT.
    IF natur-oper.tipo-compra <> 3 THEN NEXT. /* Devolu‡Æo de Cliente */

    FOR EACH item-doc-est OF docum-est NO-LOCK.
        IF item-doc-est.it-codigo < p-it-codigo OR
           item-doc-est.it-codigo > p-it-codigo-fim THEN NEXT.

        RUN pi-acompanhar IN h-acomp (INPUT "Data: "    + STRING(docum-est.dt-trans) +
                                             " Nota Fiscal: " + item-doc-est.nro-docto).
    
        CREATE tt-itens-devol.
        ASSIGN tt-itens-devol.cod-estabel = docum-est.cod-estabel
               tt-itens-devol.nota-fiscal = docum-est.nro-docto.
    
        FIND devol-cli OF item-doc-est NO-LOCK NO-ERROR.
        IF AVAIL devol-cli THEN DO.
           FIND cod-rejeicao WHERE 
                cod-rejeicao.codigo-rejei = devol-cli.codigo-rejei NO-LOCK NO-ERROR.
           IF AVAIL cod-rejeicao THEN 
              ASSIGN tt-itens-devol.descricao   = UPPER(cod-rejeicao.descricao).
           ASSIGN tt-itens-devol.serie-docto    = devol-cli.serie-docto
                  tt-itens-devol.nro-docto      = devol-cli.nro-docto.
        END.
    
        ASSIGN de-vl-desconto = 0
               de-fator = 0.

        FIND nota-fiscal WHERE
             nota-fiscal.cod-estabel  = docum-est.cod-estabel   AND
             nota-fiscal.serie        = item-doc-est.serie-comp AND
             nota-fiscal.nr-nota-fis  = item-doc-est.nro-comp   
             NO-LOCK NO-ERROR.
        IF AVAIL nota-fiscal THEN DO:
           FIND repres WHERE
                repres.cod-rep = nota-fiscal.cod-rep NO-LOCK NO-ERROR.
           IF NOT AVAIL repres THEN NEXT.

           FIND it-nota-fisc OF nota-fiscal WHERE 
                it-nota-fisc.nr-seq-fat = item-doc-est.seq-comp
                NO-LOCK NO-ERROR.
            
           ASSIGN tt-itens-devol.nome-ab-rep =  nota-fiscal.no-ab-reppri
                  tt-itens-devol.nome-abrev  =  nota-fiscal.nome-ab-cli
                  tt-itens-devol.nota-devol  =  nota-fiscal.nr-nota-fis.
    
           FIND ped-venda WHERE
                ped-venda.nome-abrev = nota-fiscal.nome-ab-cli AND
                ped-venda.nr-pedcli = nota-fiscal.nr-pedcli NO-LOCK NO-ERROR.
    
           /* Calcula Devolu‡Æo do Desconto Proporcial … NF Devolvida */
           ASSIGN de-fator = (item-doc-est.preco-total[1] / (it-nota-fisc.vl-preuni * it-nota-fisc.qt-faturada[1])).

           ASSIGN de-vl-desconto = 0.
           FIND ped-item OF ped-venda WHERE
                ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped NO-LOCK NO-ERROR.

           IF AVAIL ped-item THEN
              IF ped-item.val-desconto-total > 0 THEN
                 ASSIGN de-vl-desconto = de-vl-desconto + ped-item.val-desconto-total.

           ASSIGN de-vl-desconto = (de-vl-desconto * de-fator).
        END.

        ASSIGN tt-itens-devol.cod-cli    = STRING(docum-est.cod-emitente) 
               tt-itens-devol.data       = docum-est.dt-trans
               tt-itens-devol.it-codigo  = item-doc-est.it-codigo
               tt-itens-devol.cod-refer  = item-doc-est.cod-refer
               tt-itens-devol.valor      = item-doc-est.preco-total[1] + de-vl-desconto
               tt-itens-devol.quantidade = item-doc-est.quantidade
               tt-itens-devol.num-ped    = IF AVAIL ped-venda THEN ped-venda.nr-pedcli ELSE ""
               tt-itens-devol.motivo     = IF AVAIL cod-rejeicao
                                         THEN STRING(cod-rejeicao.codigo-rejei) + " - " +
                                              STRING(cod-rejeicao.descricao)
                                         ELSE "MOTIVO NÇO CADASTRADO".
    
        ASSIGN tt-itens-devol.motivo = tt-itens-devol.motivo + " // " +
                                       SUBSTR(docum-est.observacao,1,70).

        RUN pi-print-editor(INPUT tt-itens-devol.motivo, INPUT 200). 
        FIND FIRST tt-editor NO-LOCK NO-ERROR.
        ASSIGN tt-itens-devol.motivo = tt-editor.conteudo.

        IF AVAIL devol-cli THEN DO.
           FIND tt-nf-devol WHERE
                tt-nf-devol.nota-fiscal = devol-cli.nr-nota-fis NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-nf-devol THEN DO:
              CREATE tt-nf-devol.
              ASSIGN tt-nf-devol.nota-fiscal = devol-cli.nr-nota-fis.
           END.
        END.
    END.
END.
 
FOR EACH tt-nf-devol NO-LOCK.
    RUN pi-acompanhar IN h-acomp (INPUT "Gerando Excel.." + tt-nf-devol.nota-fiscal  ).

    FOR EACH tt-itens-devol WHERE
             tt-itens-devol.nota-devol  = tt-nf-devol.nota-fiscal AND
             tt-itens-devol.nome-ab-rep >= p-nome-ab-rep          AND
             tt-itens-devol.nome-ab-rep <= p-nome-ab-rep-fim      AND
             tt-itens-devol.cod-cli     >= p-cod-emitente         AND
             tt-itens-devol.cod-cli     <= p-cod-emitente-fim     
             BREAK BY tt-itens-devol.it-codigo
                   BY tt-itens-devol.cod-refer :
         
        FIND tt-excel WHERE
             tt-excel.nota-fiscal = tt-itens-devol.nota-fiscal AND
             tt-excel.it-codigo = tt-itens-devol.it-codigo AND
             tt-excel.cod-refer = tt-itens-devol.cod-refer NO-ERROR.
        IF NOT AVAIL tt-excel THEN DO.
           CREATE tt-excel.
           ASSIGN tt-excel.nota-fiscal      = tt-itens-devol.nota-devol
                  tt-excel.nome-ab-rep      = tt-itens-devol.nome-ab-rep
                  tt-excel.cod-cli          = tt-itens-devol.cod-cli
                  tt-excel.nome-abrev       = tt-itens-devol.nome-abrev
                  tt-excel.data             = tt-itens-devol.data
                  tt-excel.it-codigo        = tt-itens-devol.it-codigo
                  tt-excel.cod-refer        = tt-itens-devol.cod-refer
                  tt-excel.descricao        = tt-itens-devol.descricao
                  tt-excel.motivo           = tt-itens-devol.motivo
                  tt-excel.num-ped          = tt-itens-devol.num-ped
                  tt-excel.nro-docto        = tt-itens-devol.nro-docto
                  tt-excel.serie-docto      = tt-itens-devol.serie-docto.
        END.
        ASSIGN tt-excel.valor = tt-excel.valor + tt-itens-devol.valor
               tt-excel.quantidade = tt-excel.quantidade + tt-itens-devol.quantidade.
    END.
END.

RUN pi-acompanhar IN h-acomp (INPUT "Abrindo Excel:").
FIND FIRST tt-excel NO-LOCK NO-ERROR.
IF AVAIL tt-excel THEN DO:
    CREATE "excel.application" chExcelApp.
    chExcelApp:VISIBLE = FALSE. 

    chWorkBook = chExcelApp:Workbooks:ADD().
    chWorkSheet = chExcelApp:Sheets:Item(1).

    chWorkbook:Worksheets(1):activate.
    chExcelApp:ActiveWindow:Zoom = 100.

    /* Configura a Linha do Titulo da Planilha */
    ASSIGN chWorkSheet:Rows("1:1"):RowHeight = 30
           chWorkSheet:Rows("1:1"):FONT:SIZE = 18
           chWorkSheet:Rows("1:1"):FONT:bold = FALSE.

    ASSIGN chworksheet:range("A1"):VALUE = "RELATàRIO DEVOLU€ÇO DE MERCADORIAS " + FILL(" ",20) + 
                                           "PERIODO: " + STRING(dt-ini,"99/99/9999") + " A " + 
                                                         STRING(dt-fim,"99/99/9999") .

    /* Configura Alinhamento Horizontal do Titulo da Planilha */
    ChWorkSheet:range("A1:J1"):SELECT().
    ChWorksheet:range("A1:L1"):Merge.
    Chworksheet:Range("A1:L1"):HorizontalAlignment = 3. /* Centralizado */
    Chworksheet:Range("A1:L1"):VerticalAlignment   = 2. /* Centralizado */

    /* Colorir Titulo da Planilha */
    chWorkSheet:Range("A1:L1"):FONT:ColorIndex     = 18. /* Avermelhado */
    chWorkSheet:Range("A1:L1"):Interior:ColorIndex = 2.  /* Branco */

    /* Titulo das Colunas */
    ASSIGN chWorkSheet:Range("A2"):VALUE = "REPRES"
           chWorkSheet:Range("B2"):VALUE = "CODCLI"     
           chWorkSheet:Range("C2"):VALUE = "NOMECLI"
           chWorkSheet:Range("D2"):VALUE = "DATA"
           chWorkSheet:Range("E2"):VALUE = "NF VENDA"
           chWorkSheet:Range("F2"):VALUE = "PED VENDA"
           chWorkSheet:Range("G2"):VALUE = "ITEM"
           chWorkSheet:Range("H2"):VALUE = "REFER"
           chWorkSheet:Range("I2"):VALUE = "DESCRICAO"
           chWorkSheet:Range("J2"):VALUE = "QUANTIDADE"
           chWorkSheet:Range("K2"):VALUE = "VALOR"
           chWorkSheet:Range("L2"):VALUE = "MOTIVO"
           chWorkSheet:Range("M2"):VALUE = "NF DEVOL."
           chWorkSheet:Range("N2"):VALUE = "SERIE DEVOL.".

    ASSIGN chworksheet:range("J:J"):ShrinkToFit = TRUE.

    
    /* Ajustar o Tamanho Dentro da Celula */     
    ASSIGN chWorkSheet:COLUMNS("A"):ColumnWidth           = 15.
    ASSIGN chWorkSheet:COLUMNS("C"):ColumnWidth           = 15.
    ASSIGN chWorkSheet:COLUMNS("D"):ColumnWidth           = 13.
    ASSIGN chWorkSheet:COLUMNS("E"):ColumnWidth           = 10.
    ASSIGN chWorkSheet:COLUMNS("F"):ColumnWidth           = 10.
    ASSIGN chWorkSheet:COLUMNS("G"):ColumnWidth           = 10.
    ASSIGN chWorkSheet:COLUMNS("H"):ColumnWidth           = 10.
    ASSIGN chWorkSheet:COLUMNS("I"):ColumnWidth           = 60.
    ASSIGN chWorkSheet:COLUMNS("J"):ColumnWidth           = 15.
    ASSIGN chWorkSheet:COLUMNS("K"):ColumnWidth           = 13.
    ASSIGN chWorkSheet:COLUMNS("L"):ColumnWidth           = 80.
    ASSIGN chWorkSheet:COLUMNS("M"):ColumnWidth           = 15.
    ASSIGN chWorkSheet:COLUMNS("N"):ColumnWidth           = 8.

    /* Configura as Colunas da Planilha */
    ASSIGN chworksheet:range("A:C"):NumberFormat        = "@"
           chworksheet:range("E:I"):NumberFormat        = "@"
           chworksheet:range("J:K"):NumberFormat        = "###.###.##0,00"
           chworksheet:range("L:N"):NumberFormat        = "@".

    ASSIGN Chworksheet:range("H:H"):HorizontalAlignment = 4
           Chworksheet:range("I:K"):HorizontalAlignment = 4.

    /* Configura Cabe»alho das Colunas */
    chWorkSheet:Range("A2:M2"):SELECT().
    ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
           chExcelApp:SELECTION:FONT:SIZE               = 12
           chExcelApp:SELECTION:FONT:Bold               = FALSE 
           chExcelApp:SELECTION:Interior:ColorIndex     = 37
           chExcelApp:SELECTION:FONT:ColorIndex         = 11.

    ASSIGN i-linha = 2.
    FOR EACH tt-excel NO-LOCK  
             BREAK BY tt-excel.nota-fiscal.

        FIND item WHERE
             item.it-codigo = tt-excel.it-codigo NO-LOCK NO-ERROR.

       ASSIGN i-linha = i-linha + 1.

       ASSIGN chWorkSheet:Range("A" + STRING(i-linha)):VALUE = tt-excel.nome-ab-rep
              chWorkSheet:Range("B" + STRING(i-linha)):VALUE = STRING(tt-excel.cod-cli)
              chWorkSheet:Range("C" + STRING(i-linha)):VALUE = tt-excel.nome-abrev
              chWorkSheet:Range("D" + STRING(i-linha)):VALUE = tt-excel.data
              chWorkSheet:Range("E" + STRING(i-linha)):VALUE = tt-excel.nota-fiscal
              chWorkSheet:Range("F" + STRING(i-linha)):VALUE = tt-excel.num-ped
              chWorkSheet:Range("G" + STRING(i-linha)):VALUE = STRING(tt-excel.it-codigo)
              chWorkSheet:Range("H" + STRING(i-linha)):VALUE = STRING(tt-excel.cod-refer)
              chWorkSheet:Range("I" + STRING(i-linha)):VALUE = SUBSTR(item.desc-item,1,50)
              chWorkSheet:Range("J" + STRING(i-linha)):VALUE = tt-excel.quantidade
              chWorkSheet:Range("K" + STRING(i-linha)):VALUE = tt-excel.valor
              chWorkSheet:Range("L" + STRING(i-linha)):VALUE = tt-excel.motivo
              chWorkSheet:Range("M" + STRING(i-linha)):VALUE = tt-excel.nro-docto
              chWorkSheet:Range("N" + STRING(i-linha)):VALUE = tt-excel.serie-docto.

        ASSIGN de-tot-qtd = de-tot-qtd + tt-excel.quantidade.
               de-tot-vlr = de-tot-vlr + tt-excel.valor.
    END.

    ASSIGN i-linha = i-linha + 1.
    ASSIGN chWorkSheet:Range("H" + STRING(i-linha)):VALUE = "TOTAIS"
           chWorkSheet:Range("J" + STRING(i-linha)):VALUE = de-tot-qtd.
           chWorkSheet:Range("K" + STRING(i-linha)):VALUE = de-tot-vlr.
    chExcelApp:VISIBLE = TRUE.
    RELEASE OBJECT chWorkSheet.
    RELEASE OBJECT chWorkBook.
    RELEASE OBJECT chExcelApp.
END.
ELSE DO:
    MESSAGE "NÆo h  registros nesse periodo"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

    
RUN pi-finalizar in h-acomp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fn-letra) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fn-letra Procedure 
FUNCTION fn-letra RETURNS CHARACTER
  ( coluna AS INT ) :
DEF VAR col-letra AS CHAR.
CASE coluna:
    WHEN 1  THEN ASSIGN col-letra = 'A'.
    WHEN 2  THEN ASSIGN col-letra = 'B'.
    WHEN 3  THEN ASSIGN col-letra = 'C'.
    WHEN 4  THEN ASSIGN col-letra = 'D'.
    WHEN 5  THEN ASSIGN col-letra = 'E'.
    WHEN 6  THEN ASSIGN col-letra = 'F'. 
    WHEN 7  THEN ASSIGN col-letra = 'G'. 
    WHEN 8  THEN ASSIGN col-letra = 'H'. 
    WHEN 9  THEN ASSIGN col-letra = 'I'. 
    WHEN 10 THEN ASSIGN col-letra = 'J'. 
    WHEN 11 THEN ASSIGN col-letra = 'K'. 
    WHEN 12 THEN ASSIGN col-letra = 'L'. 
    WHEN 13 THEN ASSIGN col-letra = 'M'. 
    WHEN 14 THEN ASSIGN col-letra = 'N'. 
    WHEN 15 THEN ASSIGN col-letra = 'O'. 
    WHEN 16 THEN ASSIGN col-letra = 'P'. 
    WHEN 17 THEN ASSIGN col-letra = 'Q'. 
    WHEN 18 THEN ASSIGN col-letra = 'R'. 
    WHEN 19 THEN ASSIGN col-letra = 'S'. 
    WHEN 20 THEN ASSIGN col-letra = 'T'. 
    WHEN 21 THEN ASSIGN col-letra = 'U'.
    WHEN 22 THEN ASSIGN col-letra = 'V'. 
    WHEN 23 THEN ASSIGN col-letra = 'W'. 
    WHEN 24 THEN ASSIGN col-letra = 'X'. 
    WHEN 25 THEN ASSIGN col-letra = 'Y'. 

END CASE.

  RETURN col-letra.   

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

