/* Programa: ESSP0147.W
** Modulo..: Controle de Expedi‡Æo
** Objetivo: Listar a Consulta Itens X Etiquetas
** Autor...: Fábio Coelho Lanza - Janeiro/2007
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/
/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0147RP 2.04.00.000}

DEF BUFFER empresa FOR mgadm.empresa.
DEF BUFFER b-etiqueta FOR ob-etiqueta.

DEFINE TEMP-TABLE tt-param   NO-UNDO
       FIELD destino            AS INTEGER
       FIELD arquivo            AS CHAR FORMAT "x(35)"
       FIELD usuario            AS CHAR FORMAT "x(12)"
       FIELD data-exec          AS DATE
       FIELD hora-exec          AS INTEGER
       FIELD classifica         AS INTEGER
       FIELD desc-classifica    AS CHAR FORMAT "x(40)"
       FIELD cod-estabel-ini    LIKE ob-etiqueta.cod-estabel
       FIELD cod-estabel-fin    LIKE ob-etiqueta.cod-estabel
       FIELD cod-depos          LIKE ob-etiqueta.cod-depos
       FIELD localizacao-ini    LIKE ob-etiqueta.localizacao
       FIELD localizacao-fin    LIKE ob-etiqueta.localizacao
       FIELD dt-emissao-ini     LIKE ob-etiqueta.dt-emissao 
       FIELD dt-emissao-fin     LIKE ob-etiqueta.dt-emissao 
       FIELD nr-lote-ini        LIKE ob-etiqueta.nr-lote     
       FIELD nr-lote-fin        LIKE ob-etiqueta.nr-lote 
       FIELD fi-ini-it-codigo   like ob-etiqueta.it-codigo
       FIELD fi-fin-it-codigo   like ob-etiqueta.it-codigo
       FIELD fi-ini-cod-refer   like ob-etiqueta.cod-refer
       FIELD fi-fin-cod-refer   like ob-etiqueta.cod-refer
       FIELD un-ini             LIKE ob-etiqueta.un
       FIELD un-fin             LIKE ob-etiqueta.un
       FIELD qtd-maxima         LIKE ob-etiqueta.quantidade
       FIELD fi-desenho         AS CHAR FORMAT "x(4)"
       FIELD l-inc-exc          AS LOG FORMAT "Inclusive/Exclusive"
       FIELD i-situacao         AS INT
       FIELD opc-artigo         AS CHAR FORMAT "x"
       FIELD dep-dest           AS CHAR
       FIELD enviar-e-mail      AS LOG FORMAT "Sim/NÆo"
       FIELD subject-e-mail     AS CHAR FORMAT "x(40)"
       FIELD texto-e-mail       AS CHAR FORMAT "x(2000)"
       FIELD l-batch            AS LOG
       FIELD imp-param          AS LOG
       FIELD l-imp-layout       AS LOG
       FIELD l-resumo           AS LOG
       FIELD l-excel            AS LOG.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

DEF TEMP-TABLE tt-work
    FIELD cod-estabel      LIKE ob-etiqueta.cod-estabel
    FIELD dt-emissao       LIKE ob-etiqueta.dt-emissao
    FIELD doca             LIKE ob-etiqueta.localizacao
    FIELD num-etiqueta     LIKE ob-etiqueta.num-etiqueta
    FIELD cod-depos        LIKE ob-etiqueta.cod-depos
    FIELD acondicionamento AS CHAR FORMAT "x(10)"
    FIELD qualidade        AS CHAR FORMAT "x(11)"
    FIELD it-codigo        AS CHAR FORMAT "x(38)"
    FIELD cod-refer        AS CHAR FORMAT "x(6)"
    FIELD nr-lote          AS CHAR FORMAT "x(5)"
    FIELD quantidade       LIKE ob-etiqueta.quantidade
    FIELD situacao         AS CHAR FORMAT "x(10)"
    FIELD un               LIKE ITEM.un
    INDEX indice cod-estabel num-etiqueta.

DEF TEMP-TABLE tt-resumo
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD cod-refer    LIKE ob-etiqueta.cod-refer
    FIELD un           LIKE item.un
    FIELD dt-pri-ent   AS DATE FORMAT "99/99/9999"
    FIELD dt-ult-ent   AS DATE FORMAT "99/99/9999"
    FIELD qtidade-data LIKE ob-etiqueta.quantidade
    FIELD qtidade-atu  LIKE ob-etiqueta.quantidade
    FIELD qtidade-dsp  LIKE ob-etiqueta.quantidade
    INDEX indice-1 it-codigo cod-refer un.

DEF NEW SHARED TEMP-TABLE tt-saldo-estoq
    FIELD  cod-estabel   LIKE movadm.saldo-estoq.cod-estabel 
    FIELD  empresa       LIKE mgadm.empresa.nome
    FIELD  it-codigo     LIKE movadm.saldo-estoq.it-codigo
    field  cod-depos     like movadm.saldo-estoq.cod-depos
    field  cod-refer     like movadm.saldo-estoq.cod-refer 
    FIELD  qt-disponivel like movadm.saldo-estoq.qtidade-atu
    field  qt-aloc-ped   like movadm.saldo-estoq.qt-aloc-ped 
    field  qt-alocada    like movadm.saldo-estoq.qt-alocada 
    field  qtidade-atu   like movadm.saldo-estoq.qtidade-atu
    field  qt-aloc-prod  LIKE movadm.saldo-estoq.qt-aloc-prod
    FIELD  qt-aloc-pi    LIKE movadm.saldo-estoq.qtidade-atu.

DEF TEMP-TABLE tt-layout
    FIELD localizacao LIKE ob-etiqueta.localizacao
    FIELD peca        AS INTEGER
    FIELD quant       LIKE ob-etiqueta.quantidade.

DEF BUFFER b-tt-resumo FOR tt-resumo.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
DEF VAR h-acomp        AS HANDLE NO-UNDO.
DEF VAR c-situacao     AS CHAR FORMAT "x(10)".
DEF VAR i-sit-ini      AS INT.
DEF VAR i-sit-fin      AS INT.
DEF VAR c-refer        AS CHAR FORMAT "x(12)".
DEF VAR c-un           AS CHAR.
DEF VAR c-qualid       AS CHAR FORMAT "x(11)".
DEF VAR c-corte-comerc AS CHAR FORMAT "x(15)".
DEF VAR c-arquivo      AS CHAR.
DEF VAR c-destinatar   AS CHAR.
DEF VAR c-estabel      AS CHAR.
DEF VAR i-qt-rl-doca   AS INTEGER     NO-UNDO.
DEF VAR i-qt-rl-geral  AS INTEGER     NO-UNDO.
DEF VAR de-tot-doca-kg AS DECIMAL     NO-UNDO.
DEF VAR de-tot-doca-mt AS DECIMAL     NO-UNDO.
DEF VAR de-tot-ger-kg  AS DECIMAL     NO-UNDO.
DEF VAR de-tot-ger-mt  AS DECIMAL     NO-UNDO.
DEF VAR i-peca         AS INTEGER     NO-UNDO.
DEF VAR d-quant        AS DECIMAL     NO-UNDO.
DEF VAR da-dt-emissao  LIKE ob-etiqueta.dt-emissao.

/* Vari veis para Excel */
DEFINE VARIABLE chExcelApp  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkBook  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkSheet AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE i-linha     AS INTEGER    NO-UNDO.


form 
    "*--------------- Parƒmetros/Sele‡Æo ----------------*" SKIP
    tt-param.cod-estabel-ini  LABEL  "Estabelec...."
    "A"  AT 40
    tt-param.cod-estabel-fin  NO-LABEL SKIP
    tt-param.localizacao-ini  LABEL  "Doca........."
    "A"  AT 40                
    tt-param.localizacao-fin  NO-LABELS SKIP
    tt-param.dt-emissao-ini   LABEL  "Data EmissÆo."
    "A"  AT 40                
    tt-param.dt-emissao-fin   NO-LABELS SKIP
    tt-param.nr-lote-ini      LABEL  "Lote........."
    "A"  AT 40                     
    tt-param.nr-lote-fin      NO-LABELS SKIP
    tt-param.fi-ini-it-codigo LABEL  "Item........."
    "A"  AT 40                     
    tt-param.fi-fin-it-codigo NO-LABEL SKIP
    tt-param.fi-ini-cod-refer LABEL  "Referˆncia..."
    "A"  AT 40                     
    tt-param.fi-fin-cod-refer NO-LABEL            
    tt-param.fi-desenho       LABEL  "Desenho......"
    "A"  AT 40                     
    tt-param.l-inc-exc        LABEL  "Doca........."
    "A"  AT 40                     
    tt-param.opc-artigo       LABEL  "Inclus/Exclus"
    WITH FRAME f-param SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.

FORM
    tt-work.cod-estabel FORMAT "x(3)"        LABEL "Est"
    tt-work.doca        FORMAT "XXX/XXX"     LABEL "Doca" 
    tt-work.num-etiqueta FORMAT ">>>>>>>>9"  LABEL "Etiqueta"
    tt-work.cod-depos                        LABEL "Dep"
    tt-work.dt-emissao  FORMAT "99/99/9999"  LABEL "Dt-EmissÆo"
    tt-work.qualidade                        LABEL "Qualidade"
    tt-work.it-codigo                        LABEL "Item"
    tt-work.cod-refer                        LABEL "Refer." 
    tt-work.nr-lote                          LABEL "Lote" 
    tt-work.quantidade FORMAT ">,>>>,>>9.99" LABEL "Quantidade"
    tt-work.situacao                         LABEL "Situacao"
    tt-work.un                               LABEL "UN"
    WITH NO-BOX NO-LABEL 55 DOWN WIDTH 132 STREAM-IO FRAME f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST empresa
     WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 

ASSIGN c-empresa = (IF AVAIL empresa then empresa.razao-social ELSE "").

{utp/ut-liter.i ESPECÖFICOS * r}
ASSIGN c-sistema = TRIM(RETURN-VALUE).
{utp/ut-liter.i Estoque_de_Doca_X_Etiqueta * r}
ASSIGN c-titulo-relat = TRIM(RETURN-VALUE).

VIEW frame f-cabec.
VIEW frame f-rodape.

RUN utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar in h-acomp (input RETURN-VALUE).

FIND FIRST para-ped NO-LOCK NO-ERROR.

ASSIGN i-sit-ini = IF tt-param.i-situacao = 2 THEN 4 ELSE 3.
       i-sit-fin = IF tt-param.i-situacao = 1 THEN 3 ELSE 4.

FOR EACH ob-etiqueta WHERE
         ob-etiqueta.situacao     >= i-sit-ini   AND
         ob-etiqueta.situacao     <= i-sit-fin   AND
         ob-etiqueta.cod-estabel   = tt-param.cod-estabel-ini  AND
         ob-etiqueta.localizacao  >= tt-param.localizacao-ini  AND 
         ob-etiqueta.localizacao  <= tt-param.localizacao-fin  AND
         ob-etiqueta.it-codigo    >= tt-param.fi-ini-it-codigo AND
         ob-etiqueta.it-codigo    <= tt-param.fi-fin-it-codigo AND
         ob-etiqueta.cod-refer    >= tt-param.fi-ini-cod-refer AND
         ob-etiqueta.cod-refer    <= tt-param.fi-fin-cod-refer AND 
         ob-etiqueta.cod-depos    =  tt-param.cod-depos NO-LOCK,
    EACH item-ext WHERE 
         item-ext.it-codigo = ob-etiqueta.it-codigo NO-LOCK  
    BREAK BY ob-etiqueta.localizacao
          BY ob-etiqueta.num-etiqueta.

    RUN pi-acompanhar IN h-acomp (INPUT "Etiqueta: " + STRING(ob-etiqueta.num-etiqueta)).

    IF ob-etiqueta.quantidade <= 0 THEN NEXT.
    IF ob-etiqueta.quantidade > tt-param.qtd-maxima THEN NEXT.

    FIND ITEM WHERE 
         ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

    IF item.un < tt-param.un-ini OR
       item.un > tt-param.un-fin THEN NEXT.

    {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao}

    FIND qualid-tecido WHERE
         qualid-tecido.codigo = ob-etiqueta.cod-qualid NO-LOCK NO-ERROR.
    IF AVAIL qualid-tecido THEN
       ASSIGN c-qualid = qualid-tecido.descricao.

    ASSIGN c-corte-comerc = "".
    FIND corte-comerc WHERE 
         corte-comerc.codigo = ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.
    IF AVAIL corte-comerc  THEN 
       ASSIGN c-corte-comerc = corte-comerc.descricao.
     
    ASSIGN da-dt-emissao = ob-etiqueta.dt-emissao.
    FIND bc-etiqueta WHERE
         bc-etiqueta.progressivo = ob-etiqueta.progressivo NO-LOCK NO-ERROR.
    IF AVAIL bc-etiqueta THEN DO.
       ASSIGN da-dt-emissao = bc-etiqueta.dt-criacao.
       IF bc-etiqueta.dt-criacao < da-dt-emissao THEN
          ASSIGN da-dt-emissao = ob-etiqueta.dt-emissao.
    END.

    // Transferencia de Etiquetas
    IF tt-param.dep-dest <> '' THEN DO.
       FIND b-etiqueta WHERE
            b-etiqueta.cod-estabel = ob-etiqueta.cod-estabel AND
            b-etiqueta.num-etiqueta = ob-etiqueta.num-etiqueta SHARE-LOCK NO-ERROR.
       FIND FIRST usuar-depos WHERE
                  usuar-depos.cod-depos = tt-param.dep-dest NO-LOCK NO-ERROR.
       IF AVAIL usuar-depos THEN
          ASSIGN b-etiqueta.cod-estab = usuar-depos.cod-estab
                 b-etiqueta.cod-depos = usuar-depos.cod-depos.
       ELSE
          ASSIGN b-etiqueta.cod-estab = '5'
                 b-etiqueta.cod-depos = 'ARM'.
    END.

    IF tt-param.l-resumo THEN DO.
       IF da-dt-emissao <= tt-param.dt-emissao-fin THEN DO.
          FIND tt-resumo WHERE
               tt-resumo.it-codigo = ob-etiqueta.it-codigo AND
               tt-resumo.cod-refer = ob-etiqueta.cod-refer AND
               tt-resumo.un        = item.un
               NO-ERROR.
          IF NOT AVAIL tt-resumo THEN DO.
             CREATE tt-resumo.
             ASSIGN tt-resumo.it-codigo = ob-etiqueta.it-codigo
                    tt-resumo.cod-refer = ob-etiqueta.cod-refer
                    tt-resumo.un        = item.un.
          END.
          ASSIGN tt-resumo.qtidade-data = tt-resumo.qtidade-data + ob-etiqueta.quantidade.
       END.
    
       FIND tt-resumo WHERE
            tt-resumo.it-codigo = ob-etiqueta.it-codigo AND
            tt-resumo.cod-refer = ob-etiqueta.cod-refer AND
            tt-resumo.un        = item.un NO-ERROR.
       IF AVAIL tt-resumo THEN DO.
          IF tt-resumo.dt-pri-ent = ? OR 
             da-dt-emissao < tt-resumo.dt-pri-ent THEN
             ASSIGN tt-resumo.dt-pri-ent = da-dt-emissao.

          IF tt-resumo.dt-ult-ent = ? OR 
             da-dt-emissao > tt-resumo.dt-ult-ent THEN
             ASSIGN tt-resumo.dt-ult-ent = da-dt-emissao.
       END.
    END.
    ELSE DO.
       FIND tt-work WHERE
            tt-work.cod-estabel  = ob-etiqueta.cod-estabel AND
            tt-work.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
       IF NOT AVAIL tt-work THEN DO.
          CREATE tt-work.                 
          ASSIGN tt-work.cod-estabel      = ob-etiqueta.cod-estabel
                 tt-work.dt-emissao       = da-dt-emissao
                 tt-work.doca             = ob-etiqueta.localizacao
                 tt-work.num-etiqueta     = ob-etiqueta.num-etiqueta
                 tt-work.cod-depos        = ob-etiqueta.cod-depos
                 tt-work.acondicionamento = c-corte-comerc
                 tt-work.qualidade        = c-qualid
                 tt-work.it-codigo        = ob-etiqueta.it-codigo + "-" + ITEM.desc-item
                 tt-work.cod-refer        = ob-etiqueta.cod-refer
                 tt-work.nr-lote          = ob-etiqueta.nr-lote
                 tt-work.quantidade       = ob-etiqueta.quantidade
                 tt-work.situacao         = c-situacao
                 tt-work.un               = ITEM.un.
       END.                                 
    END.
END. 

/* Processa relatorio a partir das TEMP-TABLEs */
IF tt-param.l-excel THEN DO.
   CREATE "Excel.Application" chExcelApp.  
   chExcelApp:VISIBLE = TRUE.
   chWorkBook = chExcelApp:Workbooks:ADD().
   chWorkSheet = chExcelApp:Sheets:Item(1).

   chWorkbook:Worksheets(1):activate.
   chExcelApp:ActiveWindow:Zoom = 100.

   /* Configura a Linha do Titulo da Planilha */
   ASSIGN chWorkSheet:Rows("1:1"):RowHeight = 40
          chWorkSheet:Rows("1:1"):FONT:SIZE = 24
          chWorkSheet:Rows("1:1"):FONT:bold = FALSE.

   /*
   /* Inserir Logotipo da Tear e Alinhar Via Tamanho e Altura Logotipo */
   IF para-ped.estab-padrao = '1' THEN
      ASSIGN FILE-INFO:FILE-NAME = SEARCH("image/ima-dup.jpg").
   ELSE
      ASSIGN FILE-INFO:FILE-NAME = SEARCH("image/med-dup.jpg").

   ChWorkSheet:range("A1"):SELECT().
   ChWorkSheet:Pictures:INSERT(FILE-INFO:FULL-PATHNAME):SELECT. 
   chExcelApp:SELECTION:ShapeRange:ScaleWidth(1.9,FALSE,FALSE).
   chExcelApp:SELECTION:ShapeRange:ScaleHeight(0.61,FALSE,FALSE). 
   chExcelApp:SELECTION:ShapeRange:IncrementLeft(-49.5).
   chExcelApp:SELECTION:ShapeRange:IncrementTop( -48.75).
   */
END.


IF tt-param.l-resumo THEN DO.  /* s¢ gera em Excel */
   RUN esapi\connect-ima-med.p.

   FOR EACH tt-resumo BREAK BY tt-resumo.it-codigo.
       IF FIRST-OF(tt-resumo.it-codigo) THEN DO.
          RUN pi-acompanhar IN h-acomp (INPUT "Saldo Disponivel: " + STRING(tt-resumo.it-codigo)).


          EMPTY TEMP-TABLE tt-saldo-estoq.
          RUN esrp/esimce025rp.p (INPUT tt-resumo.it-codigo).

          FOR EACH tt-saldo-estoq WHERE 
                   tt-saldo-estoq.cod-estabel = tt-param.cod-estabel-ini NO-LOCK.

              FIND b-tt-resumo WHERE
                   b-tt-resumo.it-codigo = tt-saldo-estoq.it-codigo AND
                   b-tt-resumo.cod-refer = tt-saldo-estoq.cod-refer 
                   NO-LOCK NO-ERROR.
              IF AVAIL b-tt-resumo THEN 
                 ASSIGN b-tt-resumo.qtidade-dsp = b-tt-resumo.qtidade-dsp +
                                                  tt-saldo-estoq.qt-disponivel.
          END.
       END.
   END.
   IF CONNECTED('dbaux') THEN
      DISCONNECT dbaux.

   ASSIGN chworksheet:range("B1"):VALUE = "SALDO EM ESTOQUE AT A DATA " + STRING(tt-param.dt-emissao-fin,"99/99/9999").

   /* Configura Alinhamento Horizontal do Titulo da Planilha */
   ChWorkSheet:range("B1:N1"):SELECT().
   ChWorksheet:range("B1:N1"):Merge.
   Chworksheet:Range("B1:N1"):HorizontalAlignment = 3. /* Centralizado */
   Chworksheet:Range("B1:N1"):VerticalAlignment   = 2. /* Centralizado */
    
   /* Colorir Titulo da Planilha */
   chWorkSheet:Range("A1:N1"):FONT:ColorIndex     = 18. /* Avermelhado */
   chWorkSheet:Range("A1:N1"):Interior:ColorIndex = 2. /* Branco */
    
   /* Titulo das Colunas */
   ASSIGN chWorkSheet:Range("A2"):VALUE = "ITEM"
          chWorkSheet:Range("B2"):VALUE = "DESCRI€ÇO"     
          chWorkSheet:Range("C2"):VALUE = "REFERENCIA"
          chWorkSheet:Range("D2"):VALUE = "UN"
          chWorkSheet:Range("E2"):VALUE = "DT PRI ENTRADA"
          chWorkSheet:Range("F2"):VALUE = "DT ULT ENTRADA"
          chWorkSheet:Range("G2"):VALUE = "QTD EM " + STRING(tt-param.dt-emissao-fin,"99/99/9999")
          chWorkSheet:Range("H2"):VALUE = "QTD ATUAL"
          chWorkSheet:Range("I2"):VALUE = "QTD DISP. VENDA".

   /* Ajustar o Tamanho Dentro da Celula */     
   ASSIGN chworksheet:range("A2"):ShrinkToFit = TRUE    
          chworksheet:range("B2"):ShrinkToFit = TRUE    
          chworksheet:range("C2"):ShrinkToFit = TRUE
          chworksheet:range("D2"):ShrinkToFit = TRUE
          chworksheet:range("E2"):ShrinkToFit = TRUE
          chworksheet:range("F2"):ShrinkToFit = TRUE
          chworksheet:range("G2"):ShrinkToFit = TRUE
          chworksheet:range("H2"):ShrinkToFit = TRUE
          chworksheet:range("I2"):ShrinkToFit = TRUE.

   /* Tamanho das Colunas */
   ASSIGN chWorkSheet:Columns("A"):ColumnWidth = 10
          chWorkSheet:Columns("B"):ColumnWidth = 40
          chWorkSheet:Columns("C"):ColumnWidth = 10
          chWorkSheet:Columns("D"):ColumnWidth = 5
          chWorkSheet:Columns("E"):ColumnWidth = 16
          chWorkSheet:Columns("F"):ColumnWidth = 16
          chWorkSheet:Columns("G"):ColumnWidth = 16
          chWorkSheet:Columns("H"):ColumnWidth = 16
          chWorkSheet:Columns("I"):ColumnWidth = 16.
   
   /* Configura as Colunas da Planilha */
   ASSIGN chworksheet:range("A:D"):NumberFormat        = "@"
          chworksheet:range("G:I"):NumberFormat        = "###.###.##0,00"
          Chworksheet:range("G:I"):HorizontalAlignment = 4.

   /* Configura Cabe»alho das Colunas */
   chWorkSheet:Range("A2:P2"):SELECT().
   ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
          chExcelApp:SELECTION:FONT:SIZE               = 12
          chExcelApp:SELECTION:FONT:Bold               = FALSE 
          chExcelApp:SELECTION:Interior:ColorIndex     = 37
          chExcelApp:SELECTION:FONT:ColorIndex         = 11.

   ASSIGN i-linha = 2.
   FOR EACH tt-resumo BY tt-resumo.dt-pri-ent
                      BY tt-resumo.it-codigo
                      BY tt-resumo.cod-refer.

       FIND item WHERE
            item.it-codigo = tt-resumo.it-codigo NO-LOCK NO-ERROR.

       ASSIGN tt-resumo.qtidade-atu = 0.
       FOR EACH saldo-estoq WHERE
                saldo-estoq.it-codigo = tt-resumo.it-codigo AND
                saldo-estoq.cod-estabel = para-ped.estab-padrao AND
                saldo-estoq.cod-refer = tt-resumo.cod-refer NO-LOCK.
           ASSIGN tt-resumo.qtidade-atu = tt-resumo.qtidade-atu + saldo-estoq.qtidade-atu.
       END.

       ASSIGN i-linha = i-linha + 1.
       ASSIGN chWorkSheet:Range("A" + STRING(i-linha)):VALUE = tt-resumo.it-codigo
              chWorkSheet:Range("B" + STRING(i-linha)):VALUE = item.desc-item
              chWorkSheet:Range("C" + STRING(i-linha)):VALUE = tt-resumo.cod-refer
              chWorkSheet:Range("D" + STRING(i-linha)):VALUE = tt-resumo.un
              chWorkSheet:Range("E" + STRING(i-linha)):VALUE = tt-resumo.dt-pri-ent
              chWorkSheet:Range("F" + STRING(i-linha)):VALUE = tt-resumo.dt-ult-ent
              chWorkSheet:Range("G" + STRING(i-linha)):VALUE = tt-resumo.qtidade-data
              chWorkSheet:Range("H" + STRING(i-linha)):VALUE = tt-resumo.qtidade-atu
              chWorkSheet:Range("I" + STRING(i-linha)):VALUE = tt-resumo.qtidade-dsp.
   END.
   RELEASE OBJECT chWorkSheet.
   RELEASE OBJECT chWorkBook.
   RELEASE OBJECT chExcelApp.
END.
ELSE DO.
   IF tt-param.l-excel THEN DO.
      ASSIGN chworksheet:range("B1"):VALUE = "ESTOQUE DE DOCA x ETIQUETA ".

      /* Configura Alinhamento Horizontal do Titulo da Planilha */
      ChWorkSheet:range("B1:N1"):SELECT().
      ChWorksheet:range("B1:N1"):Merge.
      Chworksheet:Range("B1:N1"):HorizontalAlignment = 3. /* Centralizado */
      Chworksheet:Range("B1:N1"):VerticalAlignment   = 2. /* Centralizado */

      /* Colorir Titulo da Planilha */
      chWorkSheet:Range("A1:N1"):FONT:ColorIndex     = 18. /* Avermelhado */
      chWorkSheet:Range("A1:N1"):Interior:ColorIndex = 2. /* Branco */

      /* Titulo das Colunas */
      ASSIGN chWorkSheet:Range("A2"):VALUE = "EST"
             chWorkSheet:Range("B2"):VALUE = "DOCA"     
             chWorkSheet:Range("C2"):VALUE = "ETIQUETA"
             chWorkSheet:Range("D2"):VALUE = "NC"
             chWorkSheet:Range("E2"):VALUE = "DT EMISSÇO"
             chWorkSheet:Range("F2"):VALUE = "QUALIDADE"
             chWorkSheet:Range("G2"):VALUE = "ITEM"
             chWorkSheet:Range("H2"):VALUE = "REFER"
             chWorkSheet:Range("I2"):VALUE = "LOTE"
             chWorkSheet:Range("J2"):VALUE = "QUANTIDADE"
             chWorkSheet:Range("K2"):VALUE = "SITUA€ÇO"
             chWorkSheet:Range("L2"):VALUE = "UN".

      /* Ajustar o Tamanho Dentro da Celula */     
      ASSIGN chworksheet:range("A2"):ShrinkToFit = TRUE    
             chworksheet:range("B2"):ShrinkToFit = TRUE    
             chworksheet:range("C2"):ShrinkToFit = TRUE
             chworksheet:range("D2"):ShrinkToFit = TRUE
             chworksheet:range("E2"):ShrinkToFit = TRUE
             chworksheet:range("F2"):ShrinkToFit = TRUE
             chworksheet:range("G2"):ShrinkToFit = TRUE
             chworksheet:range("H2"):ShrinkToFit = TRUE
             chworksheet:range("I2"):ShrinkToFit = TRUE
             chworksheet:range("J2"):ShrinkToFit = TRUE
             chworksheet:range("K2"):ShrinkToFit = TRUE
             chworksheet:range("L2"):ShrinkToFit = TRUE.

      /* Tamanho das Colunas */
      ASSIGN chWorkSheet:Columns("A"):ColumnWidth = 10
             chWorkSheet:Columns("B"):ColumnWidth = 40
             chWorkSheet:Columns("C"):ColumnWidth = 10
             chWorkSheet:Columns("D"):ColumnWidth = 16
             chWorkSheet:Columns("E"):ColumnWidth = 16
             chWorkSheet:Columns("F"):ColumnWidth = 16
             chWorkSheet:Columns("G"):ColumnWidth = 16
             chWorkSheet:Columns("H"):ColumnWidth = 16
             chWorkSheet:Columns("I"):ColumnWidth = 16
             chWorkSheet:Columns("J"):ColumnWidth = 16
             chWorkSheet:Columns("K"):ColumnWidth = 16
             chWorkSheet:Columns("L"):ColumnWidth = 16.

      /* Configura as Colunas da Planilha */
      ASSIGN chworksheet:range("A:C"):NumberFormat        = "@"
             chworksheet:range("F:G"):NumberFormat        = "###.###.##0,00"
             Chworksheet:range("F:G"):HorizontalAlignment = 4.

      /* Configura Cabe»alho das Colunas */
      chWorkSheet:Range("A2:P2"):SELECT().
      ASSIGN chExcelApp:SELECTION:FONT:NAME               = "Consolas"
             chExcelApp:SELECTION:FONT:SIZE               = 12
             chExcelApp:SELECTION:FONT:Bold               = FALSE 
             chExcelApp:SELECTION:Interior:ColorIndex     = 37
             chExcelApp:SELECTION:FONT:ColorIndex         = 11.

      ASSIGN i-linha = 2.
   END.

   FOR EACH tt-work BREAK BY tt-work.doca
                          BY tt-work.num-etiqueta.
    
       ASSIGN i-qt-rl-doca  = i-qt-rl-doca  + 1
              i-qt-rl-geral = i-qt-rl-geral + 1.
    
       CASE tt-work.un.
           WHEN 'Kg' THEN ASSIGN de-tot-doca-kg = de-tot-doca-kg + tt-work.quantidade
                                 de-tot-ger-kg  = de-tot-ger-kg  + tt-work.quantidade.
    
           OTHERWISE ASSIGN de-tot-doca-mt = de-tot-doca-mt + tt-work.quantidade
                            de-tot-ger-mt  = de-tot-ger-mt  + tt-work.quantidade.
       END.
    
       IF tt-param.l-excel THEN DO.
          ASSIGN i-linha = i-linha + 1.
          ASSIGN chWorkSheet:Range("A" + STRING(i-linha)):VALUE = tt-work.cod-estabel      
                 chWorkSheet:Range("B" + STRING(i-linha)):VALUE = tt-work.doca             
                 chWorkSheet:Range("C" + STRING(i-linha)):VALUE = tt-work.num-etiqueta     
                 chWorkSheet:Range("D" + STRING(i-linha)):VALUE = tt-work.cod-depos           
                 chWorkSheet:Range("E" + STRING(i-linha)):VALUE = tt-work.dt-emissao       
                 chWorkSheet:Range("F" + STRING(i-linha)):VALUE = tt-work.qualidade        
                 chWorkSheet:Range("G" + STRING(i-linha)):VALUE = tt-work.it-codigo        
                 chWorkSheet:Range("H" + STRING(i-linha)):VALUE = tt-work.cod-refer        
                 chWorkSheet:Range("I" + STRING(i-linha)):VALUE = tt-work.nr-lote          
                 chWorkSheet:Range("J" + STRING(i-linha)):VALUE = tt-work.quantidade       
                 chWorkSheet:Range("K" + STRING(i-linha)):VALUE = tt-work.situacao         
                 chWorkSheet:Range("L" + STRING(i-linha)):VALUE = tt-work.un.
       END.
       ELSE DO.
          DISPLAY tt-work.cod-estabel
                  tt-work.doca 
                  tt-work.num-etiqueta                                
                  tt-work.cod-depos
                  tt-work.dt-emissao
                  tt-work.qualidade
                  tt-work.it-codigo 
                  tt-work.cod-refer
                  tt-work.nr-lote
                  tt-work.quantidade 
                  tt-work.situacao                  
                  tt-work.un
                  WITH FRAME f-detalhe.
        
          DOWN WITH FRAME f-detalhe.
       END.
    
       IF LAST-OF(tt-work.doca) THEN DO:
          IF tt-param.l-excel THEN DO.
              ASSIGN i-linha = i-linha + 1.
              ASSIGN chWorkSheet:Range("G" + STRING(i-linha)):VALUE = "TOTAL DOCA Mts"
                     chWorkSheet:Range("J" + STRING(i-linha)):VALUE = de-tot-doca-mt.

              ASSIGN i-linha = i-linha + 1.
              ASSIGN chWorkSheet:Range("G" + STRING(i-linha)):VALUE = "TOTAL DOCA Kgs"
                     chWorkSheet:Range("J" + STRING(i-linha)):VALUE = de-tot-doca-kg.

              ASSIGN i-linha = i-linha + 1.
              ASSIGN chWorkSheet:Range("G" + STRING(i-linha)):VALUE = "TOTAL PE€AS"
                     chWorkSheet:Range("J" + STRING(i-linha)):VALUE = i-qt-rl-doca.
          END.
          ELSE DO.
              DISPLAY "Total Doca Mts" @ tt-work.it-codigo
                      de-tot-doca-mt @ tt-work.quantidade
                      WITH FRAME f-detalhe.
              DOWN WITH FRAME f-detalhe.
        
              DISPLAY "Total Doca Kgs" @ tt-work.it-codigo
                      de-tot-doca-kg @ tt-work.quantidade
                      WITH FRAME f-detalhe.
              DOWN WITH FRAME f-detalhe.
        
              DISPLAY "Total Pe‡as " @ tt-work.it-codigo
                      i-qt-rl-doca @ tt-work.quantidade
                      WITH FRAME f-detalhe.
              DOWN 2 WITH FRAME f-detalhe.
          END.

          IF tt-param.l-imp-layout THEN DO:
             CREATE tt-layout.
             ASSIGN tt-layout.localizacao = tt-work.doca
                    tt-layout.peca        = i-qt-rl-doca
                    tt-layout.quant       = de-tot-doca-mt + de-tot-doca-kg.
          END.
    
          ASSIGN de-tot-doca-mt = 0
                 de-tot-doca-kg = 0
                 i-qt-rl-doca   = 0.
       END.
   END.

   IF tt-param.l-excel THEN DO.
       ASSIGN i-linha = i-linha + 1.
       ASSIGN chWorkSheet:Range("G" + STRING(i-linha)):VALUE = "TOTAL GERAL Mts"
              chWorkSheet:Range("J" + STRING(i-linha)):VALUE = de-tot-ger-mt.

       ASSIGN i-linha = i-linha + 1.
       ASSIGN chWorkSheet:Range("G" + STRING(i-linha)):VALUE = "TOTAL GERAL Kgs"
              chWorkSheet:Range("J" + STRING(i-linha)):VALUE = de-tot-ger-kg.

       ASSIGN i-linha = i-linha + 1.
       ASSIGN chWorkSheet:Range("G" + STRING(i-linha)):VALUE = "TOTAL PE€AS"
              chWorkSheet:Range("J" + STRING(i-linha)):VALUE = i-qt-rl-geral.
   END.
   ELSE DO.
       DOWN 1 WITH FRAME f-detalhe.
        
       DISPLAY "Total GERAL Mts" @ tt-work.it-codigo
               de-tot-ger-mt @ tt-work.quantidade
               WITH FRAME f-detalhe.
       DOWN WITH FRAME f-detalhe.
        
       DISPLAY "Total GERAL Kgs" @ tt-work.it-codigo
               de-tot-ger-kg @ tt-work.quantidade
               WITH FRAME f-detalhe.
       DOWN WITH FRAME f-detalhe.
        
       DISPLAY "Total Pe‡as" @ tt-work.it-codigo
               i-qt-rl-geral @ tt-work.quantidade
               WITH FRAME f-detalhe.
       DOWN WITH FRAME f-detalhe.
    
       IF tt-param.imp-param THEN DO:
          PAGE.
          DISPLAY tt-param.cod-estabel-ini
                  tt-param.cod-estabel-fin
                  tt-param.localizacao-ini FORMAT "XXX/XXX"
                  tt-param.localizacao-fin FORMAT "XXX/XXX"
                  tt-param.dt-emissao-ini
                  tt-param.dt-emissao-fin
                  tt-param.nr-lote-ini
                  tt-param.nr-lote-fin
                  tt-param.fi-ini-it-codigo 
                  tt-param.fi-fin-it-codigo 
                  tt-param.fi-ini-cod-refer 
                  tt-param.fi-fin-cod-refer 
                  tt-param.fi-desenho       
                  tt-param.l-inc-exc        
                  tt-param.opc-artigo       
                  WITH FRAME f-param.
       END. 
   END.

   IF tt-param.l-excel THEN DO.
      RELEASE OBJECT chWorkSheet.
      RELEASE OBJECT chWorkBook.
      RELEASE OBJECT chExcelApp.
   END.
END.

/* Por M rcio */
IF tt-param.l-imp-layout THEN DO:
   ASSIGN c-estabel = if tt-param.cod-estabel-ini = "1" THEN "IMA" ELSE "MED".
   ASSIGN c-arquivo = SESSION:TEMP-DIRECTORY + "essp0147rp-" + c-estabel + ".txt".
   OUTPUT TO VALUE(c-arquivo).
   FOR EACH tt-layout NO-LOCK:
       EXPORT DELIMITER ";"
              tt-layout.localizacao format "999999"
              tt-layout.peca        format ">>>>9"
              tt-layout.quant       format ">>>,>>9.99".
   END.
   OUTPUT CLOSE.
   ASSIGN c-arquivo = "start excel /n /t T:\especificos\excel\essp0147rp-" + c-estabel + ".xls".
   OS-COMMAND SILENT VALUE(c-arquivo).
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.

