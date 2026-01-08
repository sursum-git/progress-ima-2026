/*********************************************************************************
 **  DATASUL MINAS GERAIS                                                       **
 *********************************************************************************
 **  Cliente     : IMA Tecidos                                                  **
 **  Programa    : IMCD020RP.P                                                  **
 **  VersÆo      : 1.00.00.000 - 23/09/2002                                     **
 **  Autor       : João Gabriel Costa Rocha                                     **
 **  Descri‡Æo   : Relatório Tabela de Preços                                   **
 **                                                                             **
 **                                                                             **
 *********************************************************************************/
 /***** DEFINI€ÇO ****************************************************************/
{include/i-prgvrs.i imcd020RP 2.04.00.001}

DEF BUFFER empresa FOR mgcad.empresa.

DEFINE temp-table tt-param no-undo
    FIELD destino          as integer
    FIELD arquivo          as char format "x(35)"
    FIELD usuario          as char format "x(12)"
    FIELD data-exec        as date
    FIELD hora-exec        as integer
    FIELD c-cod-estabel    LIKE ped-venda.cod-estabel
    FIELD c-ini-it-codigo  LIKE ITEM.it-codigo
    FIELD c-fim-it-codigo  LIKE ITEM.it-codigo
    FIELD c-ini-cod-refer  LIKE referencia.cod-refer
    FIELD c-fim-cod-refer  LIKE referencia.cod-refer
    FIELD c-nr-tabpre      LIKE tb-preco.nr-tabpre
    FIELD i-ind-finan      AS INTEGER
    FIELD rd-emite         AS logical
    FIELD tg-excel         AS LOG
    FIELD tg-saldo         AS LOG.

DEF TEMP-TABLE tt-itens 
    FIELD it-codigo LIKE saldo-estoq.it-codigo
    FIELD cod-refer LIKE saldo-estoq.cod-refer
    FIELD qtidade-atu LIKE saldo-estoq.qtidade-atu
    INDEX indice1 it-codigo cod-refer.

DEFINE TEMP-TABLE tt-raw-digita
    FIELD raw-digita AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.


  
{include/i-rpvar.i}
 
 
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR  i-cont             AS INT NO-UNDO.
DEF VAR  h-acomp            AS HANDLE.
DEF VAR  c-const            AS CHAR FORMAT "x(80)".
DEF VAR  c-desc-item        AS CHAR FORMAT "x(47)".
DEF VAR  c-un               AS CHAR FORMAT "x(02)".
DEF VAR  primvez            AS CHAR FORMAT "x(01)".
DEF VAR  i-ct               AS INT FORMAT "99".
DEF VAR  i-lin              AS INT FORMAT "99".
DEF VAR  de-ind             AS DEC EXTENT 12 FORMAT "9.9999".
DEF VAR  de-saldo           AS DEC FORMAT "->>>>>,>>9.99".
DEF VAR  de-qt-min          LIKE saldo-estoq.qtidade-atu.

DEFINE VARIABLE chExcelApp  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkBook  AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkSheet AS COM-HANDLE NO-UNDO.
DEF VAR i-linha AS INTEGER NO-UNDO.

&IF DEFINED(EXCLUDE-fn-letra) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fn-letra Procedure 
FUNCTION fn-letra RETURNS CHARACTER
  ( coluna AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


DEF STREAM str-rp.

form   
     "Tabela de Pre‡o:" AT 1 tb-preco.nr-tabpre AT 18 " - " AT 26 tb-preco.descricao AT 29  " - Estab.: " AT 70 tt-param.c-cod-estabel AT 85 skip(1) 
     "Item      Descri‡Æo                                        Un                  Precos (R$/U$)"                 AT 01 SKIP  
     c-const                                                                                                         AT 63 SKIP
     "--------- ------------------------------------------------ -- -----------------------------------------------" AT 01 
     with no-box no-label page-top width 142 stream-io frame f-cab1.

{include/i-rpout.i}
{include/i-rpcab.i}

FIND FIRST param-global no-lock no-error.
IF AVAIL param-global THEN
    FIND FIRST empresa WHERE empresa.ep-codigo = param-global.empresa-pri no-lock no-error.

ASSIGN c-programa     = "IMCD020RP"
       c-versao       = "2.04"
       c-revisao      = "1.00.000"
       c-empresa      = IF AVAIL empresa THEN empresa.razao-social ELSE "EMPRESA NÇO ENCONTRADA."
       c-sistema      = "imp"
       c-titulo-relat = "Relatório Tabela de Preços".

/***** BLOCO PRINCIPAL *********************************************************/
run utp/ut-acomp.p persistent set h-acomp.
run pi-inicializar in h-acomp(input "Imprimindo Tabela de Pre‡os:  " + tt-param.c-nr-tabpre  ).


IF tt-param.tg-excel THEN DO:
   CREATE "Excel.Application" chExcelApp.
   chWorkBook = chExcelApp:Workbooks:Add("T:\especificos\ems2\ima\modelo-xlt\imcd020.xlt").
   /*chWorkBook = chExcelApp:Workbooks:Add().*/
   chWorkSheet = chExcelApp:Sheets:Item(1).
   chexcelapp:VISIBLE = FALSE.
   chWorkSheet:Range("B1"):VALUE = tt-param.c-nr-tabpre + "    Data: " + STRING(TODAY).
   ASSIGN i-linha = 2.
END.

FOR EACH saldo-estoq WHERE
         saldo-estoq.cod-estabel = tt-param.c-cod-estabel AND
         saldo-estoq.it-codigo  >= tt-param.c-ini-it-codigo AND
         saldo-estoq.it-codigo  <= tt-param.c-fim-it-codigo AND
         saldo-estoq.cod-refer  >= tt-param.c-ini-cod-refer AND
         saldo-estoq.cod-refer  <= tt-param.c-fim-cod-refer AND
         saldo-estoq.qtidade-atu > 0 NO-LOCK.

    RUN pi-acompanhar in h-acomp(input "Separando Item:  " + saldo-estoq.it-codigo + " Ref: " + saldo-estoq.cod-refer ).

    FIND tt-itens WHERE
         tt-itens.it-codigo = saldo-estoq.it-codigo AND
         tt-itens.cod-refer = saldo-estoq.cod-refer NO-ERROR.
    IF NOT AVAIL tt-itens THEN DO.
       CREATE tt-itens.
       ASSIGN tt-itens.it-codigo = saldo-estoq.it-codigo
              tt-itens.cod-refer = saldo-estoq.cod-refer.
    END.
    ASSIGN tt-itens.qtidade-atu = tt-itens.qtidade-atu + saldo-estoq.qtidade-atu.
END.

FIND FIRST tab-finan WHERE
           tab-finan.dt-ini-val <= TODAY AND 
           tab-finan.dt-fim-val >= TODAY NO-LOCK NO-ERROR.
IF AVAIL tab-finan THEN DO:
   DO i-ct = 1 TO i-ind-finan:
      IF  i-ct = 1  THEN DO:
          c-const = "     Base ".
          IF tt-param.tg-excel THEN
             chWorkSheet:Range(fn-letra(3 + i-ct) + STRING(i-linha)):VALUE = "Base".
      END.
      ELSE DO:
         ASSIGN c-const = c-const + "       " + string(tab-dia-fin[i-ct]) +  " d " .
         IF tt-param.tg-excel THEN
             chWorkSheet:Range(fn-letra(3 + i-ct) + STRING(i-linha)):VALUE = string(tab-dia-fin[i-ct]) +  " d".
      END.

      ASSIGN de-ind[i-ct] = tab-finan.tab-ind-fin[i-ct].
   END.
   IF tt-param.tg-excel AND 
      tt-param.c-cod-estabel = '1' THEN
      chWorkSheet:Range("H" + STRING(i-linha)):VALUE = "Vl.Corte".
END.

FIND FIRST para-fat NO-LOCK NO-ERROR.
FIND FIRST para-ped NO-LOCK NO-ERROR.
IF NOT AVAIL para-ped THEN RETURN 'adm-error':u.

FIND FIRST tb-preco WHERE
           tb-preco.nr-tabpre = tt-param.c-nr-tabpre NO-LOCK no-error.
IF AVAIL tb-preco THEN DO:
     

    VIEW FRAME f-cabec.
    DISP tb-preco.nr-tabpre
         tb-preco.descricao
         tt-param.c-cod-estabel
         c-const WITH  FRAME f-cab1.
    VIEW FRAME f-rodape.

    FOR EACH tt-itens NO-LOCK,
        FIRST preco-item WHERE 
              preco-item.nr-tabpre = tb-preco.nr-tabpre AND
              preco-item.it-codigo = tt-itens.it-codigo AND
              preco-item.cod-refer = tt-itens.cod-refer AND
              preco-item.situacao = 1 NO-LOCK 
        BREAK BY tt-itens.it-codigo.

        RUN pi-acompanhar in h-acomp (INPUT "Processando Item:  " + preco-item.it-codigo + " Ref: " + preco-item.cod-refer ).

        IF FIRST-OF(tt-itens.it-codigo) THEN DO:
           ASSIGN i-cont = 7.

           FIND FIRST item WHERE 
                      item.it-codigo = preco-item.it-codigo NO-LOCK NO-ERROR.
           ASSIGN c-desc-item  = ""
                  c-un         = "".
           IF AVAIL item THEN 
              ASSIGN c-desc-item  = item.desc-item
                     c-un         = ITEM.un.

           IF tt-param.tg-excel THEN DO:
              ASSIGN i-linha = i-linha + 1.
              chWorkSheet:Range("A" + STRING(i-linha)):VALUE = preco-item.it-codigo.
              chWorkSheet:Range("B" + STRING(i-linha)):VALUE = c-desc-item.
              chWorkSheet:Range("C" + STRING(i-linha)):VALUE = c-un.
           END.

           PUT SKIP tt-itens.it-codigo         AT 02 FORMAT "x(9)"
               c-desc-item                     AT 11
               c-un                            AT 60.

           ASSIGN i-lin = 62. 
           DO i-ct =  1 TO i-ind-finan:
              PUT (preco-item.preco-venda * de-ind[i-ct]) FORMAT ">>>,>>9.99"        AT i-lin.
              ASSIGN i-lin = i-lin + 12.
                 
              IF tt-param.tg-excel THEN DO:
                 chWorkSheet:Range(fn-letra(3 + i-ct) + STRING(i-linha)):VALUE = (preco-item.preco-venda * de-ind[i-ct]).

                 IF tt-param.c-cod-estabel = '1' THEN
                    chWorkSheet:Range('h' + STRING(i-linha)):VALUE = preco-item.preco-venda * de-ind[4] * 1.05.  

              END.
           END.
           PUT SKIP.
        END.

        IF tt-param.rd-emite = YES THEN DO:
           ASSIGN de-saldo = de-saldo + tt-itens.qtidade-atu.
                 
           /* Subtrai Pedidos PI */
           FOR EACH ped-venda WHERE
                    ped-venda.cod-sit-ped = 1 NO-LOCK,
               EACH ped-item OF ped-venda WHERE
                    ped-item.cod-sit-item = 1 AND
                    ped-item.it-codigo = tt-itens.it-codigo AND
                    ped-item.cod-refer = tt-itens.cod-refer NO-LOCK.

               IF ped-venda.tp-pedido = 'PI' AND
                  ped-venda.dt-entrega > TODAY THEN NEXT.

               ASSIGN de-saldo = de-saldo - ped-item.qt-pedida.
           END.

           /* Subtrai Notas em Aberto */ 
           FOR EACH nota-fiscal WHERE
                    nota-fiscal.cod-estabe = saldo-estoq.cod-estabel AND
                    nota-fiscal.serie = para-fat.serie-pad AND
                    nota-fiscal.dt-cancela = ? AND
                    nota-fiscal.dt-confirma = ? NO-LOCK,
               EACH it-nota-fisc OF nota-fiscal WHERE
                    it-nota-fisc.it-codigo = tt-itens.it-codigo AND
                    it-nota-fisc.cod-refer = tt-itens.cod-refer 
                    NO-LOCK.
 
               IF nota-fiscal.nome-abrev <> '' THEN NEXT.  /* ‚ triangular */
               ASSIGN de-saldo = de-saldo - it-nota-fisc.qt-faturada[1].
           END.

           IF de-saldo > 0 THEN DO:
              IF i-cont = 7 THEN DO:
                 PUT SKIP.
                 ASSIGN i-cont = 1
                        i-lin = 14.
              END.

              PUT TRIM(tt-itens.cod-refer) FORMAT "X(03)" AT i-lin 
                  de-saldo SPACE(1) "|" .

              ASSIGN i-lin = i-lin + 20
                     i-cont = i-cont + 1
                     de-saldo = 0.
           END.
        END.

        IF LAST-OF(tt-itens.it-codigo) THEN
           PUT SKIP(1).
    END.
END.
VIEW FRAME f-rodape.

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
 
&ANALYZE-RESUME
&ENDIF


IF tt-param.tg-excel THEN DO:
   chexcelapp:VISIBLE = TRUE.
   /*ASSIGN salvar-como = caminho + nome-arquivo + ".xls".*/
   RELEASE OBJECT chWorkSheet.
   /*chWorkBook:SaveAs(salvar-como,-4143,,,,,).
   chWorkBook:Save().        
   chWorkBook:Close().  
   chExcelApp:Quit().*/
   RELEASE OBJECT chWorkBook.
   RELEASE OBJECT chExcelApp.
END.

RUN pi-finalizar in h-acomp    .
INPUT STREAM str-rp CLOSE.
{include/i-rpclo.i &STREAM="stream str-rp"}
RETURN "Ok":U.
