/* Programa: ESPD001.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Pedidos
** Objetivo: Gerar o relatorio de Pedidos de Vendas p/Separacao de Mercadorias
** Autor...: Sandro Wiest/Gilvando de Souza Araujo - Julho/96
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL / SANTA ELISABETH
**
** Conversao para EMS 2.04:
**   Programa: ESPD001.P  =>  ESPD0001RP.P
**   Autor...: Prodb - Toninho
**   Data....: Mar‡o/2005
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESPD0001RP 2.04.00.000}

define temp-table tt-param  no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD ini-nr-pedcli     LIKE ped-venda.nr-pedcli
       FIELD fin-nr-pedcli     LIKE ped-venda.nr-pedcli
       FIELD ini-it-codigo     LIKE ped-item.it-codigo
       FIELD fin-it-codigo     LIKE ped-item.it-codigo
       FIELD ini-cod-refer     LIKE ped-item.cod-refer 
       FIELD fin-cod-refer     LIKE ped-item.cod-refer 
       FIELD ini-dt-entrega    LIKE ped-venda.dt-entrega
       FIELD fin-dt-entrega    LIKE ped-venda.dt-entrega
       FIELD ini-dt-implant    LIKE ped-venda.dt-implant
       FIELD fin-dt-implant    LIKE ped-venda.dt-implant
       FIELD cod-estabel       LIKE saldo-estoq.cod-estabel
       FIELD cod-depos         LIKE saldo-estoq.cod-depos
       FIELD so-aprov          AS LOG
       FIELD so-procpron       AS LOG
       FIELD qualidade         AS CHAR FORMAT "x"
       FIELD mercado           AS CHAR FORMAT "x"
       FIELD opc-artigo        AS CHAR FORMAT "x"
       FIELD tipo-rel          AS INT
       FIELD cart-estoque      AS LOG
       FIELD imp-prpcpt        AS LOG
       FIELD imp-param         AS LOG.

DEF TEMP-TABLE tt-raw-digita
   	FIELD raw-digita	AS RAW.

DEF TEMP-TABLE tt-work
    FIELD it-codigo    AS CHAR FORMAT "x(7)" 
    FIELD cod-refer    AS CHAR FORMAT "x(8)" 
    FIELD descricao    AS CHAR FORMAT "x(20)"
    FIELD cod-obsoleto AS CHAR FORMAT "x(2)"
    FIELD obsoleto     AS CHAR 
    FIELD acondic      AS CHAR 
    FIELD sld-estoque  LIKE saldo-estoq.qtidade-atu
    FIELD fundo        AS CHAR FORMAT "x(4)"
    INDEX indice1 it-codigo cod-refer acondic.

DEF TEMP-TABLE tt-work2
    FIELD it-codigo       AS CHAR FORMAT "x(7)" 
    FIELD cod-refer       AS CHAR FORMAT "x(8)" 
    FIELD acondic         AS CHAR 
    FIELD qtd-aberto      AS DEC FORMAT "->>>,>>9.99"
    FIELD qtd-reserva     AS DEC FORMAT "->>>,>>9.99"
    FIELD qtd-reserva-can AS DEC FORMAT "->>>,>>9.99"
    INDEX indice1 it-codigo cod-refer acondic.

/* recebimento de parƒmetros */
DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
DEF VAR h-acomp AS HANDLE NO-UNDO.

DEF VAR c-mercado             AS CHAR.
DEF VAR c-qualidade           AS CHAR.
DEF VAR c-artigo              AS CHAR.
DEF VAR l-tipo-rel            AS LOG FORMAT "Resumido/Detalhado".
DEF VAR tger-sld-estoque      AS DEC FORMAT ">>>>,>>9.99". 
DEF VAR tger-qtd-aberto       AS DEC FORMAT ">>>>,>>9.99".
DEF VAR tger-qtd-reserva      AS DEC FORMAT ">>>,>>9.99". 
DEF VAR tger-qtd-reserva-can  AS DEC FORMAT ">>,>>9.99".   
DEF VAR tger-qtd-prog         AS DEC FORMAT ">>>>,>>9".
DEF VAR tger-qtd-proc         AS DEC FORMAT ">>>>,>>9".
DEF VAR tger-qtd-pron         AS DEC FORMAT ">>>>,>>9".
DEF VAR i-cont                AS INT.

DEF VAR de-qtd-prog           LIKE ob-pcp-ref.qtd-sld-prog.
DEF VAR de-qtd-proc           LIKE ob-pcp-ref.qtd-proc.
DEF VAR de-qtd-pron           LIKE ob-pcp-ref.qtd-pron.
DEF VAR da-ult-prog           LIKE ob-pcp-ref.dt-ult-prog.
DEF VAR da-ult-proc           LIKE ob-pcp-ref.dt-ult-proc.
DEF VAR da-ult-pron           LIKE ob-pcp-ref.dt-ult-pron.

/* defini‡Æo de frames do relat¢rio */

FORM 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.cod-estabel     LABEL "Estabel..." SKIP
    tt-param.cod-depos       LABEL "Deposito.." SKIP
    tt-param.so-aprov        LABEL "So Aprov.." SKIP
    tt-param.so-procpron     LABEL "So ProPron" SKIP
    c-mercado                LABEL "Mercado..." SKIP
    c-qualidade              LABEL "Qualidade." SKIP
    c-artigo                 LABEL "Artigo...." SKIP
    l-tipo-rel               LABEL "Tipo Rel.." SKIP
    tt-param.ini-nr-pedcli   LABEL "Pedidos..."
    "A"  AT 30
    tt-param.fin-nr-pedcli   NO-LABELS SKIP
    tt-param.ini-it-codigo   LABEL "Itens....."
    "A"  AT 30
    tt-param.fin-it-codigo   NO-LABELS SKIP
    tt-param.ini-cod-refer   LABEL "Referencia"
    "A"  AT 30
    tt-param.fin-cod-refer   NO-LABELS SKIP
    tt-param.ini-dt-entrega  LABEL "Dt.Entrega"
    "A"  AT 30
    tt-param.fin-dt-entrega  NO-LABELS SKIP
    tt-param.ini-dt-implant  LABEL "Dt.Implant"
    "A"  AT 30
    tt-param.fin-dt-implant  NO-LABELS
    SKIP(1)
    WITH FRAME f-param SIDE-LABELS NO-BOX WIDTH 132 STREAM-IO.
    
FORM                                                 
    tt-work.it-codigo        LABEL "Item"          FORMAT "x(7)"
    tt-work.cod-refer        LABEL "Refer"         FORMAT "XX.XXXX.X" 
    tt-work.descricao        LABEL "Descricao"     FORMAT "x(16)"
    tt-work.cod-obsoleto     LABEL "CO"            FORMAT "x(2)"
    tt-work.obsoleto         LABEL "Obsol"         FORMAT "x(5)"
    tt-work2.acondic         LABEL "Acond."        FORMAT "x(10)" 
    tt-work2.qtd-reserva-can LABEL "Res.Cancel"    FORMAT ">>,>>9.99"
    tt-work.sld-estoque      LABEL "Estoque"       FORMAT "->>>,>>9.99"
    tt-work2.qtd-aberto      LABEL "Carteira"      FORMAT "->>>,>>9.99"
    tt-work2.qtd-reserva     LABEL "Reservado"     FORMAT ">>>,>>9.99"
    tt-work.fundo            LABEL "Fund"          FORMAT "x(4)"
    de-qtd-prog              LABEL "Program"       FORMAT ">>>>,>>9"
    de-qtd-proc              LABEL "Processo"      FORMAT ">>>>,>>9"
    de-qtd-pron              LABEL "Pronto"        FORMAT ">>>>,>>9"
    WITH FRAME f-detalhe WIDTH 134 DOWN NO-LABEL STREAM-IO.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST empresa
     WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 

ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").
    
{utp/ut-liter.i ESPECIFICOS * r}
ASSIGN c-sistema = TRIM(RETURN-VALUE).

{utp/ut-liter.i Relatorio_de_Programa‡Æo_de_Produ‡Æo * r}
ASSIGN c-titulo-relat = TRIM(RETURN-VALUE).

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

ASSIGN l-tipo-rel = tt-param.tipo-rel = 1.
IF tt-param.qualidade = "1" THEN
   ASSIGN c-qualidade = "Perfeito".
ELSE
IF tt-param.qualidade = "2" THEN
   ASSIGN c-qualidade = "Leve defeito".
ELSE
   ASSIGN c-qualidade = "Ambos".

IF tt-param.mercado = "1" THEN
   ASSIGN c-mercado = "Interno".
ELSE
IF tt-param.mercado = "2" THEN
   ASSIGN c-mercado = "Externo".
ELSE
   ASSIGN c-mercado = "Ambos".

IF tt-param.opc-artigo = "I" THEN
   ASSIGN c-artigo = "Indigo".
ELSE
IF tt-param.opc-artigo = "O" THEN
   ASSIGN c-artigo = "Outros".
ELSE
   ASSIGN c-artigo = "Ambos".

FOR EACH ped-item WHERE
         ped-item.it-codigo  >= tt-param.ini-it-codigo  AND
         ped-item.it-codigo  <= tt-param.fin-it-codigo  AND
         ped-item.cod-refer  >= tt-param.ini-cod-refer  AND
         ped-item.cod-refer  <= tt-param.fin-cod-refer  AND
         ped-item.nr-pedcli  >= tt-param.ini-nr-pedcli  AND                     
         ped-item.nr-pedcli  <= tt-param.fin-nr-pedcli  AND  
         LOOKUP(STRING(ped-item.cod-sit-item,"9"),"1,2,5,6") > 0 NO-LOCK,
   FIRST ped-item-ext WHERE 
         ped-item-ext.nome-abrev   = ped-item.nome-abrev   AND 
         ped-item-ext.nr-pedcli    = ped-item.nr-pedcli    AND 
         ped-item-ext.nr-sequencia = ped-item.nr-sequencia AND 
         ped-item-ext.it-codigo    = ped-item.it-codigo    AND 
         ped-item-ext.cod-refer    = ped-item.cod-refer    AND 
         ((SUBSTR(ped-item-ext.lote,2,1) = "P" AND tt-param.qualidade = "1") OR
          (SUBSTR(ped-item-ext.lote,2,1) = "D" AND tt-param.qualidade = "2") OR
                                                  (tt-param.qualidade = "3"))
         NO-LOCK,
   FIRST ped-venda WHERE 
         ped-venda.cod-estabel =  tt-param.cod-estabel AND
         ped-venda.nome-abrev  =  ped-item.nome-abrev AND 
         ped-venda.nr-pedcli   =  ped-item.nr-pedcli  AND 
         ped-venda.dt-implant  >= tt-param.ini-dt-implant AND 
         ped-venda.dt-implant  <= tt-param.fin-dt-implant AND 
         ped-venda.dt-entrega  >= tt-param.ini-dt-entrega AND
         ped-venda.dt-entrega  <= tt-param.fin-dt-entrega AND
         (((ped-venda.cod-sit-aval = 3 OR                            
            ped-venda.cod-sit-aval = 2) AND                          
            tt-param.so-aprov = YES) OR 
            tt-param.so-aprov = NO   OR                                  
            (ped-venda.cod-cond-pag > 0 AND                             
             ped-venda.cod-cond-pag < 4)) NO-LOCK
   BY ped-item.it-codigo
   BY ped-item.cod-refer.
    
    RUN pi-acompanhar IN h-acomp (INPUT ped-item.it-codigo + " " + ped-venda.nr-pedcli).

    FIND emitente WHERE
         emitente.cod-emitente = ped-venda.cod-emitente NO-LOCK NO-ERROR.

    CASE tt-param.mercado.
        WHEN '1' THEN 
            IF emitente.pais <> "brasil" OR 
               ped-venda.nat-oper BEGINS '7' 
               THEN NEXT.
        WHEN '2' THEN 
            IF emitente.pais = "brasil" OR
               NOT ped-venda.nat-oper BEGINS '7' THEN NEXT.
    END CASE.

    IF ped-item.cod-sit-item = 6 THEN DO.
       FIND ped-item-res WHERE
            ped-item-res.nome-abrev   = ped-item.nome-abrev AND
            ped-item-res.nr-pedcli    = ped-item.nr-pedcli AND
            ped-item-res.it-codigo    = ped-item.it-codigo AND
            ped-item-res.nr-sequencia = ped-item.nr-sequencia 
            NO-LOCK NO-ERROR.

       IF NOT AVAIL ped-item-res THEN NEXT.
    END.

    FIND item WHERE
         item.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.

    FIND item-ext WHERE item-ext.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.
    IF AVAIL item-ext AND tt-param.opc-artigo <> "A" THEN DO:
       IF (tt-param.opc-artigo = "I" AND item-ext.indigo = NO) OR
          (tt-param.opc-artigo = "O" AND item-ext.indigo = YES) THEN NEXT.
    END.

    FIND referencia WHERE 
         referencia.cod-refer = ped-item.cod-refer NO-LOCK NO-ERROR.
    IF NOT AVAIL referencia THEN NEXT.

    FIND ref-item-ext WHERE
         ref-item-ext.it-codigo = ped-item.it-codigo AND
         ref-item-ext.cod-refer = ped-item.cod-refer NO-LOCK NO-ERROR.

    FIND referencia-ext WHERE
         referencia-ext.cod-refer = referencia.cod-refer NO-LOCK NO-ERROR.

    ASSIGN de-qtd-prog = 0   
           de-qtd-proc = 0    
           de-qtd-pron = 0.
    FOR EACH ob-pcp WHERE
             ob-pcp.it-codigo = ped-item.it-codigo NO-LOCK,
        EACH ob-pcp-ref OF ob-pcp WHERE
             ob-pcp-ref.situacao = 1 AND
             ob-pcp-ref.cod-refer = ped-item.cod-refer NO-LOCK.

        ASSIGN de-qtd-prog = de-qtd-prog + ob-pcp-ref.qtd-sld-prog
               de-qtd-proc = de-qtd-proc + ob-pcp-ref.qtd-proc
               de-qtd-pron = de-qtd-pron + ob-pcp-ref.qtd-pron.
    END.

    IF tt-param.so-procpron AND
       de-qtd-proc = 0 AND 
       de-qtd-pron = 0 THEN NEXT.

    FIND FIRST tt-work WHERE
               tt-work.it-codigo = ped-item.it-codigo AND
               tt-work.cod-refer = ped-item.cod-refer AND
               tt-work.acondic = SUBSTR(ped-item-ext.acondicionamento,1,1)
               NO-ERROR.

    IF NOT AVAIL tt-work THEN DO:
       CREATE tt-work.
       ASSIGN tt-work.it-codigo = ped-item.it-codigo
              tt-work.cod-refer = ped-item.cod-refer
              tt-work.descricao = item.descricao-1 + " " + SUBSTR(referencia.descricao,1,3)
              tt-work.acondic = SUBSTR(ped-item-ext.acondicionamento,1,1).
/*     
       /*-- Verifica se o Item tem nome comercial diferenciado --*/
       IF SUBSTR(ped-item.cod-refer,1,2) <> "" THEN DO:
          FIND item-ext WHERE item-ext.it-codigo = ITEM.it-codigo 
                        NO-LOCK NO-ERROR.
          IF AVAIL item-ext THEN DO:
             DO i-cont = 1 TO 99:
                IF item-ext.cod-gr-ref[i-cont] = SUBSTR(ped-item.cod-refer,1,2) THEN DO:
                   ASSIGN tt-work.descricao = item-ext.des-gr-ref[i-cont] +
                                              " " + SUBSTR(referencia.descricao,1,3).
                   LEAVE.
                END.
             END.
          END.
       END.
*/

       ASSIGN tt-work.fundo = IF AVAIL referencia-ext THEN referencia-ext.cod-fundo
                                                      ELSE "".
       IF ITEM.tipo-con-est = 4 THEN
          ASSIGN tt-work.obsoleto = IF ref-item-ext.cod-obsoleto <= "0" 
                                    THEN "Lanc."                                              
                                    ELSE IF ref-item-ext.cod-obsoleto = "1" 
                                         THEN "F.Prd"                                              
                                         ELSE IF ref-item-ext.cod-obsoleto = "2"
                                              THEN "Colec"                                              
                                              ELSE IF ref-item-ext.cod-obsoleto = "3"
                                                   THEN "LD/Ret"
                                                   ELSE IF ref-item-ext.cod-obsoleto = "4"
                                                        THEN "Exclusiv"
                                                        ELSE "Export"
                 tt-work.cod-obsoleto = ref-item-ext.cod-obsoleto.

       /* Calcula Saldo em Estoque */
       FOR EACH saldo-estoq WHERE
                saldo-estoq.cod-estabel = tt-param.cod-estabel AND
                saldo-estoq.cod-depos   = tt-param.cod-depos AND
                saldo-estoq.it-codigo   = ped-item.it-codigo AND          
                saldo-estoq.cod-refer   = ped-item.cod-refer AND
                SUBSTR(saldo-estoq.lote,1,1) = tt-work.acondic AND
                ((SUBSTR(saldo-estoq.lote,2,1) = "P" AND tt-param.qualidade = "1") OR
                 (SUBSTR(saldo-estoq.lote,2,1) = "D" AND tt-param.qualidade = "2") OR
                                                        (tt-param.qualidade = "3")) NO-LOCK.
           ASSIGN tt-work.sld-estoque = tt-work.sld-estoque + saldo-estoq.qtidade-atu.
       END.

       /* Diminui do Saldo em Estoque as quantidade Reservadas e as Quantidades
       ** dos pedidos faturados, cujas Notas Fiscais ainda nÆo foram atualizadas
       ** em Estoque  */
       FOR EACH ped-item-res WHERE
                ped-item-res.it-codigo = ped-item.it-codigo AND
                ped-item-res.cod-refer = ped-item.cod-refer AND
                SUBSTR(ped-item-res.lote,1,1) = tt-work.acondic AND
                ((SUBSTR(ped-item-res.lote,2,1) = "P" AND tt-param.qualidade = "1") OR
                 (SUBSTR(ped-item-res.lote,2,1) = "D" AND tt-param.qualidade = "2") OR
                                                    (tt-param.qualidade = "3")) NO-LOCK:

           IF ped-item-res.faturado = YES THEN DO:
              FIND nota-fiscal WHERE 
                   nota-fiscal.cod-estabel = ped-item-res.cod-estabel AND
                   nota-fiscal.serie       = ped-item-res.serie AND
                   nota-fiscal.nr-nota-fis = STRING(ped-item-res.nr-nota-fis,"9999999")
                   NO-LOCK NO-ERROR.

              IF AVAIL nota-fiscal AND
                 nota-fiscal.ind-sit-nota <= 2 AND
                 nota-fiscal.dt-confirma = ?  AND
                 nota-fiscal.dt-cancela  = ? THEN 
                 ASSIGN tt-work.sld-estoque = tt-work.sld-estoque - ped-item-res.qt-pedida.
           END.                                                                       
           ELSE 
              ASSIGN tt-work.sld-estoque = tt-work.sld-estoque - ped-item-res.qt-pedida.
       END.           
    END.

    /* Cria tabela com todos os Acondicionamentos */
    FIND FIRST tt-work2 WHERE
               tt-work2.it-codigo = tt-work.it-codigo AND
               tt-work2.cod-refer = tt-work.cod-refer AND
               tt-work2.acondic = ped-item-ext.acondicionamento NO-ERROR.

    IF NOT AVAIL tt-work2 THEN DO.
       CREATE tt-work2.
       ASSIGN tt-work2.it-codigo = tt-work.it-codigo 
              tt-work2.cod-refer = tt-work.cod-refer 
              tt-work2.acondic = ped-item-ext.acondicionamento.
    END.

    FIND ped-item-res WHERE
         ped-item-res.nome-abrev   = ped-item.nome-abrev AND
         ped-item-res.nr-pedcli    = ped-item.nr-pedcli AND
         ped-item-res.it-codigo    = ped-item.it-codigo AND
         ped-item-res.cod-refer    = ped-item.cod-refer AND
         ped-item-res.nr-sequencia = ped-item.nr-sequencia 
         NO-LOCK NO-ERROR.

    IF AVAIL ped-item-res THEN DO.
       IF ped-item-res.faturado = YES THEN DO.
          FIND nota-fiscal WHERE
               nota-fiscal.cod-estabel = ped-item-res.cod-estabel AND
               nota-fiscal.serie       = ped-item-res.serie AND
               nota-fiscal.nr-nota-fis = STRING(ped-item-res.nr-nota-fis,"9999999")
               NO-LOCK NO-ERROR.                                             
           IF AVAIL nota-fiscal AND
              nota-fiscal.ind-sit-nota <= 2 AND
              nota-fiscal.dt-confirma   = ? AND
              nota-fiscal.dt-cancela    = ? THEN
              ASSIGN tt-work2.qtd-reserva = tt-work2.qtd-reserva + ped-item-res.qt-pedida.
       END.                                                                        
       ELSE DO.
          IF ped-item.cod-sit-item = 6 THEN 
             ASSIGN tt-work2.qtd-reserva-can = tt-work2.qtd-reserva-can + ped-item-res.qt-pedida.
          ELSE
             ASSIGN tt-work2.qtd-reserva = tt-work2.qtd-reserva + ped-item-res.qt-pedida.
       END.
    END.

    IF ped-item.cod-sit-item <> 6 OR
       (ped-item.cod-sit-item = 6 AND AVAIL ped-item-res) THEN 
       ASSIGN tt-work2.qtd-aberto = tt-work2.qtd-aberto + ped-item.qt-pedida - 
                                    ped-item.qt-atendida - ped-item.qt-pendente.
END.

/* Elimina da temp-table Item/Refer com Estoque e/ou Carteira <= 0, se tt-param.cart-estoque = yes */
IF tt-param.cart-estoque THEN DO:
   FOR EACH tt-work2:
       ASSIGN tt-work2.qtd-aberto = tt-work2.qtd-aberto - tt-work2.qtd-reserva - 
                                    tt-work2.qtd-reserva-can.
       IF tt-work2.qtd-aberto < 0 THEN
          ASSIGN tt-work2.qtd-aberto = 0.
   END.
    
   FOR EACH tt-work WHERE tt-work.sld-estoque <= 0:
       FOR EACH tt-work2 WHERE tt-work2.it-codigo = tt-work.it-codigo
                           AND tt-work2.cod-refer = tt-work.cod-refer
                           AND SUBSTR(tt-work2.acondic,1,1) = tt-work.acondic:
           DELETE tt-work2.
       END.
       DELETE tt-work.
   END.
   FOR EACH tt-work2 WHERE tt-work2.qtd-aberto <= 0:
       FOR EACH tt-work WHERE tt-work.it-codigo = tt-work2.it-codigo
                          AND tt-work.cod-refer = tt-work2.cod-refer
                          AND tt-work.acondic   = substr(tt-work2.acondic,1,1):
           DELETE tt-work.
       END.
       DELETE tt-work2.
   END.
END.

/*
FOR EACH tt-work:
    MESSAGE "Item " tt-work.it-codigo
            "Ref " tt-work.cod-refer
            "Acnd " tt-work.acondic
            "Estoq " tt-work.sld-estoque VIEW-AS ALERT-BOX.
END.
FOR EACH tt-work2:
    MESSAGE "Item " tt-work2.it-codigo
            "Refer " tt-work2.cod-refer
            "Acnd " tt-work2.acondic
            "Cart " tt-work2.qtd-aberto VIEW-AS ALERT-BOX.
END.
*/

/* Iniciando a ImpressÆo dos dados */
FOR EACH tt-work,
    EACH tt-work2 WHERE
         tt-work2.it-codigo = tt-work.it-codigo AND
         tt-work2.cod-refer = tt-work.cod-refer AND
         SUBSTR(tt-work2.acondic,1,1) = tt-work.acondic
    BREAK BY tt-work.it-codigo
          BY SUBSTR(tt-work.cod-refer,1,2)
          BY tt-work.cod-refer
          BY tt-work.acondic
          BY tt-work2.acondic:
    
    IF tt-param.cart-estoque = NO THEN DO:
       ASSIGN tt-work2.qtd-aberto = tt-work2.qtd-aberto - tt-work2.qtd-reserva - 
                                    tt-work2.qtd-reserva-can.
       IF tt-work2.qtd-aberto < 0 THEN
          ASSIGN tt-work2.qtd-aberto = 0.
    END.

    IF FIRST-OF(tt-work.cod-refer) THEN DO.
       DISP tt-work.it-codigo
            tt-work.cod-refer 
            tt-work.descricao
            tt-work.cod-obsoleto
            tt-work.obsoleto
            tt-work.fundo
            WITH FRAME f-detalhe.                      
    END.

    IF FIRST-OF(tt-work.acondic) THEN DO.
       ACCUMULATE tt-work.sld-estoque (TOTAL BY SUBSTR(tt-work.cod-refer,1,2)).
       ACCUMULATE tt-work.sld-estoque (TOTAL BY tt-work.cod-refer).
       ACCUMULATE tt-work.sld-estoque (TOTAL BY tt-work.it-codigo).

       DISP tt-work.sld-estoque 
            WITH FRAME f-detalhe.
    END.

    IF LAST-OF(tt-work2.acondic) THEN DO:  
       ACCUMULATE tt-work2.qtd-aberto      (TOTAL BY SUBSTR(tt-work.cod-refer,1,2)).
       ACCUMULATE tt-work2.qtd-reserva     (TOTAL BY SUBSTR(tt-work.cod-refer,1,2)).
       ACCUMULATE tt-work2.qtd-reserva-can (TOTAL BY SUBSTR(tt-work.cod-refer,1,2)).

       ACCUMULATE tt-work2.qtd-aberto      (TOTAL BY tt-work.cod-refer).
       ACCUMULATE tt-work2.qtd-reserva     (TOTAL BY tt-work.cod-refer).
       ACCUMULATE tt-work2.qtd-reserva-can (TOTAL BY tt-work.cod-refer).

       ACCUMULATE tt-work2.qtd-aberto      (TOTAL BY tt-work.it-codigo).
       ACCUMULATE tt-work2.qtd-reserva     (TOTAL BY tt-work.it-codigo).
       ACCUMULATE tt-work2.qtd-reserva-can (TOTAL BY tt-work.it-codigo).

       DISPLAY tt-work2.acondic
               tt-work2.qtd-reserva-can
               0 WHEN NOT FIRST-OF(tt-work.acondic) @ tt-work.sld-estoque
               tt-work2.qtd-aberto 
               tt-work2.qtd-reserva
               WITH FRAME f-detalhe.
    END.

    IF LAST-OF(tt-work.cod-refer) THEN DO.
       ASSIGN de-qtd-prog = 0   
              de-qtd-proc = 0    
              de-qtd-pron = 0.
       FOR EACH ob-pcp WHERE
                ob-pcp.it-codigo = tt-work.it-codigo NO-LOCK,
           EACH ob-pcp-ref OF ob-pcp WHERE
                ob-pcp-ref.situacao = 1 AND
                ob-pcp-ref.cod-refer = tt-work.cod-refer NO-LOCK.

           ASSIGN de-qtd-prog = de-qtd-prog + ob-pcp-ref.qtd-sld-prog
                  de-qtd-proc = de-qtd-proc + ob-pcp-ref.qtd-proc
                  de-qtd-pron = de-qtd-pron + ob-pcp-ref.qtd-pron.

           ASSIGN da-ult-prog = ob-pcp-ref.dt-ult-prog
                  da-ult-proc = ob-pcp-ref.dt-ult-proc
                  da-ult-pron = ob-pcp-ref.dt-ult-pron.
       END.

       IF tt-param.imp-prpcpt THEN DO:
          DISPLAY FILL(" ",3) + STRING(DAY(da-ult-prog),"99") + "/" +
                                STRING(MONTH(da-ult-prog),"99") FORMAT "x(8)" WHEN de-qtd-prog > 0 @ de-qtd-prog 
                  FILL(" ",3) + STRING(DAY(da-ult-proc),"99") + "/" + 
                                STRING(MONTH(da-ult-proc),"99") FORMAT "x(8)" WHEN de-qtd-proc > 0 @ de-qtd-proc
                  FILL(" ",3) + STRING(DAY(da-ult-pron),"99") + "/" +
                                STRING(MONTH(da-ult-pron),"99") FORMAT "x(8)" WHEN de-qtd-pron > 0 @ de-qtd-pron      
                  WITH FRAME f-detalhe.
       END.
       DOWN WITH FRAME f-detalhe.

       DISPLAY "TOTAL DA REFER.:"                                          @ tt-work.descricao
               (ACCUM TOTAL BY tt-work.cod-refer tt-work.sld-estoque)      @ tt-work.sld-estoque
               (ACCUM TOTAL BY tt-work.cod-refer tt-work2.qtd-aberto)      @ tt-work2.qtd-aberto
               (ACCUM TOTAL BY tt-work.cod-refer tt-work2.qtd-reserva)     @ tt-work2.qtd-reserva
               (ACCUM TOTAL BY tt-work.cod-refer tt-work2.qtd-reserva-can) @ tt-work2.qtd-reserva-can
               de-qtd-prog WHEN tt-param.imp-prpcpt 
               de-qtd-proc WHEN tt-param.imp-prpcpt  
               de-qtd-pron WHEN tt-param.imp-prpcpt  
               WITH FRAME f-detalhe.

       ACCUMULATE de-qtd-prog (TOTAL BY SUBSTR(tt-work.cod-refer,1,2)).
       ACCUMULATE de-qtd-proc (TOTAL BY SUBSTR(tt-work.cod-refer,1,2)).
       ACCUMULATE de-qtd-pron (TOTAL BY SUBSTR(tt-work.cod-refer,1,2)).

       ACCUMULATE de-qtd-prog (TOTAL BY tt-work.it-codigo).
       ACCUMULATE de-qtd-proc (TOTAL BY tt-work.it-codigo).
       ACCUMULATE de-qtd-pron (TOTAL BY tt-work.it-codigo).

       DOWN 1 WITH FRAME f-detalhe.
    END.

    IF LAST-OF(SUBSTR(tt-work.cod-refer,1,2)) THEN DO.
       DOWN WITH FRAME f-detalhe.
       DISP "TOTAL DO ACOND.:"                                                     @ tt-work.descricao
           (ACCUM TOTAL BY SUBSTR(tt-work.cod-refer,1,2) tt-work.sld-estoque)      @ tt-work.sld-estoque
           (ACCUM TOTAL BY SUBSTR(tt-work.cod-refer,1,2) tt-work2.qtd-aberto)      @ tt-work2.qtd-aberto
           (ACCUM TOTAL BY SUBSTR(tt-work.cod-refer,1,2) tt-work2.qtd-reserva)     @ tt-work2.qtd-reserva
           (ACCUM TOTAL BY SUBSTR(tt-work.cod-refer,1,2) tt-work2.qtd-reserva-can) @ tt-work2.qtd-reserva-can
           (ACCUM TOTAL BY SUBSTR(tt-work.cod-refer,1,2) de-qtd-prog)    WHEN tt-param.imp-prpcpt @ de-qtd-prog
           (ACCUM TOTAL BY SUBSTR(tt-work.cod-refer,1,2) de-qtd-proc)    WHEN tt-param.imp-prpcpt @ de-qtd-proc
           (ACCUM TOTAL BY SUBSTR(tt-work.cod-refer,1,2) de-qtd-pron)    WHEN tt-param.imp-prpcpt @ de-qtd-pron
            WITH FRAME f-detalhe.
       DOWN 1 WITH FRAME f-detalhe. 
    END.

    IF LAST-OF(tt-work.it-codigo) THEN DO.
       DOWN WITH FRAME f-detalhe.

       DISP "TOTAL DO ITEM:"                                            @ tt-work.descricao
            (ACCUM TOTAL BY tt-work.it-codigo tt-work.sld-estoque)      @ tt-work.sld-estoque
            (ACCUM TOTAL BY tt-work.it-codigo tt-work2.qtd-aberto)      @ tt-work2.qtd-aberto
            (ACCUM TOTAL BY tt-work.it-codigo tt-work2.qtd-reserva)     @ tt-work2.qtd-reserva
            (ACCUM TOTAL BY tt-work.it-codigo tt-work2.qtd-reserva-can) @ tt-work2.qtd-reserva-can
            (ACCUM TOTAL BY tt-work.it-codigo de-qtd-prog)    WHEN tt-param.imp-prpcpt @ de-qtd-prog
            (ACCUM TOTAL BY tt-work.it-codigo de-qtd-proc)    WHEN tt-param.imp-prpcpt @ de-qtd-proc
            (ACCUM TOTAL BY tt-work.it-codigo de-qtd-pron)    WHEN tt-param.imp-prpcpt @ de-qtd-pron
            WITH FRAME f-detalhe.

       ASSIGN tger-sld-estoque     = tger-sld-estoque + (ACCUM TOTAL BY tt-work.it-codigo tt-work.sld-estoque) 
              tger-qtd-aberto      = tger-qtd-aberto + (ACCUM TOTAL BY tt-work.it-codigo tt-work2.qtd-aberto)       
              tger-qtd-reserva     = tger-qtd-reserva + (ACCUM TOTAL BY tt-work.it-codigo tt-work2.qtd-reserva)      
              tger-qtd-reserva-can = tger-qtd-reserva-can + (ACCUM TOTAL BY tt-work.it-codigo tt-work2.qtd-reserva-can)
              tger-qtd-prog        = tger-qtd-prog + (ACCUM TOTAL BY tt-work.it-codigo de-qtd-prog)
              tger-qtd-proc        = tger-qtd-proc + (ACCUM TOTAL BY tt-work.it-codigo de-qtd-proc)
              tger-qtd-pron        = tger-qtd-pron + (ACCUM TOTAL BY tt-work.it-codigo de-qtd-pron).

       DOWN 1 WITH FRAME f-detalhe. 
    END.
    DOWN WITH FRAME f-detalhe.
END.
   
DOWN 1 WITH FRAME f-detalhe.
DISP "TOTAL GERAL:"         @ tt-work.descricao
     tger-sld-estoque       @ tt-work.sld-estoque        
     tger-qtd-aberto        @ tt-work2.qtd-aberto        
     tger-qtd-reserva       @ tt-work2.qtd-reserva       
     tger-qtd-reserva-can   @ tt-work2.qtd-reserva-can 
     tger-qtd-prog          WHEN tt-param.imp-prpcpt @ de-qtd-prog
     tger-qtd-proc          WHEN tt-param.imp-prpcpt @ de-qtd-proc
     tger-qtd-pron          WHEN tt-param.imp-prpcpt @ de-qtd-pron
     WITH FRAME f-detalhe.                                                          
DOWN WITH FRAME f-detalhe.                                                          

IF tt-param.imp-param THEN DO.
   PAGE.
   DISPLAY tt-param.cod-estabel
           tt-param.cod-depos
           tt-param.so-aprov
           tt-param.so-procpron
           c-qualidade
           c-mercado
           tt-param.opc-artigo
           l-tipo-rel
           tt-param.ini-nr-pedcli              
           tt-param.fin-nr-pedcli              
           tt-param.ini-it-codigo                
           tt-param.fin-it-codigo                
           tt-param.ini-cod-refer                
           tt-param.fin-cod-refer                
           tt-param.ini-dt-entrega               
           tt-param.fin-dt-entrega               
           tt-param.ini-dt-implant               
           tt-param.fin-dt-implant               
           WITH FRAME f-param.           
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.

