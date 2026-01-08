/* Programa: ESCC0003RP.P
** Sistema.: EMS da DATASUL S/A.
** Modulo..: Compras
** Objetivo: Listar Ordens de Compra por Item com Evolu‡Æo de Pre‡os.
** Autor...: Gilvando Souza Araujo - Maio/2006
** Obs.....: Especifico da TEAR TÒXTIL INDéSTRIA E COMRCIO LTDA.
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESCC0003RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino          AS INTEGER
       FIELD arquivo          AS CHAR FORMAT "x(35)"
       FIELD usuario          AS CHAR FORMAT "x(12)"
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       FIELD classifica       AS INTEGER
       FIELD desc-classifica  AS CHAR FORMAT "x(45)"
       FIELD cod-desc-item    AS INTEGER
       FIELD ge-codigo-ini    LIKE item.ge-codigo         
       FIELD ge-codigo-fin    LIKE item.ge-codigo           
       FIELD it-codigo-ini    LIKE ordem-compra.it-codigo
       FIELD it-codigo-fin    LIKE ordem-compra.it-codigo
       FIELD dt-emissao-ini   LIKE ordem-compra.data-emissao 
       FIELD dt-emissao-fin   LIKE ordem-compra.data-emissao
       FIELD dt-entrega-ini   LIKE prazo-compra.data-entrega   
       FIELD dt-entrega-fin   LIKE prazo-compra.data-entrega
       FIELD nome-abrev-ini   LIKE emitente.nome-abrev
       FIELD nome-abrev-fin   LIKE emitente.nome-abrev
       FIELD data-conv        AS INTEGER  
       FIELD desc-data-conv   AS CHAR FORMAT "x(10)"
       FIELD mo-codigo        LIKE moeda.mo-codigo     
       FIELD desc-moeda       LIKE moeda.descricao    
       FIELD nao-confirm      AS LOGICAL FORMAT "Sim/NÆo"     
       FIELD confirm          AS LOGICAL FORMAT "Sim/NÆo"     
       FIELD em-cotacao       AS LOGICAL FORMAT "Sim/NÆo"     
       FIELD terminada        AS LOGICAL FORMAT "Sim/NÆo"     
       FIELD eliminada        AS LOGICAL FORMAT "Sim/NÆo"
       FIELD cotada           AS LOGICAL FORMAT "Sim/NÆo"
       FIELD gerar-excel      AS LOG FORMAT "Sim/NÆo"
       FIELD arq-excel        AS CHAR FORMAT "x(45)"
       FIELD impr-qdo-alt     AS LOG FORMAT "Sim/NÆo"
       FIELD impr-param       AS LOG FORMAT "Sim/NÆo".

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

DEFINE TEMP-TABLE tt-work
       FIELD it-codigo    LIKE ordem-compra.it-codigo
       FIELD item-chave   LIKE ITEM.desc-item
       FIELD num-pedido   LIKE ordem-compra.num-pedido                            
       FIELD numero-ordem LIKE ordem-compra.numero-ordem                          
       FIELD parcela      LIKE prazo-compra.parcela                               
       FIELD data-emissao LIKE ordem-compra.data-emissao                          
       FIELD data-entrega LIKE prazo-compra.data-entrega                          
       FIELD quantidade   LIKE prazo-compra.quantidade                            
       FIELD preco-unit   LIKE ordem-compra.preco-unit 
       FIELD sigla-orig   LIKE moeda.sigla                                   
       FIELD preco-conv   AS DEC FORMAT ">>,>>>,>>9.99" 
       FIELD nome-abrev   LIKE emitente.nome-abrev                                
       FIELD situacao     AS CHAR FORMAT "x(12)"
       FIELD aliquota-ipi LIKE ordem-compra.aliquota-ipi.                                     

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

DEF VAR c-item AS CHAR FORMAT "x(132)".
DEF VAR de-preco-unit AS DEC FORMAT ">>,>>>,>>9.99".
DEF VAR c-situacao AS CHAR FORMAT "x(12)".
DEF VAR da-data-aux AS DATE.
DEF VAR de-cotacao-orig LIKE cotacao.cotacao[1].
DEF VAR de-cotacao-dest LIKE cotacao.cotacao[1].
DEF VAR c-sigla-orig LIKE moeda.sigla.
DEF VAR c-sigla-dest LIKE moeda.sigla.
DEF VAR de-pre-ant LIKE ordem-compra.preco-unit.
DEF VAR de-pre-ini LIKE ordem-compra.preco-unit.
DEF VAR l-imprime  AS LOG.
DEF VAR de-per-var1 AS DEC FORMAT "->>9.9".
DEF VAR de-per-var2 AS DEC FORMAT "->>9.9".
DEF VAR de-tot-vlr-aux AS DEC FORMAT ">>,>>>,>>9.99".
DEF VAR de-tot-vlr-ite AS DEC FORMAT ">>,>>>,>>9.99".
DEF VAR de-tot-vlr-ccu AS DEC FORMAT ">>,>>>,>>9.99".
DEF VAR de-tot-vlr-ger AS DEC FORMAT ">>,>>>,>>9.99".
DEF VAR de-tot-qtd-ite AS DEC FORMAT ">>,>>>,>>9.99".
DEF VAR de-tot-qtd-ccu AS DEC FORMAT ">>,>>>,>>9.99".
DEF VAR de-tot-qtd-ger AS DEC FORMAT ">>,>>>,>>9.99".

DEFINE VAR i-canal     AS INTEGER.
DEFINE VAR sys         AS INTEGER.
DEFINE VAR i-Lin       AS INTEGER.
DEFINE VAR c-lin       AS CHARACTER FORMAT "x(500)".
DEFINE VAR aux-command AS CHAR.

/*DEFINE FRAME frm_excel WITH SIZE 85.25 BY 15.42.*/
DEFINE FRAME frm_excel WITH SIZE 2 BY 2 ROW 1 COLUMN 1.
ENABLE ALL WITH FRAME frm_excel.

form
    "*-------------- Parƒmetros/Sele‡Æo ---------------*" SKIP
    tt-param.desc-classifica LABEL "Classifica‡Æo..." AT  1
    tt-param.ge-codigo-ini   LABEL "Grupo Estoque..." AT  1
    "a"                                               AT 35
    tt-param.ge-codigo-fin   NO-LABEL                      
    tt-param.it-codigo-ini   LABEL "Item............" AT  1
    "a"                                               AT 35
    tt-param.it-codigo-fin   NO-LABEL                
    tt-param.dt-emissao-ini  LABEL "Data EmissÆo...." AT  1
    "a"                                               AT 35
    tt-param.dt-emissao-fin  NO-LABELS
    tt-param.dt-entrega-ini  LABEL "Data Entrega...." AT  1 
    "a"                                               AT 35
    tt-param.dt-entrega-fin  NO-LABELS
    tt-param.nome-abrev-ini  LABEL "Fornecedor......" AT  1
    "a"                                               AT 35
    tt-param.nome-abrev-fin  NO-LABELS                     
    tt-param.desc-data-conv  LABEL "Data ConversÆo.." AT  1
    tt-param.mo-codigo       LABEL "Moeda..........." AT  1
    tt-param.desc-moeda      NO-LABELS                AT 21
    tt-param.nao-confirm     LABEL "NÆo Confirmada.." AT  1
    tt-param.confirm         LABEL "Confirmada......" AT  1
    tt-param.em-cotacao      LABEL "Em Cota‡Æo......" AT  1
    tt-param.terminada       LABEL "Terminada......." AT  1
    tt-param.eliminada       LABEL "Eliminada......." AT  1
    tt-param.cotada          LABEL "Cotada.........." AT  1
    tt-param.gerar-excel     LABEL "Gerar Excel....." AT  1
    tt-param.arq-excel       LABEL "Arquivo Excel..." AT  1
    tt-param.impr-qdo-alt    LABEL "Impr.Qdo.Alt.Prc" AT  1
    with no-box side-labels width 132 STREAM-IO frame f-param.

FORM HEADER
   "Nr-Pedido Num-Ordem  Parc Dt-EmissÆo Dt-Entrega    Quantidade Pre‡o-Origin."
   "Moed Pre‡o-Convert Fornecedor   Situa‡Æo" SKIP
   "--------- --------- ----- ---------- ---------- ------------- -------------"
   "---- ------------- ------------ ------------"
   WITH NO-LABELS NO-ATTR-SPACE NO-BOX PAGE-TOP WIDTH 132 STREAM-IO 1 DOWN FRAME f-cab-dados.

form
    ordem-compra.num-pedido    
    ordem-compra.numero-ordem  
    prazo-compra.parcela       
    ordem-compra.data-emissao  
    prazo-compra.data-entrega  
    prazo-compra.quantidade
    ordem-compra.preco-unit     FORMAT ">>>,>>9.99999"
    c-sigla-orig                
    de-preco-unit               FORMAT ">>,>>>,>>9.99"
    emitente.nome-abrev        
    c-situacao                 
    WITH NO-LABEL NO-BOX 55 DOWN WIDTH 132 WITH STREAM-IO FRAME f-detalhe.

FORM
    c-item
    WITH NO-LABEL 2 DOWN NO-BOX NO-ATTR-SPACE WIDTH 132 STREAM-IO FRAME f-item.

FORM
    "Centro de Custo:"
    sub-conta.sc-codigo
    sub-conta.descricao
    " - "
    requisitante.nome
    " - Pre‡os em:"
    c-sigla-dest
    WITH NO-LABEL 2 DOWN NO-BOX NO-ATTR-SPACE WIDTH 132 STREAM-IO FRAME f-sub-conta.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i COMPRAS * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Ordens_de_Compra_por_Item_e_Evolu‡Æo_de_Pre‡os * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
VIEW FRAME f-cab-dados.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FIND moeda WHERE moeda.mo-codigo = tt-param.mo-codigo NO-LOCK NO-ERROR.
IF AVAIL moeda THEN
   ASSIGN c-sigla-dest = moeda.sigla.
ELSE
   ASSIGN c-sigla-dest = "".

IF tt-param.gerar-excel THEN DO:
   RUN pi-abre-excel (INPUT "").
   PAUSE 3 NO-MESSAGE.
   DDE INITIATE sys FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "System".
   DDE INITIATE i-canal FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "Pasta1".

   ASSIGN c-Lin = c-empresa + " - " + c-titulo-relat + " - Emitido em: " + STRING(TODAY,"99/99/9999") +
          " - " + STRING(TIME,"hh:mm").
   DDE SEND i-canal SOURCE c-Lin ITEM "L1C1".
   ASSIGN c-Lin = "Classifica‡Æo: " + tt-param.desc-classifica + " - Per¡odo: " + 
                  STRING(tt-param.dt-emissao-ini,"99/99/9999") + " a " + 
                  STRING(tt-param.dt-emissao-fin,"99/99/9999").
   DDE SEND i-canal SOURCE c-Lin ITEM "L3C1".

   IF tt-param.classifica < 4 THEN DO:
      DDE SEND i-canal SOURCE "Nr-Pedido"       ITEM "L5C1". 
      DDE SEND i-canal SOURCE "Num-Ordem"       ITEM "L5C2".
      DDE SEND i-canal SOURCE "Parc"            ITEM "L5C3".
      DDE SEND i-canal SOURCE "Dt-EmissÆo"      ITEM "L5C4".
      DDE SEND i-canal SOURCE "Dt-Entrega"      ITEM "L5C5".
      DDE SEND i-canal SOURCE "Quantidade"      ITEM "L5C6".
      DDE SEND i-canal SOURCE "Pre‡o-Origin."   ITEM "L5C7".
      DDE SEND i-canal SOURCE "Moeda"           ITEM "L5C8".
      DDE SEND i-canal SOURCE "Pre‡o-" + TRIM(c-sigla-dest) ITEM "L5C9".
      DDE SEND i-canal SOURCE "% Var"           ITEM "L5C10".
      DDE SEND i-canal SOURCE "% IPI"           ITEM "L5C11".
      DDE SEND i-canal SOURCE "Fornecedor"      ITEM "L5C12".
      DDE SEND i-canal SOURCE "Valor da Compra" ITEM "L5C13".
      IF tt-param.impr-qdo-alt = NO THEN DO:
         DDE SEND i-canal SOURCE "Valor Acumulado" ITEM "L5C14".
         DDE SEND i-canal SOURCE "Quant.Acumulada" ITEM "L5C15".
      END.
      
      DDE EXECUTE i-canal COMMAND "[select(~"C1~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(7.86)]".
      
      DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(10.14)]". 
      
      DDE EXECUTE i-canal COMMAND "[select(~"C3~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(3.86)]". 
      
      DDE EXECUTE i-canal COMMAND "[select(~"C4~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(9.71)]". 
      
      DDE EXECUTE i-canal COMMAND "[select(~"C5~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(9.14)]". 
      
      DDE EXECUTE i-canal COMMAND "[select(~"C6~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(9.43)]".
      DDE EXECUTE sys     COMMAND "[format.number(~"#.##0~")]".
      
      DDE EXECUTE i-canal COMMAND "[select(~"C7~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(10.57)]".
      DDE EXECUTE sys     COMMAND "[format.number(~"#.##0,0000~")]".
      
      DDE EXECUTE i-canal COMMAND "[select(~"C8~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(5.57)]". 
      DDE EXECUTE sys     COMMAND "[format.number(~"###0,0~")]".
      
      DDE EXECUTE i-canal COMMAND "[select(~"C9~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(7.71)]". 
      DDE EXECUTE sys     COMMAND "[format.number(~"#.##0,00~")]".
      
      DDE EXECUTE i-canal COMMAND "[select(~"C10~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(5.29)]".
      DDE EXECUTE sys     COMMAND "[format.number(~"#.##0,00~")]".
      
      DDE EXECUTE i-canal COMMAND "[select(~"C11~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(4.29)]".
      
      DDE EXECUTE i-canal COMMAND "[select(~"C12~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(13.71)]". 
      
      DDE EXECUTE i-canal COMMAND "[select(~"C13~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(13.71)]".
      DDE EXECUTE sys     COMMAND "[format.number(~"#.##0,00~")]".
      
      DDE EXECUTE i-canal COMMAND "[select(~"C14~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(13.71)]".
      DDE EXECUTE sys     COMMAND "[format.number(~"#.##0,00~")]".
      
      DDE EXECUTE i-canal COMMAND "[select(~"C15~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(14.43)]". 
      DDE EXECUTE sys     COMMAND "[format.number(~"#.##0~")]".
   END.
   ELSE DO:
      DDE SEND i-canal SOURCE "Nr-Pedido"       ITEM "L5C1". 
      DDE SEND i-canal SOURCE "Num-Ordem"       ITEM "L5C2".
      DDE SEND i-canal SOURCE "Parc"            ITEM "L5C3".
      DDE SEND i-canal SOURCE "Dt-EmissÆo"      ITEM "L5C4".
      DDE SEND i-canal SOURCE "Dt-Entrega"      ITEM "L5C5".
      DDE SEND i-canal SOURCE "Quantidade"      ITEM "L5C6".
      DDE SEND i-canal SOURCE "Pre‡o-Origin."   ITEM "L5C7".
      DDE SEND i-canal SOURCE "Moeda"           ITEM "L5C8".
      DDE SEND i-canal SOURCE "Pre‡o-" + TRIM(c-sigla-dest) ITEM "L5C9".
      DDE SEND i-canal SOURCE "% IPI"           ITEM "L5C10".
      DDE SEND i-canal SOURCE "Fornecedor"      ITEM "L5C11".
      DDE SEND i-canal SOURCE "Valor da Compra" ITEM "L5C12".
      IF tt-param.impr-qdo-alt = NO THEN DO:
         DDE SEND i-canal SOURCE "Valor Acumulado" ITEM "L5C13".
         DDE SEND i-canal SOURCE "Quant.Acumulada" ITEM "L5C14".
      END.
      
      DDE EXECUTE i-canal COMMAND "[select(~"C1~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(7.86)]".
      
      DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(10.14)]". 
      
      DDE EXECUTE i-canal COMMAND "[select(~"C3~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(3.86)]". 
      
      DDE EXECUTE i-canal COMMAND "[select(~"C4~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(9.71)]". 
      
      DDE EXECUTE i-canal COMMAND "[select(~"C5~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(9.14)]". 
      
      DDE EXECUTE i-canal COMMAND "[select(~"C6~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(9.43)]".
      DDE EXECUTE sys     COMMAND "[format.number(~"#.##0~")]".
      
      DDE EXECUTE i-canal COMMAND "[select(~"C7~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(10.57)]".
      DDE EXECUTE sys     COMMAND "[format.number(~"#.##0,0000~")]".
      
      DDE EXECUTE i-canal COMMAND "[select(~"C8~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(5.57)]". 
      DDE EXECUTE sys     COMMAND "[format.number(~"###0,0~")]".
      
      DDE EXECUTE i-canal COMMAND "[select(~"C9~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(7.71)]". 
      DDE EXECUTE sys     COMMAND "[format.number(~"#.##0,00~")]".
      
      DDE EXECUTE i-canal COMMAND "[select(~"C10~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(4.29)]".
      
      DDE EXECUTE i-canal COMMAND "[select(~"C11~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(13.71)]". 
      
      DDE EXECUTE i-canal COMMAND "[select(~"C12~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(13.71)]".
      DDE EXECUTE sys     COMMAND "[format.number(~"#.##0,00~")]".
      
      DDE EXECUTE i-canal COMMAND "[select(~"C13~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(13.71)]".
      DDE EXECUTE sys     COMMAND "[format.number(~"#.##0,00~")]".
      
      DDE EXECUTE i-canal COMMAND "[select(~"C14~")]". 
      DDE EXECUTE sys     COMMAND "[column.width(14.43)]". 
      DDE EXECUTE sys     COMMAND "[format.number(~"#.##0~")]".
   END.
   ASSIGN i-Lin = 7.
END.

FIND moeda WHERE moeda.mo-codigo = tt-param.mo-codigo NO-LOCK NO-ERROR.
IF AVAIL moeda THEN
   ASSIGN c-sigla-dest = moeda.sigla.
ELSE
   ASSIGN c-sigla-dest = "".

IF tt-param.classifica < 4 THEN DO:
   FOR EACH ordem-compra WHERE ordem-compra.it-codigo    >= tt-param.it-codigo-ini
                           AND ordem-compra.it-codigo    <= tt-param.it-codigo-fin
                           AND ordem-compra.data-emissao >= tt-param.dt-emissao-ini
                           AND ordem-compra.data-emissao <= tt-param.dt-emissao-fin
                         NO-LOCK,
       EACH ITEM WHERE item.it-codigo = ordem-compra.it-codigo
                   AND ITEM.ge-codigo >= tt-param.ge-codigo-ini
                   AND ITEM.ge-codigo <= tt-param.ge-codigo-fin
                 NO-LOCK,
       EACH emitente WHERE emitente.cod-emitente = ordem-compra.cod-emitente
                       AND emitente.nome-abrev   >= tt-param.nome-abrev-ini
                       AND emitente.nome-abrev   <= tt-param.nome-abrev-fin
                     NO-LOCK,
       EACH prazo-compra OF ordem-compra
                         WHERE prazo-compra.data-entrega >= tt-param.dt-entrega-ini
                           AND prazo-compra.data-entrega <= tt-param.dt-entrega-fin
                           AND ((prazo-compra.situacao = 1 AND tt-param.nao-confirm) OR
                                (prazo-compra.situacao = 2 AND tt-param.confirm) OR
                                (prazo-compra.situacao = 3 AND tt-param.cotada) OR
                                (prazo-compra.situacao = 4 AND tt-param.eliminada) OR
                                (prazo-compra.situacao = 5 AND tt-param.em-cotacao) OR
                                (prazo-compra.situacao = 6 AND tt-param.terminada))
                           NO-LOCK:

       RUN pi-acompanhar in h-acomp (input "Ordem: " + string(ordem-compra.numero-ordem)).

       RUN pi-sit-ordem.

       IF ordem-compra.mo-codigo <> tt-param.mo-codigo THEN DO:
          IF tt-param.data-conv = 1 THEN
             ASSIGN da-data-aux = ordem-compra.data-emissao.
          ELSE
          IF tt-param.data-conv = 2 THEN
             ASSIGN da-data-aux = prazo-compra.data-entrega.
          ELSE
             ASSIGN da-data-aux = TODAY.

          IF ordem-compra.mo-codigo <> 0 THEN DO:
             FIND cotacao WHERE cotacao.mo-codigo   = ordem-compra.mo-codigo
                            AND cotacao.ano-periodo = STRING(YEAR(da-data-aux),"9999") +
                                                      STRING(MONTH(da-data-aux),"99")
                          NO-LOCK NO-ERROR.
             IF AVAIL cotacao THEN
                ASSIGN de-cotacao-orig = cotacao.cotacao[DAY(da-data-aux)].
             ELSE
                ASSIGN de-cotacao-orig = 0.
          END.
          IF tt-param.mo-codigo <> 0 THEN DO:
              FIND cotacao WHERE cotacao.mo-codigo   = tt-param.mo-codigo
                             AND cotacao.ano-periodo = STRING(YEAR(da-data-aux),"9999") +
                                                       STRING(MONTH(da-data-aux),"99")
                           NO-LOCK NO-ERROR.
              IF AVAIL cotacao THEN
                 ASSIGN de-cotacao-dest = cotacao.cotacao[DAY(da-data-aux)].
              ELSE
                 ASSIGN de-cotacao-dest = 0.
          END.
          IF ordem-compra.mo-codigo = 0 AND tt-param.mo-codigo <> 0 THEN DO:
             IF de-cotacao-dest <> 0 THEN
                ASSIGN de-preco-unit = ROUND(ordem-compra.preco-unit / de-cotacao-dest,2).
             ELSE
                ASSIGN de-preco-unit = 0.
          END.
          IF ordem-compra.mo-codigo <> 0 AND tt-param.mo-codigo = 0 THEN
             ASSIGN de-preco-unit = ROUND(ordem-compra.preco-unit * de-cotacao-orig,2).
          IF ordem-compra.mo-codigo <> 0 AND tt-param.mo-codigo <> 0 THEN DO:
             IF de-cotacao-orig <> 0 THEN
                ASSIGN de-preco-unit = ROUND(ordem-compra.preco-unit / de-cotacao-orig * de-cotacao-dest,2).
             ELSE
                ASSIGN de-preco-unit = 0.
          END.
       END.
       ELSE
          ASSIGN de-preco-unit = ordem-compra.preco-unit.

       FIND moeda WHERE moeda.mo-codigo = ordem-compra.mo-codigo NO-LOCK NO-ERROR.
       IF AVAIL moeda THEN
          ASSIGN c-sigla-orig = moeda.sigla.
       ELSE
          ASSIGN c-sigla-orig = "".

       CREATE tt-work.
       ASSIGN tt-work.it-codigo    = ordem-compra.it-codigo
              tt-work.item-chave   = IF tt-param.cod-desc-item = 1 THEN
                                        ITEM.it-codigo
                                     ELSE
                                        ITEM.desc-item + ITEM.it-codigo
              tt-work.num-pedido   = ordem-compra.num-pedido  
              tt-work.numero-ordem = ordem-compra.numero-ordem
              tt-work.parcela      = prazo-compra.parcela     
              tt-work.data-emissao = ordem-compra.data-emissao
              tt-work.data-entrega = prazo-compra.data-entrega
              tt-work.quantidade   = prazo-compra.quantidade  
              tt-work.preco-unit   = ordem-compra.preco-unit  
              tt-work.sigla-orig   = c-sigla-orig             
              tt-work.preco-conv   = de-preco-unit            
              tt-work.nome-abrev   = if AVAIL emitente THEN emitente.nome-abrev ELSE ""
              tt-work.situacao     = c-situacao
              tt-work.aliquota-ipi = ordem-compra.aliquota-ipi.              
   END.

   FOR EACH tt-work NO-LOCK,
       EACH ITEM WHERE ITEM.it-codigo = tt-work.it-codigo NO-LOCK
       BREAK BY tt-work.item-chave
             BY IF tt-param.classifica = 1 THEN 
                   STRING(YEAR(tt-work.data-emissao),"9999") +
                   STRING(MONTH(tt-work.data-emissao),"99") +
                   STRING(DAY(tt-work.data-emissao),"99") +
                   STRING(tt-work.preco-conv,"99999999999.99")
                ELSE
                IF tt-param.classifica = 2 THEN
                   STRING(YEAR(tt-work.data-entrega),"9999") + 
                   STRING(MONTH(tt-work.data-entrega),"99") +  
                   STRING(DAY(tt-work.data-entrega),"99") +
                   STRING(tt-work.preco-conv,"99999999999.99")
                ELSE
                   STRING(tt-work.preco-conv,"99999999999.99"):

       IF FIRST-OF(tt-work.item-chave) THEN DO:
          FOR EACH saldo-estoq WHERE saldo-estoq.it-codigo = ITEM.it-codigo NO-LOCK.
              ACCUMULATE saldo-estoq.qtidade-atu(TOTAL).
          END.
          ASSIGN c-item = "Item: " + TRIM(ITEM.it-codigo) + " " + 
                          TRIM(item.desc-item) + 
                          " - Unidade: " + item.un +
                          " - Gr Estoque: " + string(ITEM.ge-codigo,"99") +
                          " - Estoque: " + STRING(ACCUM TOTAL saldo-estoq.qtidade-atu,"->>>,>>>,>>9.99").
          DISPLAY c-item
                  with frame f-item.
          DOWN(2) with frame f-item.

          IF tt-param.gerar-excel THEN DO:
             DDE SEND i-canal SOURCE c-item ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
             ASSIGN i-Lin = i-Lin + 2.
          END.
       END.

       ASSIGN l-imprime = YES.
       IF tt-param.impr-qdo-alt = YES AND
          STRING(de-pre-ant,"99999999999.99") = STRING(tt-work.preco-conv,"99999999999.99") THEN
          ASSIGN l-imprime = NO.
       IF l-imprime THEN DO:
          DISPLAY tt-work.num-pedido    @ ordem-compra.num-pedido   
                  tt-work.numero-ordem  @ ordem-compra.numero-ordem 
                  tt-work.parcela       @ prazo-compra.parcela      
                  tt-work.data-emissao  @ ordem-compra.data-emissao 
                  tt-work.data-entrega  @ prazo-compra.data-entrega 
                  tt-work.quantidade    @ prazo-compra.quantidade   
                  tt-work.preco-unit    @ ordem-compra.preco-unit   
                  tt-work.sigla-orig    @ c-sigla-orig              
                  tt-work.preco-conv    @ de-preco-unit
                  tt-work.nome-abrev    @ emitente.nome-abrev
                  tt-work.situacao      @ c-situacao
                  WITH FRAME f-detalhe.
          DOWN WITH FRAME f-detalhe.
          
          ASSIGN de-tot-vlr-aux = tt-work.quantidade * tt-work.preco-conv
                 de-tot-vlr-ite = de-tot-vlr-ite + de-tot-vlr-aux
                 de-tot-qtd-ite = de-tot-qtd-ite + tt-work.quantidade.
          
          IF tt-param.gerar-excel THEN DO:
             IF de-pre-ant <> 0 THEN
                ASSIGN de-per-var1 = (tt-work.preco-conv - de-pre-ant) / de-pre-ant * 100.
             ELSE
                ASSIGN de-per-var1 = 0.
             
             DDE SEND i-canal SOURCE STRING(tt-work.num-pedido)   ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
             DDE SEND i-canal SOURCE STRING(tt-work.numero-ordem) ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
             DDE SEND i-canal SOURCE STRING(tt-work.parcela)      ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
             DDE SEND i-canal SOURCE STRING(tt-work.data-emissao) ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
             DDE SEND i-canal SOURCE STRING(tt-work.data-entrega) ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
             DDE SEND i-canal SOURCE STRING(tt-work.quantidade)   ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
             DDE SEND i-canal SOURCE STRING(tt-work.preco-unit)   ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
             DDE SEND i-canal SOURCE tt-work.sigla-orig           ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
             DDE SEND i-canal SOURCE STRING(tt-work.preco-conv)   ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
             DDE SEND i-canal SOURCE STRING(de-per-var1)          ITEM "L" + TRIM(STRING(i-Lin)) + "C10".
             DDE SEND i-canal SOURCE STRING(tt-work.aliquota-ipi) ITEM "L" + TRIM(STRING(i-Lin)) + "C11".
             DDE SEND i-canal SOURCE tt-work.nome-abrev           ITEM "L" + TRIM(STRING(i-Lin)) + "C12".
             DDE SEND i-canal SOURCE STRING(de-tot-vlr-aux)       ITEM "L" + TRIM(STRING(i-Lin)) + "C13".
             IF tt-param.impr-qdo-alt = NO THEN DO:
                DDE SEND i-canal SOURCE STRING(de-tot-vlr-ite)       ITEM "L" + TRIM(STRING(i-Lin)) + "C14".
                DDE SEND i-canal SOURCE STRING(de-tot-qtd-ite)       ITEM "L" + TRIM(STRING(i-Lin)) + "C15".
             END.
          
             ASSIGN i-Lin = i-Lin + 1.
          END.
       END.
       ASSIGN de-pre-ant = tt-work.preco-conv.
       
       IF LAST-OF(tt-work.item-chave) THEN DO:
          DOWN 1 with frame f-detalhe.
          ASSIGN de-tot-vlr-ite = 0
                 de-tot-qtd-ite = 0
                 de-pre-ant     = 0
                 i-Lin          = i-Lin + 1.
       END.
   END.
END.
ELSE DO: /* Classifica‡Æo = 4 - Por CCusto/Item/Data Entrega */
   FOR EACH ordem-compra WHERE ordem-compra.it-codigo    >= tt-param.it-codigo-ini
                           AND ordem-compra.it-codigo    <= tt-param.it-codigo-fin
                           AND ordem-compra.data-emissao >= tt-param.dt-emissao-ini
                           AND ordem-compra.data-emissao <= tt-param.dt-emissao-fin
                         NO-LOCK,
       EACH ITEM WHERE item.it-codigo = ordem-compra.it-codigo
                   AND ITEM.ge-codigo >= tt-param.ge-codigo-ini
                   AND ITEM.ge-codigo <= tt-param.ge-codigo-fin
                 NO-LOCK,
       EACH emitente WHERE emitente.cod-emitente = ordem-compra.cod-emitente
                       AND emitente.nome-abrev   >= tt-param.nome-abrev-ini
                       AND emitente.nome-abrev   <= tt-param.nome-abrev-fin
                     NO-LOCK,
       EACH requisitante WHERE requisitante.nome-abrev = ordem-compra.requisitante
                         NO-LOCK,
       EACH prazo-compra OF ordem-compra
                         WHERE prazo-compra.data-entrega >= tt-param.dt-entrega-ini
                           AND prazo-compra.data-entrega <= tt-param.dt-entrega-fin
                           AND ((prazo-compra.situacao = 1 AND tt-param.nao-confirm) OR
                                (prazo-compra.situacao = 2 AND tt-param.confirm) OR
                                (prazo-compra.situacao = 3 AND tt-param.cotada) OR
                                (prazo-compra.situacao = 4 AND tt-param.eliminada) OR
                                (prazo-compra.situacao = 5 AND tt-param.em-cotacao) OR
                                (prazo-compra.situacao = 6 AND tt-param.terminada))
                           NO-LOCK
      
       BREAK BY requisitante.sc-codigo
             BY item.desc-item /*ordem-compra.it-codigo*/
             BY STRING(YEAR(prazo-compra.data-entrega),"9999") +
                STRING(MONTH(prazo-compra.data-entrega),"99") +
                STRING(DAY(prazo-compra.data-entrega),"99"):
       
       RUN pi-acompanhar in h-acomp (input "Ordem: " + string(ordem-compra.numero-ordem)).
      
       RUN pi-sit-ordem.
   
       IF FIRST-OF(requisitante.sc-codigo) THEN DO:
          FIND sub-conta WHERE sub-conta.sc-codigo = requisitante.sc-codigo NO-LOCK NO-ERROR.
          IF AVAIL sub-conta THEN
             DISPLAY sub-conta.sc-codigo 
                     sub-conta.descricao
                     requisitante.nome
                     c-sigla-dest
                     with frame f-sub-conta.
          ELSE
             DISPLAY requisitante.sc-codigo @ sub-conta.sc-codigo
                     "Indefinido"           @ sub-conta.descricao
                     WITH FRAME f-sub-conta.
          DOWN(2) with frame f-sub-conta.

          IF tt-param.gerar-excel THEN DO:
             IF AVAIL sub-conta THEN
                DDE SEND i-canal SOURCE 
                         "Centro de Custo: " + sub-conta.sc-codigo +
                         " " + sub-conta.descricao +
                         " - " + requisitante.nome + 
                         " Moeda: " + c-sigla-dest
                         ITEM  "L" + TRIM(STRING(i-Lin)) + "C1".
             ELSE
                DDE SEND i-canal SOURCE "Centro de Custo: NÆo definido" ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
             ASSIGN i-Lin = i-Lin + 2.
          END.
       END.

       IF FIRST-OF(item.desc-item) THEN DO:
          FOR EACH saldo-estoq WHERE saldo-estoq.it-codigo = ITEM.it-codigo NO-LOCK.
              ACCUMULATE saldo-estoq.qtidade-atu(TOTAL).
          END.
          ASSIGN c-item = "Item: " + TRIM(ITEM.it-codigo) + " " + 
                          TRIM(item.desc-item) + 
                          " - Unidade: " + item.un +
                          " - Gr Estoque: " + string(ITEM.ge-codigo,"99") +
                          " - Estoque: " + STRING(ACCUM TOTAL saldo-estoq.qtidade-atu,"->>>,>>>,>>9.99").
          DISPLAY c-item
                  with frame f-item.
          DOWN(2) with frame f-item.
          
          IF tt-param.gerar-excel THEN DO:
             DDE SEND i-canal SOURCE c-item ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
             ASSIGN i-Lin = i-Lin + 2.
          END.
       END.
   
       IF ordem-compra.mo-codigo <> tt-param.mo-codigo THEN DO:
          IF tt-param.data-conv = 1 THEN
             ASSIGN da-data-aux = ordem-compra.data-emissao.
          ELSE
          IF tt-param.data-conv = 2 THEN
             ASSIGN da-data-aux = prazo-compra.data-entrega.
          ELSE
             ASSIGN da-data-aux = TODAY.
          
          IF ordem-compra.mo-codigo <> 0 THEN DO:
             FIND cotacao WHERE cotacao.mo-codigo   = ordem-compra.mo-codigo
                            AND cotacao.ano-periodo = STRING(YEAR(da-data-aux),"9999") +
                                                      STRING(MONTH(da-data-aux),"99")
                          NO-LOCK NO-ERROR.
             IF AVAIL cotacao THEN
                ASSIGN de-cotacao-orig = cotacao.cotacao[DAY(da-data-aux)].
             ELSE
                ASSIGN de-cotacao-orig = 0.
          END.
          IF tt-param.mo-codigo <> 0 THEN DO:
              FIND cotacao WHERE cotacao.mo-codigo   = tt-param.mo-codigo
                             AND cotacao.ano-periodo = STRING(YEAR(da-data-aux),"9999") +
                                                       STRING(MONTH(da-data-aux),"99")
                           NO-LOCK NO-ERROR.
              IF AVAIL cotacao THEN
                 ASSIGN de-cotacao-dest = cotacao.cotacao[DAY(da-data-aux)].
              ELSE
                 ASSIGN de-cotacao-dest = 0.
          END.
          IF ordem-compra.mo-codigo = 0 AND tt-param.mo-codigo <> 0 THEN DO:
             IF de-cotacao-dest <> 0 THEN
                ASSIGN de-preco-unit = ordem-compra.preco-unit / de-cotacao-dest.
             ELSE
                ASSIGN de-preco-unit = 0.
          END.
          IF ordem-compra.mo-codigo <> 0 AND tt-param.mo-codigo = 0 THEN
             ASSIGN de-preco-unit = ordem-compra.preco-unit * de-cotacao-orig.
          IF ordem-compra.mo-codigo <> 0 AND tt-param.mo-codigo <> 0 THEN DO:
             IF de-cotacao-orig <> 0 THEN
                ASSIGN de-preco-unit = ordem-compra.preco-unit / de-cotacao-orig * de-cotacao-dest.
             ELSE
                ASSIGN de-preco-unit = 0.
          END.
       END.
       ELSE
          ASSIGN de-preco-unit = ordem-compra.preco-unit.
   
       FIND moeda WHERE moeda.mo-codigo = ordem-compra.mo-codigo NO-LOCK NO-ERROR.
       IF AVAIL moeda THEN
          ASSIGN c-sigla-orig = moeda.sigla.
       ELSE
          ASSIGN c-sigla-orig = "".
       FIND moeda WHERE moeda.mo-codigo = tt-param.mo-codigo NO-LOCK NO-ERROR.
       IF AVAIL moeda THEN
          ASSIGN c-sigla-dest = moeda.sigla.
       ELSE
          ASSIGN c-sigla-dest = "".
       
       DISPLAY ordem-compra.num-pedido
               ordem-compra.numero-ordem
               prazo-compra.parcela
               ordem-compra.data-emissao
               prazo-compra.data-entrega
               prazo-compra.quantidade
               ordem-compra.preco-unit
               c-sigla-orig
               de-preco-unit
               emitente.nome-abrev  WHEN AVAIL emitente
               c-situacao
               WITH FRAME f-detalhe.
       DOWN WITH FRAME f-detalhe.

       ASSIGN de-tot-vlr-aux = prazo-compra.quantidade * de-preco-unit
              de-tot-vlr-ite = de-tot-vlr-ite + de-tot-vlr-aux
              de-tot-qtd-ite = de-tot-qtd-ite + prazo-compra.quantidade.

       IF tt-param.gerar-excel THEN DO:
          DDE SEND i-canal SOURCE STRING(ordem-compra.num-pedido)   ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
          DDE SEND i-canal SOURCE STRING(ordem-compra.numero-ordem) ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
          DDE SEND i-canal SOURCE STRING(prazo-compra.parcela)      ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
          DDE SEND i-canal SOURCE STRING(ordem-compra.data-emissao) ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
          DDE SEND i-canal SOURCE STRING(prazo-compra.data-entrega) ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
          DDE SEND i-canal SOURCE STRING(prazo-compra.quantidade)   ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
          DDE SEND i-canal SOURCE STRING(ordem-compra.preco-unit)   ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
          DDE SEND i-canal SOURCE c-sigla-orig                      ITEM "L" + TRIM(STRING(i-Lin)) + "C8".
          DDE SEND i-canal SOURCE STRING(de-preco-unit)             ITEM "L" + TRIM(STRING(i-Lin)) + "C9".
          DDE SEND i-canal SOURCE STRING(ordem-compra.aliquota-ipi) ITEM "L" + TRIM(STRING(i-Lin)) + "C10".
          DDE SEND i-canal SOURCE emitente.nome-abrev               ITEM "L" + TRIM(STRING(i-Lin)) + "C11".
          DDE SEND i-canal SOURCE STRING(de-tot-vlr-aux)            ITEM "L" + TRIM(STRING(i-Lin)) + "C12".
          DDE SEND i-canal SOURCE STRING(de-tot-vlr-ite)            ITEM "L" + TRIM(STRING(i-Lin)) + "C13".
          DDE SEND i-canal SOURCE STRING(de-tot-qtd-ite)            ITEM "L" + TRIM(STRING(i-Lin)) + "C14".

          ASSIGN i-Lin = i-Lin + 1.
       END.

       ASSIGN de-tot-vlr-ccu = de-tot-vlr-ccu + (de-preco-unit * prazo-compra.quantidade)
              de-tot-vlr-ger = de-tot-vlr-ger + (de-preco-unit * prazo-compra.quantidade)
              de-tot-qtd-ccu = de-tot-qtd-ccu + prazo-compra.quantidade
              de-tot-qtd-ger = de-tot-qtd-ger + prazo-compra.quantidade.

       IF LAST-OF(requisitante.sc-codigo) THEN DO:
          DOWN(1) WITH FRAME f-detalhe.
          DISPLAY "Total:"       @ ordem-compra.num-pedido
                  de-tot-vlr-ccu @ de-preco-unit
                  WITH FRAME f-detalhe.
          DOWN(1) with frame f-detalhe.
          
          IF tt-param.gerar-excel THEN DO:
             DDE SEND i-canal SOURCE "Total Centro Custo"   ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
             DDE SEND i-canal SOURCE STRING(de-tot-vlr-ccu) ITEM "L" + TRIM(STRING(i-Lin)) + "C13".
             DDE SEND i-canal SOURCE STRING(de-tot-qtd-ccu) ITEM "L" + TRIM(STRING(i-Lin)) + "C14".
             ASSIGN i-Lin = i-Lin + 1.
          END.
          ASSIGN de-tot-vlr-ccu = 0
                 de-tot-qtd-ccu = 0.
       END.
   
       IF LAST-OF(item.desc-item) THEN DO:
          DOWN 1 with frame f-detalhe.
           
          IF tt-param.gerar-excel THEN
             ASSIGN i-Lin = i-Lin + 1.
          
          ASSIGN de-tot-vlr-ite = 0
                 de-tot-qtd-ite = 0
                 de-pre-ant     = 0.
       END.
   END.

   DISPLAY "Total:"       @ ordem-compra.num-pedido
           de-tot-vlr-ger @ de-preco-unit
           WITH FRAME f-detalhe.

   IF tt-param.gerar-excel THEN DO:
      DDE SEND i-canal SOURCE "Total Geral"          ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
      DDE SEND i-canal SOURCE STRING(de-tot-vlr-ger) ITEM "L" + TRIM(STRING(i-Lin)) + "C13".
      DDE SEND i-canal SOURCE STRING(de-tot-qtd-ger) ITEM "L" + TRIM(STRING(i-Lin)) + "C14".
      ASSIGN de-tot-vlr-ger = 0.
   END.
END.

IF tt-param.gerar-excel THEN DO:
   ASSIGN aux-command = '[select("L1C1:L' + TRIM(STRING(i-Lin)) + 'C15")]'.
   DDE EXECUTE i-canal COMMAND aux-command.
   /* Fonte, Tamanho, Negrito, It lico, Sublinhado, Atachado, Cor(0=Autom tico, 1=Preto, 2=Branco 3=Vermelho...) */
   DDE EXECUTE sys COMMAND "[format.font(~"Arial~",9,False,False,False,False,0)]".

   OS-DELETE VALUE(tt-param.arq-excel).
   DDE EXECUTE   sys COMMAND '[save.as("' + tt-param.arq-excel + '")]'.
   DDE EXECUTE   sys COMMAND "[close(0)]". 
   DDE EXECUTE   sys COMMAND "[quit()]". 
   DDE TERMINATE sys.

   HIDE FRAME frm_excel.
   CLEAR FRAME frm_excel.
   DISABLE ALL WITH FRAME frm_excel.
END.

IF tt-param.impr-param THEN DO:
   PAGE.
   DISPLAY tt-param.desc-classifica
           tt-param.ge-codigo-ini  
           tt-param.ge-codigo-fin  
           tt-param.it-codigo-ini  
           tt-param.it-codigo-fin  
           tt-param.dt-emissao-ini 
           tt-param.dt-emissao-fin 
           tt-param.dt-entrega-ini 
           tt-param.dt-entrega-fin
           tt-param.nome-abrev-ini 
           tt-param.nome-abrev-fin 
           tt-param.desc-data-conv 
           tt-param.mo-codigo      
           tt-param.desc-moeda     
           tt-param.nao-confirm    
           tt-param.confirm        
           tt-param.em-cotacao     
           tt-param.terminada      
           tt-param.eliminada      
           tt-param.cotada 
           tt-param.gerar-excel 
           tt-param.arq-excel 
           tt-param.impr-qdo-alt
           WITH FRAME f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

PROCEDURE pi-sit-ordem:
   {esinc/i-dsrb.i ordem-compra.situacao ordem-compra.situacao c-situacao}
END PROCEDURE.

PROCEDURE pi-abre-excel.
   DEF INPUT PARAMETER p-arquivo AS CHAR.

   def var h-prog as handle no-undo.
   run utp/ut-utils.p persistent set h-prog.

   run Execute in h-prog(input "EXCEL.EXE", input p-arquivo).

   delete procedure h-prog.
   PAUSE 5 NO-MESSAGE.
END PROCEDURE.
