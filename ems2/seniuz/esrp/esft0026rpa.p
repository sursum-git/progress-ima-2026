/* Programa: ESFT0026.P
** Sistema.: EMS da DATASUL S/A.
** Modulo..: Faturamento
** Objetivo: Exporta‡Æo de Dados para Packinglist.
** Autor...: Gilvando de Souza Araujo - Julho/2007
** Obs.....: Especifico da TEAR TÒXTIL INDéSTRIA E COMRCIO LTDA.
**
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESFT0026RP 2.06.00.000}

DEFINE TEMP-TABLE tt-param no-undo
       FIELD destino       as integer
       FIELD arquivo       as char format "x(35)"
       FIELD usuario       as char format "x(12)"
       FIELD data-exec     as date
       FIELD hora-exec     as integer
       FIELD tp-relat      AS INTEGER
       FIELD nr-pedcli     LIKE nota-fiscal.nr-pedcli
       FIELD cod-estabel   LIKE nota-fiscal.cod-estabel
       FIELD serie         LIKE nota-fiscal.serie
       FIELD nr-nota-fis   LIKE nota-fiscal.nr-nota-fis
       FIELD qtd-tot-abe   AS INT
       FIELD arq-saida     AS CHAR FORMAT "x(45)"
       FIELD imp-param     AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

DEFINE TEMP-TABLE tt-work
       FIELD cod-ncm       LIKE it-nota-fisc.class-fisc
       FIELD num-etiqueta  LIKE ob-etiqueta.num-etiqueta
       FIELD it-codigo     LIKE ITEM.it-codigo
       FIELD desc-item     LIKE ITEM.desc-item 
       FIELD cod-refer     LIKE ped-item.cod-refer
       FIELD quantidade    AS DEC
       FIELD peso-bruto    AS DEC
       FIELD peso-liquido  AS DEC
       INDEX ch-work 
             cod-ncm 
             num-etiqueta 
             it-codigo 
             cod-refer.

DEFINE TEMP-TABLE tt-work1
       FIELD cod-ncm       LIKE it-nota-fisc.class-fisc
       FIELD peso-bruto    AS DEC
       FIELD peso-liquido  AS DEC
       INDEX ch-work1 
             cod-ncm.

DEFINE input parameter raw-param as raw no-undo.
DEFINE input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include para remover acentua»’o de strings */
{include/i-freeac.i}

/* defini‡Æo de vari veis  */
DEFINE var h-acomp     as handle no-undo.

DEFINE VAR i-canal         AS INT.
DEFINE VAR sys             AS INT.
DEFINE VAR i-Lin           AS INT INIT 1.
DEFINE VAR c-lin           AS CHAR FORMAT "x(500)".
DEFINE VAR aux-command     AS CHAR FORMAT "x(100)".
DEFINE VAR de-peso-bruto   AS DEC FORMAT ">>,>>>,>>9.99".
DEFINE VAR de-peso-liquido AS DEC FORMAT ">>,>>>,>>9.99".
DEFINE VAR de-peso-bru-ger AS DEC FORMAT ">>,>>>,>>9.99".
DEFINE VAR de-peso-liq-ger AS DEC FORMAT ">>,>>>,>>9.99".
DEFINE VAR c-cod-ncm       LIKE it-nota-fisc.class-fiscal.
DEFINE VAR l-total-geral   AS LOG.
DEFINE VAR c-ncm           LIKE ITEM.class-fiscal.
DEFINE VAR de-peso-emb     AS DEC.
DEFINE VAR c-loc-embarque  AS CHAR.

/*DEFINE FRAME frm_excel WITH SIZE 85.25 BY 15.42.*/
DEFINE FRAME frm_excel WITH SIZE 2 BY 2 ROW 1 COLUMN 1.
ENABLE ALL WITH FRAME frm_excel.

FORM
    "*--------- Parƒmetros/Sele‡Æo ---------*"  SKIP
    tt-param.tp-relat     LABEL "Tipo realt¢rio." SKIP
    tt-param.nr-pedcli    LABEL "Pedido........." SKIP
    tt-param.cod-estabel  LABEL "Estabelecimento" SKIP
    tt-param.serie        LABEL "S‚rie.........." SKIP
    tt-param.nr-nota-fis  LABEL "Numero da NF..." SKIP
    tt-param.qtd-tot-abe  LABEL "Tipo de Quantid" SKIP
    tt-param.arq-saida    LABEL "Arquivo Sa¡da.." 
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i FATURAMENTO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Planilha_para_Gera‡Æo_de_Packinglist * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

RUN pi-abre-excel (INPUT "").
PAUSE 3 NO-MESSAGE.
DDE INITIATE sys FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "System".
DDE INITIATE i-canal FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "Pasta1".
    
DDE INITIATE sys FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "System".
DDE INITIATE i-canal FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "Pasta1".

IF tt-param.tp-relat = 1 THEN DO: /* Por Pedido */
   FIND ped-venda WHERE ped-venda.nr-pedcli = tt-param.nr-pedcli NO-LOCK NO-ERROR.
   FIND emitente WHERE emitente.cod-emitente = ped-venda.cod-emitente NO-LOCK NO-ERROR.
   FOR EACH ped-item-res WHERE ped-item-res.nome-abrev = ped-venda.nome-abrev
                           AND ped-item-res.nr-pedcli  = ped-venda.nr-pedcli
                         NO-LOCK,
       EACH ped-item WHERE ped-item.nome-abrev = ped-venda.nome-abrev                  
                       AND ped-item.nr-pedcli  = ped-venda.nr-pedcli
                       AND ped-item.nr-sequencia = ped-item-res.nr-sequencia
                       AND ((ped-item.cod-sit-item = 1 AND tt-param.qtd-tot-abe = 2) OR
                            (tt-param.qtd-tot-abe = 1))                                
                     NO-LOCK:

       FOR EACH ped-item-rom WHERE
                ped-item-rom.nome-abrev   = ped-item-res.nome-abrev AND
                ped-item-rom.nr-pedcli    = ped-item-res.nr-pedcli AND
                ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia NO-LOCK,
           FIRST ob-etiqueta WHERE
                 ob-etiqueta.cod-estabel  = ped-item-rom.cod-estabel AND
                 ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                 NO-LOCK:

           FIND ITEM WHERE ITEM.it-codigo = ped-item-res.it-codigo NO-LOCK NO-ERROR.
           
           FIND ref-item-ext WHERE ref-item-ext.it-codigo = ped-item-res.it-codigo
                               AND ref-item-ext.cod-refer = ped-item-res.cod-refer
                             NO-LOCK NO-ERROR.

           RUN esapi/calcula-ncm.p (INPUT ref-item-ext.it-codigo,
                                    INPUT ref-item-ext.cod-refer,
                                    OUTPUT c-ncm).
           ASSIGN c-cod-ncm = c-ncm.

           FIND FIRST tt-work WHERE tt-work.cod-ncm      = c-cod-ncm
                                AND tt-work.num-etiqueta = ob-etiqueta.num-etiqueta
                                AND tt-work.it-codigo    = ped-item-res.it-codigo
                                AND tt-work.cod-refer    = ped-item-res.cod-refer
                              NO-ERROR.
           IF NOT AVAIL tt-work THEN DO:
              CREATE tt-work.
              ASSIGN tt-work.cod-ncm      = c-cod-ncm  
                     tt-work.num-etiqueta = ob-etiqueta.num-etiqueta
                     tt-work.it-codigo    = ped-item-res.it-codigo
                     tt-work.cod-refer    = ped-item-res.cod-refer.
           END.
           ASSIGN tt-work.desc-item  = ITEM.desc-item
                  tt-work.quantidade = ob-etiqueta.quantidade.
           
           FIND FIRST tt-work1 WHERE tt-work1.cod-ncm = c-cod-ncm NO-LOCK NO-ERROR.
           IF NOT AVAIL tt-work1 THEN DO:
              CREATE tt-work1.
              ASSIGN tt-work1.cod-ncm = c-cod-ncm.
           END.

           FIND item-ext WHERE
                item-ext.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

           FIND corte-comerc WHERE
                corte-comerc.codigo = ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.

           ASSIGN de-peso-emb = IF item-ext.indigo 
                                THEN corte-comerc.peso-emb-indigo
                                ELSE corte-comerc.peso-emb-outros.

           ASSIGN tt-work1.peso-bruto   = tt-work1.peso-bruto   + ob-etiqueta.peso-bruto.
                  tt-work1.peso-liquido = tt-work1.peso-liquido + ob-etiqueta.peso-bruto - de-peso-emb.
       END.
   END.
END.
ELSE DO: /* Por Nota Fiscal */
   FIND nota-fiscal WHERE
        nota-fiscal.cod-estabel = tt-param.cod-estabel AND
        nota-fiscal.serie       = tt-param.serie AND
        nota-fiscal.nr-nota-fis = tt-param.nr-nota-fis
        NO-LOCK NO-ERROR.
   
   FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR.
   FIND ped-venda WHERE ped-venda.nome-abrev = nota-fiscal.nome-ab-cli
                    AND ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli
                  NO-LOCK NO-ERROR.
   FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:
       FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
       FIND ref-item-ext WHERE ref-item-ext.it-codigo = it-nota-fisc.it-codigo
                           AND ref-item-ext.cod-refer = it-nota-fisc.cod-refer
                         NO-LOCK NO-ERROR.
       /*
       IF AVAIL ref-item-ext AND ref-item-ext.cod-ncm <> "" THEN
          ASSIGN c-cod-ncm = ref-item-ext.cod-ncm.
       ELSE
          ASSIGN c-cod-ncm = ITEM.class-fiscal.
       */

       RUN esapi/calcula-ncm.p (INPUT ref-item-ext.it-codigo,
                                INPUT ref-item-ext.cod-refer,
                                OUTPUT c-ncm).
       ASSIGN c-cod-ncm = c-ncm.

       FIND FIRST tt-work1 WHERE tt-work1.cod-ncm = c-cod-ncm NO-LOCK NO-ERROR.
       IF NOT AVAIL tt-work1 THEN DO:
          CREATE tt-work1.
          ASSIGN tt-work1.cod-ncm = c-cod-ncm.
       END.
       ASSIGN tt-work1.peso-bruto   = tt-work1.peso-bruto   + it-nota-fisc.peso-bruto.
              tt-work1.peso-liquido = tt-work1.peso-liquido + it-nota-fisc.peso-liq-fat.
   END.

   FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK,
       EACH ped-item-rom WHERE
            ped-item-rom.nome-abrev   = it-nota-fisc.nome-ab-cli AND
            ped-item-rom.nr-pedcli    = it-nota-fisc.nr-pedcli AND
            ped-item-rom.nr-sequencia = it-nota-fisc.nr-seq-ped NO-LOCK,
       FIRST ob-etiqueta WHERE
             ob-etiqueta.cod-estabel  = ped-item-rom.cod-estabel AND
             ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta NO-LOCK:

       FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.

       FIND ref-item-ext WHERE ref-item-ext.it-codigo = it-nota-fisc.it-codigo
                           AND ref-item-ext.cod-refer = it-nota-fisc.cod-refer
                         NO-LOCK NO-ERROR.
       /*
       IF AVAIL ref-item-ext AND ref-item-ext.cod-ncm <> "" THEN
          ASSIGN c-cod-ncm = ref-item-ext.cod-ncm.
       ELSE
          ASSIGN c-cod-ncm = ITEM.class-fiscal.
       */

       RUN esapi/calcula-ncm.p (INPUT ref-item-ext.it-codigo,
                                INPUT ref-item-ext.cod-refer,
                                OUTPUT c-ncm).
       ASSIGN c-cod-ncm = c-ncm.

       FIND FIRST tt-work WHERE tt-work.cod-ncm      = c-cod-ncm
                            AND tt-work.num-etiqueta = ob-etiqueta.num-etiqueta
                            AND tt-work.it-codigo    = it-nota-fisc.it-codigo
                            AND tt-work.cod-refer    = it-nota-fisc.cod-refer
                          NO-ERROR.
       IF NOT AVAIL tt-work THEN DO:
          CREATE tt-work.
          ASSIGN tt-work.cod-ncm      = c-cod-ncm             
                 tt-work.num-etiqueta = ob-etiqueta.num-etiqueta
                 tt-work.it-codigo    = it-nota-fisc.it-codigo
                 tt-work.cod-refer    = it-nota-fisc.cod-refer.
       END.
       ASSIGN tt-work.desc-item  = ITEM.desc-item
              tt-work.quantidade = ob-etiqueta.quantidade.
   END.
END.

FOR EACH tt-work BREAK BY tt-work.cod-ncm
                       BY tt-work.desc-item
                       BY tt-work.num-etiqueta:

    ACCUMULATE tt-work.num-etiqueta(COUNT BY tt-work.cod-ncm).
    ACCUMULATE tt-work.num-etiqueta(COUNT).
    ACCUMULATE tt-work.quantidade(TOTAL BY tt-work.cod-ncm).
    ACCUMULATE tt-work.quantidade(TOTAL).

    IF FIRST-OF(tt-work.cod-ncm) THEN
       RUN pi-cab-excel.
   
    RUN pi-det-excel.

    IF LAST-OF(tt-work.cod-ncm) THEN DO:
       FOR EACH tt-work1 WHERE tt-work1.cod-ncm = tt-work.cod-ncm NO-LOCK:
           ACCUMULATE tt-work1.peso-liquido(TOTAL).
           ACCUMULATE tt-work1.peso-bruto(TOTAL).
       END.
       ASSIGN de-peso-liquido = (ACCUM TOTAL tt-work1.peso-liquido)
              de-peso-bruto   = (ACCUM TOTAL tt-work1.peso-bruto).

       RUN pi-tot-ncm-excel(INPUT (ACCUM TOTAL BY tt-work.cod-ncm tt-work.quantidade), 
                            INPUT (ACCUM COUNT BY tt-work.cod-ncm tt-work.num-etiqueta),
                            INPUT (de-peso-liquido),
                            INPUT (de-peso-bruto)).

       ASSIGN de-peso-liq-ger = de-peso-liq-ger + de-peso-liquido
              de-peso-bru-ger = de-peso-bru-ger + de-peso-bruto.
       IF de-peso-liq-ger > de-peso-liquido THEN
          ASSIGN l-total-geral = YES.
    END.
END.

IF l-total-geral THEN
   RUN pi-tot-ger-excel(INPUT (ACCUM TOTAL tt-work.quantidade),  
                        INPUT (ACCUM COUNT tt-work.num-etiqueta),
                        INPUT (de-peso-liq-ger),
                        INPUT (de-peso-bru-ger)). 

DDE EXECUTE i-canal COMMAND "[select(~"C1~")]". 
DDE EXECUTE sys     COMMAND "[column.width(21.43)]".

DDE EXECUTE i-canal COMMAND "[select(~"C2~")]". 
DDE EXECUTE sys     COMMAND "[column.width(25)]".

DDE EXECUTE i-canal COMMAND "[select(~"C3~")]". 
DDE EXECUTE sys     COMMAND "[column.width(14.57)]". 
DDE EXECUTE sys     COMMAND "[format.number(~"###.###.##0,00~")]".

DDE EXECUTE i-canal COMMAND "[select(~"C4~")]". 
DDE EXECUTE sys     COMMAND "[column.width(8.57)]". 

DDE EXECUTE i-canal COMMAND "[select(~"C5~")]". 
DDE EXECUTE sys     COMMAND "[column.width(9.43)]". 

OS-DELETE VALUE(tt-param.arq-saida).
DDE EXECUTE   sys COMMAND '[save.as("' + tt-param.arq-saida + '")]'.
DDE EXECUTE   sys COMMAND "[close(0)]". 
DDE EXECUTE   sys COMMAND "[quit()]". 
DDE TERMINATE sys.

IF tt-param.imp-param THEN DO:
   display tt-param.tp-relat
           tt-param.nr-pedcli
           tt-param.cod-estabel 
           tt-param.serie       
           tt-param.nr-nota-fis 
           IF tt-param.qtd-tot-abe = 1 THEN "Total"
                                       ELSE "Aberta"
                                       @ tt-param.qtd-tot-abe
           tt-param.arq-saida       
           with frame f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
DISABLE ALL WITH FRAME frm_excel.
CLEAR FRAME frm_excel ALL NO-PAUSE.
HIDE FRAME frm_excel NO-PAUSE.
run pi-finalizar in h-acomp.
return "OK":U.

PROCEDURE pi-abre-excel.
   DEF INPUT PARAMETER p-arquivo AS CHAR.

   def var h-prog as handle no-undo.
   run utp/ut-utils.p persistent set h-prog.

   run Execute in h-prog(input "EXCEL.EXE", input p-arquivo).

   delete procedure h-prog.
   PAUSE 5 NO-MESSAGE.
END PROCEDURE.

PROCEDURE pi-cab-excel.
   IF ped-venda.cod-estabel = '1' THEN
      ASSIGN c-loc-embarque = "PARAOPEBA/MG - BRASIL".
   ELSE
      ASSIGN c-loc-embarque = "CONTAGEM/MG - BRASIL".
    
   DDE SEND i-canal SOURCE "ROMANEO"              ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
   DDE SEND i-canal SOURCE emitente.nome-emit     ITEM "L" + TRIM(STRING(i-Lin + 2)) + "C1".
   DDE SEND i-canal SOURCE emitente.endereco      ITEM "L" + TRIM(STRING(i-Lin + 3)) + "C1".
   DDE SEND i-canal SOURCE emitente.cidade + " / " + emitente.pais ITEM "L" + TRIM(STRING(i-Lin + 4)) + "C1".
   DDE SEND i-canal SOURCE "Factura Comercial n§./Flecha:" ITEM "L" + TRIM(STRING(i-Lin + 6)) + "C1".
   DDE SEND i-canal SOURCE "Local de Embarque: " + c-loc-embarque ITEM "L" + TRIM(STRING(i-Lin + 8)) + "C1".
   DDE SEND i-canal SOURCE "Local de Destino:"    ITEM "L" + TRIM(STRING(i-Lin + 10)) + "C1".
   DDE SEND i-canal SOURCE "Conocimiento de Embarque:" ITEM "L" + TRIM(STRING(i-Lin + 12)) + "C1".
   DDE SEND i-canal SOURCE "N§.Pedido del Cliente: " + ped-venda.nr-pedrep ITEM "L" + TRIM(STRING(i-Lin + 13)) + "C1".
   DDE SEND i-canal SOURCE "N§.Pedido de la F brica: " + ped-venda.nr-pedcli ITEM "L" + TRIM(STRING(i-Lin + 14)) + "C1".
   DDE SEND i-canal SOURCE "TEJIDO ..."           ITEM "L" + TRIM(STRING(i-Lin + 16)) + "C1".
   DDE SEND i-canal SOURCE TRIM(tt-work.desc-item) + " - NCM/SH: " + 
                           STRING(tt-work.cod-ncm,"9999.99.99") ITEM "L" + TRIM(STRING(i-Lin + 17)) + "C1".
   DDE SEND i-canal SOURCE "ROLLO"         ITEM "L" + TRIM(STRING(i-Lin + 19)) + "C1".
   DDE SEND i-canal SOURCE "ARTICULO"      ITEM "L" + TRIM(STRING(i-Lin + 19)) + "C2".
   DDE SEND i-canal SOURCE "CANTIDAD(MTS)" ITEM "L" + TRIM(STRING(i-Lin + 19)) + "C3".
   DDE SEND i-canal SOURCE "DISE¥OS"       ITEM "L" + TRIM(STRING(i-Lin + 19)) + "C4".
   DDE SEND i-canal SOURCE "VARIANTE"      ITEM "L" + TRIM(STRING(i-Lin + 19)) + "C5".

    ASSIGN i-Lin = i-Lin + 20.
END PROCEDURE.

PROCEDURE pi-det-excel.
   DDE SEND i-canal SOURCE STRING(tt-work.num-etiqueta)  ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
   DDE SEND i-canal SOURCE TRIM(tt-work.desc-item)       ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
   DDE SEND i-canal SOURCE STRING(tt-work.quantidade)    ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
   DDE SEND i-canal SOURCE SUBSTR(tt-work.cod-refer,3,4) ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
   DDE SEND i-canal SOURCE SUBSTR(tt-work.cod-refer,7,1) ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
 /*DDE SEND i-canal SOURCE STRING(item.peso-bruto)       ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
   DDE SEND i-canal SOURCE STRING(ITEM.peso-liquido)     ITEM "L" + TRIM(STRING(i-Lin)) + "C7". */

   ASSIGN i-Lin = i-Lin + 1.
END PROCEDURE.

PROCEDURE pi-tot-ncm-excel.
   DEFINE INPUT PARAMETER p-quantidade   AS DEC.
   DEFINE INPUT PARAMETER p-rolos        AS INT.
   DEFINE INPUT PARAMETER p-peso-liquido AS DEC.
   DEFINE INPUT PARAMETER p-peso-bruto   AS DEC.

   DDE SEND i-canal SOURCE "TOTAL" ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
   DDE SEND i-canal SOURCE STRING(p-quantidade) ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
   DDE SEND i-canal SOURCE "CANTIDAD EN ROLLOS:" ITEM "L" + TRIM(STRING(i-Lin + 1)) + "C1".
   DDE SEND i-canal SOURCE STRING(p-rolos) ITEM "L" + TRIM(STRING(i-Lin + 1)) + "C2".
   DDE SEND i-canal SOURCE "PESO NETO(KGS):" ITEM "L" + TRIM(STRING(i-Lin + 2)) + "C1".
   DDE SEND i-canal SOURCE STRING(p-peso-liquido,">>,>>9.99") ITEM "L" + TRIM(STRING(i-Lin + 2)) + "C2".
   DDE SEND i-canal SOURCE "PESO BRUTO(KGS):" ITEM "L" + TRIM(STRING(i-Lin + 3)) + "C1".
   DDE SEND i-canal SOURCE STRING(p-peso-bruto,">>,>>9.99") ITEM "L" + TRIM(STRING(i-Lin + 3)) + "C2".
   DDE SEND i-canal SOURCE "CUBAJE(Mü):" ITEM "L" + TRIM(STRING(i-Lin + 4)) + "C1".

   ASSIGN i-Lin = i-Lin + 6.
END PROCEDURE.

PROCEDURE pi-tot-ger-excel.
   DEFINE INPUT PARAMETER p-quantidade   AS DEC.
   DEFINE INPUT PARAMETER p-rolos        AS INT.
   DEFINE INPUT PARAMETER p-peso-liquido AS DEC.
   DEFINE INPUT PARAMETER p-peso-bruto   AS DEC.
   
   DDE SEND i-canal SOURCE "TOTAL GENERAL" ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
   DDE SEND i-canal SOURCE STRING(p-quantidade) ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
   DDE SEND i-canal SOURCE "CANTIDAD EN ROLLOS:" ITEM "L" + TRIM(STRING(i-Lin + 1)) + "C1".
   DDE SEND i-canal SOURCE STRING(p-rolos) ITEM "L" + TRIM(STRING(i-Lin + 1)) + "C2".
   DDE SEND i-canal SOURCE "PESO NETO(KGS):" ITEM "L" + TRIM(STRING(i-Lin + 2)) + "C1".
   DDE SEND i-canal SOURCE STRING(p-peso-liquido,">>,>>9.99") ITEM "L" + TRIM(STRING(i-Lin + 2)) + "C2".
   DDE SEND i-canal SOURCE "PESO BRUTO(KGS):" ITEM "L" + TRIM(STRING(i-Lin + 3)) + "C1".
   DDE SEND i-canal SOURCE STRING(p-peso-bruto,">>,>>9.99") ITEM "L" + TRIM(STRING(i-Lin + 3)) + "C2".
   DDE SEND i-canal SOURCE "CUBAGEM(Mü):" ITEM "L" + TRIM(STRING(i-Lin + 4)) + "C1".

   ASSIGN i-Lin = i-Lin + 6.
END PROCEDURE.
