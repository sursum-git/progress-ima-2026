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

DEFINE VAR i-canal       AS INT.
DEFINE VAR sys           AS INT.
DEFINE VAR i-Lin         AS INT INIT 1.
DEFINE VAR c-lin         AS CHAR FORMAT "x(500)".
DEFINE VAR aux-command   AS CHAR FORMAT "x(100)".
DEFINE VAR de-peso-bruto AS DEC FORMAT ">>,>>>,>>9.99".
DEFINE VAR de-peso-liqdo AS DEC FORMAT ">>,>>>,>>9.99".
DEFINE VAR i-class-fisc  LIKE it-nota-fisc.class-fisc.
DEFINE VAR l-tem-liso    AS LOG.
DEFINE VAR l-tem-estamp  AS LOG.

/*DEFINE FRAME frm_excel WITH SIZE 85.25 BY 15.42.*/
DEFINE FRAME frm_excel WITH SIZE 2 BY 2 ROW 1 COLUMN 1.
ENABLE ALL WITH FRAME frm_excel.

FORM
    "*--------- Parƒmetros/Sele‡Æo ---------*"  SKIP
    tt-param.tp-relat         LABEL "Tipo realt¢rio." SKIP
    tt-param.nr-pedcli        LABEL "Pedido........." SKIP
    tt-param.cod-estabel      LABEL "Estabelecimento" SKIP
    tt-param.serie            LABEL "S‚rie.........." SKIP
    tt-param.nr-nota-fis      LABEL "Numero da NF..." SKIP
    tt-param.qtd-tot-abe      LABEL "Tipo de Quantid" SKIP
    tt-param.arq-saida        LABEL "Arquivo Sa¡da.." 
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
    
IF tt-param.tp-relat = 1 THEN DO:
   FIND ped-venda WHERE ped-venda.nr-pedcli = tt-param.nr-pedcli NO-LOCK NO-ERROR.
   FIND emitente WHERE emitente.cod-emitente = ped-venda.cod-emitente NO-LOCK NO-ERROR.
   
   /*----- Verifica se o pedido tem referˆncias com acabamento liso e estampado -----*/
   FIND FIRST ped-item OF ped-venda WHERE SUBSTR(ped-item.cod-refer,7,1) = "0" NO-LOCK NO-ERROR.
   IF NOT AVAIL ped-item THEN
      FIND FIRST ped-item OF ped-venda WHERE ped-item.cod-refer = "" NO-LOCK NO-ERROR.
   IF AVAIL ped-item THEN
      ASSIGN l-tem-liso = YES.
   FIND FIRST ped-item OF ped-venda WHERE SUBSTR(ped-item.cod-refer,7,1) <> "0" NO-LOCK NO-ERROR.
   IF AVAIL ped-item THEN
      ASSIGN l-tem-estamp = YES.

   FIND FIRST nota-fiscal WHERE
              nota-fiscal.nome-ab-cli = ped-venda.nome-abrev AND
              nota-fiscal.nr-pedcli   = ped-venda.nr-pedcli NO-LOCK NO-ERROR.
   IF AVAIL nota-fiscal THEN DO:
      FIND FIRST it-nota-fisc OF nota-fiscal NO-LOCK NO-ERROR.
      IF AVAIL it-nota-fisc THEN DO:
         ASSIGN i-class-fisc = it-nota-fisc.class-fisc.
         FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
      END.
      ELSE
         ASSIGN i-class-fisc = "".
   END.
   ELSE DO:
      FIND FIRST ped-item OF ped-venda NO-LOCK NO-ERROR.
      IF AVAIL ped-item THEN
         FIND ITEM WHERE ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
   END.
END.
ELSE DO:
   FIND nota-fiscal WHERE
        nota-fiscal.cod-estabel = tt-param.cod-estabel AND
        nota-fiscal.serie       = tt-param.serie AND
        nota-fiscal.nr-nota-fis = tt-param.nr-nota-fis
        NO-LOCK NO-ERROR.
   /*----- Verifica se a Nota Fiscal tem referˆncias com acabamento liso e estampado -----*/
   FIND FIRST it-nota-fisc OF nota-fiscal WHERE SUBSTR(it-nota-fisc.cod-refer,7,1) = "0" NO-LOCK NO-ERROR.
   IF NOT AVAIL it-nota-fisc THEN
      FIND FIRST it-nota-fisc OF nota-fiscal WHERE it-nota-fisc.cod-refer = "" NO-LOCK NO-ERROR.
   IF AVAIL it-nota-fisc THEN
      ASSIGN l-tem-liso = YES.
   FIND FIRST it-nota-fisc OF nota-fiscal WHERE SUBSTR(it-nota-fisc.cod-refer,7,1) <> "0" NO-LOCK NO-ERROR.
   IF AVAIL it-nota-fisc THEN
      ASSIGN l-tem-estamp = YES.

   FIND FIRST it-nota-fisc OF nota-fiscal NO-LOCK NO-ERROR.
   ASSIGN i-class-fisc = it-nota-fisc.class-fisc.
   FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
   FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente NO-LOCK NO-ERROR.
END.

DDE INITIATE sys FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "System".
DDE INITIATE i-canal FRAME FRAME frm_excel:HANDLE APPLICATION "EXCEL" TOPIC "Pasta1".

IF l-tem-liso = YES THEN
   RUN pi-processa (INPUT 1).

IF l-tem-estamp THEN DO:
   IF i-Lin > 1 THEN
      ASSIGN i-Lin = i-Lin + 5.
   RUN pi-processa (INPUT 2).
END.

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

PROCEDURE pi-processa.
   DEF INPUT PARAMETER p-acao AS INT.

   RUN pi-cab-excel.

   ASSIGN de-peso-bruto = 0
          de-peso-liqdo = 0.

   IF tt-param.tp-relat = 1 THEN DO: /* Por Pedido */
      FOR EACH ped-item WHERE ped-item.nome-abrev = ped-venda.nome-abrev 
                          AND ped-item.nr-pedcli  = ped-venda.nr-pedcli
                          AND (((SUBSTR(ped-item.cod-refer,7,1) = "0" OR ped-item.cod-refer = "") AND p-acao = 1) OR
                               (SUBSTR(ped-item.cod-refer,7,1) <> "0" AND p-acao = 2))
                          AND ped-item.cod-sit-item <> 6 /* Cancel */
                          AND ((ped-item.cod-sit-item = 1 AND tt-param.qtd-tot-abe = 2) OR
                               (tt-param.qtd-tot-abe = 1))
                        NO-LOCK:
          FIND ITEM WHERE ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
          IF tt-param.qtd-tot-abe = 1 THEN
             ASSIGN de-peso-bruto = de-peso-bruto + ITEM.peso-bruto * ped-item.qt-pedida
                    de-peso-liqdo = de-peso-liqdo + ITEM.peso-liquido * ped-item.qt-pedida.
          ELSE
              ASSIGN de-peso-bruto = de-peso-bruto + ITEM.peso-bruto * (ped-item.qt-pedida - ped-item.qt-atendida)
                     de-peso-liqdo = de-peso-liqdo + ITEM.peso-liquido * (ped-item.qt-pedida - ped-item.qt-atendida).

      END.
      FOR EACH ped-item-res WHERE ped-item-res.nome-abrev = ped-venda.nome-abrev
                              AND ped-item-res.nr-pedcli  = ped-venda.nr-pedcli
                            NO-LOCK,
          EACH ped-item WHERE ped-item.nome-abrev = ped-venda.nome-abrev                  
                          AND ped-item.nr-pedcli  = ped-venda.nr-pedcli
                          AND (((SUBSTR(ped-item.cod-refer,7,1) = "0" OR ped-item.cod-refer = "") AND p-acao = 1) OR
                               (SUBSTR(ped-item.cod-refer,7,1) <> "0" AND p-acao = 2))
                          AND ped-item.nr-sequencia = ped-item-res.nr-sequencia
                          AND ((ped-item.cod-sit-item = 1 AND tt-param.qtd-tot-abe = 2) OR
                               (tt-param.qtd-tot-abe = 1))                                
                        NO-LOCK:                                                          
          FOR EACH ped-item-rom WHERE
                   ped-item-rom.nome-abrev   = ped-item-res.nome-abrev AND
                   ped-item-rom.nr-pedcli    = ped-item-res.nr-pedcli AND
                   ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia NO-LOCK,
             FIRST ob-etiqueta WHERE
                   ob-etiqueta.num-etiqueta =  ped-item-rom.num-etiqueta
                   NO-LOCK:

              FIND ITEM WHERE ITEM.it-codigo = ped-item-res.it-codigo NO-LOCK NO-ERROR.

              RUN pi-det-excel.

              ACCUMULATE ped-item-rom.num-etiqueta(COUNT).
              ACCUMULATE ob-etiqueta.quantidade(TOTAL).
          END.
      END.
   END.
   ELSE DO: /* Por Nota Fiscal */
      FOR EACH it-nota-fisc OF nota-fiscal 
          WHERE (((SUBSTR(it-nota-fisc.cod-refer,7,1) = "0" OR it-nota-fisc.cod-refer = "") AND p-acao = 1) OR
                 (SUBSTR(it-nota-fisc.cod-refer,7,1) <> "0" AND p-acao = 2))
          NO-LOCK:
          ACCUMULATE it-nota-fisc.peso-bruto(TOTAL).
          ACCUMULATE it-nota-fisc.peso-liq-fat(TOTAL).
      END.
      ASSIGN de-peso-bruto = (ACCUM TOTAL it-nota-fisc.peso-bruto)
             de-peso-liqdo = (ACCUM TOTAL it-nota-fisc.peso-liq-fat).

      FOR EACH it-nota-fisc OF nota-fiscal 
          WHERE (((SUBSTR(it-nota-fisc.cod-refer,7,1) = "0" OR it-nota-fisc.cod-refer = "")AND p-acao = 1) OR
                 (SUBSTR(it-nota-fisc.cod-refer,7,1) <> "0" AND p-acao = 2))
          NO-LOCK,
          EACH ped-item-rom WHERE
               ped-item-rom.nome-abrev   = it-nota-fisc.nome-ab-cli AND
               ped-item-rom.nr-pedcli    = it-nota-fisc.nr-pedcli AND
               ped-item-rom.nr-sequencia = it-nota-fisc.nr-seq-ped NO-LOCK,
          FIRST ob-etiqueta WHERE
                ob-etiqueta.num-etiqueta =  ped-item-rom.num-etiqueta NO-LOCK:

          FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.

          RUN pi-det-excel.

          ACCUMULATE ped-item-rom.num-etiqueta(COUNT).
          ACCUMULATE ob-etiqueta.quantidade(TOTAL).
      END.
   END.

   IF tt-param.tp-relat = 1 THEN DO: /* Por Pedido */
      ASSIGN de-peso-bruto = de-peso-bruto + ((ACCUM COUNT ped-item-rom.num-etiqueta) * 0.1).
   END.

   RUN pi-tot-excel(INPUT (ACCUM TOTAL ob-etiqueta.quantidade), INPUT (ACCUM COUNT ped-item-rom.num-etiqueta)).

END PROCEDURE.

PROCEDURE pi-cab-excel.
   DDE SEND i-canal SOURCE "ROMANEO"              ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
   DDE SEND i-canal SOURCE emitente.nome-emit     ITEM "L" + TRIM(STRING(i-Lin + 2)) + "C1".
   DDE SEND i-canal SOURCE emitente.endereco      ITEM "L" + TRIM(STRING(i-Lin + 3)) + "C1".
   DDE SEND i-canal SOURCE emitente.cidade + " / " + emitente.pais ITEM "L" + TRIM(STRING(i-Lin + 4)) + "C1".
   DDE SEND i-canal SOURCE "Factura Comercial n§./Flecha:" ITEM "L" + TRIM(STRING(i-Lin + 6)) + "C1".
   DDE SEND i-canal SOURCE "Local de Embarque: CONTAGEM/MG - BRASIL" ITEM "L" + TRIM(STRING(i-Lin + 8)) + "C1".
   DDE SEND i-canal SOURCE "Local de Destino:"    ITEM "L" + TRIM(STRING(i-Lin + 10)) + "C1".
   DDE SEND i-canal SOURCE "Conocimiento de Embarque" ITEM "L" + TRIM(STRING(i-Lin + 12)) + "C1".
   DDE SEND i-canal SOURCE "TEJIDO ..."           ITEM "L" + TRIM(STRING(i-Lin + 14)) + "C1".
   DDE SEND i-canal SOURCE TRIM(item.desc-item) + " - NCM/SH: " + 
                           STRING(i-class-fisc,"9999.99.99") ITEM "L" + TRIM(STRING(i-Lin + 15)) + "C1".
   DDE SEND i-canal SOURCE "ROLLO"         ITEM "L" + TRIM(STRING(i-Lin + 17)) + "C1".
   DDE SEND i-canal SOURCE "ARTICULO"      ITEM "L" + TRIM(STRING(i-Lin + 17)) + "C2".
   DDE SEND i-canal SOURCE "CANTIDAD(MTS)" ITEM "L" + TRIM(STRING(i-Lin + 17)) + "C3".
   DDE SEND i-canal SOURCE "DISE¥OS"       ITEM "L" + TRIM(STRING(i-Lin + 17)) + "C4".
   DDE SEND i-canal SOURCE "VARIANTE"      ITEM "L" + TRIM(STRING(i-Lin + 17)) + "C5".

   ASSIGN i-Lin = i-Lin + 18.
END PROCEDURE.

PROCEDURE pi-det-excel.
   DDE SEND i-canal SOURCE STRING(ob-etiqueta.num-etiqueta)  ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
   DDE SEND i-canal SOURCE TRIM(item.desc-item)              ITEM "L" + TRIM(STRING(i-Lin)) + "C2".
   DDE SEND i-canal SOURCE STRING(ob-etiqueta.quantidade)    ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
   DDE SEND i-canal SOURCE SUBSTR(ob-etiqueta.cod-refer,3,4) ITEM "L" + TRIM(STRING(i-Lin)) + "C4".
   DDE SEND i-canal SOURCE SUBSTR(ob-etiqueta.cod-refer,7,1) ITEM "L" + TRIM(STRING(i-Lin)) + "C5".
   DDE SEND i-canal SOURCE STRING(item.peso-bruto)           ITEM "L" + TRIM(STRING(i-Lin)) + "C6".
   DDE SEND i-canal SOURCE STRING(ITEM.peso-liquido)         ITEM "L" + TRIM(STRING(i-Lin)) + "C7".
   
   ASSIGN i-Lin = i-Lin + 1.
END PROCEDURE.

PROCEDURE pi-tot-excel.
   DEF INPUT PARAMETER p-total AS dec.
   DEF INPUT PARAMETER p-rolos AS INT.

   DDE SEND i-canal SOURCE "TOTAL" ITEM "L" + TRIM(STRING(i-Lin)) + "C1".
   DDE SEND i-canal SOURCE STRING(p-total) ITEM "L" + TRIM(STRING(i-Lin)) + "C3".
   DDE SEND i-canal SOURCE "CANTIDAD EN ROLLOS:" ITEM "L" + TRIM(STRING(i-Lin + 2)) + "C1".
   DDE SEND i-canal SOURCE STRING(p-rolos) ITEM "L" + TRIM(STRING(i-Lin + 2)) + "C2".
   DDE SEND i-canal SOURCE "PESO NETO(KGS):" ITEM "L" + TRIM(STRING(i-Lin + 4)) + "C1".
   DDE SEND i-canal SOURCE STRING(de-peso-liqdo,">>,>>9.99") ITEM "L" + TRIM(STRING(i-Lin + 4)) + "C2".
   DDE SEND i-canal SOURCE "PESO BRUTO(KGS):" ITEM "L" + TRIM(STRING(i-Lin + 6)) + "C1".
   DDE SEND i-canal SOURCE STRING(de-peso-bruto,">>,>>9.99") ITEM "L" + TRIM(STRING(i-Lin + 6)) + "C2".
   DDE SEND i-canal SOURCE "CUBAJE(Mü):" ITEM "L" + TRIM(STRING(i-Lin + 8)) + "C1".

   ASSIGN i-Lin = i-Lin + 8.
END PROCEDURE.
