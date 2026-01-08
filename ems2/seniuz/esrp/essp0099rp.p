/******************************************************************************
*   Programa : ESSP0099RP.P
*   Objetivo.: Imprimir o Romaneio do DANFE
*   Autor....: DBNet - Toninho
*   Data.....: Dez/2005
*   Alterado.: F bio Coelho Lanza em OUT/2008
*              Foi Incluida a Coluna de Peso.
**  Obs......: Especifico da TEAR TEXTIL IND.COM.LTDA.
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0099RP 2.04.00.000}

DEF BUFFER empresa FOR mgadm.empresa.

/* Definicao de variaveis globais comuns a todos os programas do EMS */
{utp/ut-glob.i}

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino          AS INTEGER
       FIELD arquivo          AS CHAR FORMAT "x(35)"
       FIELD usuario          AS CHAR FORMAT "x(12)"
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       FIELD cod-estabel      AS CHAR
       FIELD serie            AS CHAR
       FIELD ini-nr-pedcli    LIKE ped-venda.nr-pedcli 
       FIELD fin-nr-pedcli    LIKE ped-venda.nr-pedcli
       FIELD sit-rolo         AS CHAR FORMAT "x"
       FIELD ini-nr-nota-fis  LIKE nota-fiscal.nr-nota-fis
       FIELD fin-nr-nota-fis  LIKE nota-fiscal.nr-nota-fis
       FIELD ini-dt-emissao   LIKE nota-fiscal.dt-emis
       FIELD fin-dt-emissao   LIKE nota-fiscal.dt-emis
       FIELD ini-nome-abrev   LIKE nota-fiscal.nome-ab-cli
       FIELD fin-nome-abrev   LIKE nota-fiscal.nome-ab-cli
       FIELD ini-cod-rep      AS INT
       FIELD fin-cod-rep      AS INT
       FIELD origem           AS CHAR
       FIELD imp-peso         AS INT.

DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

/* defini‡Æo de vari veis  */
DEF VAR h-acomp       AS HANDLE NO-UNDO.
DEF VAR i-tot-etq     AS INT.
DEF VAR i-tot-vol     AS INT.
DEF VAR i-qt-etq      AS INT.
DEF VAR i-imp-etq     AS INT.
DEF VAR de-tot-qtd    AS DEC.
DEF VAR de-tot-peso   AS DEC.
DEF VAR c-nr-nota-fis LIKE nota-fiscal.nr-nota-fis.
DEF VAR c-nr-pedcli   LIKE ped-venda.nr-pedcli.
DEF VAR c-nome-abrev  LIKE ped-venda.nome-abrev.
DEF VAR c-transp      AS CHAR FORMAT "x(40)".

DEF VAR i-num-etiqueta LIKE ob-etiqueta.num-etiqueta EXTENT 3.
DEF VAR de-qtde        LIKE ob-etiqueta.quantidade EXTENT 3.    
DEF VAR c-nuance       LIKE ob-etiqueta.nuance EXTENT 3.    
DEF VAR i-ct           AS INT.

FORM HEADER
    FILL("-",132) FORMAT "x(132)" SKIP
    c-empresa                                                      AT 1
    "R O M A N E I O    D E    E M B A R Q U E"                    AT 50 
    "P gina: " + STRING(PAGE-NUMBER,"999")   FORMAT "x(11)"        AT 119  SKIP
     FILL("-",132) FORMAT "x(132)" SKIP
    "PEDIDO: " + c-nr-pedcli                 FORMAT "x(25)"        AT 30 
    "CLIENTE: " + c-nome-abrev               FORMAT "x(45)"        AT 75 SKIP
    "NOTA FISCAL: " + c-nr-nota-fis          FORMAT "x(25)"        AT 25
    "TRANSPORTADORA: " + c-transp            FORMAT "x(45)"        AT 68  SKIP
    "QTDE DE VOLUMES: " + STRING(i-tot-vol)  FORMAT "x(27)"        AT 21 
    "QTDE DE PE€AS: " + STRING(i-tot-etq)    FORMAT "x(27)"        AT 69 SKIP
    "PESO TOTAL: " + TRIM(STRING(de-tot-peso,"z,zzz,zz9.99")) FORMAT "x(35)" AT 26
    "QUANTIDADE TOTAL: " + TRIM(STRING(de-tot-qtd,"z,zzz,zz9.99")) FORMAT "x(35)" AT 66 SKIP 
    FILL("_",132) FORMAT "x(132)" SKIP(1)
    "D E S C R I € Ç O    D O S    V O L U M E S" AT 45
    FILL("_",132) FORMAT "x(132)"  SKIP(1)
    WITH WIDTH 134 PAGE-TOP NO-BOX FRAME f-cabec-2.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

FIND empresa WHERE 
     empresa.ep-codigo = i-ep-codigo-usuario NO-LOCK NO-ERROR.

ASSIGN c-empresa = empresa.nome.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

VIEW FRAME f-cabec-2.

IF tt-param.origem = "NF" THEN DO:
   FOR EACH nota-fiscal WHERE
            nota-fiscal.cod-estabel   = tt-param.cod-estabel     AND
            nota-fiscal.serie         = tt-param.serie           AND
            nota-fiscal.nr-nota-fis  >= tt-param.ini-nr-nota-fis AND
            nota-fiscal.nr-nota-fis  <= tt-param.fin-nr-nota-fis AND
            nota-fiscal.dt-emis-nota >= tt-param.ini-dt-emissao  AND
            nota-fiscal.dt-emis-nota <= tt-param.fin-dt-emissao  AND
            nota-fiscal.nr-pedcli    >= tt-param.ini-nr-pedcli   AND 
            nota-fiscal.nr-pedcli    <= tt-param.fin-nr-pedcli   AND
            nota-fiscal.nome-ab-cli  >= tt-param.ini-nome-abrev  AND
            nota-fiscal.nome-ab-cli  <= tt-param.fin-nome-abrev  AND
            nota-fiscal.cod-rep      >= tt-param.ini-cod-rep     AND
            nota-fiscal.cod-rep      <= tt-param.fin-cod-rep 
            NO-LOCK BREAK BY nota-fiscal.nr-nota-fis.
   
       ASSIGN i-qt-etq = 0
              i-tot-vol = 0     i-tot-etq = 0
              de-tot-peso = 0   de-tot-qtd = 0.

       FIND ped-venda-ext WHERE
            ped-venda-ext.cod-estabel = nota-fiscal.cod-estabel AND
            ped-venda-ext.nr-pedido = INT(nota-fiscal.nr-pedcli)
            NO-LOCK NO-ERROR.
       IF AVAIL ped-venda-ext THEN
          ASSIGN i-tot-vol = ped-venda-ext.qt-fardos.

       FOR EACH ped-item-res WHERE
                ped-item-res.cod-estabel = nota-fiscal.cod-estabel AND
                ped-item-res.serie       = nota-fiscal.serie AND
                ped-item-res.nr-nota-fis = INT(nota-fiscal.nr-nota-fis) AND
                ped-item-res.faturado    = YES NO-LOCK.
   
           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev   = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli    = ped-item-res.nr-pedcli  AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia NO-LOCK.
   
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel  = ped-item-rom.cod-estabel AND
                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                    NO-LOCK NO-ERROR.

               FIND ITEM WHERE
                    ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.
                                  /*
               IF AVAIL ob-etiqueta AND
                  NOT ob-etiqueta.nr-lote BEGINS 'PP' THEN
                  ASSIGN i-tot-vol = i-tot-vol + 1.
                                    */
               ASSIGN i-tot-etq = i-tot-etq + 1.

               /*
               IF ITEM.un = 'kg' THEN
                  ASSIGN de-tot-peso = de-tot-peso + ob-etiqueta.quantidade.
               ELSE
                  ASSIGN de-tot-peso = de-tot-peso + (ob-etiqueta.quantidade * ITEM.peso-bruto).
                */
               ASSIGN de-tot-qtd  = de-tot-qtd + ob-etiqueta.quantidade.
           END.
       END.
       IF de-tot-qtd = 0 THEN NEXT.
   
       ASSIGN de-tot-peso = nota-fiscal.peso-bru-tot.

       FIND transporte WHERE
            transporte.nome-abrev = nota-fiscal.nome-transp NO-LOCK NO-ERROR.
   
       FIND emitente WHERE
            emitente.nome-abrev = nota-fiscal.nome-ab-cli NO-LOCK NO-ERROR.
   
       ASSIGN c-nr-pedcli   = nota-fiscal.nr-pedcli
              c-nome-abrev  = STRING(emitente.cod-emit) + "-" + nota-fiscal.nome-ab-cli
              c-transp      = STRING(transporte.cod-transp) + "-" + nota-fiscal.nome-transp
              c-nr-nota-fis = nota-fiscal.nr-nota-fis.
   
       FIND ped-venda-ext WHERE
            ped-venda-ext.cod-estabel = nota-fiscal.cod-estabel AND
            ped-venda-ext.nr-pedido = INTEGER(nota-fiscal.nr-pedcli)
            NO-LOCK NO-ERROR.
       ASSIGN i-tot-vol = 0.
       IF AVAIL ped-venda-ext THEN
          ASSIGN i-tot-vol = ped-venda-ext.qt-fardos.

       FOR EACH ped-item-res WHERE
                ped-item-res.cod-estabel = nota-fiscal.cod-estabel AND
                ped-item-res.serie       = nota-fiscal.serie AND
                ped-item-res.nr-nota-fis = INT(nota-fiscal.nr-nota-fis) AND
                ped-item-res.faturado    = YES NO-LOCK
                BY ped-item-res.nr-sequencia.
   
           ASSIGN i-qt-etq = 0.
           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev   = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli    = ped-item-res.nr-pedcli  AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia NO-LOCK.
               ASSIGN i-qt-etq = i-qt-etq + 1.
           END.

           FIND item WHERE
                item.it-codigo = ped-item-res.it-codigo NO-LOCK NO-ERROR.
           FIND ped-item-ext OF ped-item-res NO-LOCK NO-ERROR.
           PUT TRIM(item.desc-item) + " - " + TRIM(item.it-codigo) + "." +
               ped-item-res.cod-refer +
               " (" + STRING(transporte.cod-transp) + ")" + 
               "   " + ped-item-ext.obs-it-cliente FORMAT "x(132)"
               SKIP.

           RUN imp-cabec (INPUT i-qt-etq).
   
           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev   = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli    = ped-item-res.nr-pedcli  AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                    BREAK BY ped-item-rom.nr-volume.
   
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel  = ped-item-rom.cod-estabel AND
                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                    NO-LOCK NO-ERROR.
   
               ASSIGN i-ct                 = i-ct + 1
                      i-num-etiqueta[i-ct] = ob-etiqueta.num-etiqueta 
                      de-qtde[i-ct]        = ob-etiqueta.quantidade
                      c-nuance[i-ct]       = ob-etiqueta.nuance.
        
               IF tt-param.imp-peso = 1 THEN
                  ASSIGN c-nuance[i-ct] = STRING(ob-etiqueta.peso,">>9.99").


               IF i-ct = 1 THEN 
                  PUT i-num-etiqueta[1]            AT  6
                      de-qtde[1]  FORMAT ">>>9.99" AT 17
                      c-nuance[1] FORMAT "x(6)"    AT 26.
   
               IF i-ct = 2 THEN 
                  PUT i-num-etiqueta[2]            AT 54
                      de-qtde[2]  FORMAT ">>>9.99" AT 65
                      c-nuance[2] FORMAT "x(6)"    AT 74.
                 
               IF i-ct = 3 THEN DO.
                  PUT i-num-etiqueta[3]            AT 102
                      de-qtde[3]  FORMAT ">>>9.99" AT 113
                      c-nuance[3] FORMAT "x(6)"    AT 122.
   
                  ASSIGN i-ct           = 0 
                         i-num-etiqueta = 0
                         de-qtde        = 0
                         c-nuance       = ''.
               END.
               ASSIGN i-imp-etq = i-imp-etq + 1.
   
               IF LINE-COUNTER + 4 >= PAGE-SIZE AND i-imp-etq < i-qt-etq THEN DO.
                  PAGE.
                  FIND item WHERE
                       item.it-codigo = ped-item-res.it-codigo NO-LOCK NO-ERROR.
                  FIND ped-item-ext OF ped-item-res NO-LOCK NO-ERROR.
                  PUT TRIM(item.desc-item) + " - " + TRIM(item.it-codigo) + "." +
                      ped-item-res.cod-refer +
                      " (" + STRING(transporte.cod-transp) + ")" + 
                      "   " + ped-item-ext.obs-it-cliente FORMAT "x(132)"
                      SKIP.
   
                  RUN imp-cabec (INPUT i-qt-etq).
               END.
           END.
           PUT " " SKIP(1).
           ASSIGN i-ct           = 0  
                  i-num-etiqueta = 0
                  de-qtde        = 0 
                  c-nuance       = ''.
   
           IF LINE-COUNTER + 6 >= PAGE-SIZE THEN PAGE.
       END.
   
       PUT FILL("=",55) + " I M P O R T A N T E " + FILL("=",56) FORMAT "x(132)" AT 1
           "AO RECEBER A MERCADORIA, FAVOR CONFERIR O TOTAL DE VOLUMES E A QUANTIDADE DE PE€AS. " AT 6
           "CASO HAJA A VIOLA€ÇO DO VOLUME, COM FALTA DE PE€AS, ENTRE CONTATO COM A IMATEXTIL (031) 32383100." AT 6
           "*** EM CASO DE RECLAMA€ÇO, FAVOR DEVOLVER A ETIQUETA DO VOLUME ***" AT 6
           FILL("_",132) FORMAT "x(132)" AT 1
           SKIP.
       PAGE.
       PAGE-NUMBER = 1.
   END.
END.
ELSE DO: /* Por Pedido */
   FOR EACH ped-venda WHERE
            ped-venda.nr-pedcli >= tt-param.ini-nr-pedcli AND  
            ped-venda.nr-pedcli <= tt-param.fin-nr-pedcli    
            NO-LOCK.
    
       ASSIGN i-tot-vol = 0           de-tot-peso = 0         
              de-tot-qtd = 0          i-tot-etq = 0.

       FIND ped-venda-ext WHERE
            ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
            ped-venda-ext.nr-pedido = ped-venda.nr-pedido 
            NO-LOCK NO-ERROR.
       IF AVAIL ped-venda-ext THEN
          ASSIGN i-tot-vol = ped-venda-ext.qt-fardos.

       /* TOTAIS DO CABE€ALHO */
       FOR EACH ped-item-res WHERE
                ped-item-res.cod-estabel = ped-venda.cod-estabel AND
                ped-item-res.nome-abrev = ped-venda.nome-abrev AND
                ped-item-res.nr-pedcli = ped-venda.nr-pedcli NO-LOCK.
    
           FOR EACH ped-item-rom WHERE
                    ped-item-rom.cod-estabel = ped-venda.cod-estabel AND
                    ped-item-rom.nome-abrev   = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli    = ped-item-res.nr-pedcli  AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                    NO-LOCK.
    
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel  = ped-item-rom.cod-estabel AND
                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                    NO-LOCK NO-ERROR.
    
               IF NOT AVAIL ob-etiqueta THEN
                  FIND ob-etiqueta WHERE
                       ob-etiqueta.cod-estabel  = '504' AND
                       ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                       NO-LOCK NO-ERROR.

               IF NOT AVAIL ob-etiqueta THEN NEXT.

               FIND ITEM WHERE
                    ITEM.it-codigo = ob-etiqueta.it-codigo
                    NO-LOCK NO-ERROR.
                                  /*
               IF AVAIL ob-etiqueta AND
                  NOT ob-etiqueta.nr-lote BEGINS 'PP' THEN
                  ASSIGN i-tot-vol = i-tot-vol + 1.
                                    */
               ASSIGN i-tot-etq = i-tot-etq + 1.

               IF ITEM.un = 'kg' THEN
                  ASSIGN de-tot-peso = de-tot-peso + ob-etiqueta.quantidade.
               ELSE
                  ASSIGN de-tot-peso = de-tot-peso + (ob-etiqueta.quantidade * ITEM.peso-bruto).

               ASSIGN de-tot-qtd  = de-tot-qtd + ob-etiqueta.quantidade.
           END.
       END.
       IF de-tot-qtd = 0 THEN NEXT.
    
       FIND transporte WHERE
            transporte.nome-abrev = ped-venda.nome-transp NO-LOCK NO-ERROR.
    
       ASSIGN c-nr-pedcli  = ped-venda.nr-pedcli
              c-nome-abrev = ped-venda.nome-abrev
              c-transp     = STRING(transporte.cod-transp) + "-" + ped-venda.nome-transp.
    
       ASSIGN i-ct = 0.
       FOR EACH ped-item-res WHERE
                ped-item-res.nome-abrev = ped-venda.nome-abrev AND
                ped-item-res.nr-pedcli  = ped-venda.nr-pedcli  NO-LOCK
                BY ped-item-res.nr-sequencia.

           ASSIGN i-qt-etq = 0.
           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev   = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli    = ped-item-res.nr-pedcli  AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia NO-LOCK.
               ASSIGN i-qt-etq = i-qt-etq + 1.
           END.
           IF i-qt-etq = 0 THEN NEXT.
           
           FIND item WHERE
                item.it-codigo = ped-item-res.it-codigo NO-LOCK NO-ERROR.
           FIND ped-item-ext OF ped-item-res NO-LOCK NO-ERROR.
           PUT TRIM(item.desc-item) + " - " + TRIM(item.it-codigo) + "." +
               ped-item-res.cod-refer +
               " (" + STRING(transporte.cod-transp) + ")" + 
               "   " + ped-item-ext.obs-it-cliente FORMAT "x(132)"
               SKIP.
    
           RUN imp-cabec (INPUT i-qt-etq).

           FOR EACH ped-item-rom WHERE
                    ped-item-rom.nome-abrev   = ped-item-res.nome-abrev AND
                    ped-item-rom.nr-pedcli    = ped-item-res.nr-pedcli  AND
                    ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                    BREAK BY ped-item-rom.nr-volume.
   
               FIND ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel  = ped-item-rom.cod-estabel AND
                    ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                    NO-LOCK NO-ERROR.
               IF NOT AVAIL ob-etiqueta THEN
                  FIND ob-etiqueta WHERE
                       ob-etiqueta.cod-estabel  = '504' AND
                       ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                       NO-LOCK NO-ERROR.
   
               ASSIGN i-ct                 = i-ct + 1
                      i-num-etiqueta[i-ct] = ob-etiqueta.num-etiqueta 
                      de-qtde[i-ct]        = ob-etiqueta.quantidade
                      c-nuance[i-ct]       = ob-etiqueta.nuance.
        
               IF tt-param.imp-peso = 1 THEN /* Coluna de Peso */
                  ASSIGN c-nuance[i-ct] = STRING(ob-etiqueta.peso,">>9.99").

               IF i-ct = 1 THEN
                  PUT i-num-etiqueta[1]            AT  6
                      de-qtde[1]  FORMAT ">>>9.99" AT 17
                      c-nuance[1] FORMAT "x(6)"    AT 26.
               
               IF i-ct = 2 THEN 
                  PUT i-num-etiqueta[2]            AT 54
                      de-qtde[2]  FORMAT ">>>9.99" AT 65
                      c-nuance[2]  FORMAT "x(6)"   AT 74.
                 
               IF i-ct = 3 THEN DO.
                  PUT i-num-etiqueta[3]            AT 102
                      de-qtde[3]  FORMAT ">>>9.99" AT 113
                      c-nuance[3]  FORMAT "x(6)"   AT 122.
   
                  ASSIGN i-ct           = 0
                         i-num-etiqueta = 0
                         de-qtde        = 0
                         c-nuance       = ''.
               END.
               ASSIGN i-imp-etq = i-imp-etq + 1.
   
               IF LINE-COUNTER + 4 >= PAGE-SIZE AND i-imp-etq < i-qt-etq THEN DO.
                  PAGE.
                  FIND item WHERE
                       item.it-codigo = ped-item-res.it-codigo NO-LOCK NO-ERROR.
                  FIND ped-item-ext OF ped-item-res NO-LOCK NO-ERROR.
                  PUT TRIM(item.desc-item) + " - " + TRIM(item.it-codigo) + "." +
                      ped-item-res.cod-refer +
                      " (" + STRING(transporte.cod-transp) + ")" + 
                      "   " + ped-item-ext.obs-it-cliente FORMAT "x(132)"
                      SKIP.
   
                  RUN imp-cabec (INPUT i-qt-etq).

               END.
           END.
           PUT " " SKIP(1).
           ASSIGN i-ct           = 0 
                  i-num-etiqueta = 0
                  de-qtde        = 0
                  c-nuance       = ''.
   
           IF LINE-COUNTER + 4 >= PAGE-SIZE THEN PAGE.
       END.

       PUT FILL("=",55) + " I M P O R T A N T E " + FILL("=",56) FORMAT "x(132)" AT 1
           "AO RECEBER A MERCADORIA, FAVOR CONFERIR O TOTAL DE VOLUMES E A QUANTIDADE DE PE€AS. " AT 6
           "CASO HAJA A VIOLA€ÇO DO VOLUME, COM FALTA DE PE€AS, ENTRE CONTATO COM A IMATEXTIL (031) 32383100." AT 6
           "*** EM CASO DE RECLAMA€ÇO, FAVOR DEVOLVER A ETIQUETA DO VOLUME ***" AT 6
           FILL("_",132) FORMAT "x(132)" AT 1
           SKIP.
   END.
END.


/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.


PROCEDURE imp-cabec.
   DEF INPUT PARAMETER p-tot-etq  AS INT.

   DEF VAR c-col AS CHAR INITIAL "Nuance".
   IF tt-param.imp-peso = 1 THEN
      ASSIGN c-col = "  Peso".
    
   PUT "Num. Rolo  Qtde(m)  " + c-col FORMAT "x(26)"AT 6
   IF p-tot-etq >= 2
      THEN "Num. Rolo  Qtde(m)  " + c-col
      ELSE ""  FORMAT "x(30)" AT 54
   IF p-tot-etq >= 3
      THEN "Num. Rolo  Qtde(m)  " + c-col
      ELSE ""  FORMAT "x(30)" AT 102
   SKIP.
    
   PUT "---------  -------  ------ " AT 6
   IF p-tot-etq >= 2
      THEN "---------  -------  ------" 
      ELSE "" FORMAT "x(30)" AT 54
   IF p-tot-etq >= 3
      THEN "---------  -------  ------" 
      ELSE "" FORMAT "x(30)" AT 102
   SKIP.
END.
