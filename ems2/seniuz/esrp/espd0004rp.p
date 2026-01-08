/* Programa: ESPD004.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Pedidos
** Objetivo: Gerar o relatorio de Pedidos de Vendas p/Separacao de Mercadorias
** Autor...: Sandro Wiest/Gilvando de Souza Araujo - Julho/96
** Obs.....: Especifico da CIA.RENASCENCA INDUSTRIAL / SANTA ELISABETH
**
** Conversao para EMS 2.04:
**   Programa: ESPD004.P  =>  ESPD0004RP.P
**   Autor...: Prodb - Toninho
**   Data....: 19/01/2004
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESPD0004RP 2.04.00.000}

define temp-table tt-param no-undo
       field destino            as integer
       field arquivo            as char format "x(35)"
       field usuario            as char format "x(12)"
       field data-exec          as date
       field hora-exec          as integer
       FIELD c-pedido-ini       like ped-item.nr-pedcli
       FIELD c-pedido-fim       like ped-item.nr-pedcli
       FIELD c-item-ini         LIKE ped-item.it-codigo
       FIELD c-item-fim         LIKE ped-item.it-codigo
       FIELD c-ref-ini          LIKE ped-item.cod-refer
       FIELD c-ref-fim          LIKE ped-item.cod-refer
       FIELD da-entr-ini        like ped-item.dt-entrega  
       FIELD da-entr-fim        like ped-item.dt-entrega  
       FIELD da-impl-ini        like ped-venda.dt-implant 
       FIELD da-impl-fim        like ped-venda.dt-implant 
       FIELD c-acond-ini        like ped-item.cod-refer
       FIELD c-acond-fim        like ped-item.cod-refer
       FIELD corte-comerc-ini   LIKE ped-item-ext.corte-comerc
       FIELD corte-comerc-fim   LIKE ped-item-ext.corte-comerc
       FIELD l-apaga-todas      AS LOG
       FIELD c-cod-refer1       LIKE ped-item.cod-refer
       FIELD c-cod-refer2       LIKE ped-item.cod-refer
       FIELD c-cod-refer3       LIKE ped-item.cod-refer
       FIELD c-cod-refer4       LIKE ped-item.cod-refer
       FIELD c-cod-refer5       LIKE ped-item.cod-refer
       FIELD c-cod-refer6       LIKE ped-item.cod-refer
       FIELD c-cod-refer7       LIKE ped-item.cod-refer
       FIELD c-cod-refer8       LIKE ped-item.cod-refer
       FIELD c-cod-refer9       LIKE ped-item.cod-refer
       FIELD c-cod-refer10      LIKE ped-item.cod-refer
       FIELD l-sit-aberto       as log  format "Sim/NÆo"
       FIELD l-sit-parcial      as log  format "Sim/NÆo"
       FIELD l-sit-pendentes    as log  format "Sim/NÆo"
       FIELD l-sit-suspensos    as log  format "Sim/NÆo"
       FIELD l-sit-cancelados   as log  format "Sim/NÆo"
       FIELD l-sit-so-aprovados as log  format "Sim/NÆo"
       FIELD l-res-nao          as log  format "Sim/NÆo"
       FIELD l-res-sim          as log  format "Sim/NÆo"
       FIELD l-res-parc         as log  format "Sim/NÆo"
       FIELD l-res-aceita       as log  format "Sim/NÆo"
       FIELD l-queb-desenho     AS LOG  FORMAT "Sim/NÆo"
       FIELD c-saldo-estoq      AS CHAR FORMAT "x"
       FIELD c-classificacao    AS CHAR FORMAT "x(20)"
       FIELD impr-param         AS LOGICAL
       FIELD nr-coletor         AS INT.


DEFINE TEMP-TABLE tt-raw-digita
       FIELD raw-digita AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
DEF VAR h-acomp AS HANDLE NO-UNDO.

DEF TEMP-TABLE tt-aux
    FIELD it-codigo        AS CHAR FORMAT "x(16)"
    FIELD cod-refer        LIKE ped-item.cod-refer
    FIELD nr-pedcli        AS CHAR FORMAT "x(12)"
    FIELD nr-sequencia     AS INTE FORMAT ">>,>>9"
    FIELD nome-abrev       AS CHAR FORMAT "x(12)"
    FIELD campo-1          AS CHAR FORMAT "x(10)"
    FIELD lote             AS INTEGER FORMAT 9
    FIELD campo-2          AS CHAR FORMAT "x(4)"
    FIELD quantidade       AS DECI FORMAT "->>,>>9.99"
    FIELD nome-transp      AS CHAR FORMAT "x(12)"
    FIELD dt-entrega       AS DATE FORMAT "99/99/9999"
    INDEX codigo campo-1 nr-pedcli campo-2
    INDEX transp nome-transp campo-1 dt-entrega nr-pedcli
    INDEX pedido campo-1 dt-entrega nr-pedcli nome-transp.

DEF VAR c-credito          AS CHAR FORMAT "x(3)".
DEF VAR i-mes              AS INT.
DEF VAR i-ano              AS INT.
DEF VAR c-tipo             AS CHAR FORMAT "x(4)".
DEF VAR de-tot-aux         AS DEC.
DEF VAR de-tot-aux2        AS DEC.
DEF VAR de-qtd-tot-abe     AS DEC.
DEF VAR de-qtd-tot-res     AS DEC.
DEF VAR c-descricao        LIKE item.descricao-1.
DEF VAR c-nome-abrev       LIKE ped-venda.nome-abrev.
DEF VAR c-cidade           LIKE ped-venda.cidade.
DEF VAR c-uf               LIKE ped-venda.estado.
DEF VAR c-variante         AS CHAR FORMAT "x(4)".
DEF VAR c-acondicionamento AS CHAR FORMAT "x(13)".
DEF VAR de-saldo-pedido    AS DEC FORMAT "->>>,>>9.99".
DEF VAR c-saldo            AS CHAR FORMAT "x(15)" INIT "_______________".

FORM
    tt-param.c-pedido-ini       LABEL "Pedido Venda"           AT 14
    "a"                                                        AT 50
    tt-param.c-pedido-fim       NO-LABELS
    tt-param.c-item-ini         LABEL "Item de"                AT 19
    "a"                                                        AT 50
    tt-param.c-item-fim         NO-LABELS
    tt-param.c-ref-ini          LABEL "Referencia de"          AT 13
    "a"                                                        AT 50
    tt-param.c-ref-fim          NO-LABELS
    tt-param.da-entr-ini        LABEL "Dt. Entrega de"         AT 12              
    "a"                                                        AT 50
    tt-param.da-entr-fim        NO-LABELS  
    tt-param.da-impl-ini        LABEL "Dt. Implant de"         AT 12          
    "a"                                                        AT 50
    tt-param.da-impl-fim        NO-LABELS             
    tt-param.c-acond-ini        LABEL "Acondicionamento"       AT 10     
    "a"                                                        AT 50
    tt-param.c-acond-fim        NO-LABELS 
    tt-param.corte-comerc-ini   LABEL "Corte comercial."       AT 10     
    "a"                                                        AT 50
    tt-param.corte-comerc-fim   NO-LABELS 
    tt-param.c-cod-refer1       LABEL "Referˆncias"            AT 15
    tt-param.c-cod-refer2                                      AT 50
    tt-param.c-cod-refer3
    tt-param.c-cod-refer4
    tt-param.c-cod-refer5
    tt-param.c-cod-refer6
    tt-param.c-cod-refer7
    tt-param.c-cod-refer8
    tt-param.c-cod-refer9
    tt-param.c-cod-refer10
    tt-param.l-sit-aberto       LABEL "Abertos"                AT 19          
    tt-param.l-sit-parcial      LABEL "Atend Parcial"          AT 13
    tt-param.l-sit-pendentes    LABEL "Pendentes"              AT 17         
    tt-param.l-sit-suspensos    LABEL "Suspensos"              AT 17
    tt-param.l-sit-cancelados   LABEL "Cancelados"             AT 16         
    tt-param.l-sit-so-aprovados LABEL "Credito Aprovado"       AT 10
    tt-param.l-res-nao          LABEL "NÆo Reservados"         AT 12
    tt-param.l-res-sim          LABEL "Reservados Total"       AT 10
    tt-param.l-res-parc         LABEL "Reservados Parcial"     AT 8
    tt-param.l-res-aceita       LABEL "Aceita Parcial"         AT 12
    tt-param.c-saldo-estoq      LABEL "Itens com Saldo Estoque"   AT 3 FORMAT "X(15)"
    tt-param.l-queb-desenho     LABEL "Salta Pag por Desenho/Cor" AT 1    
    WITH NO-BOX SIDE-LABELS WIDTH 132 STREAM-IO FRAME f-parlis.

FORM
    tt-aux.nome-abrev    
    tt-aux.nr-pedcli     FORMAT "x(9)"
    c-credito
    tt-aux.nr-sequencia  FORMAT "9999"
    c-acondicionamento     
    c-variante             
    de-saldo-pedido        
    c-saldo                
    c-cidade               
    c-uf                   
    tt-aux.nome-transp   
    c-tipo                 
    emitente.cod-emit      
    WITH NO-BOX 55 DOWN WIDTH 132 WITH STREAM-IO FRAME f-detalhe NO-LABEL.

FORM HEADER
     "Cliente      Pedido    Crd  Seq Acondic       VCor   Saldo Ped" at   1
     " Qtd Reservada   Cidade                    "                    at  63
     "UF Transp       Tipo Client"                                    at 106
     SKIP
     "--------------------------------------------------------------" at   1
     "--------------------------------------------------------------" at  63
     "--------"                                                       at 125
     WITH WIDTH 132 FRAME f-cabecalho STREAM-IO NO-BOX PAGE-TOP DOWN.


/* Include PadrÆo para retirar acentos */
{include/i-freeac.i}

/* bloco principal do programa */
FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST empresa
     WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 

ASSIGN c-empresa = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

{utp/ut-liter.i PEDIDOS * r}
ASSIGN c-sistema = TRIM(RETURN-VALUE).
{utp/ut-liter.i Pedidos_de_Vendas_p/Separa‡Æo_de_Mercadorias * r}
ASSIGN c-titulo-relat = TRIM(RETURN-VALUE).


RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
{utp/ut-liter.i Imprimindo *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

FOR EACH ped-item WHERE
         ped-item.it-codigo  >= tt-param.c-item-ini   AND
         ped-item.it-codigo  <= tt-param.c-item-fim   AND
         ped-item.cod-refer  >= tt-param.c-ref-ini    AND
         ped-item.cod-refer  <= tt-param.c-ref-fim    NO-LOCK USE-INDEX ch-correcao,
    EACH ped-venda OF ped-item WHERE
         ped-venda.nr-pedcli  >= tt-param.c-pedido-ini AND   
         ped-venda.nr-pedcli  <= tt-param.c-pedido-fim AND 
         ped-venda.dt-implant >= tt-param.da-impl-ini  AND
         ped-venda.dt-implant <= tt-param.da-impl-fim  AND
         ped-venda.dt-entrega >= tt-param.da-entr-ini  AND
         ped-venda.dt-entrega <= tt-param.da-entr-fim  NO-LOCK.

    RUN pi-acompanhar IN h-acomp (INPUT "  Pedido:" + ped-item.nr-pedcli + 
                                        "       Item:" + ped-item.it-codigo).

    IF tt-param.l-apaga-todas = NO AND
       (ped-item.cod-refer <> tt-param.c-cod-refer1) AND 
       (ped-item.cod-refer <> tt-param.c-cod-refer2) AND
       (ped-item.cod-refer <> tt-param.c-cod-refer3) AND
       (ped-item.cod-refer <> tt-param.c-cod-refer4) AND
       (ped-item.cod-refer <> tt-param.c-cod-refer5) AND
       (ped-item.cod-refer <> tt-param.c-cod-refer6) AND
       (ped-item.cod-refer <> tt-param.c-cod-refer7) AND
       (ped-item.cod-refer <> tt-param.c-cod-refer8) AND
       (ped-item.cod-refer <> tt-param.c-cod-refer9) AND
       (ped-item.cod-refer <> tt-param.c-cod-refer10) THEN
       NEXT.

    IF ped-item.cod-sit-item >= 3 AND ped-item.cod-sit-item <> 5 THEN NEXT.

    IF ped-venda.cod-sit-aval <> 2 AND  
       ped-venda.cod-sit-aval <> 3 AND 
       tt-param.l-sit-so-aprovados = YES THEN NEXT.

    IF tt-param.l-sit-aberto = NO AND  
       ped-venda.cod-sit-ped = 1 THEN NEXT.

    IF tt-param.l-sit-parcial = NO AND  
       ped-venda.cod-sit-ped = 2 THEN NEXT.

    IF tt-param.l-sit-pendentes = NO AND  
       ped-venda.cod-sit-ped = 4 THEN NEXT.

    IF tt-param.l-sit-suspensos = NO AND 
       ped-venda.cod-sit-ped = 5 THEN NEXT.

    IF tt-param.l-sit-cancelados = NO AND 
       ped-venda.cod-sit-ped = 6 THEN NEXT.

    FIND ped-item-ext WHERE  
         ped-item-ext.nome-abrev   = ped-item.nome-abrev AND  
         ped-item-ext.nr-pedcli    = ped-item.nr-pedcli AND  
         ped-item-ext.nr-sequencia = ped-item.nr-sequencia AND  
         ped-item-ext.it-codigo    = ped-item.it-codigo AND  
         ped-item-ext.cod-refer    = ped-item.cod-refer NO-LOCK NO-ERROR.

    IF AVAIL ped-item-ext THEN DO:
       IF SUBSTR(ped-item-ext.acondicionamento,1,4) < tt-param.c-acond-ini OR
          SUBSTR(ped-item-ext.acondicionamento,1,4) > tt-param.c-acond-fim OR 
          ped-item-ext.corte-comerc < tt-param.corte-comerc-ini OR 
          ped-item-ext.corte-comerc > tt-param.corte-comerc-fim THEN NEXT. 

       IF SUBSTR(ped-item-ext.lote,2,1) <> "P" THEN NEXT.
    END.

    ASSIGN de-tot-aux = 0.
    IF tt-param.c-saldo-estoq <> "T" THEN DO.
       FOR EACH saldo-estoq WHERE
                saldo-estoq.it-codigo = ped-item.it-codigo NO-LOCK.
           ASSIGN de-tot-aux = de-tot-aux + saldo-estoq.qtidade-atu.
       END.
       IF tt-param.c-saldo-estoq = "P" AND de-tot-aux < 0 THEN NEXT.
       IF tt-param.c-saldo-estoq = "N" AND de-tot-aux >= 0 THEN NEXT.
    END.

    ASSIGN de-tot-aux  = 0
           de-tot-aux2 = 0.

    FOR EACH ped-item-res WHERE 
             ped-item-res.it-codigo    = ped-item.it-codigo AND 
             ped-item-res.cod-refer    = ped-item.cod-refer AND 
             ped-item-res.nr-sequencia = ped-item.nr-sequencia AND 
             ped-item-res.nr-pedcli    = ped-item.nr-pedcli AND 
             ped-item-res.nome-abrev   = ped-item.nome-abrev AND 
             ped-item-res.faturado     = NO NO-LOCK:
        ASSIGN de-tot-aux = de-tot-aux + ped-item-res.qt-pedida.
    END.
    ASSIGN de-tot-aux2 = de-tot-aux2 + ped-item.qt-pedida - ped-item.qt-atendida.

    IF de-tot-aux >= de-tot-aux2 AND tt-param.l-res-sim = NO THEN NEXT.

    IF de-tot-aux < de-tot-aux2  AND de-tot-aux > 0 AND 
       tt-param.l-res-parc = NO THEN NEXT.

    IF de-tot-aux = 0 AND tt-param.l-res-nao = NO THEN NEXT.
      
    IF de-tot-aux < de-tot-aux2     AND de-tot-aux > 0 AND
       tt-param.l-res-parc = YES    AND tt-param.l-res-aceita = YES AND
       ped-venda.ind-fat-par = NO THEN NEXT.

    CREATE tt-aux.
    ASSIGN tt-aux.it-codigo    = ped-item.it-codigo
           tt-aux.cod-refer    = ped-item.cod-refer
           tt-aux.nome-abrev   = ped-item.nome-abrev
           tt-aux.nome-transp  = ped-venda.nome-transp
           tt-aux.nr-sequencia = ped-item.nr-sequencia
           tt-aux.nr-pedcli    = ped-item.nr-pedcli
           tt-aux.lote         = IF SUBSTR(ped-item-ext.lote,1,2) = "RP"
                                 THEN 1 
                                 ELSE IF SUBSTR(ped-item-ext.lote,1,2) = "PP"
                                      THEN 2
                                      ELSE IF SUBSTR(ped-item-ext.lote,1,2) = "RD"
                                           THEN 3
                                           ELSE 4
           tt-aux.quantidade   = de-tot-aux.
           tt-aux.dt-entrega   = ped-venda.dt-entrega.
/* Altera‡Æo feita para o Walter em 07/11/05 - Ele ir  testar
    IF SUBSTR(ped-item.cod-refer,7,1) = "0" THEN
       ASSIGN tt-aux.campo-2 = SUBSTR(ped-item.cod-refer,3,4)
              tt-aux.campo-1 = ped-item.it-codigo.
    ELSE
       ASSIGN tt-aux.campo-2 = SUBSTR(ped-item.cod-refer,7,1)
              tt-aux.campo-1 = ped-item.it-codigo +
                               SUBSTR(ped-item.cod-refer,3,4).
*/
    ASSIGN tt-aux.campo-2 = SUBSTR(ped-item.cod-refer,7,1)
           tt-aux.campo-1 = ped-item.it-codigo +
                            ped-item.cod-refer.
END.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

IF tt-param.destino <> 2 THEN DO.
   VIEW FRAME f-cabec.
   VIEW FRAME f-rodape.
   VIEW FRAME f-cabecalho.
END.

IF tt-param.destino = 2 THEN DO.
   FOR EACH coletor WHERE 
            coletor.id = tt-param.nr-coletor EXCLUSIVE-LOCK.
       DELETE coletor.
   END.
END.

IF tt-param.destino <> 2 THEN DO.
   IF tt-param.c-classificacao = "Transportador" THEN DO:
      FOR EACH tt-aux USE-INDEX transp
               BREAK BY tt-aux.nome-transp
                     BY tt-aux.campo-1
                     BY tt-aux.dt-entrega
                     BY tt-aux.nr-pedcli:
       
          FIND item WHERE item.it-codigo = tt-aux.it-codigo NO-LOCK.
          IF FIRST-OF(tt-aux.nome-transp) OR 
             FIRST-OF(tt-aux.campo-1) THEN 
             PUT "Item: " item.it-codigo
                 "Referencia: " tt-aux.cod-refer 
                 " " item.descricao-1.
   
          FIND ped-item WHERE 
               ped-item.it-codigo    = tt-aux.it-codigo AND 
               ped-item.cod-refer    = tt-aux.cod-refer AND
               ped-item.nr-pedcli    = tt-aux.nr-pedcli AND 
               ped-item.nr-sequencia = tt-aux.nr-sequencia AND 
               ped-item.nome-abrev   = tt-aux.nome-abrev NO-LOCK.
   
          FIND ped-item-ext WHERE 
               ped-item-ext.nome-abrev   = ped-item.nome-abrev AND 
               ped-item-ext.nr-pedcli    = ped-item.nr-pedcli AND 
               ped-item-ext.nr-sequencia = ped-item.nr-sequencia AND 
               ped-item-ext.it-codigo    = ped-item.it-codigo AND 
               ped-item-ext.cod-refer    = ped-item.cod-refer NO-LOCK NO-ERROR.
   
          FIND ped-venda WHERE 
               ped-venda.nome-abrev = ped-item.nome-abrev AND 
               ped-venda.nr-pedcli  = ped-item.nr-pedcli NO-LOCK.
   
          FIND item WHERE
               item.it-codigo = ped-item.it-codigo NO-LOCK.
   
          ASSIGN c-acondicionamento = IF AVAIL ped-item-ext
                                      THEN ped-item-ext.acondicionamento
                                      ELSE ""
                    de-saldo-pedido = ped-item.qt-pedida - ped-item.qt-atendida -
                                      tt-aux.quantidade + ped-item.qt-devolvida -
                                      ped-item.qt-pendente - ped-item.qt-alocada.
   
          ASSIGN c-variante = tt-aux.campo-2
                 c-cidade   = ped-venda.cidade
                 c-uf       = ped-venda.estado.
   
          IF ped-venda.cod-entrega <> "" THEN DO:
             find loc-entr WHERE 
                  loc-entr.cod-entrega = ped-venda.cod-entrega AND 
                  loc-entr.nome-abrev  = ped-item.nome-abrev NO-LOCK NO-ERROR.
   
             IF AVAIL loc-entr THEN
                ASSIGN c-cidade = loc-entr.cidade
                       c-uf     = loc-entr.estado.
          END.
   
          IF (ped-venda.cod-entrega = "" and ped-venda.local-entreg = "") OR
              NOT AVAIL loc-entr THEN DO:
              FIND FIRST emitente WHERE 
                         emitente.nome-abrev = ped-item.nome-abrev AND 
                         emitente.identific <> 2 NO-LOCK.
              ASSIGN c-cidade = emitente.cidade
                     c-uf     = emitente.estado.
          END.
          FIND FIRST emitente WHERE
                     emitente.nome-abrev = tt-aux.nome-abrev AND
                     emitente.identific <> 2 NO-LOCK.
   
          ASSIGN c-tipo = (IF emitente.pais = "brasil" THEN "" ELSE "EXT").
   
          IF emitente.nome-abrev BEGINS "lundgren" OR
             emitente.nome-abrev BEGINS "arthurlund" THEN
             ASSIGN c-tipo = "LUND".
   
          IF ped-venda.cod-cond-pag = 1 OR ped-venda.cod-cond-pag = 2 THEN
             ASSIGN c-credito = "AV".
          ELSE
             IF ped-venda.cod-sit-aval = 2 OR ped-venda.cod-sit-aval = 3 THEN
                ASSIGN c-credito = "Ok".
             ELSE
                ASSIGN c-credito = "Nao".
       
          DISPLAY tt-aux.nr-pedcli
                  c-credito
                  tt-aux.nr-sequencia
                  c-acondicionamento
                  de-saldo-pedido
                  c-saldo
                  tt-aux.nome-abrev
                  tt-aux.nome-transp
                  c-variante
                  c-cidade
                  c-uf
                  c-tipo
                  emitente.cod-emit
                  WITH FRAME f-detalhe.
          DOWN WITH FRAME f-detalhe.
       
          IF LAST-OF(tt-aux.nome-transp) OR
             LAST-OF(tt-aux.nr-pedcli) THEN DO:
               /*--- Busca Qtd.aberta e Reservada ---*/
             ASSIGN de-qtd-tot-abe = 0
                    de-qtd-tot-res = 0.
   
             FOR EACH ped-item WHERE
                      ped-item.nome-abrev = tt-aux.nome-abrev AND
                      ped-item.nr-pedcli  = tt-aux.nr-pedcli  AND
                      (ped-item.cod-sit-item < 3 OR ped-item.cod-sit-item = 5)
                      NO-LOCK:
                 ASSIGN de-qtd-tot-abe = de-qtd-tot-abe + ped-item.qt-pedida -
                                         ped-item.qt-atendida + ped-item.qt-devolvida -
                                         ped-item.qt-pendente - ped-item.qt-alocada.
   
                 FIND ped-item-res WHERE
                      ped-item-res.nome-abrev   = ped-item.nome-abrev AND
                      ped-item-res.nr-pedcli    = ped-item.nr-pedcli  AND
                      ped-item-res.nr-sequencia = ped-item.nr-sequencia
                      NO-LOCK NO-ERROR.
                 IF AVAIL ped-item-res THEN
                    ASSIGN de-qtd-tot-res = de-qtd-tot-res + ped-item-res.qt-pedida.
             END.
   
             /* ---------------------------------------- */
             PUT " Amostra: " ped-venda.tp-pedido
                 "Volume: _______________      Forma: _______________" AT 82
                 SKIP
                 " Observacoes: " SUBSTR(ped-venda.cond-espec,1,100) FORMAT "x(100)"
                 SKIP
                 " Qtd.Tot.Aberta/Reservada: " 
                   de-qtd-tot-abe "/"
                   de-qtd-tot-res
                 "     Data Entrega: " ped-venda.dt-entrega FORMAT "99/99/9999"
                 SKIP.
          END.
      END.
   END.
   ELSE DO:
      FOR EACH tt-aux USE-INDEX pedido
           BREAK BY tt-aux.campo-1
                 BY tt-aux.dt-entrega
                 BY tt-aux.nr-pedcli
                 BY tt-aux.nome-transp:
   
          FIND item WHERE item.it-codigo = tt-aux.it-codigo NO-LOCK.
          
          IF FIRST-OF(tt-aux.campo-1) THEN 
             PUT "Item: " item.it-codigo
                 "Referencia: " tt-aux.cod-refer
                 " " item.descricao-1.
   
          FIND ped-item WHERE
               ped-item.it-codigo    = tt-aux.it-codigo AND 
               ped-item.cod-refer    = tt-aux.cod-refer AND
               ped-item.nr-pedcli    = tt-aux.nr-pedcli AND 
               ped-item.nr-sequencia = tt-aux.nr-sequencia AND 
               ped-item.nome-abrev   = tt-aux.nome-abrev NO-LOCK.
   
          FIND ped-item-ext WHERE
               ped-item-ext.nome-abrev   = ped-item.nome-abrev and
               ped-item-ext.nr-pedcli    = ped-item.nr-pedcli and
               ped-item-ext.nr-sequencia = ped-item.nr-sequencia and
               ped-item-ext.it-codigo    = ped-item.it-codigo and
               ped-item-ext.cod-refer    = ped-item.cod-refer NO-LOCK NO-ERROR.
   
          FIND ped-venda WHERE
               ped-venda.nome-abrev = ped-item.nome-abrev and
               ped-venda.nr-pedcli  = ped-item.nr-pedcli no-lock.
   
          FIND item WHERE item.it-codigo = ped-item.it-codigo NO-LOCK.
   
          ASSIGN c-acondicionamento = IF AVAIL ped-item-ext
                                      THEN ped-item-ext.acondicionamento
                                      ELSE ""
                 de-saldo-pedido    = ped-item.qt-pedida - ped-item.qt-atendida -
                                      tt-aux.quantidade + ped-item.qt-devolvida -
                                      ped-item.qt-pendente - ped-item.qt-alocada.
   
          ASSIGN c-variante = tt-aux.campo-2
                 c-cidade   = ped-venda.cidade
                 c-uf       = ped-venda.estado.
   
          IF ped-venda.cod-entrega <> "" THEN DO:
             FIND loc-entr WHERE
                  loc-entr.cod-entrega = ped-venda.cod-entrega AND
                  loc-entr.nome-abrev  = ped-item.nome-abrev NO-LOCK NO-ERROR.
   
             IF AVAIL loc-entr THEN
                ASSIGN c-cidade = loc-entr.cidade
                       c-uf     = loc-entr.estado.
          END.
   
          IF (ped-venda.cod-entrega = "" AND ped-venda.local-entreg = "") OR
              NOT AVAIL loc-entr THEN DO:
              FIND FIRST emitente WHERE
                         emitente.nome-abrev =  ped-item.nome-abrev AND
                         emitente.identific  <> 2 NO-LOCK.
              ASSIGN c-cidade = emitente.cidade
                     c-uf     = emitente.estado.
          END.
   
          FIND FIRST emitente WHERE
                     emitente.nome-abrev = tt-aux.nome-abrev AND
                     emitente.identific <> 2 NO-LOCK.
   
          ASSIGN c-tipo = (IF emitente.pais = "brasil" THEN "" ELSE "EXT").
   
          IF emitente.nome-abrev BEGINS "lundgren" OR
             emitente.nome-abrev BEGINS "arthurlund" THEN
             ASSIGN c-tipo = "LUND".
   
          IF ped-venda.cod-cond-pag = 1 OR ped-venda.cod-cond-pag = 2 THEN
             ASSIGN c-credito = "AV".
          ELSE
             IF ped-venda.cod-sit-aval = 2 OR ped-venda.cod-sit-aval = 3 THEN
                ASSIGN c-credito = "Ok".
             ELSE
                ASSIGN c-credito = "Nao".
   
          DISPLAY tt-aux.nr-pedcli
                  c-credito
                  tt-aux.nr-sequencia
                  c-acondicionamento
                  de-saldo-pedido
                  c-saldo
                  tt-aux.nome-abrev
                  tt-aux.nome-transp
                  c-variante
                  c-cidade
                  c-uf
                  c-tipo
                  emitente.cod-emit
                  WITH FRAME f-detalhe.
          DOWN WITH FRAME f-detalhe.
       
          /*--- Busca Qtd.aberta e Reservada ---*/
   
          IF LAST-OF(tt-aux.nr-pedcli) THEN DO:
             ASSIGN de-qtd-tot-abe = 0
                    de-qtd-tot-res = 0.
   
             FOR EACH ped-item WHERE
                      ped-item.nome-abrev = tt-aux.nome-abrev AND
                      ped-item.nr-pedcli  = tt-aux.nr-pedcli  AND
                      (ped-item.cod-sit-item < 3 OR ped-item.cod-sit-item = 5)
                      NO-LOCK:
                 ASSIGN de-qtd-tot-abe = de-qtd-tot-abe + ped-item.qt-pedida -
                                         ped-item.qt-atendida + ped-item.qt-devolvida -
                                         ped-item.qt-pendente - ped-item.qt-alocada.
   
                 FIND ped-item-res WHERE
                      ped-item-res.nome-abrev   = ped-item.nome-abrev AND
                      ped-item-res.nr-pedcli    = ped-item.nr-pedcli AND
                      ped-item-res.nr-sequencia = ped-item.nr-sequencia NO-LOCK NO-ERROR.
   
                 IF AVAIL ped-item-res THEN
                    ASSIGN de-qtd-tot-res = de-qtd-tot-res + ped-item-res.qt-pedida.
             END.
   
             /* ---------------------------------------- */
             PUT " Amostra: " ped-venda.tp-pedido
                 "Volume: _______________      Forma: _______________" AT 82
                 SKIP
                 " Observacoes: " SUBSTR(ped-venda.cond-espec,1,100) FORMAT "x(100)"
                 SKIP
                 " Qtd.Tot.Aberta/Reservada: " 
                   de-qtd-tot-abe "/"
                   de-qtd-tot-res
                 "     Data Entrega: " ped-venda.dt-entrega FORMAT "99/99/9999" 
                 SKIP.
          END.
          IF tt-param.l-queb-desenho AND LAST-OF(tt-aux.campo-1) THEN PAGE.
      END.
   END.
END.
ELSE DO.
    FOR EACH tt-aux USE-INDEX pedido
         BREAK BY tt-aux.it-codigo
               BY tt-aux.cod-refer
               BY tt-aux.dt-entrega
               BY tt-aux.nr-pedcli.

        FIND item WHERE item.it-codigo = tt-aux.it-codigo NO-LOCK.

        IF FIRST-OF(tt-aux.cod-refer) THEN
           PUT "#"
               tt-aux.it-codigo FORMAT "X(6)"
               tt-aux.cod-refer FORMAT "X(7)"
               tt-aux.lote
               item.desc-item 
               SKIP.

        FIND ped-item WHERE
             ped-item.it-codigo    = tt-aux.it-codigo AND 
             ped-item.cod-refer    = tt-aux.cod-refer AND
             ped-item.nr-pedcli    = tt-aux.nr-pedcli AND 
             ped-item.nr-sequencia = tt-aux.nr-sequencia AND 
             ped-item.nome-abrev   = tt-aux.nome-abrev NO-LOCK.

        FIND ped-item-ext WHERE
             ped-item-ext.nome-abrev   = ped-item.nome-abrev and
             ped-item-ext.nr-pedcli    = ped-item.nr-pedcli and
             ped-item-ext.nr-sequencia = ped-item.nr-sequencia and
             ped-item-ext.it-codigo    = ped-item.it-codigo and
             ped-item-ext.cod-refer    = ped-item.cod-refer NO-LOCK NO-ERROR.

        FIND ped-venda WHERE
             ped-venda.nome-abrev = ped-item.nome-abrev and
             ped-venda.nr-pedcli  = ped-item.nr-pedcli no-lock.

        FIND item WHERE item.it-codigo = ped-item.it-codigo NO-LOCK.

        ASSIGN de-saldo-pedido = ped-item.qt-pedida - ped-item.qt-atendida -
                                 tt-aux.quantidade + ped-item.qt-devolvida -
                                 ped-item.qt-pendente - ped-item.qt-alocada.

        ASSIGN c-variante = tt-aux.campo-2
               c-cidade   = ped-venda.cidade
               c-uf       = ped-venda.estado.

        IF ped-venda.cod-entrega <> "" THEN DO:
           FIND loc-entr WHERE
                loc-entr.cod-entrega = ped-venda.cod-entrega AND
                loc-entr.nome-abrev  = ped-item.nome-abrev NO-LOCK NO-ERROR.

           IF AVAIL loc-entr THEN
              ASSIGN c-cidade = loc-entr.cidade
                     c-uf     = loc-entr.estado.
        END.

        IF (ped-venda.cod-entrega = "" AND ped-venda.local-entreg = "") OR
            NOT AVAIL loc-entr THEN DO:
            FIND FIRST emitente WHERE
                       emitente.nome-abrev =  ped-item.nome-abrev AND
                       emitente.identific  <> 2 NO-LOCK.
            ASSIGN c-cidade = emitente.cidade
                   c-uf     = emitente.estado.
        END.
        ASSIGN c-nome-abrev = fn-free-accent(tt-aux.nome-abrev).

        IF INDEX(tt-aux.nome-abrev,'&') > 0 THEN
           OVERLAY(c-nome-abrev,INDEX(tt-aux.nome-abrev,'&',1)) = '&'.

        PUT "*"
            c-nome-abrev FORMAT "x(12)"
            tt-aux.nr-pedcli
            c-cidade
            c-uf
            tt-aux.nome-transp
            de-saldo-pedido FORMAT "999999.999"
            SKIP.

        FIND coletor WHERE
             coletor.id           = tt-param.nr-coletor AND
             coletor.nome-abrev   = tt-aux.nome-abrev AND
             coletor.nr-pedcli    = tt-aux.nr-pedcli AND
             coletor.it-codigo    = tt-aux.it-codigo AND
             coletor.cod-refer    = tt-aux.cod-refer AND
             coletor.nr-sequencia = tt-aux.nr-sequencia AND
             coletor.nr-lote      = tt-aux.lote
             NO-ERROR.

        IF NOT AVAIL coletor THEN DO.
           CREATE coletor.
           ASSIGN coletor.id           = tt-param.nr-coletor 
                  coletor.nr-pedcli    = tt-aux.nr-pedcli
                  coletor.nome-abrev   = tt-aux.nome-abrev
                  coletor.it-codigo    = tt-aux.it-codigo
                  coletor.cod-refer    = tt-aux.cod-refer
                  coletor.nr-sequencia = tt-aux.nr-sequencia
                  coletor.nr-lote      = tt-aux.lote.
        END.
        ASSIGN coletor.qt-pedida    = de-saldo-pedido.
    END.
END.

IF tt-param.impr-param THEN DO.
   PAGE.
   PUT "*****----- PAR¶METROS ------*****"
       SKIP(1).

   ASSIGN tt-param.c-saldo-estoq = IF tt-param.c-saldo-estoq = "P"
                                   THEN "Positivo"
                                   ELSE IF tt-param.c-saldo-estoq = "N"
                                        THEN "Negativo"
                                        ELSE "Ambos".
   DISPLAY tt-param.c-pedido-ini       
           tt-param.c-pedido-fim       
           tt-param.c-item-ini         
           tt-param.c-item-fim 
           tt-param.c-ref-ini         
           tt-param.c-ref-fim 
           tt-param.da-entr-ini        
           tt-param.da-entr-fim        
           tt-param.da-impl-ini        
           tt-param.da-impl-fim        
           tt-param.c-acond-ini        
           tt-param.c-acond-fim 
           tt-param.corte-comerc-ini 
           tt-param.corte-comerc-fim 
           tt-param.c-cod-refer1 
           tt-param.c-cod-refer2 
           tt-param.c-cod-refer3 
           tt-param.c-cod-refer4 
           tt-param.c-cod-refer5 
           tt-param.c-cod-refer6 
           tt-param.c-cod-refer7 
           tt-param.c-cod-refer8 
           tt-param.c-cod-refer9 
           tt-param.c-cod-refer10
           tt-param.l-sit-aberto       
           tt-param.l-sit-parcial      
           tt-param.l-sit-pendentes    
           tt-param.l-sit-suspensos    
           tt-param.l-sit-cancelados   
           tt-param.l-sit-so-aprovados 
           tt-param.l-res-nao          
           tt-param.l-res-sim          
           tt-param.l-res-parc         
           tt-param.l-res-aceita       
           tt-param.c-saldo-estoq
           tt-param.l-queb-desenho     
           WITH FRAME f-parlis.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
RUN pi-finalizar IN h-acomp.
RETURN "OK":U.

