 /* include de controle de vers∆o */
{include/i-prgvrs.i ESSP0105RP 2.04.00.001}

DEF VAR c-seg-usuario AS CHAR NO-UNDO.

DEFINE TEMP-TABLE tt-param
       FIELD destino          AS INTEGER 
       FIELD arq-destino      AS CHAR
       FIELD arq-entrada      AS CHAR 
       FIELD todos            AS INTEGER 
       FIELD usuario          AS CHAR
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       FIELD cod-estabel      AS CHAR
       FIELD tg-pedido        AS LOG 
       FIELD tg-item          AS LOG 
       FIELD tg-avulsa        AS LOG 
       FIELD tg-inventario    AS LOG
       FIELD tg-cancel-res    AS LOG
       FIELD tg-reimp-etq     AS LOG
       FIELD tg-localiz       AS LOG
       FIELD tg-conf-localiz  AS LOG
       FIELD tg-imp-sit-etq   AS LOG
       FIELD tg-transf-etq    AS LOG
       FIELD nr-pedcli        LIKE ped-venda.nr-pedcli
       FIELD nome-abrev       LIKE ped-venda.nome-abrev
       FIELD nr-coletor       AS INT
       FIELD nro-docto        AS INT
       FIELD dt-invent        AS DATE.

DEF TEMP-TABLE tt-raw-digita
   	FIELD raw-digita	      AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.

DEF TEMP-TABLE tt-ped-item LIKE ped-item 
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE RowErrors NO-UNDO
       FIELD ErrorSequence    AS INTEGER
       FIELD ErrorNumber      AS INTEGER
       FIELD ErrorDescription AS CHARACTER
       FIELD ErrorParameters  AS CHARACTER
       FIELD ErrorType        AS CHARACTER
       FIELD ErrorHelp        AS CHARACTER
       FIELD ErrorSubType     AS CHARACTER.

DEF VAR h-bodi154 AS HANDLE.
DEF VAR h-bodi154com AS HANDLE.

DEFINE TEMP-TABLE tt-etq-lidas LIKE ob-etiqueta
       FIELD localiz-ant LIKE ob-etiqueta.localizacao.

DEFINE TEMP-TABLE tt-imp-invent LIKE ob-etiqueta
       FIELD local-invent LIKE ob-etiqueta.localizacao
       FIELD inventariada AS CHAR
       FIELD cadastrada   AS CHAR.

DEFINE TEMP-TABLE tt-imp-ped
       FIELD id          AS   INT
       FIELD nome-abrev  LIKE ped-venda.nome-abrev
       FIELD nr-pedcli   LIKE ped-venda.nr-pedcli
       FIELD volume-ini  LIKE ped-item-res.volume-ini
       FIELD volume-fim  LIKE ped-item-res.volume-fim
       FIELD nome-transp LIKE ped-venda.nome-transp
       INDEX indice1 IS PRIMARY UNIQUE id.

DEFINE TEMP-TABLE tt-imp-item
       FIELD id           AS   INT
       FIELD it-codigo    LIKE ped-item.it-codigo
       FIELD cod-refer    LIKE ped-item.cod-refer
       FIELD nr-lote      AS   INT
       FIELD quantidade   AS   DEC FORMAT ">>9.9"
       FIELD volume-it    LIKE ped-item-res.volume-ini
       FIELD nr-ob        LIKE ob-etiqueta.nr-ob
       FIELD sequencia    LIKE ob-etiqueta.nr-sequencia
       FIELD lote         AS   CHAR
       FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
       FIELD erro         AS   CHAR FORMAT "x(50)"
       INDEX indice1 IS PRIMARY id
       INDEX indice2 volume-it.

DEF TEMP-TABLE tt-localiz 
    FIELD cod-localiz LIKE ob-etiqueta.localiz.

DEF BUFFER b-ped-item-ext FOR ped-item-ext.
DEF BUFFER b-imp-item FOR tt-imp-item.    

DEF STREAM s-imp.
DEF STREAM s-etq.
DEF VAR h-acomp       AS HANDLE NO-UNDO.
DEF VAR c-linha       AS CHAR FORMAT "x(70)" NO-UNDO.
DEF VAR c-procedure   AS CHAR.
DEF VAR i-ind         AS INT.
DEF VAR l-tem-reserva AS LOG INIT NO.
DEF VAR nr-ult-seq    LIKE ped-item.nr-sequencia.

DEF VAR i-sit-aval    LIKE ped-venda.cod-sit-aval.
DEF VAR i-pc-lidas    AS INT.
DEF VAR i-tot-lidas   AS INT.
DEF VAR i-cod-mess    LIKE ped-venda.cod-message-alert.      
DEF VAR da-dt-mess    LIKE ped-venda.dt-mensagem.
DEF VAR c-desc-for    LIKE ped-venda.desc-forc-cr.           
DEF VAR l-dsp-fat     LIKE ped-venda.dsp-pre-fat.
DEF VAR de-qt-pedida  AS DEC.
DEF VAR i-tp-embal    AS INT.


/* Inventario */
DEF VAR i-docto  AS INT.
DEF VAR i-nr-seq AS INT.
DEF VAR i-qt-pecas AS INT.
DEF VAR c-obs      AS CHAR.
DEF VAR c-situacao AS CHAR FORMAT "x(20)".

/* Separaá∆o por item */
DEF VAR c-it-codigo LIKE ped-item.it-codigo.
DEF VAR c-cod-refer LIKE ped-item.cod-refer.

DEF VAR i-num-bar AS INT.
DEF VAR i-col     AS INT.
DEF VAR c-lin     AS CHAR.
DEF VAR c-nota    AS CHAR FORMAT "x(9)".


 /* Variaveis da Rotina de Impress∆o */
DEFINE VAR c-saida        AS CHAR.
DEFINE VAR c-inventariada AS CHAR.
DEFINE VAR c-etiqueta     AS CHAR.
DEFINE VAR de-qtd-lidas   AS DEC.
DEFINE VAR de-tot-lidas   AS DEC.
DEFINE VAR i-saida        AS INT.
DEFINE VAR i-num-copias   AS INT.
DEFINE VAR i-Lin          AS INT.
DEFINE VAR i-pag          AS INT.
DEFINE VAR i-ct           AS INT.
DEFINE VAR l-ok           AS LOG.
DEFINE VAR h-prog AS HANDLE NO-UNDO.

DEF BUFFER empresa FOR mgcad.empresa.
DEF BUFFER b-etiqueta FOR ob-etiqueta.
DEF VAR i-num-etiqueta LIKE ob-etiqueta.num-etiqueta.

{esinc/sz-pcl.i}

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

{include/i-rpout.i &STREAM="stream str-rp" &TOFILE=tt-param.arq-destino}

/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
{include/i-rpcab.i &STREAM="str-rp"}

/* bloco principal do programa */
FIND FIRST param-global NO-LOCK NO-ERROR.
FIND first empresa
     WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 

ASSIGN c-programa = "ESSP0105RP"
       c-versao	  = "2.04"
       c-revisao  = ".00.001"
       c-empresa  = (IF AVAIL empresa THEN empresa.razao-social ELSE "").

{utp/ut-liter.i PEDIDOS * r}
ASSIGN c-sistema = TRIM(RETURN-VALUE).

{utp/ut-liter.i Importa_Dados_do_Coletor * r}
ASSIGN c-titulo-relat = TRIM(RETURN-VALUE).

VIEW STREAM str-rp FRAME f-cabec.
VIEW STREAM str-rp FRAME f-rodape.

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.

{utp/ut-liter.i Importando_Dados_Coletor *}
RUN pi-inicializar IN h-acomp (INPUT RETURN-VALUE).

/* bloco principal do programa */
IF SEARCH(tt-param.arq-entrada) = ? THEN DO.
   MESSAGE "N∆o foi encontrado Arquivo " tt-param.arq-entrada " para Importar..."
           VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

FIND FIRST ob-param NO-LOCK NO-ERROR.
IF NOT AVAIL ob-param THEN DO.
   MESSAGE "ParÉmetros n∆o Cadastrados...." VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

{utp/ut-liter.i Importando_Reservas *}
RUN pi-inicializar in h-acomp (input RETURN-VALUE).

ASSIGN i-ind = 0 
       i-qt-pecas = 0
       i-pc-lidas = 0 
       c-it-codigo = ""
       c-cod-refer = ""
       c-procedure = IF tt-param.tg-transf-etq
                     THEN "pi-transf-etq"
                     ELSE IF tt-param.tg-item 
                          THEN "pi-imp-item"
                          ELSE IF tt-param.tg-conf-localiz
                               THEN "pi-conf-localiz"
                               ELSE IF tt-param.tg-inventario 
                                    THEN "pi-imp-inventario"
                                    ELSE IF tt-param.tg-cancel-res
                                         THEN "pi-imp-cancel-res" 
                                         ELSE IF tt-param.tg-reimp-etq 
                                              THEN "pi-imp-etq"
                                              ELSE IF tt-param.tg-localiz 
                                                   THEN "pi-imp-localiz"
                                                   ELSE ?.


INPUT STREAM s-imp FROM VALUE(tt-param.arq-entrada) NO-ECHO.
REPEAT ON STOP UNDO, LEAVE:
    ASSIGN c-linha = "".
    IMPORT STREAM s-imp DELIMITER "$&#" c-linha.
    IF c-linha = "" THEN NEXT.
	RUN pi-acompanhar IN h-acomp (INPUT c-linha).

    RUN VALUE(c-procedure). 
END.
INPUT STREAM s-imp CLOSE.

{utp/ut-liter.i Gravando_Reservas *}
RUN pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH tt-imp-item NO-LOCK. /* daf */
    /* N∆o valida pedidos para Cancel de Reserevas, Invent†rio e Reimpressao Etiquetas*/
    IF NOT tt-param.tg-cancel-res AND
       NOT tt-param.tg-inventario AND 
       NOT tt-param.tg-localiz AND
       NOT tt-param.tg-reimp-etq THEN DO.

       FIND tt-imp-ped OF tt-imp-item NO-LOCK NO-ERROR.   /* daf */

       IF NOT AVAIL tt-imp-ped THEN DO.
          ASSIGN tt-imp-item.erro = "Erro: Itens enviadas pelo Coletor est† sem Pedido...".
          NEXT.
       END.

       FIND ped-venda WHERE
            ped-venda.nome-abrev = tt-imp-ped.nome-abrev AND
            ped-venda.nr-pedcli = tt-imp-ped.nr-pedcli NO-LOCK NO-ERROR.

       IF NOT AVAIL ped-venda THEN DO.
          ASSIGN tt-imp-item.erro = "Erro: Pedido de Venda n∆o Cadastrado...".
          NEXT.
       END.
       ASSIGN tt-imp-ped.nome-transp = ped-venda.nome-transp.

       IF ped-venda.cod-sit-ped = 3 OR ped-venda.cod-sit-ped = 6 THEN DO.
          ASSIGN tt-imp-item.erro = 'Erro: Situacao do Pedido n∆o permite Reservas...'.
          NEXT.
       END.

       FIND FIRST ped-item OF ped-venda WHERE
                  ped-item.it-codigo = tt-imp-item.it-codigo AND
                  ped-item.cod-refer = tt-imp-item.cod-refer AND
                  ped-item.cod-sit-item <> 3 AND
                  ped-item.cod-sit-item <> 6 NO-LOCK NO-ERROR.

       /* Se for avulsa n∆o valida item do pedido */
       IF NOT AVAIL ped-item AND 
          NOT tt-param.tg-avulsa THEN DO.
          ASSIGN tt-imp-item.erro = 'Erro: Situacao do Item n∆o permite Reservas...'.
          NEXT.
       END.
    END.

    FIND ob-etiqueta WHERE
         ob-etiqueta.nr-ob = tt-imp-item.nr-ob AND
         ob-etiqueta.nr-seq = tt-imp-item.sequencia
         NO-LOCK NO-ERROR.

    IF AMBIGUOUS ob-etiqueta THEN DO.
       ASSIGN tt-imp-item.erro = "Erro: Etiqueta est† DUPLICADA no Sistema...".
       NEXT.
    END.

    FIND ob-etiqueta WHERE
         ob-etiqueta.nr-ob = tt-imp-item.nr-ob AND
         ob-etiqueta.nr-seq = tt-imp-item.sequencia NO-LOCK NO-ERROR.

    IF NOT AVAIL ob-etiqueta THEN DO.
       ASSIGN tt-imp-item.erro = "Erro: Etiqueta n∆o Cadastrada no Sistema...".
       NEXT.
    END.
    ASSIGN tt-imp-item.num-etiqueta = ob-etiqueta.num-etiqueta.

    IF ob-etiqueta.num-etiqueta = 0 THEN DO.
       ASSIGN tt-imp-item.erro = 'Erro: Etiqueta sem N£mero de Identificaá∆o, deve ser Emitida no novo Projeto'.
       NEXT.
    END.

    IF tt-imp-item.it-codigo <> ob-etiqueta.it-codigo THEN DO.
       ASSIGN tt-imp-item.erro = 'Erro: Item enviado pelo Coletor difere da Etiqueta...'.
       NEXT.
    END.

    IF INT(tt-imp-item.cod-refer) <> INT(ob-etiqueta.cod-refer) THEN DO.
       ASSIGN tt-imp-item.erro = 'Erro: Referencia enviada pelo Coletor difere da Etiqueta...'.
       NEXT.
    END.

    IF tt-imp-item.quantidade <> ob-etiqueta.quantidade THEN DO.
       ASSIGN tt-imp-item.erro = 'Erro: Quantidade enviada pelo Coletor difere da Etiqueta...'.
       NEXT.
    END.

    IF tt-imp-item.lote <> ob-etiqueta.nr-lote THEN DO.
       ASSIGN tt-imp-item.erro = 'Erro: Lote enviado pelo Coletor difere da Etiqueta...'.
       NEXT.
    END.

    IF ob-etiqueta.situacao <> 3 THEN DO.
       ASSIGN tt-imp-item.erro = "Erro: Situacao da Etiqueta n∆o permite Reserva...".
       NEXT.
    END.

    IF (SUBSTR(tt-imp-item.lote,1,1) = "R" AND tt-imp-item.quantidade > ob-param.mt-max-rolo) OR
       (SUBSTR(tt-imp-item.lote,1,1) = "P" AND tt-imp-item.quantidade > ob-param.mt-max-peca) THEN DO.
       ASSIGN tt-imp-item.erro = "Erro: Metragem Lida pelo Coletor Ç superior a Permitida nos ParÉmetros...".
       NEXT.
    END.

    FIND item WHERE
         item.it-codigo = tt-imp-item.it-codigo NO-LOCK NO-ERROR.
    IF NOT AVAIL item THEN DO.
       ASSIGN tt-imp-item.erro = "Erro: Item enviado pelo Coletor n∆o est† Cadastrado".
       NEXT.
    END.

    IF item.tipo-con-est = 4 THEN DO.
       FIND ref-item WHERE
            ref-item.it-codigo = tt-imp-item.it-codigo AND
            ref-item.cod-refer = tt-imp-item.cod-refer
            NO-LOCK NO-ERROR.
       IF NOT AVAIL ref-item THEN DO.
          ASSIGN tt-imp-item.erro = "Erro: Referància/Item enviados pelo Coletor n∆o est∆o Cadastrados".
          NEXT.
       END.
    END.
END.

IF tt-param.tg-localiz OR tt-param.tg-conf-localiz THEN DO:
   ASSIGN i-col = 1
          c-lin = "".

   PUT STREAM str-rp "Etiqueta     Item      Referància    Quantidade  Doca     Doca Anterior" SKIP.
   PUT STREAM str-rp "-----------  --------  ----------  ------------  -------  -------------" SKIP.

   FOR EACH tt-etq-lidas NO-LOCK
       BREAK BY tt-etq-lidas.it-codigo
             BY tt-etq-lidas.cod-refer
             BY tt-etq-lidas.num-etiqueta.
    
       ACCUMULATE tt-etq-lidas.num-etiqueta (COUNT BY tt-etq-lidas.it-codigo).
       ACCUMULATE tt-etq-lidas.quantidade (TOTAL BY tt-etq-lidas.it-codigo).
    
       ACCUMULATE tt-etq-lidas.num-etiqueta (COUNT BY tt-etq-lidas.cod-refer).
       ACCUMULATE tt-etq-lidas.quantidade (TOTAL BY tt-etq-lidas.cod-refer).
    
       PUT STREAM str-rp
           tt-etq-lidas.num-etiqueta  AT 1
           tt-etq-lidas.it-codigo     AT 14  FORMAT "x(8)" 
           tt-etq-lidas.cod-refer     AT 24
           tt-etq-lidas.quantidade    AT 38
           tt-etq-lidas.localizacao   AT 50
           tt-etq-lidas.localiz-ant   AT 59
           SKIP.
    
       ASSIGN i-qt-pecas = i-qt-pecas + 1.
    
       IF LAST-OF(tt-etq-lidas.cod-refer) THEN DO.
          IF tt-etq-lidas.cod-refer <> "" THEN DO.
             PUT STREAM str-rp
                 "------------" AT 36
                 SKIP.

             PUT STREAM str-rp
                 "Total da Ref....." AT 14
                 (ACCUM TOTAL BY tt-etq-lidas.cod-refer tt-etq-lidas.quantidade) AT 38
                 "  Peáas Lidas: "     AT 48
                 STRING((ACCUM COUNT BY tt-etq-lidas.cod-refer tt-etq-lidas.num-etiqueta))
                 SKIP(1).
          END.
          PUT STREAM str-rp SKIP.
       END.
        
       IF LAST-OF(tt-etq-lidas.it-codigo) AND
          tt-etq-lidas.it-codigo <> '' THEN DO.

          PUT STREAM str-rp
              "Total do Item...." AT 14
              (ACCUM TOTAL BY tt-etq-lidas.it-codigo tt-etq-lidas.quantidade) AT 38
              "  Peáas Lidas: "  AT 48
              STRING((ACCUM COUNT BY tt-etq-lidas.it-codigo tt-etq-lidas.num-etiqueta))
              SKIP(2).
       END.
   END.
    
   PUT STREAM str-rp SKIP.
   PUT STREAM str-rp
       "Total Geral......" AT 14
       (ACCUM TOTAL tt-etq-lidas.quantidade) AT 38
       "  Peáas Lidas: "  AT 48
       STRING((ACCUM COUNT tt-etq-lidas.num-etiqueta))
       SKIP.

   IF tt-param.tg-conf-localiz THEN DO.
      PUT STREAM str-rp SKIP(5).
      PUT STREAM str-rp 
           "PEÄAS NA DOCA N«O LIDAS"
            SKIP.
    
      FOR EACH tt-localiz NO-LOCK.
          FOR EACH ob-etiqueta WHERE
                   ob-etiqueta.cod-estabel = tt-param.cod-estabel AND
                   ob-etiqueta.localiz = tt-localiz.cod-localiz SHARE-LOCK.
    
              IF ob-etiqueta.situacao <> 3 AND
                 ob-etiqueta.situacao <> 4 THEN NEXT. 
    
              FIND tt-etq-lidas WHERE
                   tt-etq-lidas.cod-estabel = tt-param.cod-estabel AND
                   tt-etq-lidas.num-etiqueta = ob-etiqueta.num-etiqueta 
                   NO-LOCK NO-ERROR.
              IF NOT AVAIL tt-etq-lidas THEN DO.
                 PUT STREAM str-rp
                     ob-etiqueta.num-etiqueta  AT 1
                     ob-etiqueta.it-codigo     AT 14  FORMAT "x(8)" 
                     ob-etiqueta.cod-refer     AT 24
                     ob-etiqueta.quantidade    AT 38
                     ob-etiqueta.localiz       AT 50
                     SKIP.
    
                 ASSIGN ob-etiqueta.localiz = ''.
              END.
          END.
      END.
   END.
END.


IF tt-param.tg-inventario THEN DO.
   PUT STREAM str-rp 
       SKIP(2)
       "TOTAL DE PECAS: " i-qt-pecas
       SKIP(5)
       FILL("-",40) AT 30 FORMAT "x(40)" SKIP
       "INVENTARIANTE" AT 45.
END.

IF tt-param.tg-pedido OR
   tt-param.tg-item OR
   tt-param.tg-avulsa THEN
   RUN pi-ver-volumes.

{include/i-rpclo.i &STREAM="stream str-rp"}
RUN pi-finalizar in h-acomp.
RETURN "Ok":U.


/* Procedures Diversas */
PROCEDURE pi-imp-inventario.
    ASSIGN i-qt-pecas = i-qt-pecas + 1.

    RUN pi-grava-inventario (INPUT INT(SUBSTR(c-linha,7,9)),
                             INPUT SUBSTR(c-linha,1,6)).
END PROCEDURE.


PROCEDURE pi-imp-localiz.
    FIND ob-etiqueta WHERE
         ob-etiqueta.cod-estabel = tt-param.cod-estabel AND
         ob-etiqueta.num-etiqueta = INT(SUBSTR(c-linha,7,9))
         SHARE-LOCK NO-ERROR.

    ASSIGN i-qt-pecas = i-qt-pecas + 1.

    IF AVAIL ob-etiqueta THEN DO:
       CREATE tt-etq-lidas.
       BUFFER-COPY ob-etiqueta TO tt-etq-lidas
            ASSIGN tt-etq-lidas.localiz-ant = ob-etiqueta.localizacao.

       ASSIGN ob-etiqueta.localizacao = SUBSTR(c-linha,1,6)
              tt-etq-lidas.localizacao = SUBSTR(c-linha,1,6).

       ASSIGN SUBSTR(ob-etiqueta.char-1,50,200) = "ROLO LOCALIZAÄ«O DOCA :" + SUBSTR(c-linha,1,6) + "USUARIO :" + tt-param.usuario + "DATA :" + STRING(TODAY) + "HORA :" + STRING(TIME,"HH:MM:SS"). 

       /* Volta Etiqueta para Estoque */
       IF ob-etiqueta.situacao <> 3 AND
          ob-etiqueta.situacao <> 4 THEN
          ASSIGN ob-etiqueta.situacao = 3.  

       ASSIGN i-pc-lidas = i-pc-lidas + 1.
    END.
    ELSE DO:
       PUT STREAM str-rp
           SKIP
           "DOCA: " (SUBSTR(c-linha,1,6)) FORMAT "999/999"
           "  ETIQUETA: " SUBSTR(c-linha,7,9) FORMAT "x(9)" " N∆o Cadastrada..."
           SKIP.
    END.
END PROCEDURE.

PROCEDURE pi-conf-localiz.
    FIND ob-etiqueta WHERE
         ob-etiqueta.cod-estabel = tt-param.cod-estabel AND
         ob-etiqueta.num-etiqueta = INT(SUBSTR(c-linha,7,9))
         SHARE-LOCK NO-ERROR.

    ASSIGN i-qt-pecas = i-qt-pecas + 1.

    IF AVAIL ob-etiqueta THEN DO:

        CREATE tt-etq-lidas.
        BUFFER-COPY ob-etiqueta TO tt-etq-lidas
               ASSIGN tt-etq-lidas.localiz-ant = ob-etiqueta.localizacao.

       /* Altera a Localizaá∆o da Etiqueta */
       ASSIGN ob-etiqueta.localizacao = SUBSTR(c-linha,1,6)
              tt-etq-lidas.localizacao = SUBSTR(c-linha,1,6).

       ASSIGN SUBSTR(ob-etiqueta.char-1,50,200) = "ROLO LOCALIZAÄ«O DOCA :" + SUBSTR(c-linha,1,6) + "USUARIO :" + tt-param.usuario + "DATA :" + STRING(TODAY) + "HORA :" + STRING(TIME,"HH:MM:SS"). 

       /* Volta Etiqueta para Estoque */
       IF ob-etiqueta.situacao <> 3 AND
          ob-etiqueta.situacao <> 4 THEN
          ASSIGN ob-etiqueta.situacao = 3.  

       ASSIGN i-pc-lidas = i-pc-lidas + 1.
    END.
    ELSE DO:
       PUT STREAM str-rp
           SKIP
           "DOCA: " (SUBSTR(c-linha,1,6)) FORMAT "999/999"
           "  ETIQUETA: " SUBSTR(c-linha,7,9) FORMAT "x(9)" " N∆o Cadastrada..."
           SKIP.
    END.

    FIND tt-localiz WHERE
         tt-localiz.cod-localiz = SUBSTR(c-linha,1,6) NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-localiz THEN DO.
       CREATE tt-localiz.
       ASSIGN tt-localiz.cod-localiz = SUBSTR(c-linha,1,6).
    END.

END PROCEDURE.

PROCEDURE pi-imp-etq.
    FIND ob-etiqueta WHERE
         ob-etiqueta.cod-estabel  = tt-param.cod-estabel AND
         ob-etiqueta.num-etiqueta = INT(SUBSTR(c-linha,7,9))
         NO-LOCK NO-ERROR.

    IF NOT AVAIL ob-etiqueta THEN DO.
       PUT STREAM str-rp
            "Erro: Etiqueta N∆o Cadastrada no Sistema..." SKIP
            " Num Etiqueta: "  INT(SUBSTR(c-linha,7,9))
            SKIP(1).
        NEXT.
    END.
    RUN esapi/imp-etq-estoque.p (INPUT ob-etiqueta.cod-estabel,
                                 INPUT ob-etiqueta.num-etiqueta,
                                 INPUT NO).
END PROCEDURE.

PROCEDURE pi-grava-inventario.

    DEF INPUT PARAMETER p-num-etiqueta LIKE ob-etiqueta.num-etiqueta.
    DEF INPUT PARAMETER p-localiz LIKE ob-etiqueta.localizacao.

    FIND ob-etiqueta WHERE
         ob-etiqueta.cod-estabel  = tt-param.cod-estabel AND
         ob-etiqueta.num-etiqueta = p-num-etiqueta
         SHARE-LOCK NO-ERROR.

    IF NOT AVAIL ob-etiqueta THEN DO.
       PUT STREAM str-rp
           " Num Etiqueta: " p-num-etiqueta
           " Localizacao: "  p-localiz FORMAT "999/999"
           " ****** NAO CADASTRADA ******** "
           c-linha FORMAT "x(50)"
           SKIP.
       NEXT.
    END.

    FIND inv-acab WHERE
         inv-acab.cod-estabel  = tt-param.cod-estabel AND
         inv-acab.data-invent  = tt-param.dt-invent   AND
         inv-acab.num-etiqueta = ob-etiqueta.num-etiqueta NO-ERROR.
    IF AVAIL inv-acab THEN DO.
       PUT STREAM str-rp
           " Estabelecimento: " tt-param.cod-estabel
           " Num Etiqueta: " p-num-etiqueta
           " Localizacao: "  p-localiz FORMAT "999/999"
           " ****** Jµ INVENTARIADA ******** "
           c-linha FORMAT "x(70)"
           SKIP.
       NEXT.
    END.

    {esinc/i-dsrb.i ob-etiqueta.situacao ob-etiqueta.situacao c-situacao}.

    FIND corte-comerc WHERE
         corte-comerc.codigo = ob-etiqueta.corte-comerc NO-LOCK NO-ERROR.

    PUT STREAM str-rp
        ob-etiqueta.num-etiqueta
        ob-etiqueta.quantidade FORMAT ">>9.99"
        " "
        p-localiz FORMAT "999/999"
        " "
        ob-etiqueta.it-codigo FORMAT "x(7)" 
        ob-etiqueta.cod-refer
        ob-etiqueta.nr-lote
        IF AVAIL corte-comerc
           THEN corte-comerc.descricao
           ELSE "** NAO CADASTRADO **" FORMAT "X(20)" 
        c-situacao 
        SKIP.

    FIND LAST inv-acab WHERE
              inv-acab.cod-estabel = tt-param.cod-estabel AND
              inv-acab.data-invent = tt-param.dt-invent   AND
              inv-acab.docto       = tt-param.nro-docto 
              USE-INDEX indice1 NO-LOCK NO-ERROR.

    ASSIGN i-nr-seq = IF AVAIL inv-acab
                      THEN inv-acab.seq + 1
                      ELSE 0.

    ASSIGN i-nr-seq = i-nr-seq + 1.
    CREATE inv-acab.
    ASSIGN inv-acab.cod-estabel  = ob-etiqueta.cod-estabel
           inv-acab.it-codigo    = ob-etiqueta.it-codigo
           inv-acab.cod-refer    = ob-etiqueta.cod-refer
           inv-acab.data-invent  = tt-param.dt-invent
           inv-acab.docto        = tt-param.nro-docto
           inv-acab.seq          = i-nr-seq
           inv-acab.qtd-inv      = ob-etiqueta.quantidade
           inv-acab.lote         = ob-etiqueta.nr-lote + ob-etiqueta.cod-refer
           inv-acab.num-etiqueta = ob-etiqueta.num-etiqueta.
           inv-acab.situacao     = 1.

    ASSIGN ob-etiqueta.localizacao = p-localiz. 

END PROCEDURE.


PROCEDURE pi-imprime.

    RUN utp/ut-utils.p PERSISTENT SET h-prog.

    RUN esapi/saida-imp.p (OUTPUT i-saida,
                           OUTPUT c-saida,
                           OUTPUT i-num-copias,
                           OUTPUT l-ok).

    CASE i-saida:
        WHEN 1 THEN DO.
            OUTPUT TO VALUE(c-saida) CONVERT TARGET "ibm850" PAGED PAGE-SIZE 62.
            PUT CONTROL "~033E~033(s19H".    
        END.
        WHEN 2 THEN
            OUTPUT TO VALUE(c-saida).
        WHEN 3 THEN DO.
            ASSIGN c-saida = SESSION:TEMP-DIRECTORY + "essp0105.tmp".
            OUTPUT TO VALUE(c-saida) CONVERT SOURCE "ibm850".
        END.
    END CASE.

    DO i-ct = 1 TO i-num-copias.
       ASSIGN i-lin        = 99
              de-qtd-lidas =  0
              de-tot-lidas =  0
              i-pc-lidas   =  0
              i-tot-lidas  =  0
              i-pag        =  1.

       FOR EACH tt-imp-invent NO-LOCK
           BREAK BY tt-imp-invent.local-invent
                 BY tt-imp-invent.it-codigo
                 BY tt-imp-invent.cod-refer
                 BY tt-imp-invent.num-etiqueta.
    
           IF i-lin > 62 THEN DO:
              RUN pi-imp-cabec.
              ASSIGN i-lin = 7.
           END.

           {esinc/i-dsrb.i tt-imp-invent.situacao tt-imp-invent.situacao c-situacao}.

           ASSIGN c-obs = "".
           IF tt-imp-invent.situacao = 5 THEN DO: /* Etiqueta Faturada */
              FIND ped-item-rom WHERE
                   ped-item-rom.cod-estabel  = tt-imp-invent.cod-estabel  AND
                   ped-item-rom.num-etiqueta = tt-imp-invent.num-etiqueta NO-LOCK NO-ERROR.
              IF AVAIL ped-item-rom THEN DO:
                 FIND ped-item-res WHERE
                      ped-item-res.nome-abrev   = ped-item-rom.nome-abrev AND
                      ped-item-res.nr-pedcli    = ped-item-rom.nr-pedcli  AND
                      ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia NO-LOCK NO-ERROR.
                 IF AVAIL ped-item-res THEN DO:
                    FIND nota-fiscal WHERE
                         nota-fiscal.cod-estabel       = ped-item-res.cod-estabel AND
                         INT(nota-fiscal.nr-nota-fis)  = ped-item-res.nr-nota-fis NO-LOCK NO-ERROR.
                    ASSIGN c-obs = "Nota Fiscal : "  + nota-fiscal.nr-nota-fis +
                                   " Data Emissao: " + STRING(nota-fiscal.dt-emis-nota, "99/99/9999") +
                                   " Pedido: "       + nota-fiscal.nr-pedcli. 
                 END.
              END.
           END.

           PUT tt-imp-invent.num-etiqueta                         AT   1
               tt-imp-invent.nr-ob         FORMAT ">>>,>>9"       AT  13
               tt-imp-invent.dt-emissao    FORMAT "99/99/9999"    AT  21
               tt-imp-invent.quantidade    FORMAT ">>>,>>9.99"    AT  32
               tt-imp-invent.local-invent  FORMAT "999/999"       AT  45
               tt-imp-invent.localizacao   FORMAT "999/999"       AT  56
               tt-imp-invent.it-codigo     FORMAT "x(6)"          AT  64
               tt-imp-invent.cod-refer     FORMAT "x(7)"          AT  74
               tt-imp-invent.nr-lote       FORMAT "x(2)"          AT  83
               c-situacao                  FORMAT "x(15)"         AT  87.
            IF tt-imp-invent.inventariada <> "" THEN
               PUT tt-imp-invent.inventariada  FORMAT "x(38)"         AT 103.
            IF tt-imp-invent.cadastrada <> "" THEN
               PUT tt-imp-invent.cadastrada    FORMAT "x(38)"         AT 103.
            
            IF c-obs <> "" THEN
               PUT c-obs FORMAT "x(38)"  AT 103.
            
           ASSIGN de-qtd-lidas = de-qtd-lidas + tt-imp-invent.quantidade
                  i-lin        = i-lin   + 1
                  i-pc-lidas   = i-pc-lidas + 1.

           IF LAST-OF(tt-imp-invent.local-invent) THEN DO:
              IF i-lin > 62 THEN DO:
                 RUN pi-imp-cabec.
                 ASSIGN i-lin = 7.
              END.
              PUT "TOTAL DOCA...." AT 1.
              PUT de-qtd-lidas FORMAT ">>>,>>9.99" AT  32.
              PUT "TOTAL PEÄAS LIDAS...." AT 43.
              PUT i-pc-lidas FORMAT ">>>,>>9" AT 64 SKIP(1).
              ASSIGN i-tot-lidas  = i-tot-lidas  + i-pc-lidas
                     de-tot-lidas = de-tot-lidas + de-qtd-lidas
                     i-lin        = i-lin + 2
                     i-pc-lidas   = 0
                     de-qtd-lidas = 0.
           END.
       END.
       
       IF i-tot-lidas <> 0 THEN DO:
           IF i-lin > 58 THEN DO:
              RUN pi-imp-cabec.
              ASSIGN i-lin = 7.
           END.
          PUT "TOTAL GERAL...." AT 1.
          PUT de-tot-lidas FORMAT ">>>,>>9.99" AT  32.
          PUT "TOTAL GERAL LIDAS...." AT 43.
          PUT i-tot-lidas FORMAT ">>>,>>9" AT 64 SKIP(2).
          ASSIGN i-tot-lidas  = 0
                 de-tot-lidas = 0
                 i-pc-lidas   = 0
                 i-tot-lidas  = 0.
          PUT "---------------------------------------" AT 38 SKIP.
          PUT "I N V E N T A R I A N T E" AT 45.
       END.
       
       IF i-saida = 1 THEN DO:
         PAGE.
         PUT "" AT 1.
       END.
       
    END.
    
    OUTPUT CLOSE.

END PROCEDURE.
