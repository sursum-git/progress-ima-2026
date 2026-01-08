 /* include de controle de vers∆o */
{include/i-prgvrs.i ESSP0105RP 2.04.00.001}

DEFINE TEMP-TABLE tt-param
       FIELD destino          AS INTEGER 
       FIELD arq-destino      AS CHAR
       FIELD arq-entrada      AS CHAR 
       FIELD todos            AS INTEGER 
       FIELD usuario          AS CHAR
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       FIELD tg-pedido        AS LOG 
       FIELD tg-item          AS LOG 
       FIELD tg-avulsa        AS LOG 
       FIELD tg-inventario    AS LOG
       FIELD tg-cancel-res    AS LOG
       FIELD tg-reimp-etq     AS LOG
       FIELD tg-localiz       AS LOG
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

DEF BUFFER b-ped-item-ext FOR ped-item-ext.
DEF BUFFER b-imp-item FOR tt-imp-item.    

DEF STREAM s-imp.
DEF STREAM s-etq.
DEF VAR h-acomp       AS HANDLE NO-UNDO.
DEF VAR c-linha       AS CHAR NO-UNDO.
DEF VAR c-procedure   AS CHAR.
DEF VAR i-ind         AS INT.
DEF VAR l-tem-reserva AS LOG INIT NO.
DEF VAR nr-ult-seq    LIKE ped-item.nr-sequencia.

DEF VAR i-sit-aval    LIKE ped-venda.cod-sit-aval.
DEF VAR i-pc-lidas    AS INT.
DEF VAR i-cod-mess    LIKE ped-venda.cod-message-alert.      
DEF VAR da-dt-mess    LIKE ped-venda.dt-mensagem.
DEF VAR c-desc-for    LIKE ped-venda.desc-forc-cr.           
DEF VAR l-dsp-fat     LIKE ped-venda.dsp-pre-fat.
DEF VAR de-qt-pedida  LIKE coletor.qt-pedida.
DEF VAR i-tp-embal    AS INT.


/* Inventario */
DEF VAR i-docto  LIKE inv-acab.docto.
DEF VAR i-nr-seq LIKE inv-acab.seq.
DEF VAR i-qt-pecas AS INT.

/* Separaá∆o por item */
DEF VAR c-it-codigo LIKE ped-item.it-codigo.
DEF VAR c-cod-refer LIKE ped-item.cod-refer.

DEF VAR i-num-bar AS INT.

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
       c-procedure = IF tt-param.tg-pedido
                     THEN "pi-imp-pedido"
                     ELSE IF tt-param.tg-item 
                          THEN "pi-imp-item"
                          ELSE IF tt-param.tg-avulsa
                               THEN "pi-imp-avulsa"
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

FOR EACH tt-imp-item.
    /* N∆o valida pedidos para Cancel de Reserevas, Invent†rio e Reimpressao Etiquetas*/
    IF NOT tt-param.tg-cancel-res AND
       NOT tt-param.tg-inventario AND 
       NOT tt-param.tg-localiz AND
       NOT tt-param.tg-reimp-etq THEN DO.

       FIND tt-imp-ped OF tt-imp-item NO-ERROR.

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
         ob-etiqueta.nr-seq = tt-imp-item.sequencia NO-ERROR.

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


IF tt-param.tg-localiz THEN DO:
   PUT STREAM str-rp
       SKIP(2)
       "TOTAL DE PEÄAS LIDAS......: " i-qt-pecas FORMAT ">>9"
       SKIP.
   PUT STREAM str-rp
       SKIP(2)
       "TOTAL DE PEÄAS LOCALIZADAS: " i-pc-lidas FORMAT ">>9"
       SKIP.
   IF i-qt-pecas <> i-pc-lidas  THEN DO:
      PUT STREAM str-rp
          SKIP(2)
       "DIFERENCA.................: " i-qt-pecas - i-pc-lidas FORMAT ">>9"
       " <----- A T E N Ä A O ----->"
          SKIP.

   END.
END.

IF tt-param.tg-inventario THEN DO.
   PUT SKIP(2)
       "TOTAL DE PECAS: " i-qt-pecas
       SKIP.
END.
ELSE DO.
   ASSIGN c-procedure = REPLACE(c-procedure,"imp","grava").
   RUN VALUE(c-procedure).
END.

IF tt-param.tg-pedido OR
   tt-param.tg-item OR
   tt-param.tg-avulsa THEN
   RUN pi-ver-volumes.

{include/i-rpclo.i &STREAM="stream str-rp"}
RUN pi-finalizar in h-acomp.
RETURN "Ok":U.


/* Procedures Diversas */
PROCEDURE pi-imp-pedido.
    IF SUBSTR(c-linha,1,1) = "#" THEN DO.
       ASSIGN i-ind = i-ind + 1.
       CREATE tt-imp-ped.
       ASSIGN tt-imp-ped.id         = i-ind
              tt-imp-ped.nome-abrev = SUBSTR(c-linha,2,12)
              tt-imp-ped.nr-pedcli  = SUBSTR(c-linha,14,10)
              tt-imp-ped.volume-ini = INT(SUBSTR(c-linha,26,6))
              tt-imp-ped.volume-fim = INT(SUBSTR(c-linha,32,6)).
    END.
    ELSE DO.
       CREATE tt-imp-item.
       ASSIGN tt-imp-item.id         = i-ind
              tt-imp-item.it-codigo  = SUBSTR(c-linha,2,6)
              tt-imp-item.cod-refer  = SUBSTR(c-linha,8,7)
              tt-imp-item.nr-lote    = INT(SUBSTR(c-linha,15,1))
              tt-imp-item.quantidade = DEC(SUBSTR(c-linha,16,4)) / 10
              tt-imp-item.volume-it  = INT(SUBSTR(c-linha,20,6))
              tt-imp-item.nr-ob      = INT(SUBSTR(c-linha,26,5))
              tt-imp-item.sequencia  = INT(SUBSTR(c-linha,31,3)).

       ASSIGN tt-imp-item.cod-refer = IF INT(tt-imp-item.cod-refer) = 0 
                                      THEN ''
                                      ELSE tt-imp-item.cod-refer
              tt-imp-item.lote = IF tt-imp-item.nr-lote = 1 THEN "RP"
                                 ELSE IF tt-imp-item.nr-lote = 2 THEN "PP"
                                      ELSE IF tt-imp-item.nr-lote = 3 THEN "RD"
                                           ELSE IF tt-imp-item.nr-lote = 4 THEN "PD"
                                                ELSE "".

       IF tt-imp-item.lote BEGINS 'P' AND
          tt-imp-item.nr-ob < 10000 THEN
          ASSIGN tt-imp-item.nr-ob = tt-imp-item.nr-ob + 100000.
    END.
END PROCEDURE.

PROCEDURE pi-imp-item.
    IF SUBSTR(c-linha,1,1) = "#" THEN DO.
       ASSIGN c-it-codigo = SUBSTR(c-linha,2,6)
              c-cod-refer = SUBSTR(c-linha,8,7).
    END.
    ELSE DO.
        FIND tt-imp-ped WHERE
             tt-imp-ped.nome-abrev = SUBSTR(c-linha,2,12) AND
             tt-imp-ped.nr-pedcli  = SUBSTR(c-linha,14,10) NO-ERROR.

        IF NOT AVAIL tt-imp-ped THEN DO.
           ASSIGN i-ind = i-ind + 1.
           CREATE tt-imp-ped.
           ASSIGN tt-imp-ped.id         = i-ind
                  tt-imp-ped.nome-abrev = SUBSTR(c-linha,2,12)
                  tt-imp-ped.nr-pedcli  = SUBSTR(c-linha,14,10).
        END.

        CREATE tt-imp-item.
        ASSIGN tt-imp-item.id         = tt-imp-ped.id
               tt-imp-item.it-codigo  = c-it-codigo
               tt-imp-item.cod-refer  = c-cod-refer
               tt-imp-item.quantidade = DEC(SUBSTR(c-linha,26,4)) / 10
               tt-imp-item.volume-it  = INT(SUBSTR(c-linha,30,6))
               tt-imp-item.nr-ob      = INT(SUBSTR(c-linha,36,5))
               tt-imp-item.sequencia  = INT(SUBSTR(c-linha,41,3))
               tt-imp-item.nr-lote    = INT(SUBSTR(c-linha,44,1)).

        ASSIGN tt-imp-item.cod-refer = IF INT(tt-imp-item.cod-refer) = 0 
                                       THEN ''
                                       ELSE tt-imp-item.cod-refer
              tt-imp-item.lote = IF tt-imp-item.nr-lote = 1 THEN "RP"
                                  ELSE IF tt-imp-item.nr-lote = 2 THEN "PP"
                                       ELSE IF tt-imp-item.nr-lote = 3 THEN "RD"
                                            ELSE IF tt-imp-item.nr-lote = 4 THEN "PD"
                                                 ELSE "CA".

        IF tt-imp-item.lote BEGINS 'P' AND
           tt-imp-item.nr-ob < 10000 THEN
           ASSIGN tt-imp-item.nr-ob = tt-imp-item.nr-ob + 100000.
    END.
END PROCEDURE.

PROCEDURE pi-imp-avulsa.
    FIND tt-imp-ped WHERE
         tt-imp-ped.nome-abrev = tt-param.nome-abrev AND
         tt-imp-ped.nr-pedcli = tt-param.nr-pedcli NO-ERROR.
    IF NOT AVAIL tt-imp-ped THEN DO.
       CREATE tt-imp-ped.
       ASSIGN tt-imp-ped.id = 1
              tt-imp-ped.nome-abrev = tt-param.nome-abrev
              tt-imp-ped.nr-pedcli = tt-param.nr-pedcli.
    END.

    IF SUBSTR(c-linha,1,1) = "&" THEN DO.
        CREATE tt-imp-item.
        ASSIGN tt-imp-item.id         = 1
               tt-imp-item.it-codigo  = SUBSTR(c-linha,2,6)
               tt-imp-item.cod-refer  = SUBSTR(c-linha,8,7)
               tt-imp-item.quantidade = DEC(SUBSTR(c-linha,15,4)) / 10
               tt-imp-item.volume-it  = INT(SUBSTR(c-linha,19,6))
               tt-imp-item.nr-ob      = INT(SUBSTR(c-linha,25,5))
               tt-imp-item.sequencia  = INT(SUBSTR(c-linha,30,3))
               tt-imp-item.nr-lote    = INT(SUBSTR(c-linha,33,1)).

        ASSIGN tt-imp-item.cod-refer = IF INT(tt-imp-item.cod-refer) = 0 
                                       THEN ''
                                       ELSE tt-imp-item.cod-refer
               tt-imp-item.lote = IF tt-imp-item.nr-lote = 1 THEN "RP"
                                  ELSE IF tt-imp-item.nr-lote = 2 THEN "PP"
                                       ELSE IF tt-imp-item.nr-lote = 3 THEN "RD"
                                            ELSE IF tt-imp-item.nr-lote = 4 THEN "PD"
                                                 ELSE "".

        IF tt-imp-item.lote BEGINS 'P' AND
           tt-imp-item.nr-ob < 10000 THEN
           ASSIGN tt-imp-item.nr-ob = tt-imp-item.nr-ob + 100000.
    END.
END PROCEDURE.

PROCEDURE pi-imp-inventario.
    ASSIGN i-qt-pecas = i-qt-pecas + 1.

    RUN pi-grava-inventario (INPUT INT(SUBSTR(c-linha,7,9)),
                             INPUT SUBSTR(c-linha,1,6)).
END PROCEDURE.


PROCEDURE pi-imp-cancel-res.
    IF SUBSTR(c-linha,1,1) = "#" THEN DO.
       ASSIGN i-ind = i-ind + 1.
       CREATE tt-imp-ped.
       ASSIGN tt-imp-ped.id         = i-ind
              tt-imp-ped.nome-abrev = SUBSTR(c-linha,2,12)
              tt-imp-ped.nr-pedcli  = SUBSTR(c-linha,14,10)
              tt-imp-ped.volume-ini = INT(SUBSTR(c-linha,26,6))
              tt-imp-ped.volume-fim = INT(SUBSTR(c-linha,32,6)).
    END.
    ELSE DO.
       CREATE tt-imp-item.
       ASSIGN tt-imp-item.id         = i-ind
              tt-imp-item.it-codigo  = SUBSTR(c-linha,2,6)
              tt-imp-item.cod-refer  = SUBSTR(c-linha,8,7)
              tt-imp-item.nr-lote    = INT(SUBSTR(c-linha,15,1))
              tt-imp-item.quantidade = DEC(SUBSTR(c-linha,16,4)) / 10
              tt-imp-item.volume-it  = INT(SUBSTR(c-linha,20,6))
              tt-imp-item.nr-ob      = INT(SUBSTR(c-linha,26,5))
              tt-imp-item.sequencia  = INT(SUBSTR(c-linha,31,3)).

       ASSIGN tt-imp-item.cod-refer = IF INT(tt-imp-item.cod-refer) = 0 
                                      THEN ''
                                      ELSE tt-imp-item.cod-refer
               tt-imp-item.lote = IF tt-imp-item.nr-lote = 1 THEN "RP"
                                  ELSE IF tt-imp-item.nr-lote = 2 THEN "PP"
                                      ELSE IF tt-imp-item.nr-lote = 3 THEN "RD"
                                           ELSE IF tt-imp-item.nr-lote = 4 THEN "PD"
                                                ELSE "".

       IF tt-imp-item.lote BEGINS 'P' AND
          tt-imp-item.nr-ob < 10000 THEN
          ASSIGN tt-imp-item.nr-ob = tt-imp-item.nr-ob + 100000.
    END.
END PROCEDURE.

PROCEDURE pi-imp-localiz.
    FIND ob-etiqueta WHERE
         ob-etiqueta.num-etiqueta = INT(SUBSTR(c-linha,7,9))
         SHARE-LOCK NO-ERROR.
    ASSIGN i-qt-pecas = i-qt-pecas + 1.
    IF AVAIL ob-etiqueta THEN DO:
       ASSIGN ob-etiqueta.localizacao = SUBSTR(c-linha,1,6).
       ASSIGN i-pc-lidas = i-pc-lidas + 1.
    END.
    ELSE DO:
       PUT STREAM str-rp
           SKIP
           "DOCA: " (SUBSTR(c-linha,1,6)) FORMAT "999/999"
           "  ETIQUETA: " SUBSTR(c-linha,7,9) " N∆o foi Localizada"
           SKIP.
    END.

END PROCEDURE.

/* Grava Dados no Banco... */

PROCEDURE pi-grava-pedido.
    FOR EACH tt-imp-ped,
        EACH tt-imp-item OF tt-imp-ped NO-LOCK
             BREAK BY tt-imp-ped.nr-pedcli
                   BY tt-imp-item.it-codigo
                   BY tt-imp-item.cod-refer
                   BY tt-imp-item.volume-it.

        RUN pi-acompanhar IN h-acomp (INPUT tt-imp-ped.nome-abrev + " " + 
                                            tt-imp-ped.nr-pedcli).


        FIND ped-item-rom WHERE
             ped-item-rom.nr-ob = tt-imp-item.nr-ob AND
             ped-item-rom.nr-seq-etq = tt-imp-item.sequencia
             NO-LOCK NO-ERROR.
        IF AVAIL ped-item-rom THEN DO.
           PUT STREAM str-rp
               "Erro: Peáa enviada pelo Coletor j† est† Reservada...." SKIP
               " Pedido:" tt-imp-ped.nr-pedcli
               " Cliente:" tt-imp-ped.nome-abrev
               " Item:" tt-imp-item.it-codigo
               " Refer:" tt-imp-item.cod-refer
               " Lote:" tt-imp-item.lote
               " Volume:" tt-imp-item.volume-it SKIP
               " OB:" tt-imp-item.nr-ob
               " Sequencia:" tt-imp-item.sequencia  
               " Num Etiqueta:" tt-imp-item.num-etiqueta
               " Qtd:" tt-imp-item.quantidade SKIP
               " Reservado para:" ped-item-rom.nr-pedcli ped-item-rom.nome-abrev
               SKIP(1).
           NEXT.
        END.

        IF tt-imp-item.erro <> "" THEN DO.
           PUT STREAM str-rp
               tt-imp-item.erro SKIP
               " Pedido:" tt-imp-ped.nr-pedcli
               " Cliente:" tt-imp-ped.nome-abrev
               " Item:" tt-imp-item.it-codigo
               " Refer:" tt-imp-item.cod-refer
               " Lote:" tt-imp-item.lote
               " Volume:" tt-imp-item.volume-it SKIP
               " OB:" tt-imp-item.nr-ob
               " Sequencia:" tt-imp-item.sequencia  
               " Num Etiqueta:" tt-imp-item.num-etiqueta
               " Qtd:" tt-imp-item.quantidade 
               SKIP(1).
           NEXT.
        END.

        FIND coletor WHERE
             coletor.id         = tt-param.nr-coletor AND
             coletor.nome-abrev = tt-imp-ped.nome-abrev AND
             coletor.nr-pedcli  = tt-imp-ped.nr-pedcli AND
             coletor.it-codigo  = tt-imp-item.it-codigo AND
             coletor.cod-refer  = tt-imp-item.cod-refer AND
             coletor.nr-lote    = tt-imp-item.nr-lote NO-ERROR.

        IF AMBIGUOUS coletor THEN DO.
           FIND FIRST coletor WHERE
                      coletor.id         = tt-param.nr-coletor   AND
                      coletor.nome-abrev = tt-imp-ped.nome-abrev AND
                      coletor.nr-pedcli  = tt-imp-ped.nr-pedcli AND
                      coletor.it-codigo  = tt-imp-item.it-codigo AND
                      coletor.cod-refer  = tt-imp-item.cod-refer AND
                      coletor.nr-lote    = tt-imp-item.nr-lote AND
                      coletor.qt-pedida  > coletor.qt-reservada
                      NO-ERROR.
           IF NOT AVAIL coletor THEN DO.
              FIND LAST coletor WHERE
                        coletor.id         = tt-param.nr-coletor AND
                        coletor.nome-abrev = tt-imp-ped.nome-abrev AND
                        coletor.nr-pedcli  = tt-imp-ped.nr-pedcli AND
                        coletor.it-codigo  = tt-imp-item.it-codigo AND
                        coletor.cod-refer  = tt-imp-item.cod-refer AND
                        coletor.nr-lote    = tt-imp-item.nr-lote NO-ERROR.
           END.
        END.

        IF NOT AVAIL coletor THEN 
           FIND coletor WHERE
                coletor.id         = tt-param.nr-coletor AND
                coletor.nr-pedcli  = tt-imp-ped.nr-pedcli AND
                coletor.it-codigo  = tt-imp-item.it-codigo AND
                coletor.cod-refer  = tt-imp-item.cod-refer AND
                coletor.nr-lote    = tt-imp-item.nr-lote NO-ERROR.

        IF NOT AVAIL coletor THEN DO.
           PUT STREAM str-rp
               "Erro: Registro n∆o encontrado na tabela coletor" SKIP
               " Pedido:" tt-imp-ped.nr-pedcli
               " Cliente:" tt-imp-ped.nome-abrev
               " Item:" tt-imp-item.it-codigo
               " Refer:" tt-imp-item.cod-refer
               " Lote:" tt-imp-item.lote
               " Volume:" tt-imp-item.volume-it SKIP
               " OB:" tt-imp-item.nr-ob
               " Sequencia:" tt-imp-item.sequencia  
               " Num Etiqueta:" tt-imp-item.num-etiqueta
               " Qtd:" tt-imp-item.quantidade 
               SKIP(1).
           NEXT.
        END.

        
        /* Valida Corte Comercial */
        FIND ob-etiqueta WHERE
             ob-etiqueta.nr-ob = tt-imp-item.nr-ob AND
             ob-etiqueta.nr-seq = tt-imp-item.sequencia
             NO-LOCK NO-ERROR.
        
        IF NOT AVAIL ob-etiqueta AND
           tt-imp-item.lote BEGINS 'P' AND
           tt-imp-item.nr-ob < 10000 THEN
           FIND ob-etiqueta WHERE
                ob-etiqueta.nr-ob = tt-imp-item.nr-ob + 100000 AND
                ob-etiqueta.nr-seq = tt-imp-item.sequencia NO-ERROR.

        FIND ped-item-ext WHERE
             ped-item-ext.nome-abrev   = coletor.nome-abrev   AND
             ped-item-ext.nr-pedcli    = coletor.nr-pedcli    AND
             ped-item-ext.nr-sequencia = coletor.nr-sequencia AND
             ped-item-ext.it-codigo    = coletor.it-codigo    AND
             ped-item-ext.cod-refer    = coletor.cod-refer NO-ERROR.    

        IF NOT AVAIL ped-item-ext THEN DO.
           PUT STREAM str-rp
                "Erro: N∆o encontrado Informaá∆o Complementar do Item (ped-item-ext)" SKIP
                " Pedido:" tt-imp-ped.nr-pedcli
                " Cliente:" tt-imp-ped.nome-abrev
                " Item:" tt-imp-item.it-codigo
                " Refer:" tt-imp-item.cod-refer
                " Lote:" tt-imp-item.lote
                " Sequencia:" coletor.nr-sequencia
                " Volume:" tt-imp-item.volume-it SKIP
                " OB:" tt-imp-item.nr-ob
                " Sequencia:" tt-imp-item.sequencia  
                " Num Etiqueta:" tt-imp-item.num-etiqueta
                " Qtd:" tt-imp-item.quantidade 
                SKIP(1).
            NEXT.
        END.

        IF ped-item-ext.corte-comerc <> ob-etiqueta.corte-comerc THEN DO.
           PUT STREAM str-rp
               "Erro: Corte Comercial da Etiqueta diferente do Item do Pedido" SKIP
               " Pedido:" tt-imp-ped.nr-pedcli
               " Cliente:" tt-imp-ped.nome-abrev
               " Item:" tt-imp-item.it-codigo
               " Refer:" tt-imp-item.cod-refer
               " Lote:" tt-imp-item.lote
               " Volume:" tt-imp-item.volume-it SKIP
               " OB:" tt-imp-item.nr-ob
               " Sequencia:" tt-imp-item.sequencia  
               " Num Etiqueta:" tt-imp-item.num-etiqueta
               " Qtd:" tt-imp-item.quantidade 
               SKIP(1).
           NEXT.
        END.
        /* Fim Valida Corte comercial */
        
        
        FIND LAST ped-item-res WHERE
                  ped-item-res.nome-abrev   = coletor.nome-abrev   AND
                  ped-item-res.nr-pedcli    = coletor.nr-pedcli    AND
                  ped-item-res.nr-sequencia = coletor.nr-sequencia AND
                  ped-item-res.it-codigo    = coletor.it-codigo    AND
                  ped-item-res.cod-refer    = coletor.cod-refer NO-ERROR.    

        IF AVAIL ped-item-res THEN DO.
           IF ped-item-res.dt-trans <> tt-param.data-exec AND
              ped-item-res.hr-trans <> STRING(tt-param.hora-exec,"HH:MM:SS") THEN DO.
              PUT STREAM str-rp
                  "Erro! Ja existe Reserva para a Sequencia/Item" SKIP
                  " Pedido:" ped-item-res.nr-pedcli
                  "Cliente:" ped-item-res.nome-abrev
                  " Seq:" ped-item-res.nr-sequencia
                  " Item:" ped-item-res.it-codigo
                  " Volume:" tt-imp-item.volume-it
                  SKIP(1).
           END.
        END.
        ELSE DO.
           CREATE ped-item-res.
           ASSIGN ped-item-res.nome-abrev   = coletor.nome-abrev
                  ped-item-res.nr-pedcli    = coletor.nr-pedcli
                  ped-item-res.nr-sequencia = coletor.nr-sequencia
                  ped-item-res.it-codigo    = coletor.it-codigo
                  ped-item-res.cod-refer    = coletor.cod-refer
                  ped-item-res.nome-transp  = tt-imp-ped.nome-transp
                  ped-item-res.volume-ini   = tt-imp-item.volume-it
                  ped-item-res.sigla-emb    = tt-imp-item.lote 
                  ped-item-res.desc-dentro = IF tt-imp-item.nr-lote = 1 OR
                                                tt-imp-item.nr-lote = 3 
                                             THEN "ROLOS"
                                             ELSE "PECAS".
        END.

        IF (ped-item-res.volume-fim > 0) AND
           (ped-item-res.volume-fim + 1) < tt-imp-item.volume-it THEN DO.
            ASSIGN coletor.qt-pedida = coletor.qt-reservada.

            FIND NEXT coletor WHERE
                      coletor.id         = tt-param.nr-coletor AND
                      coletor.nome-abrev = tt-imp-ped.nome-abrev AND
                      coletor.nr-pedcli  = tt-imp-ped.nr-pedcli AND
                      coletor.it-codigo  = tt-imp-item.it-codigo AND
                      coletor.cod-refer  = tt-imp-item.cod-refer AND
                      coletor.nr-lote    = tt-imp-item.nr-lote NO-ERROR.

            IF NOT AVAIL coletor THEN DO.
               FIND ped-venda WHERE
                    ped-venda.nome-abrev = ped-item-res.nome-abrev AND
                    ped-venda.nr-pedcli = ped-item-res.nr-pedcli NO-LOCK NO-ERROR.
                    
               FIND ped-item OF ped-venda WHERE
                    ped-item.it-codigo = ped-item-res.it-codigo AND
                    ped-item.cod-refer = ped-item-res.cod-refer AND
                    ped-item.nr-sequencia = ped-item-res.nr-sequencia
                    NO-LOCK NO-ERROR.

               RUN pi-busca-seq.

               FIND ped-item OF ped-venda WHERE
                    ped-item.it-codigo = ped-item-res.it-codigo AND
                    ped-item.cod-refer = ped-item-res.cod-refer AND
                    ped-item.nr-sequencia = ped-item-res.nr-sequencia
                    NO-LOCK NO-ERROR.
               RUN pi-manut-peditem (INPUT "CI"). /* Cria novo Item com Reserva */

               CREATE coletor.
               ASSIGN coletor.id           = tt-param.nr-coletor 
                      coletor.nome-abrev   = tt-imp-ped.nome-abrev 
                      coletor.nr-pedcli    = tt-imp-ped.nr-pedcli 
                      coletor.nr-sequencia = nr-ult-seq
                      coletor.it-codigo    = tt-imp-item.it-codigo 
                      coletor.cod-refer    = tt-imp-item.cod-refer 
                      coletor.nr-lote      = tt-imp-item.nr-lote
                      coletor.qt-pedida    = ped-item.qt-pedida.

               FIND ped-item OF ped-venda WHERE
                    ped-item.it-codigo = ped-item-res.it-codigo AND
                    ped-item.cod-refer = ped-item-res.cod-refer AND
                    ped-item.nr-sequencia = ped-item-res.nr-sequencia
                    NO-LOCK NO-ERROR.
               RUN pi-manut-peditem (INPUT "AI"). /* Ajusta Item Original */
            END.

            FIND LAST ped-item-res WHERE
                      ped-item-res.nome-abrev   = coletor.nome-abrev   AND
                      ped-item-res.nr-pedcli    = coletor.nr-pedcli    AND
                      ped-item-res.nr-sequencia = coletor.nr-sequencia AND
                      ped-item-res.it-codigo    = coletor.it-codigo    AND
                      ped-item-res.cod-refer    = coletor.cod-refer NO-ERROR.    

            IF NOT AVAIL ped-item-res THEN DO:
               CREATE ped-item-res.
               ASSIGN ped-item-res.nome-abrev   = coletor.nome-abrev
                      ped-item-res.nr-pedcli    = coletor.nr-pedcli
                      ped-item-res.nr-sequencia = coletor.nr-sequencia
                      ped-item-res.it-codigo    = coletor.it-codigo
                      ped-item-res.cod-refer    = coletor.cod-refer
                      ped-item-res.nome-transp  = tt-imp-ped.nome-transp
                      ped-item-res.volume-ini   = tt-imp-item.volume-it
                      ped-item-res.sigla-emb    = tt-imp-item.lote
                      ped-item-res.desc-dentro  = IF tt-imp-item.nr-lote = 1 OR
                                                     tt-imp-item.nr-lote = 3 
                                                  THEN "ROLOS"
                                                  ELSE "PECAS".
            END.
        END.

        ASSIGN ped-item-res.volume-fim = tt-imp-item.volume-it
               ped-item-res.qt-pedida  = ped-item-res.qt-pedida + tt-imp-item.quantidade
               ped-item-res.dt-trans   = tt-param.data-exec                          
               ped-item-res.hr-trans   = STRING(tt-param.hora-exec,"HH:MM:SS")
               ped-item-res.lote       = tt-imp-item.lote + tt-imp-item.cod-refer
               coletor.qt-reservada    = coletor.qt-reservada + tt-imp-item.quantidade.
             
        CREATE ped-item-rom.
        ASSIGN ped-item-rom.nome-abrev = ped-item-res.nome-abrev
               ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli
               ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
               ped-item-rom.nr-ob = tt-imp-item.nr-ob
               ped-item-rom.nr-seq-etq = tt-imp-item.sequencia
               ped-item-rom.num-etiqueta = tt-imp-item.num-etiqueta
               ped-item-rom.nr-volume = tt-imp-item.volume-it
               ped-item-rom.quantidade = tt-imp-item.quantidade.

        IF NOT FIRST-OF(tt-imp-item.cod-refer) AND
           ped-item-res.volume-ini = ped-item-res.volume-fim AND
           ped-item-res.qt-pedida > tt-imp-item.quantidade THEN
           ASSIGN ped-item-res.sigla-emb = "FD".

        FIND ob-etiqueta WHERE
             ob-etiqueta.nr-ob = tt-imp-item.nr-ob AND
             ob-etiqueta.nr-seq = tt-imp-item.sequencia NO-ERROR.
        IF NOT AVAIL ob-etiqueta AND 
           tt-imp-item.lote BEGINS 'P' AND
           tt-imp-item.nr-ob < 10000 THEN
           FIND ob-etiqueta WHERE
                ob-etiqueta.nr-ob = tt-imp-item.nr-ob + 100000 AND
                ob-etiqueta.nr-seq = tt-imp-item.sequencia NO-ERROR.

        ASSIGN ob-etiqueta.situacao = 4.

    END.

    FOR EACH tt-imp-ped,
        EACH coletor WHERE
             coletor.id         = tt-param.nr-coletor AND
             coletor.nome-abrev = tt-imp-ped.nome-abrev AND
             coletor.nr-pedcli = tt-imp-ped.nr-pedcli NO-LOCK,
        EACH ped-item-res WHERE
             ped-item-res.nome-abrev   = coletor.nome-abrev AND
             ped-item-res.nr-pedcli    = coletor.nr-pedcli  AND
             ped-item-res.it-codigo    = coletor.it-codigo  AND
             ped-item-res.cod-refer    = coletor.cod-refer NO-LOCK.

        FIND ped-venda WHERE
             ped-venda.nome-abrev = ped-item-res.nome-abrev AND
             ped-venda.nr-pedcli = ped-item-res.nr-pedcli NO-LOCK NO-ERROR.

        FIND ped-item OF ped-venda WHERE
             ped-item.it-codigo = ped-item-res.it-codigo AND
             ped-item.cod-refer = ped-item-res.cod-refer AND
             ped-item.nr-sequencia = ped-item-res.nr-sequencia
             NO-LOCK NO-ERROR.

        IF ped-item.qt-pedida <> ped-item-res.qt-pedida AND
           ped-item-res.qt-pedida > 0 THEN DO.
           IF ped-item.qt-pedida < ped-item-res.qt-pedida THEN DO.
              IF (ped-item-res.qt-pedida - ped-item.qt-pedida) <= (ped-item.qt-pedida * 0.10) AND
                 (ped-item-res.qt-pedida - ped-item.qt-pedida) < 500 THEN 
                 RUN pi-manut-peditem (INPUT "AI"). /* Ajusta Item Original */
              ELSE 
                 PUT STREAM str-rp
                     "Erro! Quatidade Reservada Ç superior a TolerÉncia definida" SKIP
                     "Cliente:" ped-item-res.nome-abrev
                     " Pedido:" ped-item-res.nr-pedcli
                     " Seq:" ped-item-res.nr-sequencia
                     " Qt Pedida:" ped-item.qt-pedida
                     " Qt Reservada:" ped-item-res.qt-pedida
                     SKIP(1).
           END.
           ELSE DO.
              IF (ped-item.qt-pedida - ped-item-res.qt-pedida <= ped-item.qt-pedida * 0.05 AND
                  ped-item.qt-pedida - ped-item-res.qt-pedida < 150 ) OR
                 (ped-item.qt-pedida - ped-item-res.qt-pedida) < 30 THEN 
                 RUN pi-manut-peditem (INPUT "AI"). /* Ajusta Item Original */
              ELSE DO.
                  RUN pi-busca-seq.
                  FIND ped-item OF ped-venda WHERE
                       ped-item.it-codigo = ped-item-res.it-codigo AND
                       ped-item.cod-refer = ped-item-res.cod-refer AND
                       ped-item.nr-sequencia = ped-item-res.nr-sequencia
                       NO-LOCK NO-ERROR.
                  RUN pi-manut-peditem (INPUT "CI"). /* Cria novo Item com Reserva */

                  FIND ped-item OF ped-venda WHERE
                       ped-item.it-codigo = ped-item-res.it-codigo AND
                       ped-item.cod-refer = ped-item-res.cod-refer AND
                       ped-item.nr-sequencia = ped-item-res.nr-sequencia
                       NO-LOCK NO-ERROR.
                  RUN pi-manut-peditem (INPUT "AI"). /* Ajusta Item Original */
              END.
           END.
        END.
    END.

    FOR EACH coletor WHERE 
             coletor.id = tt-param.nr-coletor EXCLUSIVE-LOCK.
        DELETE coletor.
    END.
END PROCEDURE.


PROCEDURE pi-grava-item.
    FOR EACH tt-imp-ped,
        EACH tt-imp-item OF tt-imp-ped NO-LOCK
             BREAK BY tt-imp-ped.nr-pedcli
                   BY tt-imp-item.it-codigo
                   BY tt-imp-item.cod-refer
                   BY tt-imp-item.volume-it.

        RUN pi-acompanhar IN h-acomp (INPUT tt-imp-ped.nome-abrev + " " + 
                                            tt-imp-ped.nr-pedcli).

        FIND ped-item-rom WHERE
             ped-item-rom.nr-ob = tt-imp-item.nr-ob AND
             ped-item-rom.nr-seq-etq = tt-imp-item.sequencia
             NO-LOCK NO-ERROR.
        IF AVAIL ped-item-rom THEN DO.
           PUT STREAM str-rp
               "Erro: Peáa enviada pelo Coletor j† est† Reservada...." SKIP
               " Pedido:" tt-imp-ped.nr-pedcli
               " Cliente:" tt-imp-ped.nome-abrev
               " Item:" tt-imp-item.it-codigo
               " Refer:" tt-imp-item.cod-refer
               " Lote:" tt-imp-item.lote
               " Volume:" tt-imp-item.volume-it SKIP
               " OB:" tt-imp-item.nr-ob
               " Sequencia:" tt-imp-item.sequencia  
               " Num Etiqueta:" tt-imp-item.num-etiqueta
               " Qtd:" tt-imp-item.quantidade SKIP
               " Reservado para:" ped-item-rom.nr-pedcli ped-item-rom.nome-abrev
               SKIP(1).
           NEXT.
        END.

        IF tt-imp-item.erro <> "" THEN DO.
           PUT STREAM str-rp
               tt-imp-item.erro SKIP
               " Pedido:" tt-imp-ped.nr-pedcli
               " Cliente:" tt-imp-ped.nome-abrev
               " Item:" tt-imp-item.it-codigo
               " Refer:" tt-imp-item.cod-refer
               " Lote:" tt-imp-item.lote
               " Volume:" tt-imp-item.volume-it SKIP
               " OB:" tt-imp-item.nr-ob
               " Sequencia:" tt-imp-item.sequencia  
               " Num Etiqueta:" tt-imp-item.num-etiqueta
               " Qtd:" tt-imp-item.quantidade 
               SKIP(1).
           NEXT.
        END.

        FIND coletor WHERE
             coletor.id         = tt-param.nr-coletor AND
             coletor.nome-abrev = tt-imp-ped.nome-abrev AND
             coletor.nr-pedcli  = tt-imp-ped.nr-pedcli AND
             coletor.it-codigo  = tt-imp-item.it-codigo AND
             coletor.cod-refer  = tt-imp-item.cod-refer AND
             coletor.nr-lote    = tt-imp-item.nr-lote NO-ERROR.
        IF AMBIGUOUS coletor THEN DO.
           FIND FIRST coletor WHERE
                      coletor.id         = tt-param.nr-coletor AND
                      coletor.nome-abrev = tt-imp-ped.nome-abrev AND
                      coletor.nr-pedcli  = tt-imp-ped.nr-pedcli AND
                      coletor.it-codigo  = tt-imp-item.it-codigo AND
                      coletor.cod-refer  = tt-imp-item.cod-refer AND
                      coletor.nr-lote    = tt-imp-item.nr-lote AND
                      coletor.qt-pedida  > coletor.qt-reservada
                      NO-ERROR.
           IF NOT AVAIL coletor THEN DO.
              FIND LAST coletor WHERE
                        coletor.id         = tt-param.nr-coletor AND
                        coletor.nome-abrev = tt-imp-ped.nome-abrev AND
                        coletor.nr-pedcli  = tt-imp-ped.nr-pedcli AND
                        coletor.it-codigo  = tt-imp-item.it-codigo AND
                        coletor.cod-refer  = tt-imp-item.cod-refer AND
                        coletor.nr-lote    = tt-imp-item.nr-lote NO-ERROR.
           END.
        END.

        IF NOT AVAIL coletor THEN 
           FIND coletor WHERE
                coletor.id         = tt-param.nr-coletor AND
                coletor.nr-pedcli  = tt-imp-ped.nr-pedcli AND
                coletor.it-codigo  = tt-imp-item.it-codigo AND
                coletor.cod-refer  = tt-imp-item.cod-refer AND
                coletor.nr-lote    = tt-imp-item.nr-lote NO-ERROR.

        IF NOT AVAIL coletor THEN DO.
           PUT STREAM str-rp
               "Erro: Registro n∆o encontrado na tabela coletor" SKIP
               " Pedido:" tt-imp-ped.nr-pedcli
               " Cliente:" tt-imp-ped.nome-abrev
               " Item:" tt-imp-item.it-codigo
               " Refer:" tt-imp-item.cod-refer
               " Volume:" tt-imp-item.volume-it
               " Qtd:" tt-imp-item.quantidade 
               SKIP(1).
           NEXT.
        END.

        /* Valida Corte Comercial */
        FIND ob-etiqueta WHERE
             ob-etiqueta.nr-ob = tt-imp-item.nr-ob AND
             ob-etiqueta.nr-seq = tt-imp-item.sequencia
             NO-LOCK NO-ERROR.
        IF NOT AVAIL ob-etiqueta AND 
           tt-imp-item.lote BEGINS 'P' AND
           tt-imp-item.nr-ob < 10000 THEN
           FIND ob-etiqueta WHERE
                ob-etiqueta.nr-ob = tt-imp-item.nr-ob + 100000 AND
                ob-etiqueta.nr-seq = tt-imp-item.sequencia NO-LOCK NO-ERROR.

        FIND ped-item-ext WHERE
             ped-item-ext.nome-abrev   = coletor.nome-abrev   AND
             ped-item-ext.nr-pedcli    = coletor.nr-pedcli    AND
             ped-item-ext.nr-sequencia = coletor.nr-sequencia AND
             ped-item-ext.it-codigo    = coletor.it-codigo    AND
             ped-item-ext.cod-refer    = coletor.cod-refer NO-ERROR.    

        IF ped-item-ext.corte-comerc <> ob-etiqueta.corte-comerc THEN DO.
           PUT STREAM str-rp
               "Erro: Corte Comercial da Etiqueta diferente do Item do Pedido" SKIP
               " Pedido:" tt-imp-ped.nr-pedcli
               " Cliente:" tt-imp-ped.nome-abrev
               " Item:" tt-imp-item.it-codigo
               " Refer:" tt-imp-item.cod-refer
               " Lote:" tt-imp-item.lote
               " Volume:" tt-imp-item.volume-it SKIP
               " OB:" tt-imp-item.nr-ob
               " Sequencia:" tt-imp-item.sequencia  
               " Num Etiqueta:" tt-imp-item.num-etiqueta
               " Qtd:" tt-imp-item.quantidade 
               SKIP(1).
           NEXT.
        END.
        /* Fim Valida Corte */
        

        FIND LAST ped-item-res WHERE
                  ped-item-res.nome-abrev   = coletor.nome-abrev   AND
                  ped-item-res.nr-pedcli    = coletor.nr-pedcli    AND
                  ped-item-res.nr-sequencia = coletor.nr-sequencia AND
                  ped-item-res.it-codigo    = coletor.it-codigo    AND
                  ped-item-res.cod-refer    = coletor.cod-refer NO-ERROR.    

        IF AVAIL ped-item-res THEN DO.
           IF ped-item-res.dt-trans <> tt-param.data-exec AND
              ped-item-res.hr-trans <> STRING(tt-param.hora-exec,"HH:MM:SS") THEN DO.
              PUT STREAM str-rp
                  "Erro! Ja existe Reserva para a Sequencia/Item" SKIP
                   "Cliente:" ped-item-res.nome-abrev
                   " Pedido:" ped-item-res.nr-pedcli
                   " Seq:" ped-item-res.nr-sequencia
                   " Item:" ped-item-res.it-codigo
                   " Volume:" tt-imp-item.volume-it
                   SKIP(1).
              NEXT.
           END.
        END.
        ELSE DO.
           CREATE ped-item-res.
           ASSIGN ped-item-res.nome-abrev   = coletor.nome-abrev
                  ped-item-res.nr-pedcli    = coletor.nr-pedcli
                  ped-item-res.nr-sequencia = coletor.nr-sequencia
                  ped-item-res.it-codigo    = coletor.it-codigo
                  ped-item-res.cod-refer    = coletor.cod-refer
                  ped-item-res.nome-transp  = tt-imp-ped.nome-transp
                  ped-item-res.volume-ini   = tt-imp-item.volume-it
                  ped-item-res.sigla-emb    = tt-imp-item.lote
                  ped-item-res.desc-dentro  = IF tt-imp-item.nr-lote = 1 OR
                                                 tt-imp-item.nr-lote = 3 
                                              THEN "ROLOS"
                                              ELSE "PECAS".
        END.

        IF (ped-item-res.volume-fim > 0) AND
           (ped-item-res.volume-fim + 1) < tt-imp-item.volume-it THEN DO.
            ASSIGN coletor.qt-pedida = coletor.qt-reservada.

            FIND NEXT coletor WHERE
                      coletor.id         = tt-param.nr-coletor AND
                      coletor.nome-abrev = tt-imp-ped.nome-abrev AND
                      coletor.nr-pedcli  = tt-imp-ped.nr-pedcli AND
                      coletor.it-codigo  = tt-imp-item.it-codigo AND
                      coletor.cod-refer  = tt-imp-item.cod-refer AND
                      coletor.nr-lote    = tt-imp-item.nr-lote NO-ERROR.

            IF NOT AVAIL coletor THEN DO.
               FIND ped-venda WHERE
                    ped-venda.nome-abrev = ped-item-res.nome-abrev AND
                    ped-venda.nr-pedcli = ped-item-res.nr-pedcli NO-LOCK NO-ERROR.
                    
               FIND ped-item OF ped-venda WHERE
                    ped-item.it-codigo = ped-item-res.it-codigo AND
                    ped-item.cod-refer = ped-item-res.cod-refer AND
                    ped-item.nr-sequencia = ped-item-res.nr-sequencia
                    NO-LOCK NO-ERROR.

               RUN pi-busca-seq.

               FIND ped-item OF ped-venda WHERE
                    ped-item.it-codigo = ped-item-res.it-codigo AND
                    ped-item.cod-refer = ped-item-res.cod-refer AND
                    ped-item.nr-sequencia = ped-item-res.nr-sequencia
                    NO-LOCK NO-ERROR.
               RUN pi-manut-peditem (INPUT "CI"). /* Cria novo Item com Reserva */

               CREATE coletor.
               ASSIGN coletor.id           = tt-param.nr-coletor 
                      coletor.nome-abrev   = tt-imp-ped.nome-abrev 
                      coletor.nr-pedcli    = tt-imp-ped.nr-pedcli 
                      coletor.nr-sequencia = nr-ult-seq
                      coletor.it-codigo    = tt-imp-item.it-codigo 
                      coletor.cod-refer    = tt-imp-item.cod-refer 
                      coletor.nr-lote      = tt-imp-item.nr-lote
                      coletor.qt-pedida    = ped-item.qt-pedida.

               FIND ped-item OF ped-venda WHERE
                    ped-item.it-codigo = ped-item-res.it-codigo AND
                    ped-item.cod-refer = ped-item-res.cod-refer AND
                    ped-item.nr-sequencia = ped-item-res.nr-sequencia
                    NO-LOCK NO-ERROR.
               RUN pi-manut-peditem (INPUT "AI"). /* Ajusta Item Original */
            END.

            FIND LAST ped-item-res WHERE
                      ped-item-res.nome-abrev   = coletor.nome-abrev   AND
                      ped-item-res.nr-pedcli    = coletor.nr-pedcli    AND
                      ped-item-res.nr-sequencia = coletor.nr-sequencia AND
                      ped-item-res.it-codigo    = coletor.it-codigo    AND
                      ped-item-res.cod-refer    = coletor.cod-refer NO-ERROR.    

            IF NOT AVAIL ped-item-res THEN DO:
               CREATE ped-item-res.
               ASSIGN ped-item-res.nome-abrev   = coletor.nome-abrev
                      ped-item-res.nr-pedcli    = coletor.nr-pedcli
                      ped-item-res.nr-sequencia = coletor.nr-sequencia
                      ped-item-res.it-codigo    = coletor.it-codigo
                      ped-item-res.cod-refer    = coletor.cod-refer
                      ped-item-res.nome-transp  = tt-imp-ped.nome-transp
                      ped-item-res.volume-ini   = tt-imp-item.volume-it
                      ped-item-res.sigla-emb    = tt-imp-item.lote
                      ped-item-res.desc-dentro  = IF tt-imp-item.nr-lote = 1 OR
                                                     tt-imp-item.nr-lote = 3 
                                                  THEN "ROLOS"
                                                  ELSE "PECAS".
            END.
        END.

        ASSIGN ped-item-res.volume-fim = tt-imp-item.volume-it
               ped-item-res.qt-pedida  = ped-item-res.qt-pedida + tt-imp-item.quantidade
               ped-item-res.dt-trans   = tt-param.data-exec
               ped-item-res.hr-trans   = STRING(tt-param.hora-exec,"HH:MM:SS")
               ped-item-res.lote       = tt-imp-item.lote + tt-imp-item.cod-refer
               coletor.qt-reservada    = coletor.qt-reservada + tt-imp-item.quantidade.

        CREATE ped-item-rom.
        ASSIGN ped-item-rom.nome-abrev = ped-item-res.nome-abrev
               ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli
               ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
               ped-item-rom.nr-ob = tt-imp-item.nr-ob
               ped-item-rom.nr-seq-etq = tt-imp-item.sequencia
               ped-item-rom.num-etiqueta = tt-imp-item.num-etiqueta
               ped-item-rom.nr-volume = tt-imp-item.volume-it
               ped-item-rom.quantidade = tt-imp-item.quantidade.

        IF NOT FIRST-OF(tt-imp-item.cod-refer) AND
           ped-item-res.volume-ini = ped-item-res.volume-fim AND
           ped-item-res.qt-pedida > tt-imp-item.quantidade THEN
           ASSIGN ped-item-res.sigla-emb = "FD".

        FIND ob-etiqueta WHERE
             ob-etiqueta.nr-ob = tt-imp-item.nr-ob AND
             ob-etiqueta.nr-seq = tt-imp-item.sequencia NO-ERROR.
        IF NOT AVAIL ob-etiqueta AND 
           tt-imp-item.lote BEGINS 'P' AND
           tt-imp-item.nr-ob < 10000 THEN
           FIND ob-etiqueta WHERE
                ob-etiqueta.nr-ob = tt-imp-item.nr-ob + 100000 AND
                ob-etiqueta.nr-seq = tt-imp-item.sequencia NO-ERROR.

        ASSIGN ob-etiqueta.situacao = 4.

    END.

    FOR EACH tt-imp-ped,
        EACH coletor WHERE
             coletor.id         = tt-param.nr-coletor AND
             coletor.nome-abrev = tt-imp-ped.nome-abrev AND
             coletor.nr-pedcli  = tt-imp-ped.nr-pedcli NO-LOCK,
        EACH ped-item-res WHERE
             ped-item-res.nome-abrev   = coletor.nome-abrev AND
             ped-item-res.nr-pedcli    = coletor.nr-pedcli  AND
             ped-item-res.it-codigo    = coletor.it-codigo  AND
             ped-item-res.cod-refer    = coletor.cod-refer NO-LOCK.

        FIND ped-venda WHERE
             ped-venda.nome-abrev = ped-item-res.nome-abrev AND
             ped-venda.nr-pedcli = ped-item-res.nr-pedcli NO-LOCK NO-ERROR.

        FIND ped-item OF ped-venda WHERE
             ped-item.it-codigo = ped-item-res.it-codigo AND
             ped-item.cod-refer = ped-item-res.cod-refer AND
             ped-item.nr-sequencia = ped-item-res.nr-sequencia
             NO-LOCK NO-ERROR.

        IF ped-item.qt-pedida <> ped-item-res.qt-pedida AND
           ped-item-res.qt-pedida > 0 THEN DO.
           IF ped-item.qt-pedida < ped-item-res.qt-pedida THEN DO.
              IF (ped-item-res.qt-pedida - ped-item.qt-pedida) <= (ped-item.qt-pedida * 0.10) AND
                 (ped-item-res.qt-pedida - ped-item.qt-pedida) < 500 THEN 
                 RUN pi-manut-peditem (INPUT "AI"). /* Ajusta Item Original */
              ELSE
                 PUT STREAM str-rp
                     "Erro! Quatidade Reservada Ç superior a TolerÉncia definida" SKIP
                     "Cliente:" ped-item-res.nome-abrev
                     " Pedido:" ped-item-res.nr-pedcli
                     " Seq:" ped-item-res.nr-sequencia
                     " Qt Pedida:" ped-item.qt-pedida
                     " Qt Reservada:" ped-item-res.qt-pedida
                     SKIP(1).
           END.
           ELSE DO.
              IF (ped-item.qt-pedida - ped-item-res.qt-pedida <= ped-item.qt-pedida * 0.05 AND
                  ped-item.qt-pedida - ped-item-res.qt-pedida < 150 ) OR
                 (ped-item.qt-pedida - ped-item-res.qt-pedida) < 30 THEN 
                 RUN pi-manut-peditem (INPUT "AI"). /* Ajusta Item Original */
              ELSE DO.
                  RUN pi-busca-seq.
                  FIND ped-item OF ped-venda WHERE
                       ped-item.it-codigo = ped-item-res.it-codigo AND
                       ped-item.cod-refer = ped-item-res.cod-refer AND
                       ped-item.nr-sequencia = ped-item-res.nr-sequencia
                       NO-LOCK NO-ERROR.
                  RUN pi-manut-peditem (INPUT "CI"). /* Cria novo Item com Reserva */

                  FIND ped-item OF ped-venda WHERE
                       ped-item.it-codigo = ped-item-res.it-codigo AND
                       ped-item.cod-refer = ped-item-res.cod-refer AND
                       ped-item.nr-sequencia = ped-item-res.nr-sequencia
                       NO-LOCK NO-ERROR.
                  RUN pi-manut-peditem (INPUT "AI"). /* Ajusta Item Original */
              END.
           END.
        END.
    END.

    FOR EACH coletor WHERE 
             coletor.id = tt-param.nr-coletor EXCLUSIVE-LOCK.
        DELETE coletor.
    END.
END PROCEDURE.


PROCEDURE pi-grava-avulsa.
    ASSIGN i-nr-seq = 0.
    FOR EACH tt-imp-ped,
        EACH tt-imp-item OF tt-imp-ped NO-LOCK
             BREAK BY tt-imp-ped.nr-pedcli
                   BY tt-imp-item.volume-it
                   BY tt-imp-item.it-codigo
                   BY tt-imp-item.cod-refer.

        FIND ped-item-rom WHERE
             ped-item-rom.nr-ob = tt-imp-item.nr-ob AND
             ped-item-rom.nr-seq-etq = tt-imp-item.sequencia
             NO-LOCK NO-ERROR.
        IF AVAIL ped-item-rom THEN DO.
           PUT STREAM str-rp
               "Erro: Peáa enviada pelo Coletor j† est† Reservada...." SKIP
               " Pedido:" tt-imp-ped.nr-pedcli
               " Cliente:" tt-imp-ped.nome-abrev
               " Item:" tt-imp-item.it-codigo
               " Refer:" tt-imp-item.cod-refer
               " Lote:" tt-imp-item.lote
               " Volume:" tt-imp-item.volume-it SKIP
               " OB:" tt-imp-item.nr-ob
               " Sequencia:" tt-imp-item.sequencia  
               " Num Etiqueta:" tt-imp-item.num-etiqueta
               " Qtd:" tt-imp-item.quantidade SKIP
               " Reservado para:" ped-item-rom.nr-pedcli ped-item-rom.nome-abrev
               SKIP(1).
           NEXT.
        END.

        IF tt-imp-item.erro <> "" THEN DO.
           PUT STREAM str-rp
               tt-imp-item.erro SKIP
               " Pedido:" tt-imp-ped.nr-pedcli
               " Cliente:" tt-imp-ped.nome-abrev
               " Item:" tt-imp-item.it-codigo
               " Refer:" tt-imp-item.cod-refer
               " Lote:" tt-imp-item.lote
               " Volume:" tt-imp-item.volume-it SKIP
               " OB:" tt-imp-item.nr-ob
               " Sequencia:" tt-imp-item.sequencia  
               " Num Etiqueta:" tt-imp-item.num-etiqueta
               " Qtd:" tt-imp-item.quantidade 
               SKIP(1).
           NEXT.
        END.

        FIND b-imp-item WHERE
             b-imp-item.volume-it = tt-imp-item.volume-it NO-LOCK NO-ERROR.

        FIND LAST ped-item-res WHERE
                  ped-item-res.nome-abrev   = tt-imp-ped.nome-abrev AND
                  ped-item-res.nr-pedcli    = tt-imp-ped.nr-pedcli  AND
                  ped-item-res.it-codigo    = tt-imp-item.it-codigo AND
                  ped-item-res.cod-refer    = tt-imp-item.cod-refer AND
                  ped-item-res.lote         = tt-imp-item.lote + tt-imp-item.cod-refer
                  NO-ERROR.    

        IF NOT AVAIL ped-item-res THEN DO:
           ASSIGN i-nr-seq = TRUNCATE(i-nr-seq / 10,0) * 10 + 10.

           FIND ped-item-res WHERE
                ped-item-res.nome-abrev   = tt-imp-ped.nome-abrev AND
                ped-item-res.nr-pedcli    = tt-imp-ped.nr-pedcli  AND
                ped-item-res.it-codigo    = tt-imp-item.it-codigo AND
                ped-item-res.cod-refer    = tt-imp-item.cod-refer AND
                ped-item-res.lote         = tt-imp-item.lote + tt-imp-item.cod-refer AND
                ped-item-res.nr-sequencia = i-nr-seq NO-ERROR.

           IF AVAIL ped-item-res THEN DO.
              ASSIGN nr-ult-seq = i-nr-seq + 1.
              REPEAT.
                   FIND ped-item OF ped-venda WHERE
                        ped-item.it-codigo = tt-imp-item.it-codigo AND
                        ped-item.cod-refer = tt-imp-item.cod-refer AND
                        ped-item.nr-sequencia = nr-ult-seq
                        NO-LOCK NO-ERROR.
                   IF NOT AVAIL ped-item THEN LEAVE.
                   ASSIGN nr-ult-seq = nr-ult-seq + 1.
              END.
              ASSIGN i-nr-seq = nr-ult-seq.
           END.

           CREATE ped-item-res.
           ASSIGN ped-item-res.nome-abrev   = tt-imp-ped.nome-abrev
                  ped-item-res.nr-pedcli    = tt-imp-ped.nr-pedcli
                  ped-item-res.nr-sequencia = i-nr-seq
                  ped-item-res.it-codigo    = tt-imp-item.it-codigo
                  ped-item-res.cod-refer    = tt-imp-item.cod-refer
                  ped-item-res.nome-transp  = tt-imp-ped.nome-transp
                  ped-item-res.volume-ini   = tt-imp-item.volume-it
                  ped-item-res.lote         = tt-imp-item.lote + tt-imp-item.cod-refer
                  ped-item-res.sigla-emb    = tt-imp-item.lote
                  ped-item-res.desc-dentro  = IF tt-imp-item.nr-lote = 1 OR
                                                 tt-imp-item.nr-lote = 3 
                                              THEN "ROLOS"
                                              ELSE "PECAS".
        END.
        ELSE IF (AMBIGUOUS b-imp-item AND 
                 ped-item-res.volume-ini <> tt-imp-item.volume-it) OR
                (ped-item-res.volume-fim  > 0 AND
                 ped-item-res.volume-fim + 1 < tt-imp-item.volume-it) THEN DO.

             ASSIGN i-nr-seq = TRUNCATE(i-nr-seq / 10,0) * 10 + 10.

             FIND ped-item-res WHERE
                  ped-item-res.nome-abrev   = tt-imp-ped.nome-abrev AND
                  ped-item-res.nr-pedcli    = tt-imp-ped.nr-pedcli  AND
                  ped-item-res.it-codigo    = tt-imp-item.it-codigo AND
                  ped-item-res.cod-refer    = tt-imp-item.cod-refer AND
                  ped-item-res.lote         = tt-imp-item.lote + tt-imp-item.cod-refer AND
                  ped-item-res.nr-sequencia = i-nr-seq NO-ERROR.

             IF AVAIL ped-item-res THEN DO.
                ASSIGN nr-ult-seq = i-nr-seq + 1.
                REPEAT.
                     FIND ped-item OF ped-venda WHERE
                          ped-item.it-codigo = tt-imp-item.it-codigo AND
                          ped-item.cod-refer = tt-imp-item.cod-refer AND
                          ped-item.nr-sequencia = nr-ult-seq
                          NO-LOCK NO-ERROR.
                     IF NOT AVAIL ped-item THEN LEAVE.
                     ASSIGN nr-ult-seq = nr-ult-seq + 1.
                END.
                ASSIGN i-nr-seq = nr-ult-seq.
             END.

             CREATE ped-item-res.
             ASSIGN ped-item-res.nome-abrev   = tt-imp-ped.nome-abrev
                    ped-item-res.nr-pedcli    = tt-imp-ped.nr-pedcli
                    ped-item-res.nr-sequencia = i-nr-seq
                    ped-item-res.it-codigo    = tt-imp-item.it-codigo
                    ped-item-res.cod-refer    = tt-imp-item.cod-refer
                    ped-item-res.nome-transp  = tt-imp-ped.nome-transp
                    ped-item-res.volume-ini   = tt-imp-item.volume-it
                    ped-item-res.lote         = tt-imp-item.lote + tt-imp-item.cod-refer
                    ped-item-res.sigla-emb    = tt-imp-item.lote
                    ped-item-res.desc-dentro  = IF tt-imp-item.nr-lote = 1 OR
                                                   tt-imp-item.nr-lote = 3 
                                                THEN "ROLOS"
                                                ELSE "PECAS".
        END.

        ASSIGN ped-item-res.volume-fim = tt-imp-item.volume-it
               ped-item-res.qt-pedida  = ped-item-res.qt-pedida + tt-imp-item.quantidade
               ped-item-res.dt-trans   = tt-param.data-exec
               ped-item-res.hr-trans   = STRING(tt-param.hora-exec,"HH:MM:SS").

        CREATE ped-item-rom.
        ASSIGN ped-item-rom.nome-abrev = ped-item-res.nome-abrev
               ped-item-rom.nr-pedcli = ped-item-res.nr-pedcli
               ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
               ped-item-rom.nr-ob = tt-imp-item.nr-ob
               ped-item-rom.nr-seq-etq = tt-imp-item.sequencia
               ped-item-rom.num-etiqueta = tt-imp-item.num-etiqueta
               ped-item-rom.nr-volume = tt-imp-item.volume-it
               ped-item-rom.quantidade = tt-imp-item.quantidade.

        IF AMBIGUOUS b-imp-item THEN
           ASSIGN ped-item-res.sigla-emb = "FD".

        FIND ob-etiqueta WHERE
             ob-etiqueta.nr-ob = tt-imp-item.nr-ob AND
             ob-etiqueta.nr-seq = tt-imp-item.sequencia NO-ERROR.
        IF NOT AVAIL ob-etiqueta AND 
           tt-imp-item.lote BEGINS 'P' AND
           tt-imp-item.nr-ob < 10000 THEN
           FIND ob-etiqueta WHERE
                ob-etiqueta.nr-ob = tt-imp-item.nr-ob + 100000 AND
                ob-etiqueta.nr-seq = tt-imp-item.sequencia NO-ERROR.

        ASSIGN ob-etiqueta.situacao = 4.

    END.

    FOR EACH tt-imp-ped NO-LOCK,
        EACH ped-item-res WHERE
             ped-item-res.nome-abrev = tt-imp-ped.nome-abrev AND
             ped-item-res.nr-pedcli = tt-imp-ped.nr-pedcli NO-LOCK.

        FIND ped-venda WHERE
             ped-venda.nome-abrev = ped-item-res.nome-abrev AND
             ped-venda.nr-pedcli = ped-item-res.nr-pedcli NO-ERROR.

        IF NOT AVAIL ped-venda THEN NEXT.

        FIND ped-item OF ped-venda WHERE
             ped-item.it-codigo = ped-item-res.it-codigo AND
             ped-item.cod-refer = ped-item-res.cod-refer AND
             ped-item.nr-sequencia = ped-item-res.nr-sequencia
             NO-LOCK NO-ERROR.
        IF NOT AVAIL ped-item THEN 
           RUN pi-manut-peditem (INPUT "CA"). /* Cria novo Item separaá∆o Avulsa */
    END.
END PROCEDURE.


PROCEDURE pi-grava-inventario.
    DEF INPUT PARAMETER p-num-etiqueta LIKE ob-etiqueta.num-etiqueta.
    DEF INPUT PARAMETER p-localiz LIKE ob-etiqueta.localizacao.

    FIND ob-etiqueta WHERE
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
         inv-acab.data-invent = tt-param.dt-invent AND
         inv-acab.num-etiqueta = ob-etiqueta.num-etiqueta NO-ERROR.
    IF AVAIL inv-acab THEN DO.
       PUT STREAM str-rp
           " Num Etiqueta: " p-num-etiqueta
           " Localizacao: "  p-localiz FORMAT "999/999"
           " ****** Jµ INVENTARIADA ******** "
           c-linha FORMAT "x(50)"
           SKIP.
       NEXT.
    END.

    FIND LAST inv-acab WHERE
              inv-acab.data-invent = tt-param.dt-invent AND
              inv-acab.docto = tt-param.nro-docto 
              USE-INDEX indice1 NO-LOCK NO-ERROR.

    ASSIGN i-nr-seq = IF AVAIL inv-acab
                      THEN inv-acab.seq + 1
                      ELSE 0.

    ASSIGN i-nr-seq = i-nr-seq + 1.
    CREATE inv-acab.
    ASSIGN inv-acab.it-codigo    = ob-etiqueta.it-codigo
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


PROCEDURE pi-grava-cancel-res.
    FOR EACH tt-imp-ped,
        EACH tt-imp-item OF tt-imp-ped NO-LOCK
             BREAK BY tt-imp-ped.nr-pedcli
                   BY tt-imp-item.it-codigo
                   BY tt-imp-item.cod-refer.

        RUN pi-acompanhar IN h-acomp (INPUT tt-imp-ped.nome-abrev + " " + 
                                            tt-imp-ped.nr-pedcli).

        IF tt-imp-item.erro <> "" THEN DO.
           PUT STREAM str-rp
               tt-imp-item.erro SKIP
               " Pedido:" tt-imp-ped.nr-pedcli
               " Cliente:" tt-imp-ped.nome-abrev
               " Item:" tt-imp-item.it-codigo
               " Refer:" tt-imp-item.cod-refer
               " Lote:" tt-imp-item.lote
               " Volume:" tt-imp-item.volume-it
               " OB:" tt-imp-item.nr-ob
               " Sequencia:" tt-imp-item.sequencia  
               " Num Etiqueta:" tt-imp-item.num-etiqueta
               " Qtd:" tt-imp-item.quantidade 
               SKIP(1).
           NEXT.
        END.

        FIND ped-item-rom WHERE
             ped-item-rom.nome-abrev = tt-imp-ped.nome-abrev AND
             ped-item-rom.nr-pedcli = tt-imp-ped.nr-pedcli AND
             ped-item-rom.nr-ob = tt-imp-item.nr-ob AND
             ped-item-rom.nr-seq-etq = tt-imp-item.sequencia NO-ERROR.

        IF NOT AVAIL ped-item-rom THEN DO.
           PUT STREAM str-rp
               "Erro: Peáa enviada pelo Coletor N«O est† Reservada para esse Pedido..." SKIP
               " Pedido:" tt-imp-ped.nr-pedcli
               " Cliente:" tt-imp-ped.nome-abrev
               " Item:" tt-imp-item.it-codigo
               " Refer:" tt-imp-item.cod-refer
               " Lote:" tt-imp-item.lote
               " Volume:" tt-imp-item.volume-it SKIP
               " OB:" tt-imp-item.nr-ob
               " Sequencia:" tt-imp-item.sequencia  
               " Num Etiqueta:" tt-imp-item.num-etiqueta
               " Qtd:" tt-imp-item.quantidade 
               SKIP(1).
           NEXT.
        END.

        IF ped-item-rom.quantidade <> tt-imp-item.quantidade THEN DO.
           PUT STREAM str-rp
               "Erro: Quantidade da Peáa Enviada pelo Coletor N«O confere com Romaneio ..." SKIP
               " Pedido:" tt-imp-ped.nr-pedcli
               " Cliente:" tt-imp-ped.nome-abrev
               " Item:" tt-imp-item.it-codigo
               " Refer:" tt-imp-item.cod-refer
               " Lote:" tt-imp-item.lote
               " Volume:" tt-imp-item.volume-it SKIP
               " OB:" tt-imp-item.nr-ob
               " Sequencia:" tt-imp-item.sequencia  
               " Num Etiqueta:" tt-imp-item.num-etiqueta
               " Qtd:" tt-imp-item.quantidade 
               SKIP(1).
           NEXT.
        END.

        FIND ped-item-res WHERE
             ped-item-res.nome-abrev   = ped-item-rom.nome-abrev  AND
             ped-item-res.nr-pedcli    = ped-item-rom.nr-pedcli   AND
             ped-item-res.nr-sequencia = ped-item-rom.nr-sequencia NO-ERROR.

        IF NOT AVAIL ped-item-res THEN DO.
           PUT STREAM str-rp
               "Erro! N∆o existe Reserva para a Sequencia/Item..." SKIP
               " Pedido:" ped-item-res.nr-pedcli
               " Cliente:" ped-item-res.nome-abrev
               " Seq:" ped-item-res.nr-sequencia
               " Item:" ped-item-res.it-codigo
               " Volume:" tt-imp-item.volume-it
               SKIP(1).
            NEXT.
        END.

        IF ped-item-res.faturado THEN DO.
           PUT STREAM str-rp
               "Erro! Reserva para a Sequencia/Item j† esta Faturada..." SKIP
               " Pedido:" ped-item-res.nr-pedcli
               " Cliente:" ped-item-res.nome-abrev
               " Seq:" ped-item-res.nr-sequencia
               " Item:" ped-item-res.it-codigo
               " Volume:" tt-imp-item.volume-it
               SKIP(1).
           NEXT.
        END.

        FIND coletor WHERE
             coletor.id         = tt-param.nr-coletor AND
             coletor.nome-abrev = ped-item-res.nome-abrev AND
             coletor.nr-pedcli  = ped-item-res.nr-pedcli AND
             coletor.it-codigo  = ped-item-res.it-codigo AND
             coletor.cod-refer  = ped-item-res.cod-refer AND
             coletor.nr-sequencia = ped-item-res.nr-sequencia
             NO-LOCK NO-ERROR.

        IF NOT AVAIL coletor THEN DO.
           PUT STREAM str-rp
               "Erro: Registro n∆o encontrado na tabela Coletor..." SKIP
               " Pedido:" tt-imp-ped.nr-pedcli
               " Cliente:" tt-imp-ped.nome-abrev
               " Item:" tt-imp-item.it-codigo
               " Refer:" tt-imp-item.cod-refer
               " Lote:" tt-imp-item.lote
               " Volume:" tt-imp-item.volume-it SKIP
               " OB:" tt-imp-item.nr-ob
               " Sequencia:" tt-imp-item.sequencia  
               " Num Etiqueta:" tt-imp-item.num-etiqueta
               " Qtd:" tt-imp-item.quantidade
               SKIP(1).
           NEXT.
        END.
        ASSIGN ped-item-res.qt-pedida = ped-item-res.qt-pedida - tt-imp-item.quantidade.

        DELETE ped-item-rom.

        FIND ob-etiqueta WHERE
             ob-etiqueta.nr-ob = tt-imp-item.nr-ob AND
             ob-etiqueta.nr-seq = tt-imp-item.sequencia NO-ERROR.
        IF NOT AVAIL ob-etiqueta AND 
           tt-imp-item.lote BEGINS 'P' AND
           tt-imp-item.nr-ob < 10000 THEN
           FIND ob-etiqueta WHERE
                ob-etiqueta.nr-ob = tt-imp-item.nr-ob + 100000 AND
                ob-etiqueta.nr-seq = tt-imp-item.sequencia NO-ERROR.

        ASSIGN ob-etiqueta.situacao = 3.

    END.

    FOR EACH tt-imp-ped,
        EACH coletor WHERE
             coletor.id         = tt-param.nr-coletor AND
             coletor.nome-abrev = tt-imp-ped.nome-abrev AND
             coletor.nr-pedcli  = tt-imp-ped.nr-pedcli NO-LOCK,
        EACH ped-item-res WHERE
             ped-item-res.nome-abrev   = coletor.nome-abrev AND
             ped-item-res.nr-pedcli    = coletor.nr-pedcli  AND
             ped-item-res.it-codigo    = coletor.it-codigo  AND
             ped-item-res.cod-refer    = coletor.cod-refer  AND
             ped-item-res.nr-sequencia = coletor.nr-sequencia NO-LOCK.

        IF ped-item-res.qt-pedida <> 0 THEN DO.
           PUT STREAM str-rp
               "Erro: Quantidade da Reservada N«O foi totalmente cancelada...." SKIP
               " Pedido:" ped-item-res.nr-pedcli
               " Cliente:" ped-item-res.nome-abrev
               " Item:" ped-item-res.it-codigo
               " Refer:" ped-item-res.cod-refer
               " Sequencia:" ped-item-res.nr-sequencia
               " Quantidade:" ped-item-res.qt-pedida  FORMAT "->>>>,>>9.9999"
               SKIP(1).
           NEXT.
        END.
        DELETE ped-item-res.
    END.
END PROCEDURE.


PROCEDURE pi-manut-peditem:
    DEF INPUT PARAMETER p-acao AS CHAR.

    FOR EACH tt-ped-item.
        DELETE tt-ped-item.
    END.

    ASSIGN i-sit-aval = ped-venda.cod-sit-aval
           i-cod-mess = ped-venda.cod-message-alert
           da-dt-mess =  ped-venda.dt-mensagem
           c-desc-for = ped-venda.desc-forc-cr
            l-dsp-fat =  ped-venda.dsp-pre-fat. 

    IF NOT VALID-HANDLE(h-bodi154) or
       h-bodi154:TYPE      <> "PROCEDURE":U OR
       h-bodi154:FILE-NAME <> "dibo/bodi154.p":U THEN
       RUN dibo/bodi154.p PERSISTENT SET h-bodi154.

    IF NOT VALID-HANDLE(h-bodi154com) OR
       h-bodi154com:TYPE      <> "PROCEDURE":U OR
       h-bodi154com:FILE-NAME <> "dibo/bodi154com.p":U THEN
       RUN dibo/bodi159com.p PERSISTENT SET h-bodi154com.

    CASE p-acao.
        WHEN "CA" THEN DO. /* Cria Item separaá∆o Avulsa */
            CREATE tt-ped-item.
            ASSIGN tt-ped-item.nome-abrev = ped-venda.nome-abrev
                   tt-ped-item.nr-pedcli = ped-venda.nr-pedcli
                   tt-ped-item.nr-sequencia = ped-item-res.nr-sequencia
                   tt-ped-item.it-codigo = ped-item-res.it-codigo
                   tt-ped-item.cod-refer = ped-item-res.cod-refer
                   tt-ped-item.qt-pedida = ped-item-res.qt-pedida
                   tt-ped-item.vl-liq-abe = 0.01
                   tt-ped-item.vl-preori = 0.01
                   tt-ped-item.nat-operacao = ped-venda.nat-operacao
                   tt-ped-item.cod-entrega = "Padr∆o"
                   tt-ped-item.dt-entrega = TODAY.
            RUN openQueryStatic IN h-bodi154(INPUT "Main":U).
        END.
        WHEN "CI" THEN DO. /* Cria novo Item */
            CREATE tt-ped-item.
            BUFFER-COPY ped-item TO tt-ped-item 
                   ASSIGN tt-ped-item.nr-sequencia = nr-ult-seq
                          tt-ped-item.qt-pedida = ped-item.qt-pedida - ped-item-res.qt-pedida
                          tt-ped-item.vl-liq-abe = tt-ped-item.qt-pedida * tt-ped-item.vl-preori.

            RUN openQueryStatic IN h-bodi154(INPUT "Main":U).
        END.
        WHEN "AI" THEN DO.  /* Ajusta Item Original */ 
            CREATE tt-ped-item.
            BUFFER-COPY ped-item TO tt-ped-item 
                   ASSIGN tt-ped-item.qt-pedida = ped-item-res.qt-pedida
                          tt-ped-item.vl-liq-abe = tt-ped-item.qt-pedida * tt-ped-item.vl-preori. 

            RUN setConstraintKey IN h-bodi154 (INPUT tt-ped-item.nome-abrev,
                                               INPUT tt-ped-item.nr-pedcli,
                                               INPUT tt-ped-item.nr-sequencia,
                                               INPUT tt-ped-item.it-codigo,
                                               INPUT tt-ped-item.cod-refer).

            RUN openQueryStatic in h-bodi154 (input "Key":U).        
        END.
    END CASE.

    RUN emptyRowErrors IN h-bodi154.
    RUN setRecord IN h-bodi154(INPUT TABLE tt-ped-item).

    IF p-acao BEGINS "C" THEN 
       RUN createRecord IN h-bodi154.
    ELSE
       RUN updateRecord IN h-bodi154.
    
    RUN getRowErrors IN h-bodi154(OUTPUT TABLE RowErrors).
    
    IF CAN-FIND(FIRST RowErrors 
                WHERE RowErrors.ErrorSubType = "ERROR":U  ) THEN DO:
       FOR EACH rowerrors WHERE
                RowErrors.ErrorSubType = "ERROR":U:
           PUT STREAM str-rp
               "Erro ao Gravar o Item no Pedido" SKIP
               " Pedido:" tt-imp-ped.nr-pedcli
               " Cliente:" tt-imp-ped.nome-abrev
               " Item:" tt-imp-item.it-codigo
               " Refer:" tt-imp-item.cod-refer
               " Volume:" tt-imp-item.volume-it
               " Qtd:" tt-imp-item.quantidade SKIP
               "Erro:" rowerrors.errornumber " - " 
               rowerrors.errordescription FORMAT "x(50)"
               SKIP(1).
       END.
    END.
    ELSE DO.
        IF p-acao = "CI" THEN DO. /* Reservas e Item novo */
           FIND b-ped-item-ext OF ped-item NO-LOCK NO-ERROR.
           CREATE ped-item-ext.
           BUFFER-COPY b-ped-item-ext TO ped-item-ext
                       ASSIGN ped-item-ext.nr-sequencia = tt-ped-item.nr-sequencia.
        END.

        IF p-acao = "CA" THEN DO.  /* Item Sep Avulsa */
           FIND ped-item OF tt-ped-item NO-LOCK NO-ERROR.

           CREATE ped-item-ext.
           ASSIGN ped-item-ext.nome-abrev   = ped-item.nome-abrev
                  ped-item-ext.nr-pedcli    = ped-item.nr-pedcli
                  ped-item-ext.nr-sequencia = ped-item-res.nr-sequencia
                  ped-item-ext.it-codigo    = ped-item.it-codigo
                  ped-item-ext.cod-refer    = ped-item.cod-refer
                  ped-item-ext.lote         = SUBSTR(ped-item-res.lote,1,2)
                  ped-item-ext.reservado    = NO.

           IF ped-item-ext.lote BEGINS "R" THEN
              ASSIGN i-tp-embal = 1.
           ELSE
              ASSIGN i-tp-embal = 2.

           FIND corte-comerc WHERE
                corte-comerc.compr-min <= ped-item.qt-pedida AND
                corte-comerc.compr-max >= ped-item.qt-pedida AND
                corte-comerc.tp-embalag = i-tp-embal NO-LOCK NO-ERROR.

           IF AVAIL corte-comerc THEN
              ASSIGN ped-item-ext.acondicionam = corte-comerc.descricao
                     ped-item-ext.corte-comerc = corte-comerc.codigo.
        END.

        /* Completa o Pedido */
        RUN emptyRowErrors IN h-bodi154.
        RUN completeOrder IN h-bodi154com (INPUT ROWID(ped-venda),
                                          OUTPUT TABLE Rowerrors).
        FOR EACH rowerrors WHERE
                 RowErrors.ErrorSubType = "ERROR":U:
            PUT STREAM str-rp
                "Erro ao Completar o Pedido" SKIP
                "Erro:" rowerrors.errornumber " - "
                 rowerrors.errordescription FORMAT "x(50)"
                SKIP(1).
        END.
    END.
    
    FIND ped-item OF tt-ped-item NO-LOCK NO-ERROR.
    IF NOT AVAIL ped-item THEN NEXT.

    FIND ped-venda OF ped-item USE-INDEX ch-pedido NO-ERROR.

    FIND emitente WHERE
         emitente.nome-abrev = ped-venda.nome-abrev NO-LOCK NO-ERROR.

    /* Avaliaá∆o de CrÇdito Autom†tico */
    IF emitente.ind-cre-cli = 2 THEN
       ASSIGN i-sit-aval = 3.

    ASSIGN ped-venda.cod-sit-aval = i-sit-aval
           ped-venda.cod-message-alert = i-cod-mess 
           ped-venda.dt-mensagem = da-dt-mess 
           ped-venda.desc-forc-cr = c-desc-for 
           ped-venda.dsp-pre-fat = l-dsp-fat.

    IF VALID-HANDLE(h-bodi154) THEN
       DELETE PROCEDURE h-bodi154.

    IF VALID-HANDLE(h-bodi154com) THEN
       DELETE PROCEDURE h-bodi154com.

END PROCEDURE.


PROCEDURE pi-busca-seq.
    ASSIGN nr-ult-seq = ped-item.nr-sequencia + 1.
    REPEAT.
        FIND ped-item OF ped-venda WHERE
             ped-item.it-codigo = coletor.it-codigo AND
             ped-item.cod-refer = coletor.cod-refer AND
             ped-item.nr-sequencia = nr-ult-seq
             NO-LOCK NO-ERROR.
        IF NOT AVAIL ped-item THEN LEAVE.

        ASSIGN nr-ult-seq = nr-ult-seq + 1.
    END.
END PROCEDURE.

PROCEDURE pi-ver-volumes.
    FOR EACH tt-imp-ped NO-LOCK,
        EACH tt-imp-item OF tt-imp-ped NO-LOCK
        BREAK BY tt-imp-item.volume-it.
        ACCUMULATE tt-imp-item.sequencia (COUNT BY tt-imp-item.volume-it).

        IF LAST-OF(tt-imp-item.volume-it) THEN DO.
           IF SUBSTR(tt-imp-item.lote,1,1) = "R" AND
              (ACCUM COUNT BY tt-imp-item.volume-it tt-imp-item.sequencia) > 2 THEN DO.
              PUT STREAM str-rp
                  "Erro: Fardo tem mais que DOIS (2) Rolos..." SKIP
                  " Pedido:" tt-imp-ped.nr-pedcli
                  " Cliente:" tt-imp-ped.nome-abrev
                  " Item:" tt-imp-item.it-codigo
                  " Refer:" tt-imp-item.cod-refer
                  " Lote:" tt-imp-item.lote
                  " Volume:" tt-imp-item.volume-it
                  SKIP(1).
           END.

           IF SUBSTR(tt-imp-item.lote,1,1) = "P" AND
              (ACCUM COUNT BY tt-imp-item.volume-it tt-imp-item.sequencia) > 5 THEN DO.
              PUT STREAM str-rp
                  "Erro: Fardo tem mais que CINCO (5) Peáas..." SKIP
                  " Pedido:" tt-imp-ped.nr-pedcli
                  " Cliente:" tt-imp-ped.nome-abrev
                  " Item:" tt-imp-item.it-codigo
                  " Refer:" tt-imp-item.cod-refer
                  " Lote:" tt-imp-item.lote
                  " Volume:" tt-imp-item.volume-it
                  SKIP(1).
           END.
        END.
    END.
END PROCEDURE.

PROCEDURE pi-imp-etq.
    IF SUBSTR(c-linha,1,1) = "&" THEN DO.
       CREATE tt-imp-item.
       ASSIGN tt-imp-item.it-codigo  = SUBSTR(c-linha,2,6)
              tt-imp-item.cod-refer  = SUBSTR(c-linha,8,7)
              tt-imp-item.quantidade = DEC(SUBSTR(c-linha,15,4)) / 10
              tt-imp-item.volume-it  = INT(SUBSTR(c-linha,19,6))
              tt-imp-item.nr-ob      = INT(SUBSTR(c-linha,25,5))
              tt-imp-item.sequencia  = INT(SUBSTR(c-linha,30,3))
              tt-imp-item.nr-lote    = INT(SUBSTR(c-linha,33,1)).

       ASSIGN tt-imp-item.lote = IF tt-imp-item.nr-lote = 1 THEN "RP"
                                 ELSE IF tt-imp-item.nr-lote = 2 THEN "PP"
                                      ELSE IF tt-imp-item.nr-lote = 3 THEN "RD"
                                           ELSE IF tt-imp-item.nr-lote = 4 THEN "PD"
                                                ELSE "".
    END.
END PROCEDURE.

PROCEDURE pi-grava-etq.
    DEF VAR c-form-epl   AS CHAR FORMAT "x(30)".
    DEF VAR c-prog-epl   AS CHAR FORMAT "x(50)".
    DEF VAR i-ct         AS INT.
    DEF VAR c-desc-item  AS CHAR FORMAT "x(33)".
    DEF VAR c-composicao LIKE composi.descricao EXTENT 3.
    DEF VAR v-defeito    AS CHAR EXTENT 3.
    DEF VAR i-lote       AS INT.
    DEF VAR c-qualid     AS CHAR.
    DEF VAR c-comando    AS CHAR.
    DEF VAR c-code-ant   AS CHAR.
    DEF VAR i-nr-seq     LIKE ob-etiqueta.nr-sequencia.

    ASSIGN c-prog-epl = SESSION:TEMP-DIRECTORY + "etq-fin.epl"
           c-form-epl = "n:\especificos\etiqueta\form-etq.epl".

    FOR EACH tt-imp-item.
        FIND ob-etiqueta WHERE
             ob-etiqueta.nr-ob = tt-imp-item.nr-ob AND
             ob-etiqueta.nr-seq = tt-imp-item.sequencia NO-ERROR.
        
        IF NOT AVAIL ob-etiqueta AND
           tt-imp-item.lote BEGINS 'P' AND
           tt-imp-item.nr-ob < 10000 THEN
           FIND ob-etiqueta WHERE
                ob-etiqueta.nr-ob = tt-imp-item.nr-ob + 100000 AND
                ob-etiqueta.nr-seq = tt-imp-item.sequencia NO-ERROR.

        IF NOT AVAIL ob-etiqueta THEN DO.
           PUT STREAM str-rp
                "Erro: Etiqueta N∆o Cadastrada no Sistema..." SKIP
                " OB:" tt-imp-item.nr-ob
                " Sequencia:" tt-imp-item.sequencia  
                " Num Etiqueta:" ob-etiqueta.num-etiqueta 
                SKIP(1).
            NEXT.
        END.

        IF ob-etiqueta.num-etiqueta <> 0 THEN DO.
           PUT STREAM str-rp
               "Erro: Etiqueta j† est† com N£mero Novo..." SKIP
               " OB:" tt-imp-item.nr-ob
               " Sequencia:" tt-imp-item.sequencia  
               " Num Etiqueta:" ob-etiqueta.num-etiqueta 
               SKIP(1).
           NEXT.
        END.

        IF ob-etiqueta.nr-lote BEGINS "R" THEN
           ASSIGN i-tp-embal = 1.
        ELSE
           ASSIGN i-tp-embal = 2.

        FIND corte-comerc WHERE
             corte-comerc.compr-min <= ob-etiqueta.quantidade AND
             corte-comerc.compr-max >= ob-etiqueta.quantidade AND
             corte-comerc.tp-embalag = i-tp-embal NO-LOCK NO-ERROR.

        IF NOT AVAIL corte-comerc THEN DO.
           PUT STREAM str-rp
                "Erro: Corte Comercail Invalido para a Etiqueta..." SKIP
                " OB:" tt-imp-item.nr-ob
                " Sequencia:" tt-imp-item.sequencia  
                " Quantidade:" ob-etiqueta.quantidade
                SKIP(1).
            NEXT.
        END.

        IF SUBSTR(tt-imp-item.lote,2,1) = "D" THEN 
           ASSIGN c-qualid = 'D'.

        IF SUBSTR(tt-imp-item.lote,2,1) = "P" THEN DO.
           FIND FIRST mov-est-acbd WHERE
                      mov-est-acbd.data-mov = ob-etiqueta.dt-emissao AND
                      mov-est-acbd.num-lote = ob-etiqueta.nr-ob AND
                      mov-est-acbd.nr-carro = ob-etiqueta.nr-carro AND
                      mov-est-acbd.acondic  = ob-etiqueta.acondic AND
                      mov-est-acbd.nr-sequencia = ob-etiqueta.nr-sequencia AND
                      mov-est-acbd.classif = "RG" NO-LOCK NO-ERROR. 
           IF AVAIL mov-est-acbd THEN
              ASSIGN c-qualid = "C".
           ELSE
              ASSIGN c-qualid = "B".
        END.

        ASSIGN ob-etiqueta.num-etiqueta = NEXT-VALUE(seq-etq-estoq)
               ob-etiqueta.situacao = 3
               ob-etiqueta.acondic = corte-comerc.descricao
               ob-etiqueta.cod-qualid = c-qualid. 

        FIND ped-item-rom WHERE
             ped-item-rom.nr-ob = ob-etiqueta.nr-ob AND
             ped-item-rom.nr-seq-etq = ob-etiqueta.nr-sequencia NO-ERROR.

        IF AVAIL ped-item-rom THEN 
           DELETE ped-item-rom.

        FIND ITEM WHERE
             ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

        IF NOT AVAIL ITEM THEN DO.
           PUT STREAM str-rp
               "Erro: Item n∆o Cadastrado para a Etiqueta..." SKIP
               " OB:" tt-imp-item.nr-ob
               " Sequencia:" tt-imp-item.sequencia  
               " Item:" ob-etiqueta.it-codigo
               SKIP(1).
           NEXT.
        END.

        ASSIGN c-desc-item = ITEM.descricao-1 + TRIM(ITEM.descricao-2).
    
        IF ITEM.tipo-con-est = 4 THEN DO.
           FIND referencia WHERE
                referencia.cod-refer = ob-etiqueta.cod-refer NO-LOCK NO-ERROR.
    
           FIND FIRST ref-item-ext WHERE 
                      ref-item-ext.it-codigo = ob-etiqueta.it-codigo AND 
                      ref-item-ext.cod-refer = ob-etiqueta.cod-refer NO-LOCK NO-ERROR.
    
           IF AVAIL ref-item-ext THEN
              ASSIGN c-desc-item = c-desc-item + " " + referencia.descricao.
        END.

        FIND FIRST item-ext WHERE
                   item-ext.it-codigo = ITEM.it-codigo NO-LOCK NO-ERROR.
    
        IF AVAIL item-ext THEN DO.
           FIND FIRST composi WHERE
                      composi.cod-composi = item-ext.cod-composi NO-LOCK NO-ERROR.
    
           ASSIGN c-composicao = "".
           IF AVAIL composi THEN DO.
              DO i-ct = 1 TO NUM-ENTRIES(composi.descricao).
                 ASSIGN c-composicao[INT(i-ct / 2)] = c-composicao[INT(i-ct / 2)] + ENTRY(i-ct,composi.descricao).
              END.
           END.
        END.

        ASSIGN i-ct = 0
               v-defeito = "".
        FOR EACH mov-est-acbd WHERE
                 mov-est-acbd.data-mov = ob-etiqueta.dt-emissao AND
                 mov-est-acbd.num-lote = ob-etiqueta.nr-ob AND
                 mov-est-acbd.nr-carro = ob-etiqueta.nr-carro AND
                 mov-est-acbd.acondic  = ob-etiqueta.acondic AND
                 mov-est-acbd.nr-sequencia = ob-etiqueta.nr-sequencia AND
                 mov-est-acbd.classif = "LD" NO-LOCK. 
    
            ASSIGN i-ct = i-ct + 1.
            IF i-ct > 3 THEN LEAVE.
            IF v-defeito[i-ct] = "" THEN DO.
               ASSIGN v-defeito[i-ct] = mov-est-acbd.cod-tipo-def + "   " + mov-est-acbd.cod-defeito.
            END.
        END.
        ASSIGN v-defeito[3] = " E ".

        FIND qualid-tecido WHERE
             qualid-tecido.codigo = ob-etiqueta.cod-qualid NO-LOCK NO-ERROR.

        IF NOT AVAIL qualid-tecido THEN DO.
           PUT STREAM str-rp
               "Erro: Qualidade do Tecido n∆o Cadastrada..." SKIP
               " OB:" tt-imp-item.nr-ob
               " Sequencia:" tt-imp-item.sequencia  
               " Qualidade:" ob-etiqueta.cod-qualid
               SKIP(1).
            NEXT.
        END.

        CASE ob-etiqueta.nr-lote.
            WHEN "RP" THEN ASSIGN i-lote = 1.
            WHEN "PP" THEN ASSIGN i-lote = 2.
            WHEN "RD" THEN ASSIGN i-lote = 3.
            WHEN "PD" THEN ASSIGN i-lote = 4.
        END CASE.

        ASSIGN c-code-ant = TRIM(ob-etiqueta.it-codigo) + 
                            STRING(INT(ob-etiqueta.cod-refer),"9999999") +
                            STRING(ob-etiqueta.quantidade * 10,"9999") +
                            STRING(ob-etiqueta.nr-ob,"99999") + 
                            STRING(ob-etiqueta.nr-sequencia,"999") + 
                            STRING(i-lote,"9").

        ASSIGN i-num-bar = INT(STRING(ob-etiqueta.num-etiqueta) + fn-calc-digito(INPUT STRING(ob-etiqueta.num-etiqueta,"999999999"))).

        OS-COPY VALUE(c-form-epl) VALUE(c-prog-epl).

        OUTPUT STREAM s-etq TO VALUE(c-prog-epl) APPEND.
           PUT STREAM s-etq UNFORMATTED 
               "B180,50,1,2,2,5,75,N," '"' c-code-ant '"' SKIP
               "A220,60,0,1,3,4,N," '"' TRIM(ob-etiqueta.it-codigo) '"' SKIP
               "A420,60,0,1,2,4,N," '"' TRIM(SUBSTR(ob-etiqueta.cod-refer,1,2)) TRIM(SUBSTR(ob-etiqueta.cod-refer,3,4)) '"' SKIP
               "A640,60,0,1,2,4,N," '"' TRIM(SUBSTR(ob-etiqueta.cod-refer,7,1)) '"' SKIP
               "A220,130,0,2,1,1,N," '"' TRIM(SUBSTR(c-desc-item,1,40)) '"' SKIP
               IF qualid-tecido.impr-tarja THEN "LE210,125,480,30" ELSE "" SKIP
               "A220,200,0,2,1,1,N," '"' STRING(ob-etiqueta.nr-ob,">>>>>9") '"' SKIP
               "A340,200,0,2,1,1,N," '"' IF AVAIL item-ext THEN STRING(item-ext.largura,"9.99") ELSE "" '"' SKIP
               "A430,200,0,2,1,1,N," '"' STRING(ob-etiqueta.quantidade,">>9.99") '"' SKIP
               "A550,200,0,2,1,1,N," '"' STRING(ob-etiqueta.nr-cortes,">9") '"' SKIP
               "A640,190,0,1,3,3,N," '"' TRIM(ob-etiqueta.nuance) '"' SKIP
               IF qualid-tecido.class-qualid = 2 THEN "LE610,170,80,135" ELSE "" SKIP
               "A220,265,0,2,1,1,N," '"' TRIM(c-composicao[1]) '"' SKIP
               "A220,285,0,2,1,1,N," '"' TRIM(c-composicao[2]) '"' SKIP
               "A610,270,0,2,1,1,N," '"' TRIM(ob-etiqueta.acondic) '"' SKIP
               "A240,330,0,4,3,4,N," '"' STRING(ob-etiqueta.num-etiqueta,"999999999") '"' SKIP
               "B295,430,0,1,3,7,60,N," '"' STRING(i-num-bar,"9999999999") '"' SKIP
               "A620,520,0,2,1,1,N," '"' TRIM(v-defeito[1]) '"' SKIP
               "A620,540,0,2,1,1,N," '"' TRIM(v-defeito[2]) '"' SKIP
               "A620,570,0,1,2,2,N," '"' TRIM(v-defeito[3]) '"' SKIP.
    
           IF AVAIL item-ext THEN DO. /*Escolhendo a imagem para jogar na etiqueta*/
              CASE item-ext.cod-rlgp:
                   WHEN 1 THEN
                      PUT STREAM s-etq UNFORMATTED
                          "GG260,535," '"imag0204"' SKIP
                          "GG310,530," '"imag0102"' SKIP 
                          "GG370,530," '"imag0302"' SKIP 
                          "GG425,535," '"imag0402"' SKIP 
                          "GG480,530," '"imag0604"' SKIP. 
                   WHEN 2 THEN
                      PUT STREAM s-etq UNFORMATTED
                          "GG260,535," '"imag0204"' SKIP
                          "GG310,530," '"imag0102"' SKIP 
                          "GG370,530," '"imag0303"' SKIP 
                          "GG425,535," '"imag0402"' SKIP 
                          "GG480,530," '"imag0604"' SKIP. 
                   WHEN 3 THEN 
                      PUT STREAM s-etq UNFORMATTED
                          "GG260,535," '"imag0206"' SKIP
                          "GG310,530," '"imag0102"' SKIP 
                          "GG370,530," '"imag0303"' SKIP 
                          "GG425,535," '"imag0402"' SKIP 
                          "GG480,530," '"imag0604"' SKIP. 
                   WHEN 4 THEN                           
                      PUT STREAM s-etq UNFORMATTED
                          "GG260,535," '"imag0203"' SKIP
                          "GG310,530," '"imag0102"' SKIP 
                          "GG370,530," '"imag0301"' SKIP 
                          "GG425,535," '"imag0503"' SKIP 
                          "GG480,530," '"imag0604"' SKIP. 
              END CASE.
           END.
    
           PUT STREAM s-etq UNFORMATTED
               "P1" SKIP.
        OUTPUT STREAM s-etq CLOSE.

        ASSIGN c-comando = "copy /Y /b " + c-prog-epl + " lpt1". 
        OS-COMMAND SILENT VALUE(c-comando). 

    END.
END PROCEDURE.

PROCEDURE pi-grava-localiz.

END PROCEDURE.
