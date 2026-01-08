/* Programa: ESSP0186.W
** Modulo..: Controle de Expediá∆o
** Objetivo: Gerar a POSIÄ«O DE ESTOQUE DISPONIVEL, e enviar emails para
**           representante com 7 relatorios anexados.
** Autor...: F†bio Coelho Lanza - ABRIL/2009
** Obs.....: Especifico da TEAR TEXTIL IND.COM.LTDA.
**
*/
/* include de controle de vers∆o */
{include/i-prgvrs.i ESSP086RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino          AS INTEGER
       FIELD arquivo          AS CHAR FORMAT "x(35)"
       FIELD usuario          AS CHAR FORMAT "x(12)"
       FIELD data-exec        AS DATE
       FIELD hora-exec        AS INTEGER
       FIELD classifica       AS INTEGER
       FIELD desc-classifica  AS CHAR FORMAT "x(40)"
       FIELD dt-limite        AS CHAR
       FIELD enviar-e-mail    AS LOG FORMAT "Sim/N∆o"
       FIELD l-batch          AS LOG.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

DEF TEMP-TABLE tt-itens
    FIELD cod-estabel  LIKE ob-etiqueta.cod-estabel
    FIELD it-codigo    LIKE ob-etiqueta.it-codigo
    FIELD cod-refer    LIKE ob-etiqueta.cod-refer
    FIELD lote         LIKE ob-etiqueta.nr-lote
    FIELD qt-estoque   LIKE ob-etiqueta.quantidade
    FIELD qt-pedida    LIKE ob-etiqueta.quantidade
    FIELD qt-benefic   LIKE ob-etiqueta.quantidade
    FIELD visualiza    AS   LOG INIT NO
    INDEX indice1 IS PRIMARY it-codigo cod-refer lote.
    
DEF TEMP-TABLE tt-movto
    FIELD row-tt-itens   AS ROWID
    FIELD corte-comerc   LIKE ob-etiqueta.corte-comerc
    FIELD qt-estoque     LIKE ob-etiqueta.quantidade   LABEL "Estoque"
    FIELD qt-res-antc    LIKE ob-etiqueta.quantidade   LABEL "Res.Antec."
    FIELD qt-ped-reserva LIKE ob-etiqueta.quantidade   LABEL "Ped.Reserva"
    FIELD qt-trf         AS   DEC FORMAT "->>>,>>9.99" LABEL "Transform"
    FIELD qt-benefic     LIKE ob-etiqueta.quantidade   LABEL "Benefic"
    FIELD qt-carteira    LIKE ped-item.qt-pedida       LABEL "Carteira"
    FIELD qt-res-cart    LIKE ob-etiqueta.quantidade   LABEL "Cart.Forn."
    FIELD qt-saldo       LIKE saldo-estoq.qtidade-atu  LABEL "SALDO"
    FIELD visualiza      AS   LOG INIT NO
    INDEX indice1 IS PRIMARY row-tt-itens corte-comerc.

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

/* include padr∆o para impress∆o de campos editores em relat¢rio  */
{include/tt-edit.i}

/* definiá∆o de vari†veis  */
DEF NEW GLOBAL SHARED VAR c-seg-usuario AS CHAR NO-UNDO.

DEF VAR h-acomp        AS HANDLE NO-UNDO.
DEF VAR r-tt-movto     AS ROWID.
DEF VAR c-work         AS CHAR.

DEF VAR c-dt-limite    AS CHAR FORMAT "99/9999".
DEF VAR da-dt-entrega  AS DATE FORMAT "99/99/9999".
DEF VAR l-dep-corte    AS LOG INITIAL NO.
DEF VAR c-lotes        AS CHAR FORMAT "x(18)".
DEF VAR c-dia          AS CHAR.
DEF VAR i-lin          AS INT.
DEF VAR i-pag          AS INT.

DEF VAR c-mensagem     AS CHAR.
DEF VAR c-remetente    AS CHAR.
DEF VAR c-destinatario LIKE param-dis.destinatario.
DEF VAR c-arq-anexo    as char.
DEF VAR l-prim-vez     AS LOG INIT YES.

/* bloco principal do programa */
find first param-global no-lock no-error.
find first empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

RUN esapi/ret-udm.p (INPUT tt-param.dt-limite, OUTPUT c-dia).
ASSIGN da-dt-entrega = DATE(c-dia + SUBSTR(tt-param.dt-limite,1,2) + SUBSTR(tt-param.dt-limite,3,4)).

IF NOT tt-param.l-batch THEN DO.
   run utp/ut-acomp.p persistent set h-acomp.
   {utp/ut-liter.i Imprimindo *}
   run pi-inicializar in h-acomp (input RETURN-VALUE).
END.

/* Altera parÉmetro, quando execuá∆o for em batch */
IF tt-param.l-batch THEN
   ASSIGN tt-param.enviar-e-mail = YES.

/* Processa a 1ß Relatorio */
ASSIGN c-arq-anexo = SESSION:TEMP-DIRECTORY + "ITENS DISPONIVEIS - LANCAMENTO E PRODUCAO - DATA " + STRING(TODAY,"99-99-9999") + ".TXT".
RUN pi-processa (INPUT YES,     /* PP */
                 INPUT YES,     /* PD */
                 INPUT YES,     /* RP */
                 INPUT YES,     /* RD */
                 INPUT NO,      /* SC */
                 INPUT NO,      /* CA */
                 INPUT YES,     /* Cod Obsoleto 0 - Lanáamento */
                 INPUT NO,      /* Cod Obsoleto 1 - Fora Produá∆o */
                 INPUT YES,     /* Cod Obsoleto 2 - Em Produá∆o */
                 INPUT NO,      /* Cod Obsoleto 3 - Retalho */
                 INPUT NO,      /* Cod Obsoleto 4 - Exclusividade */
                 INPUT NO,      /* Cod Obsoleto 5 - Exportaá∆o */
                 INPUT "A",     /* Tipo Artigo: Ambos */
                 NO).           /* Considerar Rolos do Deposito Corte de Amostras */

RUN pi-gera-rel (INPUT "÷TENS DISPON÷VEIS - LANÄAMENTO E PRODUÄ«O - DATA " + STRING(TODAY,"99-99-9999")).

/* Processa a 2ß Relatorio */
ASSIGN c-arq-anexo = SESSION:TEMP-DIRECTORY + "ITENS DISPONIVEIS - FORA PROD RET EXCL EXPORT - DATA " + STRING(TODAY,"99-99-9999") + ".TXT".
RUN pi-processa (INPUT YES,     /* PP */
                 INPUT YES,     /* PD */
                 INPUT YES,     /* RP */
                 INPUT YES,     /* RD */
                 INPUT NO,      /* SC */
                 INPUT NO,      /* CA */
                 INPUT NO,      /* Cod Obsoleto 0 - Lanáamento */
                 INPUT YES,     /* Cod Obsoleto 1 - Fora Produá∆o */
                 INPUT NO,      /* Cod Obsoleto 2 - Em Produá∆o */
                 INPUT YES,     /* Cod Obsoleto 3 - Retalho */
                 INPUT YES,     /* Cod Obsoleto 4 - Exclusividade */
                 INPUT YES,     /* Cod Obsoleto 5 - Exportaá∆o */
                 INPUT "A",     /* Tipo Artigo: Ambos */
                 NO).           /* Considerar Rolos do Deposito Corte de Amostras */

RUN pi-gera-rel (INPUT "÷TENS DISPON÷VEIS - FORA PROD/RET/EXCL/EXPORT - DATA " + STRING(TODAY,"99-99-9999")).

IF NOT tt-param.l-batch THEN
   run pi-finalizar in h-acomp.

/* Dispara Emails para Representantes */
IF tt-param.enviar-e-mail = YES  THEN DO.
   ASSIGN c-remetente = "teartextil@teartextil.com.br".
   FOR EACH repres NO-LOCK:
       IF repres.e-mail <> "" THEN DO:
          ASSIGN c-destinatario = repres.e-mail + "," + "teartextil@teartextil.com.br".

          ASSIGN c-mensagem = "Prezado Representante, " + CHR(13) + 
                              "Em anexo arquivos com a nossa posiá∆o de estoque em " + STRING(TODAY,"99-99-9999") + "." + CHR(13) + CHR(13) +
                              "Atenciosamente, " + CHR(13) + CHR(13)  +
                              "DEPARTAMENTO COMERCIAL" + CHR(13) +
                              "Tear Textil Ind Com Ltda" + CHR(13) +
                              "(" + SESSION:TEMP-DIRECTORY + ")".
          ASSIGN c-arq-anexo = SESSION:TEMP-DIRECTORY + "ITENS DISPONIVEIS - LANCAMENTO E PRODUCAO - DATA "     + STRING(TODAY,"99-99-9999") + ".TXT," +
                               SESSION:TEMP-DIRECTORY + "ITENS DISPONIVEIS - FORA PROD RET EXCL EXPORT - DATA " + STRING(TODAY,"99-99-9999") + ".TXT". 

          RUN esapi/esapi002.p (INPUT c-remetente,                         /* e-mail remetente */
                                INPUT c-destinatario,                      /* e-mail destinat†rio */
                                INPUT "Estoque Disponivel em " + STRING(TODAY,"99-99-9999") + " - " + repres.nome-abrev, /* Assunto */
                                INPUT c-mensagem,                          /* Mensagem */
                                INPUT c-arq-anexo,                         /*arquivo anexo*/
                                INPUT YES).                                /* Mostra Erros */ 

          IF l-prim-vez THEN DO: /* Pessoal interno da Empresa */
             FIND FIRST espec.param-dis NO-LOCK NO-ERROR.
             ASSIGN l-prim-vez     = NO
                    c-destinatario = param-dis.emails-disp + "," + "teartextil@teartextil.com.br".
             RUN esapi/esapi002.p (INPUT c-remetente,                         /* e-mail remetente */
                                   INPUT c-destinatario,                      /* e-mail destinat†rio */
                                   INPUT "Estoque Disponivel em " + STRING(TODAY,"99-99-9999") + " - Interno", /* Assunto */
                                   INPUT c-mensagem,                          /* Mensagem */
                                   INPUT c-arq-anexo,                         /*arquivo anexo*/
                                   INPUT YES).                                /* Mostra Erros */ 
          END.
          
       END.
   END.
 
   /* Mata os Arquivos Gerados */
   OS-DELETE VALUE(SESSION:TEMP-DIRECTORY + "ITENS DISPONIVEIS - LANCAMENTO E PRODUCAO - DATA "     + STRING(TODAY,"99-99-9999") + ".TXT").
   OS-DELETE VALUE(SESSION:TEMP-DIRECTORY + "ITENS DISPONIVEIS - FORA PROD RET EXCL EXPORT - DATA " + STRING(TODAY,"99-99-9999") + ".TXT").
END.

IF tt-param.l-batch = NO THEN DO:
   MESSAGE "Os Arquivos de Estoque Disponivel Foram Gerados em: " SKIP(1)
           UPPER(SESSION:TEMP-DIRECTORY) + "ITENS DISPONIVEIS - LANCAMENTO E PRODUCAO - DATA "     + STRING(TODAY,"99-99-9999") + ".TXT" SKIP
           UPPER(SESSION:TEMP-DIRECTORY) + "ITENS DISPONIVEIS - FORA PROD RET EXCL EXPORT - DATA " + STRING(TODAY,"99-99-9999") + ".TXT"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.


/* P  R  O  C  E  D  I  M  E  N  T  O  S */
/* ------------------------------------- */
PROCEDURE pi-processa.

  DEF INPUT PARAMETER p-pp               AS LOG.
  DEF INPUT PARAMETER p-pd               AS LOG.
  DEF INPUT PARAMETER p-rp               AS LOG.
  DEF INPUT PARAMETER p-rd               AS LOG.
  DEF INPUT PARAMETER p-sc               AS LOG.
  DEF INPUT PARAMETER p-ca               AS LOG.
  DEF INPUT PARAMETER p-cod-obsoleto-0   AS LOG.
  DEF INPUT PARAMETER p-cod-obsoleto-1   AS LOG.
  DEF INPUT PARAMETER p-cod-obsoleto-2   AS LOG.
  DEF INPUT PARAMETER p-cod-obsoleto-3   AS LOG.
  DEF INPUT PARAMETER p-cod-obsoleto-4   AS LOG.
  DEF INPUT PARAMETER p-cod-obsoleto-5   AS LOG.
  DEF INPUT PARAMETER p-tipo-artigo      AS CHAR.
  DEF INPUT PARAMETER p-dep-corte        AS LOG.
 
  FOR EACH tt-movto.
      DELETE tt-movto.
  END.

  FOR EACH tt-itens.
      DELETE tt-itens.
  END.
  ASSIGN c-lotes = "".
  ASSIGN c-lotes = c-lotes + IF p-pp = YES THEN "PP," ELSE ",".
  ASSIGN c-lotes = c-lotes + IF p-pd = YES THEN "PD," ELSE ",".
  ASSIGN c-lotes = c-lotes + IF p-rp = YES THEN "RP," ELSE ",".
  ASSIGN c-lotes = c-lotes + IF p-rd = YES THEN "RD," ELSE ",".
  ASSIGN c-lotes = c-lotes + IF p-sc = YES THEN "SC," ELSE ",".
  ASSIGN c-lotes = c-lotes + IF p-ca = YES THEN "CA," ELSE ",".

  FOR EACH ob-etiqueta WHERE
           LOOKUP(STRING(ob-etiqueta.situacao,"9"),"3,4")  > 0 AND
           LOOKUP(ob-etiqueta.nr-lote,c-lotes)            <> 0 NO-LOCK:

      IF NOT tt-param.l-batch THEN
         RUN pi-acompanhar IN h-acomp (INPUT "Etiqueta: " + STRING(ob-etiqueta.num-etiqueta)).

      FIND item WHERE
           item.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

      IF NOT p-dep-corte AND
         ob-etiqueta.localizacao BEGINS '7' AND
         ob-etiqueta.localizacao <= '700099' 
         THEN NEXT.

      FIND item-ext WHERE
           item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

      IF p-tipo-artigo <> 'A' AND AVAIL item-ext THEN
          IF (item-ext.indigo = YES AND p-tipo-artigo <> "I") OR
             (item-ext.indigo = NO  AND p-tipo-artigo <> "O") THEN NEXT.

      FIND ref-item-ext WHERE
           ref-item-ext.it-codigo = ob-etiqueta.it-codigo AND
           ref-item-ext.cod-refer = ob-etiqueta.cod-refer NO-LOCK NO-ERROR.
      IF NOT AVAIL ref-item-ext THEN NEXT.

      IF AVAIL ref-item-ext THEN DO:
         IF (ref-item-ext.cod-obsoleto = '0' AND p-cod-obsoleto-0 = NO) OR
            (ref-item-ext.cod-obsoleto = '1' AND p-cod-obsoleto-1 = NO) OR
            (ref-item-ext.cod-obsoleto = '2' AND p-cod-obsoleto-2 = NO) OR
            (ref-item-ext.cod-obsoleto = '3' AND p-cod-obsoleto-3 = NO) OR
            (ref-item-ext.cod-obsoleto = '4' AND p-cod-obsoleto-4 = NO) OR
            (ref-item-ext.cod-obsoleto = '5' AND p-cod-obsoleto-5 = NO) THEN NEXT.
      END.

      FIND tt-itens WHERE
           tt-itens.cod-estabel = ob-etiqueta.cod-estabel AND
           tt-itens.it-codigo = ob-etiqueta.it-codigo AND
           tt-itens.cod-refer = ob-etiqueta.cod-refer AND
           tt-itens.lote = ob-etiqueta.nr-lote        NO-LOCK NO-ERROR.
      IF NOT AVAIL tt-itens THEN DO.
         CREATE tt-itens.
         ASSIGN tt-itens.cod-estabel = ob-etiqueta.cod-estabel
                tt-itens.it-codigo = ob-etiqueta.it-codigo 
                tt-itens.cod-refer = ob-etiqueta.cod-refer
                tt-itens.lote      = ob-etiqueta.nr-lote.
      END.
      ASSIGN tt-itens.qt-estoque = tt-itens.qt-estoque + ob-etiqueta.quantidade.

      FIND tt-movto WHERE
           tt-movto.row-tt-itens = ROWID(tt-itens) AND
           tt-movto.corte-comerc = ob-etiqueta.corte-comerc  NO-LOCK NO-ERROR.
      IF NOT AVAIL tt-movto THEN DO.
         CREATE tt-movto.
         ASSIGN tt-movto.row-tt-itens = ROWID(tt-itens)
                tt-movto.corte-comerc = ob-etiqueta.corte-comerc.
      END.
      ASSIGN tt-movto.qt-estoque = tt-movto.qt-estoque + ob-etiqueta.quantidade
             tt-movto.qt-saldo = tt-movto.qt-saldo + ob-etiqueta.quantidade.
               
      IF ob-etiqueta.situacao = 4 THEN DO. /* Etiqueta Reservada */
         FIND ped-item-rom WHERE
              ped-item-rom.cod-estabel  = ob-etiqueta.cod-estabel AND
              ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK NO-ERROR.
         IF AVAIL ped-item-rom THEN DO.
            FIND ped-venda WHERE
                 ped-venda.nr-pedcli  = ped-item-rom.nr-pedcli  AND
                 ped-venda.nome-abrev = ped-item-rom.nome-abrev NO-LOCK NO-ERROR.

            FIND ped-item WHERE
                 ped-item.nr-pedcli    = ped-item-rom.nr-pedcli    AND
                 ped-item.nome-abrev   = ped-item-rom.nome-abrev   AND
                 ped-item.nr-sequencia = ped-item-rom.nr-sequencia NO-LOCK NO-ERROR.
            IF NOT AVAIL ped-item THEN NEXT.

            IF ped-venda.dt-entrega > da-dt-entrega THEN DO.
               ASSIGN tt-movto.qt-res-antc = tt-movto.qt-res-antc + ob-etiqueta.quantidade
                      tt-movto.qt-saldo    = tt-movto.qt-saldo    - ob-etiqueta.quantidade.
               NEXT.
            END.
            ASSIGN tt-movto.qt-res-cart = tt-movto.qt-res-cart + ob-etiqueta.quantidade.

         END.
      END.
  END.
                 
  RUN pi-carteira (INPUT p-cod-obsoleto-0,
                   INPUT p-cod-obsoleto-1,
                   INPUT p-cod-obsoleto-2,
                   INPUT p-cod-obsoleto-3,
                   INPUT p-cod-obsoleto-4,
                   INPUT p-cod-obsoleto-5,
                   INPUT p-tipo-artigo,
                   INPUT c-lotes).

  RUN pi-reserva (INPUT p-cod-obsoleto-0,
                  INPUT p-cod-obsoleto-1,
                  INPUT p-cod-obsoleto-2,
                  INPUT p-cod-obsoleto-3,
                  INPUT p-cod-obsoleto-4,
                  INPUT p-cod-obsoleto-5,
                  INPUT p-tipo-artigo,
                  INPUT c-lotes).     

  RUN pi-trf  (INPUT p-cod-obsoleto-0,
               INPUT p-cod-obsoleto-1,
               INPUT p-cod-obsoleto-2,
               INPUT p-cod-obsoleto-3,
               INPUT p-cod-obsoleto-4,
               INPUT p-cod-obsoleto-5,
               INPUT p-tipo-artigo,
               INPUT c-lotes).     

  RUN pi-pcp.

  FOR EACH tt-movto.
      FIND corte-comerc WHERE
           corte-comerc.codigo = tt-movto.corte-comerc 
           NO-LOCK NO-ERROR.
       
      IF AVAIL corte-comerc AND ABS(tt-movto.qt-saldo) < corte-comerc.compr-min THEN
        ASSIGN tt-movto.qt-saldo = 0.
  END.

  FOR EACH tt-itens.
      ASSIGN tt-itens.visualiza = NO.
      FOR EACH tt-movto WHERE
               tt-movto.row-tt-itens = ROWID(tt-itens).
          ASSIGN tt-movto.visualiza = NO.
          IF tt-movto.qt-saldo > 0 THEN DO.
             ASSIGN tt-movto.visualiza = YES
                    tt-itens.visualiza = YES.
          END.
      END.
  END.

END PROCEDURE.

PROCEDURE pi-carteira.

  DEF INPUT PARAMETER p-cod-obsoleto-0 AS LOG.
  DEF INPUT PARAMETER p-cod-obsoleto-1 AS LOG.
  DEF INPUT PARAMETER p-cod-obsoleto-2 AS LOG.
  DEF INPUT PARAMETER p-cod-obsoleto-3 AS LOG.
  DEF INPUT PARAMETER p-cod-obsoleto-4 AS LOG.
  DEF INPUT PARAMETER p-cod-obsoleto-5 AS LOG.
  DEF INPUT PARAMETER p-tipo-artigo      AS CHAR.
  DEF INPUT PARAMETER p-lotes            AS CHAR.

  FOR EACH ped-item WHERE        
           (ped-item.cod-sit-item = 1 OR
            ped-item.cod-sit-item = 2 OR
            ped-item.cod-sit-item = 5) NO-LOCK,
     FIRST ped-item-ext OF ped-item 
           WHERE LOOKUP(SUBSTR(ped-item-ext.lote,1,2),p-lotes) <> 0 
           NO-LOCK.
                        

      FIND ped-venda OF ped-item NO-LOCK NO-ERROR.

      IF ped-venda.dt-entrega > da-dt-entrega THEN NEXT.

      IF NOT tt-param.l-batch THEN 
         RUN pi-acompanhar IN h-acomp (INPUT "Item: " + ped-item.it-codigo + " Pedido: " + ped-venda.nr-pedcli).

      FIND item WHERE
           item.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.

      FIND item-ext WHERE
           item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.

      IF p-tipo-artigo <> 'A' AND AVAIL item-ext THEN
         IF (item-ext.indigo = YES AND p-tipo-artigo <> "I") OR
            (item-ext.indigo = NO  AND p-tipo-artigo <> "O") THEN NEXT.

      FIND ref-item-ext WHERE
           ref-item-ext.it-codigo = ped-item.it-codigo AND
           ref-item-ext.cod-refer = ped-item.cod-refer NO-LOCK NO-ERROR.
      IF NOT AVAIL ref-item-ext THEN NEXT.

      IF AVAIL ref-item-ext THEN DO:
         IF (ref-item-ext.cod-obsoleto = '0' AND p-cod-obsoleto-0 = NO) OR
            (ref-item-ext.cod-obsoleto = '1' AND p-cod-obsoleto-1 = NO) OR
            (ref-item-ext.cod-obsoleto = '2' AND p-cod-obsoleto-2 = NO) OR
            (ref-item-ext.cod-obsoleto = '3' AND p-cod-obsoleto-3 = NO) OR
            (ref-item-ext.cod-obsoleto = '4' AND p-cod-obsoleto-4 = NO) OR
            (ref-item-ext.cod-obsoleto = '5' AND p-cod-obsoleto-5 = NO) THEN NEXT.
      END.

      FIND tt-itens WHERE
           tt-itens.cod-estabel = ped-venda.cod-estabel AND
           tt-itens.it-codigo = ped-item.it-codigo AND
           tt-itens.cod-refer = ped-item.cod-refer AND
           tt-itens.lote      = SUBSTR(ped-item-ext.lote,1,2)
           NO-LOCK NO-ERROR.

      IF NOT AVAIL tt-itens THEN DO.
         CREATE tt-itens.
         ASSIGN tt-itens.cod-estabel = ped-venda.cod-estabel 
                tt-itens.it-codigo = ped-item.it-codigo 
                tt-itens.cod-refer = ped-item.cod-refer
                tt-itens.lote      = UPPER(SUBSTR(ped-item-ext.lote,1,2)).
      END.
      ASSIGN tt-itens.qt-pedida = tt-itens.qt-pedida + ped-item.qt-pedida.

      FIND tt-movto WHERE
           tt-movto.row-tt-itens = ROWID(tt-itens) AND
           tt-movto.corte-comerc = ped-item-ext.corte-comerc
           NO-LOCK NO-ERROR.

      IF NOT AVAIL tt-movto THEN DO.
         CREATE tt-movto.
         ASSIGN tt-movto.row-tt-itens = ROWID(tt-itens)
                tt-movto.corte-comerc = ped-item-ext.corte-comerc.
      END.
      ASSIGN tt-movto.qt-carteira = tt-movto.qt-carteira + ped-item.qt-pedida
             tt-movto.qt-saldo = tt-movto.qt-saldo - ped-item.qt-pedida.
  END.

END PROCEDURE.

PROCEDURE pi-reserva.

   DEF INPUT PARAMETER p-cod-obsoleto-0 AS LOG.
   DEF INPUT PARAMETER p-cod-obsoleto-1 AS LOG.
   DEF INPUT PARAMETER p-cod-obsoleto-2 AS LOG.
   DEF INPUT PARAMETER p-cod-obsoleto-3 AS LOG.
   DEF INPUT PARAMETER p-cod-obsoleto-4 AS LOG.
   DEF INPUT PARAMETER p-cod-obsoleto-5 AS LOG.
   DEF INPUT PARAMETER p-tipo-artigo      AS CHAR.
   DEF INPUT PARAMETER p-lotes            AS CHAR.

   FOR EACH ped-reserva-it WHERE
            LOOKUP(ped-reserva-it.nr-lote,p-lotes) <> 0 NO-LOCK.

       FIND ped-reserva OF ped-reserva-it NO-LOCK NO-ERROR.
       IF ped-reserva.situacao <> 1 THEN NEXT.

       IF NOT tt-param.l-batch THEN 
          RUN pi-acompanhar IN h-acomp (INPUT "Reserva: " + STRING(ped-reserva.num-reserva)).

       FIND item WHERE
            item.it-codigo = ped-reserva-it.it-codigo NO-LOCK NO-ERROR.
    
       FIND item-ext WHERE
            item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.
    
       IF p-tipo-artigo <> 'A' AND AVAIL item-ext THEN
          IF (item-ext.indigo = YES AND p-tipo-artigo <> "I") OR
             (item-ext.indigo = NO  AND p-tipo-artigo <> "O") THEN NEXT.
   
       FIND ref-item-ext WHERE
            ref-item-ext.it-codigo = ped-reserva-it.it-codigo AND
            ref-item-ext.cod-refer = ped-reserva-it.cod-refer 
            NO-LOCK NO-ERROR.
       IF NOT AVAIL ref-item-ext THEN NEXT.

       IF AVAIL ref-item-ext THEN DO:
          IF (ref-item-ext.cod-obsoleto = '0' AND p-cod-obsoleto-0 = NO) OR
             (ref-item-ext.cod-obsoleto = '1' AND p-cod-obsoleto-1 = NO) OR
             (ref-item-ext.cod-obsoleto = '2' AND p-cod-obsoleto-2 = NO) OR
             (ref-item-ext.cod-obsoleto = '3' AND p-cod-obsoleto-3 = NO) OR
             (ref-item-ext.cod-obsoleto = '4' AND p-cod-obsoleto-4 = NO) OR
             (ref-item-ext.cod-obsoleto = '5' AND p-cod-obsoleto-5 = NO) THEN NEXT.
       END.

       FIND tt-itens WHERE
            tt-itens.cod-estabel = ped-reserva.cod-estabel AND
            tt-itens.it-codigo = ped-reserva-it.it-codigo AND
            tt-itens.cod-refer = ped-reserva-it.cod-refer AND
            tt-itens.lote      = ped-reserva-it.nr-lote NO-ERROR.

       IF NOT AVAIL tt-itens THEN DO.
          CREATE tt-itens.
          ASSIGN tt-itens.cod-estabel = ped-reserva.cod-estabel 
                 tt-itens.it-codigo = ped-reserva-it.it-codigo 
                 tt-itens.cod-refer = ped-reserva-it.cod-refer
                 tt-itens.lote      = ped-reserva-it.nr-lote.
       END.

       FOR EACH ped-reserva-etq OF ped-reserva-it NO-LOCK.
           FIND ped-reserva WHERE
                ped-reserva.num-reserva = ped-reserva-etq.num-reserva NO-LOCK NO-ERROR.
           FIND ob-etiqueta WHERE
                ob-etiqueta.cod-estabel  = ped-reserva.cod-estabel AND
                ob-etiqueta.num-etiqueta = ped-reserva-etq.num-etiqueta
                NO-LOCK NO-ERROR.

           FIND tt-movto WHERE
                tt-movto.row-tt-itens = ROWID(tt-itens) AND
                tt-movto.corte-comerc = ped-reserva-it.corte-comerc
                NO-LOCK NO-ERROR.

           IF NOT AVAIL tt-movto THEN DO.
              CREATE tt-movto.
              ASSIGN tt-movto.row-tt-itens = ROWID(tt-itens)
                     tt-movto.corte-comerc = ped-reserva-it.corte-comerc.
           END.
           ASSIGN tt-movto.qt-ped-reserva = tt-movto.qt-ped-reserva + ob-etiqueta.quantidade
                  tt-movto.qt-saldo       = tt-movto.qt-saldo       - ob-etiqueta.quantidade.
       END.
   END.

END PROCEDURE.

PROCEDURE pi-trf.

   DEF INPUT PARAMETER p-cod-obsoleto-0 AS LOG.
   DEF INPUT PARAMETER p-cod-obsoleto-1 AS LOG.
   DEF INPUT PARAMETER p-cod-obsoleto-2 AS LOG.
   DEF INPUT PARAMETER p-cod-obsoleto-3 AS LOG.
   DEF INPUT PARAMETER p-cod-obsoleto-4 AS LOG.
   DEF INPUT PARAMETER p-cod-obsoleto-5 AS LOG.
   DEF INPUT PARAMETER p-tipo-artigo      AS CHAR.
   DEF INPUT PARAMETER p-lotes            AS CHAR.

   FOR EACH ob-trf WHERE 
            ob-trf.situacao = 1 AND
            LOOKUP(ob-trf.nr-lote,p-lotes) <> 0 NO-LOCK.

       IF NOT tt-param.l-batch THEN 
          RUN pi-acompanhar IN h-acomp (INPUT "Transformaá∆o: " + STRING(ob-trf.num-trf)).

       FIND item WHERE
            item.it-codigo = ob-trf.it-codigo NO-LOCK NO-ERROR.
    
       FIND item-ext WHERE
            item-ext.it-codigo = item.it-codigo NO-LOCK NO-ERROR.
    
       IF p-tipo-artigo <> 'A' THEN 
          IF AVAIL item-ext AND
             (item-ext.indigo = YES AND p-tipo-artigo <> "I") OR
             (item-ext.indigo = NO  AND p-tipo-artigo <> "O") THEN NEXT.
    
       FIND ref-item-ext WHERE
            ref-item-ext.it-codigo = ob-trf.it-codigo AND
            ref-item-ext.cod-refer = ob-trf.cod-refer 
            NO-LOCK NO-ERROR.
       IF NOT AVAIL ref-item-ext THEN NEXT.

       IF AVAIL ref-item-ext THEN DO:
          IF (ref-item-ext.cod-obsoleto = '0' AND p-cod-obsoleto-0 = NO) OR
             (ref-item-ext.cod-obsoleto = '1' AND p-cod-obsoleto-1 = NO) OR
             (ref-item-ext.cod-obsoleto = '2' AND p-cod-obsoleto-2 = NO) OR
             (ref-item-ext.cod-obsoleto = '3' AND p-cod-obsoleto-3 = NO) OR
             (ref-item-ext.cod-obsoleto = '4' AND p-cod-obsoleto-4 = NO) OR
             (ref-item-ext.cod-obsoleto = '5' AND p-cod-obsoleto-5 = NO) THEN NEXT.
       END.

       FIND FIRST ob-etq-trf OF ob-trf NO-LOCK NO-ERROR.

       IF AVAIL ob-etq-trf THEN DO.
          FIND tt-itens WHERE
               tt-itens.cod-estabel = ob-etq-trf.cod-estabel AND
               tt-itens.it-codigo = ob-trf.it-codigo AND
               tt-itens.cod-refer = ob-trf.cod-refer AND
               tt-itens.lote      = ob-trf.nr-lote NO-ERROR.
    
          IF NOT AVAIL tt-itens THEN DO.
             CREATE tt-itens.
             ASSIGN tt-itens.cod-estabel = ob-etq-trf.cod-estabel 
                    tt-itens.it-codigo = ob-trf.it-codigo 
                    tt-itens.cod-refer = ob-trf.cod-refer
                    tt-itens.lote      = ob-trf.nr-lote.
          END.
       END.

       FOR EACH ob-etq-trf OF ob-trf NO-LOCK.
           FIND ob-etiqueta WHERE
                ob-etiqueta.cod-estabel  = ob-etq-trf.cod-estabel AND
                ob-etiqueta.num-etiqueta = ob-etq-trf.num-etiqueta
                NO-LOCK NO-ERROR.
           IF NOT AVAIL ob-etiqueta THEN NEXT.

           FIND tt-movto WHERE
                tt-movto.row-tt-itens = ROWID(tt-itens) AND
                tt-movto.corte-comerc = ob-trf.corte-comerc
                NO-LOCK NO-ERROR.

           IF NOT AVAIL tt-movto THEN DO.
              CREATE tt-movto.
              ASSIGN tt-movto.row-tt-itens = ROWID(tt-itens)
                     tt-movto.corte-comerc = ob-trf.corte-comerc.
           END.
           ASSIGN tt-movto.qt-trf   = tt-movto.qt-trf   + ob-etiqueta.quantidade
                  tt-movto.qt-saldo = tt-movto.qt-saldo + ob-etiqueta.quantidade.

           FIND tt-movto WHERE
                tt-movto.row-tt-itens = ROWID(tt-itens) AND
                tt-movto.corte-comerc = ob-etiqueta.corte-comerc
                NO-LOCK NO-ERROR.

           IF NOT AVAIL tt-movto THEN DO.
              CREATE tt-movto.
              ASSIGN tt-movto.row-tt-itens = ROWID(tt-itens)
                     tt-movto.corte-comerc = ob-etiqueta.corte-comerc.
           END.
           ASSIGN tt-movto.qt-estoque = tt-movto.qt-estoque + ob-etiqueta.quantidade 
                  tt-movto.qt-trf     = tt-movto.qt-trf     - ob-etiqueta.quantidade.
       END.
   END.

END PROCEDURE.

PROCEDURE pi-pcp.

  FOR EACH ob-pcp WHERE
           ob-pcp.situacao = 1,
      EACH ob-pcp-ref OF ob-pcp NO-LOCK. 

      IF NOT tt-param.l-batch THEN 
         RUN pi-acompanhar IN h-acomp (INPUT "Programaá∆o: " + STRING(ob-pcp.num-progr)).

      FIND FIRST tt-itens WHERE
                 tt-itens.cod-estabel = ob-pcp.cod-estabel AND
                 tt-itens.it-codigo = ob-pcp.it-codigo AND
                 tt-itens.cod-refer = ob-pcp-ref.cod-refer NO-ERROR.

      IF NOT AVAIL tt-itens THEN DO.
         CREATE tt-itens.
         ASSIGN tt-itens.cod-estabel = ob-pcp.cod-estabel 
                tt-itens.it-codigo = ob-pcp.it-codigo 
                tt-itens.cod-refer = ob-pcp-ref.cod-refer
                tt-itens.lote = 'RP'.
      END.

      FOR EACH tt-itens WHERE
               tt-itens.it-codigo = ob-pcp.it-codigo AND
               tt-itens.cod-refer = ob-pcp-ref.cod-refer NO-LOCK.

          IF tt-itens.lote <> 'PP' AND
             tt-itens.lote <> 'RP' THEN NEXT.

          ASSIGN tt-itens.qt-benefic = tt-itens.qt-benefic + ob-pcp-ref.qtd-sld-prog +
                                       ob-pcp-ref.qtd-proc + ob-pcp-ref.qtd-pron.
      END.
  END.
END PROCEDURE.

PROCEDURE pi-gera-rel.

  DEF INPUT PARAMETER p-titulo AS CHAR.

  OUTPUT TO VALUE(c-arq-anexo) CONVERT SOURCE "ibm850".

  ASSIGN i-lin = 99
         i-pag =  1.
  
  FOR EACH tt-itens WHERE tt-itens.visualiza = YES NO-LOCK,
      EACH tt-movto WHERE tt-movto.row-tt-itens = ROWID(tt-itens) 
                      AND tt-movto.visualiza    = YES NO-LOCK
      BREAK BY tt-itens.it-codigo
            BY tt-itens.cod-refer
            BY tt-itens.lote
            BY tt-movto.corte-comerc:
        
      IF i-lin > 62 THEN DO:
         RUN pi-imp-cabec (INPUT p-titulo).
         ASSIGN i-lin = 7.
      END.

      IF FIRST-OF(tt-itens.it-codigo) THEN DO:
         FIND ITEM WHERE 
              ITEM.it-codigo = tt-itens.it-codigo NO-LOCK NO-ERROR.
         PUT tt-itens.it-codigo         FORMAT "x(6)"  AT  1
             item.desc-item             FORMAT "x(35)" AT  8
             item.un                    FORMAT "x(3)"  AT 44.
      END.
        
      IF FIRST-OF(tt-itens.cod-refer) THEN
          PUT STRING(tt-itens.cod-refer, "99.9999.9") FORMAT "x(9)"  AT 48.
        
      IF FIRST-OF(tt-itens.lote) THEN DO:
         FIND ref-item-ext WHERE
              ref-item-ext.it-codigo = tt-itens.it-codigo AND
              ref-item-ext.cod-refer = tt-itens.cod-refer NO-LOCK NO-ERROR.
         PUT tt-itens.lote  FORMAT "x(2)"  AT 60.
         IF AVAIL ref-item-ext THEN
            PUT ref-item-ext.cor FORMAT "x(20)"  AT 64.
      END.
      FIND corte-comerc WHERE
           corte-comerc.codigo = tt-movto.corte-comerc NO-LOCK NO-ERROR.
      ASSIGN c-work = IF AVAIL corte-comerc THEN corte-comerc.descricao
                                           ELSE "".

      PUT c-work            FORMAT "x(10)"           AT 85
          tt-movto.qt-saldo FORMAT ">>>,>>>,>>9.99"  AT 96.
           
      ACCUMULATE tt-movto.qt-saldo (TOTAL BY tt-itens.it-codigo).
      ACCUMULATE tt-movto.qt-saldo (TOTAL BY tt-itens.cod-refer).
      ACCUMULATE tt-movto.qt-saldo (TOTAL).
           
      IF LAST-OF(tt-itens.cod-refer) THEN
         PUT (ACCUM TOTAL BY tt-itens.cod-refer tt-movto.qt-saldo)  FORMAT ">>>,>>>,>>9.99" AT 114.
        
      IF LAST-OF(tt-itens.it-codigo) THEN DO:
          IF i-lin > 62 THEN DO:
             RUN pi-imp-cabec (INPUT p-titulo).
             ASSIGN i-lin = 7.
          END.
          PUT "TOTAL DO ITEM"                                        FORMAT "x(13)"          AT 64
              (ACCUM TOTAL BY tt-itens.it-codigo tt-movto.qt-saldo)  FORMAT ">>>,>>>,>>9.99" AT 96.
          PUT "" AT 1.
      END.
  END.
    
  IF (ACCUM TOTAL tt-movto.qt-saldo) <> 0 THEN DO:
      PUT "TOTAL GERAL"                    FORMAT "x(11)"          AT 64
          (ACCUM TOTAL tt-movto.qt-saldo)  FORMAT ">>>,>>>,>>9.99" AT 96.
  END.
  OUTPUT CLOSE.

END PROCEDURE.

PROCEDURE pi-imp-cabec.

  DEF INPUT PARAMETER p-titulo AS CHAR.
  
  PUT "TEAR TEXTIL INDUSTRIA E COMERCIO LTDA."  AT   1
      "DATA: "                                  AT  68
      STRING(TODAY,"99/99/9999") FORMAT "X(10)" AT  74
      "HORA: "                                  AT 114
      STRING(TIME,"hh:mm:ss")                   AT 120
      SKIP(1).
  PUT p-titulo FORMAT "x(64)" AT 37 
      SKIP(1).

  PUT "Item   Descriá∆o                           Und Referància Lote Cor/Desenho          Corte Coml     Quantidade  Total Referància" AT 1.
  PUT "------ ----------------------------------- --- ---------- ---- -------------------- ---------- --------------  ----------------" AT 1.
  ASSIGN i-pag = i-pag + 1.                                                                              

END PROCEDURE.
