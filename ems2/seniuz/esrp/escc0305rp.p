/* Programa: ESCC0305.P
** Sistema.: Magnus da DATASUL S/A.
** Modulo..: Compras              
** Objetivo: Gerar Pedido de Compra para tabela wt-rpb
** Autor...: PRODB-Antonio G. Souza (Nov 2003)
*/

define temp-table tt-param no-undo
    field destino          as integer
    field arquivo          as char format "x(35)"
    field usuario          as char format "x(12)"
    field data-exec        as date
    field hora-exec        as integer
    FIELD pedido-ini       LIKE pedido-compr.num-pedido 
    FIELD pedido-fin       LIKE pedido-compr.num-pedido
    FIELD e-mail           AS LOG.

define temp-table tt-raw-digita 
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

def buffer b-ordem-compra for ordem-compra.

/* Vari veis do Report Builder */
DEF VAR RB-REPORT-LIBRARY    AS CHARACTER INITIAL "".
DEF VAR RB-REPORT-NAME       AS CHARACTER INITIAL "".
DEF VAR RB-DB-CONNECTION     AS CHARACTER INITIAL "".
DEF VAR RB-INCLUDE-RECORDS   AS CHARACTER INITIAL "".
DEF VAR RB-FILTER            AS CHARACTER INITIAL "".
DEF VAR RB-MEMO-FILE         AS CHARACTER INITIAL "".
DEF VAR RB-PRINT-DESTINATION AS CHARACTER INITIAL "".
DEF VAR RB-PRINTER-NAME      AS CHARACTER INITIAL "".
DEF VAR RB-PRINTER-PORT      AS CHARACTER INITIAL "".
DEF VAR RB-OUTPUT-FILE       AS CHARACTER INITIAL "".
DEF VAR RB-NUMBER-COPIES     AS INTEGER   INITIAL 1.
DEF VAR RB-BEGIN-PAGE        AS INTEGER   INITIAL 0.
DEF VAR RB-END-PAGE          AS INTEGER   INITIAL 0.
DEF VAR RB-TEST-PATTERN      AS LOGICAL   INITIAL NO.
DEF VAR RB-WINDOW-TITLE      AS CHARACTER INITIAL "".
DEF VAR RB-DISPLAY-ERRORS    AS LOGICAL   INITIAL YES.
DEF VAR RB-DISPLAY-STATUS    AS LOGICAL   INITIAL YES.
DEF VAR RB-NO-WAIT           AS LOGICAL   INITIAL NO.
DEF VAR RB-OTHER-PARAMETERS  AS CHARACTER INITIAL "".

DEF VAR c-descr-item AS CHAR.
DEF VAR de-tot-bru AS DEC FORMAT ">>,>>>,>>9.99".
DEF VAR c-arq-email AS CHAR FORMAT "x(30)".
DEF VAR c-comando AS CHAR.

FOR EACH pedido-compr WHERE  
         pedido-compr.num-pedido >= tt-param.pedido-ini AND
         pedido-compr.num-pedido <= tt-param.pedido-fin NO-LOCK.

   FIND emitente WHERE 
        emitente.cod-emitente = pedido.cod-emitente
        NO-LOCK.

   FIND FIRST cont-emit WHERE
              cont-emit.cod-emitente =  emitente.cod-emitente
              NO-LOCK NO-ERROR.
        
   IF tt-param.e-mail AND
      cont-emit.e-mail = "" THEN DO.
      MESSAGE "Fornecedor sem e-mail Cadastrado, Imposs¡vel enviar e-mail"
              VIEW-AS ALERT-BOX.
      RETURN.
   END.

   CREATE wt-rpb.
   ASSIGN wt-rpb.rpb-rowid = STRING(ROWID(wt-rpb)) 
          wt-rpb.rpb-id = tt-param.usuario +
                          STRING(tt-param.data-exec,"99/99/9999") + 
                          STRING(tt-param.hora-exec,"HH:MM:SS"). 

   ASSIGN wt-rpb.rpb-integer[1]   = pedido-compr.num-pedido
          wt-rpb.rpb-integer[2]   = emitente.cod-emitente
          wt-rpb.rpb-date[1]      = pedido-compr.data-pedido
          wt-rpb.rpb-character[1] = emitente.nome-emit                          
          wt-rpb.rpb-character[2] = emitente.endereco + " - " + emitente.bairro + " - " + 
                                    emitente.cidade + " - " + emitente.estado + " - " + "Cep: " + STRING(emitente.cep)
          wt-rpb.rpb-character[3] = emitente.telefone[1] + "  /  " + emitente.telefax
          wt-rpb.rpb-character[4] = emitente.cgc                                
          wt-rpb.rpb-character[5] =  IF AVAIL cont-emit
                                     THEN cont-emit.nome ELSE ""
          wt-rpb.rpb-character[6] = emitente.ins-estadual.
   
   FOR EACH ordem-compra WHERE
            ordem-compra.num-pedido =  pedido.num-pedido AND
            ordem-compra.situacao   <> "E" NO-LOCK,
       EACH prazo-compra WHERE
            prazo-compra.numero-ordem = ordem-compra.numero-ordem NO-LOCK:
       
       FIND item WHERE
            item.it-codigo = prazo-compra.it-codigo no-lock.

       ASSIGN c-descr-item = "".
       IF ordem-compra.it-codigo <> "" THEN 
          ASSIGN c-descr-item = item.descricao-1 + item.descricao-2.

       CREATE wt-rpbline.
       ASSIGN wt-rpbline.rpb-rowid = wt-rpb.rpb-rowid.

       ASSIGN wt-rpbline.rpbl-character[1] = prazo-compra.it-codigo
              wt-rpbline.rpbl-character[2] = IF c-descr-item <> ""
                                             THEN c-descr-item
                                             ELSE ordem-compra.narrativa[1]
              wt-rpbline.rpbl-character[3] = prazo-compra.un
              wt-rpbline.rpbl-character[4] = ordem-compra.requisitante   
              wt-rpbline.rpbl-decimal[1]   = prazo-compra.quantidade
              wt-rpbline.rpbl-decimal[2]   = ordem-compra.preco-fornec
              wt-rpbline.rpbl-decimal[3]   = ordem-compra.aliquota-ipi
              wt-rpbline.rpbl-decimal[4]   = (prazo-compra.quantidade * ordem-compra.preco-fornec) +
                                             ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.aliquota-ipi * 0.01) +
                                             ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.valor-taxa * 0.01) -
                                             ((prazo-compra.quantidade * ordem-compra.preco-fornec) * ordem-compra.perc-descto * 0.01)
              wt-rpbline.rpbl-date[1]      = prazo-compra.data-entrega
              wt-rpbline.rpbl-character[5] = IF c-descr-item <> ""
                                             THEN ordem-compra.narrativa[1]
                                             ELSE ordem-compra.narrativa[2]
              wt-rpbline.rpbl-character[6] = IF c-descr-item <> ""
                                             THEN ordem-compra.narrativa[2]
                                             ELSE ordem-compra.narrativa[3]
              wt-rpbline.rpbl-character[7] = IF c-descr-item <> ""
                                             THEN ordem-compra.narrativa[3]
                                             ELSE ordem-compra.narrativa[4]
              wt-rpbline.rpbl-character[8] = IF c-descr-item <> ""
                                             THEN ordem-compra.narrativa[4]
                                             ELSE ordem-compra.narrativa[5]
              wt-rpbline.rpbl-character[9] = IF c-descr-item <> ""
                                             THEN ordem-compra.narrativa[5]
                                             ELSE "".
   END.

   FIND FIRST b-ordem-compra WHERE 
              b-ordem-compra.num-pedido = pedido-compr.num-pedido NO-LOCK. 

   FIND transporte OF pedido-compr NO-LOCK NO-ERROR.
   FIND cond-pagto OF pedido-compr NO-LOCK NO-ERROR.

   FIND comprador WHERE
        comprador.cod-comprado = b-ordem-compra.cod-comprado
        NO-LOCK NO-ERROR.

   ASSIGN wt-rpb.rpb-character[7] = IF AVAIL cond-pagto
                                    THEN cond-pagto.descricao 
                                    ELSE ""
          wt-rpb.rpb-character[8] = IF AVAIL transporte
                                    THEN transporte.nome
                                    ELSE ""
          wt-rpb.rpb-character[9] = IF pedido-compr.frete
                                    THEN "CIF" ELSE "FOB"
          wt-rpb.rpb-character[10] = IF AVAIL comprador
                                     THEN comprador.nome
                                     ELSE ""
          wt-rpb.rpb-character[11] = pedido-compr.comentarios[1]
          wt-rpb.rpb-character[12] = pedido-compr.comentarios[2]
          wt-rpb.rpb-character[13] = pedido-compr.comentarios[3]
          wt-rpb.rpb-character[14] = pedido-compr.comentarios[4]
          wt-rpb.rpb-character[15] = pedido-compr.comentarios[5]
          wt-rpb.rpb-character[16] = pedido-compr.comentarios[6]
          wt-rpb.rpb-character[17] = IF AVAIL comprador
                                     THEN "Fone:" + comprador.char-2 + "   E-mail: " + comprador.u-char-1
                                     ELSE "".
          
END. /* pedido-compra */


/* Executa do Report Builder  */

FIND FIRST wt-rpb WHERE
           wt-rpb.rpb-id = tt-param.usuario +
                           STRING(tt-param.data-exec,"99/99/9999") + 
                           STRING(tt-param.hora-exec,"HH:MM:SS") 
           NO-LOCK NO-ERROR.

IF NOT AVAIL wt-rpb THEN RETURN.

ASSIGN RB-REPORT-LIBRARY = "rpbcc.prl"
       RB-REPORT-NAME = "escc0305"
       RB-DB-CONNECTION = "espec = -db espec -H RENA -S mgesp -N TCP"
       RB-INCLUDE-RECORDS = "O"
       RB-FILTER = "wt-rpb.rpb-id = '" + wt-rpb.rpb-id + "'" 
       RB-PRINT-DESTINATION = IF tt-param.destino = 3
                              THEN "D" ELSE ""
       RB-PRINTER-NAME = IF tt-param.destino = 1
                         THEN tt-param.arquivo
                         ELSE "".

RUN aderb/_printrb.p (
        RB-REPORT-LIBRARY,
        RB-REPORT-NAME,
        RB-DB-CONNECTION,
        RB-INCLUDE-RECORDS,
        RB-FILTER,
        RB-MEMO-FILE,
        RB-PRINT-DESTINATION,
        RB-PRINTER-NAME,
        RB-PRINTER-PORT,
        RB-OUTPUT-FILE,
        RB-NUMBER-COPIES,
        RB-BEGIN-PAGE,
        RB-END-PAGE,
        RB-TEST-PATTERN,
        RB-WINDOW-TITLE,
        RB-DISPLAY-ERRORS,
        RB-DISPLAY-STATUS,
        RB-NO-WAIT,
        RB-OTHER-PARAMETERS).

IF tt-param.e-mail THEN DO.
   FOR EACH wt-rpb WHERE
            wt-rpb.rpb-id = tt-param.usuario + 
                            STRING(tt-param.data-exec,"99/99/9999") + 
                            STRING(tt-param.hora-exec,"HH:MM:SS") NO-LOCK.
       ASSIGN c-arq-email = "C:\PEDCOMPRA\PED" + STRING(wt-rpb.rpb-integer[1]) + ".PDF".
       /*IF SEARCH(VALUE(c-arq-email)) = ? THEN DO.
          MESSAGE "NÆo foi encontrado o arquivo " c-arq-email SKIP
                  ", Imposs¡vel enviar e-mail"
                  VIEW-AS ALERT-BOX.
          NEXT.
       END. 
       ASSIGN c-comando = "c:\postie\postie.exe -host:www.teartextil.com.br -from:compras@teartextil.com.br -to:rodrigo.pimenta@teartextil.com.br -s:"Pedido de Compra" -msg:"Segue anexo Pedido de compra". 
       OS-COMMAND SILENT VALUE(c-comando). */
   END.
END.

FOR EACH wt-rpb WHERE
         wt-rpb.rpb-id = tt-param.usuario + 
                         STRING(tt-param.data-exec,"99/99/9999") + 
                         STRING(tt-param.hora-exec,"HH:MM:SS"). 
    FOR EACH wt-rpbline OF wt-rpb.
        DELETE wt-rpbline.
    END.
    DELETE wt-rpb.
END.

