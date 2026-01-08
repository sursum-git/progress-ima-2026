/* Programa: ESSP0138.P
** Sistema.: EMS da DATASUL S/A.
** Modulo..: Faturamento         
** Objetivo: Listar o relat¢rio Mapa de Separaá∆o/Carregamento
** Autor...: Gilvando de Souza Araujo - Novembro/2006
** Obs.....: Especifico da TEAR T“XTIL INDÈSTRIA E COMêRCIO LTDA.
**
** Alterado: F†bio Coelho Lanza em JULHO-2010 (Coluna Transportadora)
                                   JANEIRO-2011 (Coluna Lote)
**
*/

/* include de controle de vers∆o */
{include/i-prgvrs.i ESSP0138RP 2.04.00.000}

DEFINE TEMP-TABLE tt-param NO-UNDO
       FIELD destino            AS INTEGER
       FIELD arquivo            AS CHAR FORMAT "x(35)"
       FIELD usuario            AS CHAR FORMAT "x(12)"
       FIELD data-exec          AS DATE
       FIELD hora-exec          AS INTEGER
       FIELD classifica         AS INTEGER
       FIELD desc-classifica    AS CHAR FORMAT "x(40)"
       FIELD cod-estabel        LIKE nota-fiscal.cod-estabel
       FIELD serie              LIKE nota-fiscal.serie
       FIELD nr-pedcli-ini      LIKE ped-venda.nr-pedcli
       FIELD nr-pedcli-fin      LIKE ped-venda.nr-pedcli
       FIELD nr-nota-fis-ini    LIKE nota-fiscal.nr-nota-fis
       FIELD nr-nota-fis-fin    LIKE nota-fiscal.nr-nota-fis
       FIELD transp-ini         LIKE nota-fiscal.nome-transp
       FIELD transp-fin         LIKE nota-fiscal.nome-transp
       FIELD estado-ini         LIKE emitente.estado
       FIELD estado-fin         LIKE emitente.estado
       FIELD nome-ab-cli-ini    LIKE nota-fiscal.nome-ab-cli
       FIELD nome-ab-cli-fin    LIKE nota-fiscal.nome-ab-cli
       FIELD no-ab-reppri-ini   LIKE nota-fiscal.no-ab-reppri
       FIELD no-ab-reppri-fin   LIKE nota-fiscal.no-ab-reppri
       FIELD corte-com-ini      LIKE corte-comerc.codigo
       FIELD corte-com-fin      LIKE corte-comerc.codigo
       FIELD tipo-rel           AS INT
       FIELD peso-max-carga     AS INTEGER
       FIELD enviar-e-mail      AS LOG FORMAT "Sim/N∆o"
       FIELD e-mail-remet       AS CHAR FORMAT "x(45)"
       FIELD subject-e-mail     AS CHAR FORMAT "x(40)"
       FIELD texto-e-mail       AS CHAR FORMAT "x(2000)"
       FIELD impr-param         AS LOG.

DEFINE TEMP-TABLE tt-digita NO-UNDO
       FIELD nr-nota-fis LIKE nota-fiscal.nr-nota-fis
       FIELD nr-pedcli   LIKE ped-venda.nr-pedcli
       INDEX id-nota nr-nota-fis
       INDEX id-ped  nr-pedcli.

define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.

create tt-param.
raw-transfer raw-param to tt-param.

FOR EACH tt-raw-digita.
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.

DEFINE TEMP-TABLE tt-work
       FIELD nome-transp  LIKE nota-fiscal.nome-transp
       FIELD seq-caminhao AS INTEGER
       FIELD nr-nota-fis  LIKE it-nota-fisc.nr-nota-fis
       FIELD nr-seq-fat   LIKE it-nota-fisc.nr-seq-fat
       FIELD it-codigo    LIKE it-nota-fisc.it-codigo
       FIELD cod-refer    LIKE it-nota-fisc.cod-refer
       FIELD nr-lote      LIKE ob-etiqueta.nr-lote
       FIELD quantidade   LIKE ob-etiqueta.quantidade      
       FIELD peso-bruto   LIKE it-nota-fisc.peso-bruto
       FIELD num-etiqueta LIKE ob-etiqueta.num-etiqueta
       FIELD acondic      LIKE ob-etiqueta.acondic
       FIELD nome-abrev   LIKE nota-fiscal.nome-abrev
       FIELD localizacao  AS CHAR FORMAT "999/999"
       FIELD nr-volume    LIKE ped-item-rom.nr-volume
       FIELD ind-volume   AS INT
       INDEX ch-work nr-nota-fis
                     nr-seq-fat
       INDEX ch-volume nr-volume.  

/* include padr∆o para vari†veis de relat¢rio  */
{include/i-rpvar.i}

/* include padr∆o para impress∆o de campos editores em relat¢rio  */
{include/tt-edit.i}

/* definiá∆o de vari†veis  */
def var h-acomp as handle no-undo.
DEF VAR i-cont-par        AS INTEGER.
DEF VAR i-cont-cam        AS INTEGER.
DEF VAR i-cont-ger        AS INTEGER.
DEF VAR de-qtd-par        LIKE ob-etiqueta.quantidade.
DEF VAR de-qtd-cam        LIKE ob-etiqueta.quantidade.
DEF VAR de-qtd-ger        LIKE ob-etiqueta.quantidade.
DEF VAR de-peso-par       LIKE it-nota-fisc.peso-bruto.
DEF VAR de-peso-cam       LIKE it-nota-fisc.peso-bruto.
DEF VAR de-peso-ger       LIKE it-nota-fisc.peso-bruto.
DEF VAR i-seq-caminhao    AS INTEGER INIT 1.
DEF VAR c-tipo-total      AS CHAR FORMAT "x(17)".
DEF VAR c-num-etiqueta    AS CHAR FORMAT "x(11)".
DEF VAR l-tem-peca        AS LOG.
DEF VAR c-aux-e-mail      AS CHAR.
DEF VAR i-e-mail-enviado  AS INT.
DEF VAR i-e-mail-nenviado AS INT.
DEF VAR c-destinatar      AS CHAR.
DEF VAR i-tot-vol-not     AS INT.
DEF VAR i-tot-vol-tra     AS INT.
DEF VAR i-tot-not         AS INT.
DEF VAR i-ind-volume      AS INT.
DEF VAR de-tot-pes-tra    LIKE nota-fiscal.peso-bru-tot.

form 
    "*------------ ParÉmetros/Seleá∆o ------------*"     AT  1
    tt-param.desc-classifica  LABEL "Classificaá∆o.."  AT  1
    tt-param.cod-estabel      LABEL "Estabelecimento"  AT  1
    tt-param.serie            LABEL "SÇrie da NF...."  AT  1
    tt-param.nr-nota-fis-ini  LABEL "N£mero da NF..."  FORMAT "x(8)" AT  1
    "A"  AT 31
    tt-param.nr-nota-fis-fin  NO-LABEL                 FORMAT "x(8)" AT 33 
    tt-param.transp-ini       LABEL "Transportador.."  AT  1
    "A"  AT 31
    tt-param.transp-fin       NO-LABELS                AT 33
    tt-param.estado-ini       LABEL "Estado........."  AT  1
    "A"  AT 31
    tt-param.estado-fin       NO-LABELS                AT 33
    tt-param.nome-ab-cli-ini  LABEL "Cliente........"  AT  1
    "A"  AT 31
    tt-param.nome-ab-cli-fin  NO-LABELS                AT 33
    tt-param.no-ab-reppri-ini LABEL "Representante.."  AT  1
    "A"  AT 31
    tt-param.no-ab-reppri-fin NO-LABELS                AT 33
    tt-param.corte-com-ini    LABEL "Corte Comercial"  AT  1
    "A"  AT 31              
    tt-param.corte-com-fin    NO-LABELS                AT 33
    tt-param.peso-max-carga   LABEL "Peso M†x.Carga."  AT  1
    tt-param.enviar-e-mail    LABEL "Enviar e-mail.."  AT  1
    tt-param.e-mail-remet     LABEL "E-mail remetent"  AT  1
    i-e-mail-enviado          LABEL "E-mail env....."  AT  1
    i-e-mail-nenviado         LABEL "E-mail n/env..."  AT  1
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

FORM
    "Transportador: "
    transporte.cod-transp
    " - "
    transporte.nome
    " - Caminh∆o: "
    tt-work.seq-caminhao
    SKIP(1)
    WITH NO-LABEL 1 DOWN NO-BOX NO-ATTR-SPACE WIDTH 132 STREAM-IO FRAME f-cab-transp.

FORM
    "Total" c-tipo-total " - Lotes:"
    i-cont-par
    " - Metragem:"
    de-qtd-par
    " - Peso:"
    de-peso-par
    SKIP(1)
    WITH NO-LABEL 1 DOWN NO-BOX NO-ATTR-SPACE WIDTH 132 STREAM-IO FRAME f-subtotal.

FORM HEADER
    "PRODUTO REFER“NCIA LOTE DESCRIÄ«O                        RUA/DOCA NUMERO ROLO  METRAGEM NOTA FISCAL CLIENTE      TRANSPORTADORA " AT 1 
    "------- ---------- ---- -------------------------------- -------- ----------- --------- ----------- ------------ ---------------" AT 1
    WITH NO-LABELS NO-ATTR-SPACE NO-BOX PAGE-TOP WIDTH 132 STREAM-IO 1 DOWN FRAME f-cab-nf.

FORM HEADER
    "PRODUTO REFER“NCIA LOTE DESCRIÄ«O                        RUA/DOCA NUMERO ROLO  METRAGEM PEDIDO      CLIENTE      TRANSPORTADORA " AT 1 
    "------- ---------- ---- -------------------------------- -------- ----------- --------- ----------- ------------ ---------------" AT 1
    WITH NO-LABELS NO-ATTR-SPACE NO-BOX PAGE-TOP WIDTH 132 STREAM-IO 1 DOWN FRAME f-cab-ped.

FORM
    tt-work.it-codigo     AT   1 FORMAT "x(7)"
    tt-work.cod-refer     AT   9 FORMAT "XX.XXXX-X"
    tt-work.nr-lote       AT  20 FORMAT "x(4)"
    ITEM.desc-item        AT  25 FORMAT "x(33)"
    tt-work.localizacao   AT  58 FORMAT "999/999" 
    tt-work.num-etiqueta  AT  67 FORMAT ">>>>>>>>>>9" 
    tt-work.quantidade    AT  79 FORMAT ">>,>>9.99" 
    tt-work.nr-nota-fis   AT  89 FORMAT "x(11)"
    tt-work.nome-abrev    AT 101 FORMAT "x(12)"
    tt-work.nome-transp   AT 114 FORMAT "x(15)"
    WITH NO-LABEL NO-BOX 55 DOWN WIDTH 132 WITH STREAM-IO FRAME f-detalhe.

/* include padr∆o para output de relat¢rios */
{include/i-rpout.i}

/* include com a definiá∆o da frame de cabeáalho e rodapÇ */
{include/i-rpcab.i}

/* bloco principal do programa */
find first param-global no-lock no-error.
find first mgcad.empresa
     where empresa.ep-codigo = param-global.empresa-prin no-lock no-error. 

assign c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i FATURAMENTO * r}
assign c-sistema = trim(return-value).
{utp/ut-liter.i Mapa_de_Separaá∆o/Carregamento * r}
assign c-titulo-relat = trim(return-value).

view frame f-cabec.
IF tt-param.tipo-rel = 1 THEN
   VIEW FRAME f-cab-nf.
ELSE
    VIEW FRAME f-cab-ped.
view frame f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

DEF STREAM email.
ASSIGN c-aux-e-mail = SESSION:TEMP-DIRECTORY + "aux-e-mail.txt".

DEF BUFFER b-tt-work FOR tt-work.


CASE tt-param.tipo-rel.
    WHEN 1 THEN DO: /* SELEÄ«O NOTA FISCAL */
        FIND FIRST tt-digita NO-LOCK NO-ERROR.
        IF AVAIL tt-digita THEN DO: /* Houve digitaá∆o */
           FOR EACH tt-digita:
               FIND nota-fiscal WHERE 
                    nota-fiscal.cod-estabel = tt-param.cod-estabel AND 
                    nota-fiscal.serie       = tt-param.serie       AND 
                    nota-fiscal.nr-nota-fis = tt-digita.nr-nota-fis
                    NO-LOCK NO-ERROR.

               RUN pi-acompanhar IN h-acomp (INPUT "NF: " + nota-fiscal.nr-nota-fis).

               IF nota-fiscal.nome-transp  < tt-param.transp-ini       OR
                  nota-fiscal.nome-transp  > tt-param.transp-fin       OR 
                  nota-fiscal.estado       < tt-param.estado-ini       OR 
                  nota-fiscal.estado       > tt-param.estado-fin       OR 
                  nota-fiscal.nome-ab-cli  < tt-param.nome-ab-cli-ini  OR
                  nota-fiscal.nome-ab-cli  > tt-param.nome-ab-cli-fin  OR 
                  nota-fiscal.no-ab-reppri < tt-param.no-ab-reppri-ini OR 
                  nota-fiscal.no-ab-reppri > tt-param.no-ab-reppri-fin THEN NEXT.

               FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK,
                   EACH ped-item-rom WHERE 
                        ped-item-rom.nome-abrev   = it-nota-fisc.nome-ab-cli AND 
                        ped-item-rom.nr-pedcli    = it-nota-fisc.nr-pedcli   AND 
                        ped-item-rom.nr-sequencia = it-nota-fisc.nr-seq-ped NO-LOCK,
                   FIRST ob-etiqueta WHERE
                         ob-etiqueta.cod-estabel  = ped-item-rom.cod-estabel AND
                         ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta NO-LOCK: 

                   FIND ITEM WHERE 
                        ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.

                   CREATE tt-work.
                   ASSIGN tt-work.nome-transp  = nota-fiscal.nome-transp
                          tt-work.seq-caminhao = 0
                          tt-work.nr-nota-fis  = it-nota-fisc.nr-nota-fis   
                          tt-work.nr-seq-fat   = it-nota-fisc.nr-seq-fat    
                          tt-work.it-codigo    = it-nota-fisc.it-codigo     
                          tt-work.cod-refer    = it-nota-fisc.cod-refer  
                          tt-work.nr-lote      = ob-etiqueta.nr-lote
                          tt-work.quantidade   = ob-etiqueta.quantidade
                          tt-work.peso-bruto   = ROUND(ob-etiqueta.quantidade * 
                                                      (it-nota-fisc.peso-bruto / it-nota-fisc.qt-faturada[1]),2)    
                          tt-work.num-etiqueta = ob-etiqueta.num-etiqueta   
                          tt-work.acondic      = ob-etiqueta.acondic
                          tt-work.localizacao  = ob-etiqueta.localizacao
                          tt-work.nome-abrev   = it-nota-fisc.nome-ab-cli
                          tt-work.nr-volume    = ped-item-rom.nr-volume
                          tt-work.ind-volume   = 1.
               END.
           END.
        END.
        ELSE DO: /* Houve seleá∆o */
           FOR EACH nota-fiscal WHERE
                    nota-fiscal.cod-estabel  =  tt-param.cod-estabel     AND 
                    nota-fiscal.serie        =  tt-param.serie           AND 
                    nota-fiscal.nr-nota-fis  >= tt-param.nr-nota-fis-ini AND
                    nota-fiscal.nr-nota-fis  <= tt-param.nr-nota-fis-fin AND
                    nota-fiscal.dt-cancela    = ? NO-LOCK:

               RUN pi-acompanhar IN h-acomp (INPUT "NF: " + nota-fiscal.nr-nota-fis).

               IF nota-fiscal.nome-transp  < tt-param.transp-ini       OR
                  nota-fiscal.nome-transp  > tt-param.transp-fin       OR 
                  nota-fiscal.estado       < tt-param.estado-ini       OR 
                  nota-fiscal.estado       > tt-param.estado-fin       OR 
                  nota-fiscal.nome-ab-cli  < tt-param.nome-ab-cli-ini  OR
                  nota-fiscal.nome-ab-cli  > tt-param.nome-ab-cli-fin  OR 
                  nota-fiscal.no-ab-reppri < tt-param.no-ab-reppri-ini OR 
                  nota-fiscal.no-ab-reppri > tt-param.no-ab-reppri-fin THEN NEXT.

               FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK,
                   EACH ped-item-rom WHERE
                        ped-item-rom.nome-abrev   = it-nota-fisc.nome-ab-cli AND
                        ped-item-rom.nr-pedcli    = it-nota-fisc.nr-pedcli   AND
                        ped-item-rom.nr-sequencia = it-nota-fisc.nr-seq-ped  NO-LOCK,
                  FIRST ob-etiqueta WHERE
                        ob-etiqueta.cod-estabel  = ped-item-rom.cod-estabel AND
                        ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta NO-LOCK.

                   FIND ITEM WHERE ITEM.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.

                   CREATE tt-work.
                   ASSIGN tt-work.nome-transp  = nota-fiscal.nome-transp
                          tt-work.seq-caminhao = 0
                          tt-work.nr-nota-fis  = it-nota-fisc.nr-nota-fis   
                          tt-work.nr-seq-fat   = it-nota-fisc.nr-seq-fat    
                          tt-work.it-codigo    = it-nota-fisc.it-codigo     
                          tt-work.cod-refer    = it-nota-fisc.cod-refer  
                          tt-work.nr-lote      = ob-etiqueta.nr-lote
                          tt-work.quantidade   = ob-etiqueta.quantidade
                          tt-work.peso-bruto   = ROUND(ob-etiqueta.quantidade * 
                                                      (it-nota-fisc.peso-bruto / it-nota-fisc.qt-faturada[1]),2)    
                          tt-work.num-etiqueta = ob-etiqueta.num-etiqueta   
                          tt-work.acondic      = ob-etiqueta.acondic
                          tt-work.localizacao  = ob-etiqueta.localizacao
                          tt-work.nome-abrev   = it-nota-fisc.nome-ab-cli
                          tt-work.nr-volume    = ped-item-rom.nr-volume
                          tt-work.ind-volume   = 1.
               END.
           END.
        END.
    END.
    WHEN 2 THEN DO: /* SELEÄ«O PEDIDOS */
        FIND FIRST tt-digita NO-LOCK NO-ERROR.
        IF AVAIL tt-digita THEN DO: /* Houve digitaá∆o */
           FOR EACH tt-digita:
               FIND ped-venda WHERE
                    ped-venda.nr-pedcli = tt-digita.nr-pedcli NO-LOCK NO-ERROR.
               IF NOT AVAIL ped-venda THEN NEXT.
               IF ped-venda.cod-estabel <> tt-param.cod-estabel THEN NEXT.

               FOR EACH ped-item OF ped-venda NO-LOCK.

                   RUN pi-acompanhar IN h-acomp (INPUT "PEDIDO: " + ped-venda.nr-pedcli).

                   FOR EACH ped-item-res WHERE
                            ped-item-res.cod-estabel  = ped-venda.cod-estabel AND
                            ped-item-res.nome-abrev   = ped-venda.nome-abrev  AND
                            ped-item-res.nr-pedcli    = ped-venda.nr-pedcli   AND
                            ped-item-res.nr-sequencia = ped-item.nr-sequencia AND
                            ped-item-res.faturado     = NO  
                            NO-LOCK.

                       FOR EACH ped-item-rom WHERE
                                ped-item-rom.cod-estabel  = ped-item-res.cod-estabel AND
                                ped-item-rom.nome-abrev   = ped-item-res.nome-abrev AND
                                ped-item-rom.nr-pedcli    = ped-item-res.nr-pedcli  AND
                                ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                                NO-LOCK.

                           FIND ob-etiqueta WHERE
                                ob-etiqueta.cod-estabel  = ped-item-rom.cod-estabel AND
                                ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                                NO-LOCK NO-ERROR.

                           IF NOT AVAIL ob-etiqueta THEN NEXT.

                           FIND ITEM WHERE 
                                ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

                           CREATE tt-work.
                           ASSIGN tt-work.nome-transp  = ped-venda.nome-transp
                                  tt-work.seq-caminhao = 0
                                  tt-work.nr-nota-fis  = ped-venda.nr-pedcli   
                                  tt-work.nr-seq-fat   = ped-item.nr-sequencia
                                  tt-work.it-codigo    = ped-item.it-codigo     
                                  tt-work.cod-refer    = ped-item.cod-refer  
                                  tt-work.nr-lote      = ob-etiqueta.nr-lote
                                  tt-work.quantidade   = ob-etiqueta.quantidade
                                  tt-work.num-etiqueta = ob-etiqueta.num-etiqueta   
                                  tt-work.peso-bruto   = ob-etiqueta.peso-bruto
                                  tt-work.acondic      = ob-etiqueta.acondic
                                  tt-work.localizacao  = ob-etiqueta.localizacao
                                  tt-work.nome-abrev   = ped-venda.nome-abrev
                                  tt-work.ind-volume   = 1.
                       END.
                   END.
               END.
           END.
        END.
        ELSE DO: /* Houve Seleá∆o */
           FOR EACH ped-venda WHERE
                    ped-venda.nr-pedcli >= tt-param.nr-pedcli-ini AND
                    ped-venda.nr-pedcli <= tt-param.nr-pedcli-fin NO-LOCK.
               IF ped-venda.cod-estabel <> tt-param.cod-estabel THEN NEXT.
               FOR EACH ped-item OF ped-venda NO-LOCK.

                   RUN pi-acompanhar IN h-acomp (INPUT "PEDIDO: " + ped-venda.nr-pedcli).

                   FOR EACH ped-item-res WHERE
                            ped-item-res.cod-estabel  = ped-venda.cod-estabel AND
                            ped-item-res.nome-abrev   = ped-venda.nome-abrev  AND
                            ped-item-res.nr-pedcli    = ped-venda.nr-pedcli   AND
                            ped-item-res.nr-sequencia = ped-item.nr-sequencia AND
                            ped-item-res.faturado     = NO 
                            NO-LOCK.

                       FOR EACH ped-item-rom WHERE
                                ped-item-rom.cod-estabel  = ped-item-res.cod-estabel AND
                                ped-item-rom.nome-abrev   = ped-item-res.nome-abrev  AND
                                ped-item-rom.nr-pedcli    = ped-item-res.nr-pedcli   AND
                                ped-item-rom.nr-sequencia = ped-item-res.nr-sequencia
                                NO-LOCK.

                           FIND ob-etiqueta WHERE
                                ob-etiqueta.cod-estabel  = ped-item-rom.cod-estabel AND
                                ob-etiqueta.num-etiqueta = ped-item-rom.num-etiqueta
                                NO-LOCK NO-ERROR.

                           IF NOT AVAIL ob-etiqueta THEN NEXT.

                           FIND ITEM WHERE 
                                ITEM.it-codigo = ob-etiqueta.it-codigo NO-LOCK NO-ERROR.

                           CREATE tt-work.
                           ASSIGN tt-work.nome-transp  = ped-venda.nome-transp
                                  tt-work.seq-caminhao = 0
                                  tt-work.nr-nota-fis  = ped-venda.nr-pedcli   
                                  tt-work.nr-seq-fat   = ped-item.nr-sequencia
                                  tt-work.it-codigo    = ped-item.it-codigo     
                                  tt-work.cod-refer    = ped-item.cod-refer  
                                  tt-work.nr-lote      = ob-etiqueta.nr-lote
                                  tt-work.quantidade   = ob-etiqueta.quantidade
                                  tt-work.num-etiqueta = ob-etiqueta.num-etiqueta   
                                  tt-work.peso-bruto   = ob-etiqueta.peso-bruto
                                  tt-work.acondic      = ob-etiqueta.acondic
                                  tt-work.localizacao  = ob-etiqueta.localizacao
                                  tt-work.nome-abrev   = ped-venda.nome-abrev
                                  tt-work.ind-volume   = 1.
                       END.
                   END.
               END.
           END.
        END.
    END.
END CASE.


CASE tt-param.classifica:
    WHEN 1 THEN DO: /* Por Transportador/Localizaá∆o */
        FOR EACH tt-work BREAK BY tt-work.nome-transp
                               BY tt-work.localizacao
                               BY tt-work.it-codigo
                               BY tt-work.cod-refer
                               BY tt-work.nr-nota-fis:
            IF LAST-OF(tt-work.nome-transp) THEN
               ASSIGN i-seq-caminhao = 1
                      de-peso-par    = 0.
            IF de-peso-par + tt-work.peso-bruto > tt-param.peso-max-carga THEN
               ASSIGN i-seq-caminhao = i-seq-caminhao + 1
                      de-peso-par    = 0.

            ASSIGN tt-work.seq-caminhao = i-seq-caminhao
                   de-peso-par          = de-peso-par + tt-work.peso-bruto.
        END.

        ASSIGN de-peso-par = 0.
        FOR EACH tt-work BREAK BY tt-work.nome-transp
                               BY tt-work.seq-caminhao
                               BY tt-work.localizacao
                               BY tt-work.it-codigo
                               BY tt-work.cod-refer
                               BY tt-work.nr-nota-fis:
            ASSIGN i-cont-par  = i-cont-par + tt-work.ind-volume
                   i-cont-cam  = i-cont-cam + tt-work.ind-volume
                   i-cont-ger  = i-cont-ger + tt-work.ind-volume                
                   de-qtd-par  = de-qtd-par + tt-work.quantidade
                   de-qtd-cam  = de-qtd-cam + tt-work.quantidade
                   de-qtd-ger  = de-qtd-ger + tt-work.quantidade
                   de-peso-par = de-peso-par + tt-work.peso-bruto
                   de-peso-cam = de-peso-cam + tt-work.peso-bruto
                   de-peso-ger = de-peso-ger + tt-work.peso-bruto.

            FIND ITEM WHERE
                 ITEM.it-codigo = tt-work.it-codigo NO-LOCK.

            IF FIRST-OF(tt-work.seq-caminhao) THEN DO:
               FIND transporte WHERE 
                    transporte.nome-abrev = tt-work.nome-transp NO-LOCK NO-ERROR.
               IF AVAIL transporte THEN
                  DISPLAY transporte.cod-transp
                          transporte.nome
                          tt-work.seq-caminhao
                          WITH FRAME f-cab-transp.
            END.

            IF INT(tt-work.num-etiqueta) <> 0 THEN
               ASSIGN c-num-etiqueta = STRING(tt-work.num-etiqueta,"999999999").
            ELSE
               ASSIGN c-num-etiqueta = "Vol: " + STRING(tt-work.nr-volume,"999999").

            DISPLAY tt-work.it-codigo  
                    tt-work.cod-refer 
                    tt-work.nr-lote
                    ITEM.desc-item
                    tt-work.localizacao  
                    c-num-etiqueta       @ tt-work.num-etiqueta
                    tt-work.quantidade   
                    tt-work.nr-nota-fis  
                    tt-work.nome-abrev
                    tt-work.nome-transp
                    WITH FRAME f-detalhe.
            DOWN WITH FRAME f-detalhe.

            IF LAST-OF(tt-work.localizacao) THEN DO:
               ASSIGN c-tipo-total = "da Localizaá∆o".
               DISPLAY c-tipo-total
                       i-cont-par
                       de-qtd-par
                       de-peso-par
                       WITH FRAME f-subtotal.
               DOWN WITH FRAME f-subtotal.

               ASSIGN i-cont-par  = 0
                      de-qtd-par  = 0
                      de-peso-par = 0.
            END.

            IF LAST-OF(tt-work.seq-caminhao) THEN DO:
               ASSIGN c-tipo-total = "do Caminh∆o".
               DISPLAY c-tipo-total
                       i-cont-cam  @ i-cont-par 
                       de-qtd-cam  @ de-qtd-par 
                       de-peso-cam @ de-peso-par
                       WITH FRAME f-subtotal.
               DOWN WITH FRAME f-subtotal.

               ASSIGN i-cont-cam  = 0
                      de-qtd-cam  = 0
                      de-peso-cam = 0.
            END.
        END.
    END.
    WHEN 2 THEN DO: /* Por Transportador/Produto */
        FOR EACH tt-work BREAK BY tt-work.nome-transp
                               BY tt-work.it-codigo
                               BY tt-work.cod-refer
                               BY tt-work.localizacao
                               BY tt-work.nr-nota-fis:
            IF LAST-OF(tt-work.nome-transp) THEN
               ASSIGN i-seq-caminhao = 1
                      de-peso-par    = 0.
            IF de-peso-par + tt-work.peso-bruto > tt-param.peso-max-carga THEN
               ASSIGN i-seq-caminhao = i-seq-caminhao + 1
                      de-peso-par    = 0.
            ASSIGN tt-work.seq-caminhao = i-seq-caminhao
                   de-peso-par          = de-peso-par + tt-work.peso-bruto.
        END.
        ASSIGN de-peso-par = 0.
        FOR EACH tt-work BREAK BY tt-work.nome-transp
                               BY tt-work.seq-caminhao
                               BY tt-work.it-codigo
                               BY tt-work.cod-refer
                               BY tt-work.localizacao
                               BY tt-work.nr-nota-fis:
            ASSIGN i-cont-par  = i-cont-par + tt-work.ind-volume
                   i-cont-cam  = i-cont-cam + tt-work.ind-volume
                   i-cont-ger  = i-cont-ger + tt-work.ind-volume
                   de-qtd-par  = de-qtd-par + tt-work.quantidade
                   de-qtd-cam  = de-qtd-cam + tt-work.quantidade
                   de-qtd-ger  = de-qtd-ger + tt-work.quantidade
                   de-peso-par = de-peso-par + tt-work.peso-bruto
                   de-peso-cam = de-peso-cam + tt-work.peso-bruto
                   de-peso-ger = de-peso-ger + tt-work.peso-bruto.

            FIND ITEM WHERE 
                 ITEM.it-codigo = tt-work.it-codigo NO-LOCK.

            IF FIRST-OF(tt-work.seq-caminhao) THEN DO:
               FIND transporte WHERE
                    transporte.nome-abrev = tt-work.nome-transp NO-LOCK NO-ERROR.
               IF AVAIL transporte THEN
                  DISPLAY transporte.cod-transp
                          transporte.nome
                          tt-work.seq-caminhao
                          WITH FRAME f-cab-transp.
            END.

            DISPLAY tt-work.it-codigo   
                    tt-work.cod-refer 
                    tt-work.nr-lote
                    ITEM.desc-item      
                    tt-work.localizacao 
                    tt-work.num-etiqueta
                    tt-work.quantidade 
                    tt-work.nr-nota-fis 
                    tt-work.nome-abrev
                    tt-work.nome-transp
                    WITH FRAME f-detalhe.
            DOWN WITH FRAME f-detalhe.

            IF LAST-OF(tt-work.cod-refer) THEN DO:
               ASSIGN c-tipo-total = "do Produto/Refer.".
               DISPLAY c-tipo-total
                       i-cont-par
                       de-qtd-par
                       de-peso-par
                       WITH FRAME f-subtotal.
               DOWN WITH FRAME f-subtotal.

               ASSIGN i-cont-par  = 0
                      de-qtd-par  = 0
                      de-peso-par = 0.
            END.

            IF LAST-OF(tt-work.seq-caminhao) THEN DO:
               ASSIGN c-tipo-total = "do Caminh∆o".
               DISPLAY c-tipo-total
                       i-cont-cam  @ i-cont-par 
                       de-qtd-cam  @ de-qtd-par 
                       de-peso-cam @ de-peso-par
                       WITH FRAME f-subtotal.
               DOWN WITH FRAME f-subtotal.

               ASSIGN i-cont-cam  = 0
                      de-qtd-cam  = 0
                      de-peso-cam = 0.
            END.
        END.
    END.
    WHEN 3 THEN DO: /* Transpotador/Nota Fiscal */
        FOR EACH tt-work BREAK BY tt-work.nome-transp
                               BY tt-work.nr-nota-fis
                               BY tt-work.localizacao
                               BY tt-work.it-codigo
                               BY tt-work.cod-refer:
            IF LAST-OF(tt-work.nome-transp) THEN
               ASSIGN i-seq-caminhao = 1
                      de-peso-par    = 0.
            IF de-peso-par + tt-work.peso-bruto > tt-param.peso-max-carga THEN
               ASSIGN i-seq-caminhao = i-seq-caminhao + 1
                      de-peso-par    = 0.

            ASSIGN tt-work.seq-caminhao = i-seq-caminhao
                   de-peso-par          = de-peso-par + tt-work.peso-bruto.
        END.
        ASSIGN de-peso-par = 0.
        FOR EACH tt-work BREAK BY tt-work.nome-transp
                               BY tt-work.seq-caminhao
                               BY tt-work.nr-nota-fis
                               BY tt-work.localizacao
                               BY tt-work.it-codigo
                               BY tt-work.cod-refer:
            ASSIGN i-cont-par  = i-cont-par + tt-work.ind-volume
                   i-cont-cam  = i-cont-cam + tt-work.ind-volume
                   i-cont-ger  = i-cont-ger + tt-work.ind-volume
                   de-qtd-par  = de-qtd-par + tt-work.quantidade
                   de-qtd-cam  = de-qtd-cam + tt-work.quantidade
                   de-qtd-ger  = de-qtd-ger + tt-work.quantidade
                   de-peso-par = de-peso-par + tt-work.peso-bruto
                   de-peso-cam = de-peso-cam + tt-work.peso-bruto
                   de-peso-ger = de-peso-ger + tt-work.peso-bruto.

            FIND ITEM WHERE
                 ITEM.it-codigo = tt-work.it-codigo NO-LOCK.

            IF FIRST-OF(tt-work.seq-caminhao) THEN DO:
               FIND transporte WHERE 
                    transporte.nome-abrev = tt-work.nome-transp NO-LOCK NO-ERROR.
               IF AVAIL transporte THEN
                  DISPLAY transporte.cod-transp
                          transporte.nome
                          tt-work.seq-caminhao
                          WITH FRAME f-cab-transp.
            END.

            DISPLAY tt-work.it-codigo   
                    tt-work.cod-refer 
                    tt-work.nr-lote
                    ITEM.desc-item      
                    tt-work.localizacao 
                    tt-work.num-etiqueta
                    tt-work.quantidade 
                    tt-work.nr-nota-fis 
                    tt-work.nome-abrev
                    tt-work.nome-transp
                    WITH FRAME f-detalhe.
            DOWN WITH FRAME f-detalhe.

            IF LAST-OF(tt-work.nr-nota-fis) THEN DO:
               IF tt-param.tipo-rel = 1 THEN
                  ASSIGN c-tipo-total = "da Nota Fiscal".
               ELSE
                  ASSIGN c-tipo-total = "do Pedido".

               DISPLAY c-tipo-total
                       i-cont-par
                       de-qtd-par
                       de-peso-par
                       WITH FRAME f-subtotal.
               DOWN WITH FRAME f-subtotal.

               ASSIGN i-cont-par  = 0
                      de-qtd-par  = 0
                      de-peso-par = 0.
            END.

            IF LAST-OF(tt-work.seq-caminhao) THEN DO:
               ASSIGN c-tipo-total = "do Caminh∆o".
               DISPLAY c-tipo-total
                       i-cont-cam  @ i-cont-par 
                       de-qtd-cam  @ de-qtd-par 
                       de-peso-cam @ de-peso-par
                       WITH FRAME f-subtotal.
               DOWN WITH FRAME f-subtotal.

               ASSIGN i-cont-cam  = 0
                      de-qtd-cam  = 0
                      de-peso-cam = 0.
            END.
        END.
    END.
    WHEN 4 THEN DO: /* Por Localizaá∆o */
        FOR EACH tt-work BREAK BY tt-work.localizacao
                               BY tt-work.it-codigo
                               BY tt-work.cod-refer
                               BY tt-work.nr-nota-fis:
            ASSIGN i-cont-par  = i-cont-par + 1 
                   i-cont-ger  = i-cont-ger + 1 
                   de-qtd-par  = de-qtd-par + tt-work.quantidade
                   de-qtd-ger  = de-qtd-ger + tt-work.quantidade
                   de-peso-par = de-peso-par + tt-work.peso-bruto
                   de-peso-ger = de-peso-ger + tt-work.peso-bruto.

            FIND ITEM WHERE
                 ITEM.it-codigo = tt-work.it-codigo NO-LOCK.

            DISPLAY tt-work.it-codigo  
                    tt-work.cod-refer  
                    tt-work.nr-lote
                    ITEM.desc-item
                    tt-work.localizacao  
                    tt-work.num-etiqueta 
                    tt-work.quantidade   
                    tt-work.nr-nota-fis  
                    tt-work.nome-abrev
                    tt-work.nome-transp
                    WITH FRAME f-detalhe.
            DOWN WITH FRAME f-detalhe.

            IF LAST-OF(tt-work.localizacao) THEN DO:
               ASSIGN c-tipo-total = "da Localizaá∆o".
               DISPLAY c-tipo-total
                       i-cont-par
                       de-qtd-par
                      de-peso-par 
                       WITH FRAME f-subtotal.
               DOWN WITH FRAME f-subtotal.

               ASSIGN i-cont-par  = 0
                      de-qtd-par  = 0
                      de-peso-par = 0.
            END.
        END.
    END.
    WHEN 5 THEN DO: /* Por Produto */
        FOR EACH tt-work BREAK BY tt-work.it-codigo 
                               BY tt-work.cod-refer 
                               BY tt-work.localizacao
                               BY tt-work.nr-nota-fis:
           ASSIGN i-cont-par  = i-cont-par + tt-work.ind-volume
                  i-cont-ger  = i-cont-ger + tt-work.ind-volume
                  de-qtd-par  = de-qtd-par + tt-work.quantidade
                  de-qtd-ger  = de-qtd-ger + tt-work.quantidade
                  de-peso-par = de-peso-par + tt-work.peso-bruto
                  de-peso-ger = de-peso-ger + tt-work.peso-bruto.

           FIND ITEM WHERE
                ITEM.it-codigo = tt-work.it-codigo NO-LOCK.

           DISPLAY tt-work.it-codigo  
                   tt-work.cod-refer  
                   tt-work.nr-lote
                   ITEM.desc-item
                   tt-work.localizacao  
                   tt-work.num-etiqueta 
                   tt-work.quantidade   
                   tt-work.nr-nota-fis  
                   tt-work.nome-abrev
                   tt-work.nome-transp
                   WITH FRAME f-detalhe.
           DOWN WITH FRAME f-detalhe.

           IF LAST-OF(tt-work.cod-refer) THEN DO:
              ASSIGN c-tipo-total = "do Produto/Ref.".
              DISPLAY c-tipo-total
                      i-cont-par
                      de-qtd-par
                      de-peso-par
                      WITH FRAME f-subtotal.
              DOWN WITH FRAME f-subtotal.

              ASSIGN i-cont-par  = 0
                     de-qtd-par  = 0
                     de-peso-par = 0.
           END.
       END.
    END.
    WHEN 6 THEN DO: /* Por Nota Fiscal/Pedido */
        FOR EACH tt-work BREAK BY tt-work.nr-nota-fis
                               BY tt-work.localizacao
                               BY tt-work.it-codigo
                               BY tt-work.cod-refer
                               BY tt-work.num-etiqueta:
            ASSIGN i-cont-par  = i-cont-par + 1 /*tt-work.ind-volume*/
                   i-cont-cam  = i-cont-cam + 1 /*tt-work.ind-volume*/
                   i-cont-ger  = i-cont-ger + 1 /*tt-work.ind-volume*/
                   de-qtd-par  = de-qtd-par + tt-work.quantidade
                   de-qtd-cam  = de-qtd-cam + tt-work.quantidade
                   de-qtd-ger  = de-qtd-ger + tt-work.quantidade
                   de-peso-par = de-peso-par + tt-work.peso-bruto
                   de-peso-cam = de-peso-cam + tt-work.peso-bruto
                   de-peso-ger = de-peso-ger + tt-work.peso-bruto.

            FIND ITEM WHERE
                 ITEM.it-codigo = tt-work.it-codigo NO-LOCK.

            DISPLAY tt-work.it-codigo  
                    tt-work.cod-refer 
                    tt-work.nr-lote
                    ITEM.desc-item
                    tt-work.localizacao  
                    tt-work.num-etiqueta 
                    tt-work.quantidade   
                    tt-work.nr-nota-fis  
                    tt-work.nome-abrev
                    tt-work.nome-transp
                    WITH FRAME f-detalhe.
            DOWN WITH FRAME f-detalhe.

            IF LAST-OF(tt-work.nr-nota-fis) THEN DO:
               IF tt-param.tipo-rel = 1 THEN
                  ASSIGN c-tipo-total = "da Nota Fiscal".
               ELSE
                  ASSIGN c-tipo-total = "do Pedido".

               DISPLAY c-tipo-total
                       i-cont-par
                       de-qtd-par
                       de-peso-par
                       WITH FRAME f-subtotal.
               DOWN WITH FRAME f-subtotal.

               ASSIGN i-cont-par  = 0
                      de-qtd-par  = 0
                      de-peso-par = 0.
            END.
        END.
    END.
END CASE.
ASSIGN c-tipo-total = "Geral".
DISPLAY c-tipo-total
        i-cont-ger  @ i-cont-par
        de-qtd-ger  @ de-qtd-par
        de-peso-ger @ de-peso-par
        WITH FRAME f-subtotal.
DOWN WITH FRAME f-subtotal.

ASSIGN i-cont-ger  = 0
       de-qtd-ger  = 0
       de-peso-ger = 0.


IF tt-param.enviar-e-mail THEN DO:
   FOR EACH tt-work BREAK BY tt-work.nome-transp
                          BY tt-work.nr-nota-fis:
       IF FIRST-OF(tt-work.nome-transp) THEN DO:
          FIND transporte WHERE
               transporte.nome-abrev = tt-work.nome-transp NO-LOCK NO-ERROR.
          OUTPUT STREAM email TO value(c-aux-e-mail).
          PUT STREAM email
              c-empresa " - "
              "DADOS PARA COLETA DE MERCADORIAS PARA TRANSPORTE "
              "- DATA: " TODAY " " STRING(TIME,"HH:MM")
              SKIP(1)
              "Transportador: " 
              transporte.cod-transp " - " 
              transporte.nome
              SKIP(1)
              "Nr.Nota Cliente                                 CNPJ              " AT  1
              "Frete Volumes    Peso da Nota     Valor da Nota"                    AT 68
              "------- --------------------------------------- ------------------" AT  1
              "----- ------- --------------- -----------------"                    AT 68
              SKIP.
       END.

       ASSIGN i-tot-vol-not = i-tot-vol-not + 1
              i-tot-vol-tra = i-tot-vol-tra + 1.

       IF LAST-OF(tt-work.nr-nota-fis) THEN DO:
          FIND nota-fiscal WHERE nota-fiscal.cod-estabel = tt-param.cod-estabel
                             AND nota-fiscal.serie       = tt-param.serie
                             AND nota-fiscal.nr-nota-fis = tt-work.nr-nota-fis
                           NO-LOCK NO-ERROR.
          FIND emitente WHERE emitente.cod-emitente = nota-fiscal.cod-emitente
                        NO-LOCK NO-ERROR.

          PUT STREAM email
              tt-work.nr-nota-fis      FORMAT "x(7)"                   AT  1
              emitente.nome-emit                                       AT  9
              nota-fiscal.cgc          FORMAT "99.999.999/9999-99"     AT 49
              IF nota-fiscal.cidade-cif <> "" THEN "CIF" 
                                              ELSE "FOB" FORMAT "x(5)" AT 68
              i-tot-vol-not            FORMAT ">>>,>>9"                AT 74
              nota-fiscal.peso-bru-tot FORMAT ">>>,>>>,>>9.999"        AT 82 
              nota-fiscal.vl-tot-nota  FORMAT ">>,>>>,>>>,>>9.99"      AT 98
              SKIP.
          ASSIGN i-tot-vol-not  = 0
                 i-tot-not      = i-tot-not + 1
                 de-tot-pes-tra = de-tot-pes-tra + nota-fiscal.peso-bru-tot.
       END.

       IF LAST-OF(tt-work.nome-transp) THEN DO:
          PUT STREAM email
              "Total:"                                AT  1
              i-tot-not      FORMAT ">>>,>>9"         AT  9
              i-tot-vol-tra  FORMAT ">>>,>>9"         AT 74
              de-tot-pes-tra FORMAT ">>>,>>>,>>9.999" AT 82
              SKIP.
          OUTPUT STREAM email CLOSE.

          ASSIGN i-tot-vol-tra  = 0
                 i-tot-not      = 0
                 de-tot-pes-tra = 0.
      
          /*
          IF "gilvando@teartextil.com.br" MATCHES "*@*" THEN DO:
          */
          IF transporte.e-mail MATCHES "*@*" THEN DO:
             /*
             ASSIGN c-destinatar = "paulo.pires@teartextil.com.br" + "," + 
                                   "gilvando@teartextil.com.br" + "," +
                                   "albino.junior@teartextil.com.br".
             */
             ASSIGN c-destinatar = transporte.e-mail + "," + tt-param.e-mail-remet + "," + 
                                   "teartextil@teartextil.com.br".
             /* Envio de e-mail via Blat ---------------------------------------------
             RUN esapi/esapi002.p (INPUT tt-param.e-mail-remet, /* e-mail remetente */
                                   INPUT c-destinatar, /* e-mail destinat†rio */
                                   INPUT tt-param.subject-e-mail, /* Assunto */
                                   INPUT tt-param.texto-e-mail, /* Mensagem */
                                   INPUT c-aux-e-mail, /*arquivo anexo*/
                                   INPUT YES). /* Mostra Erros */
             */

             /* Envio de e-mail via Outlook ou MSOutlook -------------------------*/
             RUN esapi/esapi001.p (INPUT c-destinatar, /* e-mail destinat†rio */
                                   INPUT tt-param.subject-e-mail, /* Assunto */   
                                   INPUT tt-param.texto-e-mail, /* Mensagem */    
                                   INPUT c-aux-e-mail, /*arquivo anexo*/          
                                   INPUT YES). /* Mostra Erros */                 

             ASSIGN i-e-mail-enviado = i-e-mail-enviado + 1.
          END.
          ELSE DO:
             OUTPUT STREAM email CLOSE.
             ASSIGN i-e-mail-nenviado = i-e-mail-nenviado + 1.
          END.
          OS-DELETE VALUE(c-aux-e-mail).
       END.
   END.
END.

IF tt-param.impr-param THEN DO:
   PAGE.
   display tt-param.desc-classifica
           tt-param.cod-estabel
           tt-param.serie
           tt-param.nr-nota-fis-ini  
           tt-param.nr-nota-fis-fin  
           tt-param.transp-ini      
           tt-param.transp-fin      
           tt-param.estado-ini      
           tt-param.estado-fin      
           tt-param.nome-ab-cli-ini 
           tt-param.nome-ab-cli-fin 
           tt-param.no-ab-reppri-ini
           tt-param.no-ab-reppri-fin
           tt-param.corte-com-ini   
           tt-param.corte-com-fin
           tt-param.peso-max-carga
           tt-param.enviar-e-mail
           tt-param.e-mail-remet
           i-e-mail-enviado      
           i-e-mail-nenviado     
           with frame f-param.
END.


/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

