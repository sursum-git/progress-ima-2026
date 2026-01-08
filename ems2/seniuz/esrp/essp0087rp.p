/* Programa: ESSP0087
** Sistema.: Magnus da Datasul
** Modulo..: Especifico
** Objetivo: Relatorio de ImpressÆo para Conferˆncia do Inventario da Expedi‡Æo
** Data....: 
** Obs.....: 
**
*/

/* include de controle de versÆo */
{include/i-prgvrs.i ESSP0087RP 2.04.00.000}

DEFINE temp-table tt-param  no-undo
       field destino           as integer
       field arquivo           as char format "x(35)"
       field usuario           as char format "x(12)"
       field data-exec         as date
       field hora-exec         as integer
       field classifica        as integer
       field desc-classifica   as char format "x(40)"
       FIELD cod-estabel       AS CHAR
       FIELD dt-invent         AS DATE
       FIELD ini-it-codigo     LIKE inv-acab.it-codigo
       FIELD fin-it-codigo     LIKE inv-acab.it-codigo
       FIELD ini-localiz       LIKE ob-etiqueta.localiz
       FIELD fin-localiz       LIKE ob-etiqueta.localiz
       FIELD tipo-conf         AS INTEGER
       FIELD imp-param         AS LOG.
                                            
define temp-table tt-raw-digita
       field raw-digita as raw.

def input parameter raw-param as raw no-undo.
def input parameter table for tt-raw-digita.


create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* include padrÆo para impressÆo de campos editores em relat¢rio  */
{include/tt-edit.i}

/* defini‡Æo de vari veis  */
DEF BUFFER b-inv-acab FOR inv-acab.

def var h-acomp as handle no-undo.

DEF VAR c-descricao  AS CHAR FORMAT "x(30)"           LABEL "Descricao".
DEF VAR c-un         LIKE item.un.
DEF VAR de-qtd-un    AS DEC  FORMAT ">>>,>>>,>>9.99"  LABEL "Total" INIT 0.
DEF VAR de-etq-un    AS DEC  FORMAT ">>>,>>>,>>9.99"                INIT 0.
DEF VAR de-qtd-ger   AS DEC  FORMAT ">>>,>>>,>>9.99"  LABEL "Total" INIT 0.
DEF VAR de-etq-ger   AS DEC  FORMAT ">>>,>>>,>>9.99"                INIT 0.

DEF VAR c-texto      AS CHAR FORMAT "x(20)".

FORM 
    "*------------ Parƒmetros/Sele‡Æo -------------*" SKIP
    tt-param.dt-invent        LABEL "Data............" SKIP
    tt-param.ini-it-codigo    LABEL "Item............"
    "A"  AT 30                
    tt-param.fin-it-codigo    NO-LABELS SKIP
    skip(1)
    with FRAME f-param side-labels no-box width 132 STREAM-IO.

FORM
   inv-acab.data-invent
   inv-acab.num-etiqueta FORMAT ">>>>>>>>9"  COLUMN-LABEL "Etiqueta" 
   inv-acab.it-codigo    FORMAT "x(8)" 
   c-descricao           FORMAT "x(30)" 
   inv-acab.cod-refer    FORMAT "x(5)"       COLUMN-LABEL "Ref" 
   inv-acab.qtd-inv      FORMAT ">,>>>,>>9.99"
   c-un                  FORMAT "x(3)"
   inv-acab.localizacao  FORMAT "999/999"
   c-texto               FORMAT "x(47)"
   WITH NO-BOX 55 DOWN WIDTH 152 STREAM-IO frame f-detalhe.

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST mgcad.empresa
     WHERE empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR. 

ASSIGN c-empresa = (if avail empresa then empresa.razao-social else "").

{utp/ut-liter.i ESPECÖFICOS * r}
ASSIGN c-sistema = trim(return-value).
{utp/ut-liter.i Conferˆncia_do_Invent rio_da_Expedi‡Æo * r}
ASSIGN c-titulo-relat = trim(return-value).

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

CASE tt-param.tipo-conf.
    WHEN 1 OR WHEN 3 THEN 
        RUN pi-simples-conf.
    WHEN 2 THEN 
        RUN pi-conf-localiz.
END CASE.


IF tt-param.imp-param THEN DO:
   PAGE.
   DISPLAY tt-param.cod-estabel
           tt-param.dt-invent
           tt-param.ini-it-codigo
           tt-param.fin-it-codigo
           with FRAME f-param.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

PROCEDURE pi-simples-conf.

    FOR EACH inv-acab WHERE
             inv-acab.cod-estabel  = tt-param.cod-estabel AND
             inv-acab.data-invent  = tt-param.dt-invent   AND
             inv-acab.it-codigo   >= tt-param.ini-it-codigo AND
             inv-acab.it-codigo   <= tt-param.fin-it-codigo AND
             inv-acab.localizacao >= tt-param.ini-localiz AND
             inv-acab.localizacao <= tt-param.fin-localiz NO-LOCK
             BREAK BY inv-acab.un
                   BY inv-acab.localizacao
                   BY inv-acab.it-codigo.

        RUN pi-acompanhar in h-acomp (INPUT "Etiqueta Inventario: " + STRING(inv-acab.num-etiqueta)).

        ASSIGN c-descricao = ''
               c-un = ''.

        FIND item WHERE 
             item.it-codigo = inv-acab.it-codigo NO-LOCK NO-ERROR.
        IF AVAIL item THEN 
           ASSIGN c-descricao = item.desc-item
                  c-un = item.un.
        ELSE
           ASSIGN c-descricao = "Item nao cadastrado !!!".

        ASSIGN de-qtd-un = de-qtd-un + inv-acab.qtd-inv
               de-etq-un = de-etq-un + 1.

        ASSIGN c-texto = "".
        FIND b-inv-acab WHERE
             b-inv-acab.cod-estab = inv-acab.cod-estab AND
             b-inv-acab.data-invent = inv-acab.data-invent AND
             b-inv-acab.num-etiqueta = inv-acab.num-etiqueta NO-LOCK NO-ERROR.
        IF AMBIGUOUS b-inv-acab THEN DO.
           FOR EACH b-inv-acab WHERE
                    b-inv-acab.cod-estab = inv-acab.cod-estab AND
                    b-inv-acab.data-invent = inv-acab.data-invent AND
                    b-inv-acab.num-etiqueta = inv-acab.num-etiqueta NO-LOCK.
               ASSIGN c-texto = IF c-texto = ""
                                THEN "INVENTARIADA NAS DOCAS " + b-inv-acab.localiz
                                ELSE c-texto + "," + b-inv-acab.localiz.
           END.
        END.

        IF tt-param.tipo-conf = 1 AND 
           c-texto = "" THEN NEXT.

        DISPLAY inv-acab.data-invent
                inv-acab.num-etiqueta
                inv-acab.it-codigo
                c-descricao
                inv-acab.cod-refer
                inv-acab.qtd-inv
                c-un
                inv-acab.localizacao
                c-texto
                WITH FRAME f-detalhe.

        DOWN WITH FRAME f-detalhe.

        IF LAST-OF(inv-acab.un) THEN DO.
           DOWN WITH FRAME f-detalhe.
           DISPLAY "------------" @ inv-acab.qtd-inv
                   WITH FRAME f-detalhe.

           DOWN WITH FRAME f-detalhe.
           DISPLAY "      Quantidade " + c-un @ c-descricao
                   de-qtd-un                  @ inv-acab.qtd-inv
                   WITH FRAME f-detalhe.
           DOWN WITH FRAME f-detalhe.

           DISPLAY "        Qt Pe‡as " + c-un @ c-descricao
                   de-etq-un                  @ inv-acab.qtd-inv        
                   WITH FRAME f-detalhe.
           DOWN 2 WITH FRAME f-detalhe.

           ASSIGN de-qtd-ger = de-qtd-ger + de-qtd-un
                  de-etq-ger = de-etq-ger + de-etq-un.

           ASSIGN de-qtd-un = 0
                  de-etq-un = 0.
        END.
    END.
    DOWN 1 WITH FRAME f-detalhe.
    DISPLAY "    Quantidade Total " @ c-descricao
            de-qtd-ger              @ inv-acab.qtd-inv
            WITH FRAME f-detalhe.
    DOWN WITH FRAME f-detalhe.

    DISPLAY "            Qt Pe‡as " @ c-descricao
            de-etq-ger              @ inv-acab.qtd-inv        
            WITH FRAME f-detalhe.
    DOWN WITH FRAME f-detalhe.

END PROCEDURE.

PROCEDURE pi-conf-localiz.
    FOR EACH b-inv-acab WHERE
             b-inv-acab.cod-estabel  = tt-param.cod-estabel AND
             b-inv-acab.data-invent  = tt-param.dt-invent   AND
             b-inv-acab.it-codigo   >= tt-param.ini-it-codigo AND
             b-inv-acab.it-codigo   <= tt-param.fin-it-codigo AND
             b-inv-acab.localizacao >= tt-param.ini-localiz AND
             b-inv-acab.localizacao <= tt-param.fin-localiz NO-LOCK
             BREAK BY b-inv-acab.localizacao.

        IF FIRST-OF(b-inv-acab.localiz) THEN DO.
           FOR EACH ob-etiqueta WHERE
                    ob-etiqueta.cod-estabel = tt-param.cod-estabel AND
                    ob-etiqueta.situacao    = 3 AND 
                    ob-etiqueta.it-codigo  >= tt-param.ini-it-codigo AND
                    ob-etiqueta.it-codigo  <= tt-param.fin-it-codigo AND
                    ob-etiqueta.localiz     = b-inv-acab.localiz NO-LOCK
                    BREAK BY ob-etiqueta.un
                          BY ob-etiqueta.localizacao
                          BY ob-etiqueta.it-codigo.
        
               RUN pi-acompanhar in h-acomp (INPUT "Etiqueta: " + STRING(ob-etiqueta.num-etiqueta)).
        
               ASSIGN c-descricao = ''
                      c-un = ''.
        
                FIND item WHERE 
                     item.it-codigo = ob-etiqueta.it-codigo 
                     NO-LOCK NO-ERROR.
                IF AVAIL item THEN 
                   ASSIGN c-descricao = ITEM.desc-item
                          c-un = item.un.
                ELSE
                   ASSIGN c-descricao = "Item nao cadastrado...".
        
                ASSIGN de-qtd-un = de-qtd-un + ob-etiqueta.quantidade
                       de-etq-un = de-etq-un + 1.
        
                FIND inv-acab WHERE
                     inv-acab.cod-estabel = ob-etiqueta.cod-estabel AND
                     inv-acab.data-invent = tt-param.dt-invent AND 
                     inv-acab.num-etiqueta = ob-etiqueta.num-etiqueta 
                     NO-LOCK NO-ERROR.
        
                IF AVAIL inv-acab THEN
                   DISPLAY inv-acab.data-invent
                           inv-acab.num-etiqueta
                           inv-acab.it-codigo
                           c-descricao
                           inv-acab.cod-refer
                           inv-acab.qtd-inv
                           c-un
                           inv-acab.localiz
                           "OK" @ c-texto
                           WITH FRAME f-detalhe.
                ELSE DO.
                   DISPLAY ob-etiqueta.num-etiqueta @ inv-acab.num-etiqueta
                           ob-etiqueta.it-codigo    @ inv-acab.it-codigo
                           c-descricao
                           ob-etiqueta.cod-refer    @ inv-acab.cod-refer
                           ob-etiqueta.quantidade   @ inv-acab.qtd-inv
                           c-un
                           ob-etiqueta.localiz      @ inv-acab.localizacao
                           WITH FRAME f-detalhe.
        
                   IF AMBIGUOUS inv-acab THEN DO.
                      ASSIGN c-texto = "".
                      FOR EACH inv-acab WHERE
                               inv-acab.cod-estab = ob-etiqueta.cod-estab AND
                               inv-acab.data-invent = tt-param.dt-invent AND
                               inv-acab.num-etiqueta = ob-etiqueta.num-etiqueta NO-LOCK.
                          ASSIGN c-texto = IF c-texto = ""
                                           THEN "INVENTARIADA NAS DOCAS " + inv-acab.localiz
                                           ELSE c-texto + "," + inv-acab.localiz.
                      END.
                      DISPLAY c-texto 
                           WITH FRAME f-detalhe.
                   END.
                   ELSE
                      DISPLAY "NÇO INVENTARIDA " @ c-texto
                            WITH FRAME f-detalhe.
                END.
                DOWN WITH FRAME f-detalhe.
        
                IF LAST-OF(ob-etiqueta.un) THEN DO.
                    DOWN 1 WITH FRAME f-detalhe.
                    DISPLAY "      Quantidade " + c-un @ c-descricao
                            de-qtd-un                  @ inv-acab.qtd-inv
                            WITH FRAME f-detalhe.
                    DOWN WITH FRAME f-detalhe.
        
                    DISPLAY "        Qt Pe‡as " + c-un @ c-descricao
                            de-etq-un                  @ inv-acab.qtd-inv        
                            WITH FRAME f-detalhe.
                    DOWN 2 WITH FRAME f-detalhe.
        
                    ASSIGN de-qtd-ger = de-qtd-ger + de-qtd-un
                           de-etq-ger = de-etq-ger + de-etq-un.
        
                    ASSIGN de-qtd-un = 0
                           de-etq-un = 0.
                END.
           END.
        END.
    END.
    DOWN 1 WITH FRAME f-detalhe.

    DISPLAY "    Quantidade Total " @ c-descricao
            de-qtd-ger              @ inv-acab.qtd-inv
            WITH FRAME f-detalhe.
    DOWN WITH FRAME f-detalhe.

    DISPLAY "            Qt Pe‡as " @ c-descricao
            de-etq-ger              @ inv-acab.qtd-inv        
            WITH FRAME f-detalhe.
    DOWN WITH FRAME f-detalhe.
END PROCEDURE.

