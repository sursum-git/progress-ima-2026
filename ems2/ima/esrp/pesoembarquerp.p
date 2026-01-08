/* PROGRAMA: pesoembarquerp.P                                               **
** DATA    : 24/07/2008                                                     **
** AUTOR   : Anderson Fagner Dias Silva                                     **
**           Eduardo Magno Anastacio                                        **
** OBJETIVO: Peso Total do Embarque                                         **
******************************************************************************/
DEF BUFFER empresa FOR mgcad.empresa.

/* include de controle de versÆo */
{include/i-prgvrs.i pesoembarque 2.04.00.001}
{utp/ut-glob.i}
/* defini‡Æo das temp-tables para recebimento de parƒmetros */


define temp-table tt-param no-undo
    field destino                   as integer
    field arquivo                   as char format "x(35)"
    field usuario                   as char format "x(12)"
    field data-exec                 as date
    field hora-exec                 as integer
    field classifica                as integer
    field desc-classifica           as char format "x(40)"
    field modelo-rtf                as char format "x(35)"
    field l-habilitaRtf             as LOG
    field i-imprime                 as integer
    field log-ped-venda-observacoes as logical
    field fi-emb-ini               LIKE pre-fatur.nr-embarque
    field fi-emb-fim               LIKE pre-fatur.nr-embarque
    FIELD nr-pedcli-ini             LIKE ped-venda.nr-pedcli
    FIELD nr-pedcli-fim             LIKE ped-venda.nr-pedcli.



DEFINE NEW SHARED temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    FIELD it-codigo        LIKE ITEM.it-codigo
    FIELD desc-item         LIKE ITEM.desc-item
    index id ordem.

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita    AS  RAW .

DEF TEMP-TABLE tt-embarque
    FIELD embarque    AS INT
    FIELD ITEM        AS INT
    FIELD refer       AS CHAR
    FIELD qt-atendida AS CHAR
    FIELD peso-bruto  AS CHAR
    FIELD un          AS DEC
    FIELD peso        AS CHAR.


/*DEF VAR  tot-qt-geral  AS DEC FORMAT ">>>,>>>,>>9,99".*/

DEF VAR peso LIKE ped-item.qt-alocada.
DEF VAR tot-peso LIKE ped-item.qt-alocada.

/*DEF VAR  tot-vl-geral  AS DEC FORMAT ">>>,>>>,>>9,99".*/
/* recebimento de parƒmetros */
def input parameter raw-param as raw no-undo.
def input parameter TABLE for tt-raw-digita.
DEF NEW GLOBAL SHARED TEMP-TABLE tt-digita-2 NO-UNDO
FIELD codigo AS INT LABEL "C¢digo"
FIELD nome-abrev AS CHAR FORMAT "x(13)" LABEL "Nome Abreviado"
FIELD rz-social  AS CHAR FORMAT "x(20)" LABEL "RazÆo Social".

    
create tt-param.
raw-transfer raw-param to tt-param.

/* include padrÆo para vari veis de relat¢rio  */
{include/i-rpvar.i}

/* defini‡Æo de vari veis  */
def var h-acomp as handle no-undo.

/* defini‡Æo de frames do relat¢rio */

/* include padrÆo para output de relat¢rios */
{include/i-rpout.i}

/* include com a defini‡Æo da frame de cabe‡alho e rodap‚ */
{include/i-rpcab.i}

/* bloco principal do programa */
/* LOCALIZACAO DO NOME DA EMPRESA PARA COMPOR O CABECALHO PADRAO */
FIND empresa NO-LOCK WHERE empresa.ep-codigo = i-ep-codigo-usuario NO-ERROR.
IF NOT AVAIL empresa THEN
   RETURN "ADM-ERROR":U.

/* VALORIZACAO DAS OUTRAS VARIAVEIS QUE COMPOEM O CABECALHO PADRAO */

ASSIGN c-programa     = "pesoembarquerp":U
       c-versao       = "2.04":U
       c-revisao      = ".00.001":U
       c-empresa      = empresa.razao-social
       c-sistema      = "EMS":U.

ASSIGN c-titulo-relat = "Peso Total do Embarque":U.              

view  frame f-cabec.
view  frame f-rodape.
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH ped-venda WHERE
         ped-venda.nr-pedcli >= tt-param.nr-pedcli-ini AND
         ped-venda.nr-pedcli <= tt-param.nr-pedcli-fim NO-LOCK,
    EACH ped-item OF ped-venda WHERE
         ped-item.cod-sit-item = 1 NO-LOCK
         BREAK BY ped-venda.nr-pedcli
               BY ped-item.it-codigo  
               BY ped-item.cod-refer.
                

    RUN  pi-acompanhar IN  h-acomp (INPUT STRING(ped-venda.nr-pedcli) + '-' + 
                                        STRING(ped-item.it-codigo) + '-' + 
                                        STRING(ped-item.cod-refer)).
    
    FIND ITEM WHERE 
         ITEM.it-codigo = ped-item.it-codigo NO-LOCK NO-ERROR.
    
    ASSIGN peso = item.peso-bruto * ped-item.qt-pedida
           tot-peso = tot-peso + peso.

    IF AVAIL ITEM THEN
       IF FIRST-OF (ped-venda.nr-pedcli) THEN DO:
          PUT "Pedido:" AT 1
              ped-venda.nr-pedcli AT 11 
              SKIP (1)
              "Item"                AT 3  
              "Refer"               AT 11 
              "Qt Pedida"           AT 20 
              "Peso Bruto"          AT 38
              "UN"                  AT 52
              "Peso Total"          AT 61
                    
              SKIP                        
              "--------"            AT 1  
              "-----"               AT 11 
              "---------------"     AT 18 
              "---------------"     AT 35
              "----"                AT 52
              "---------------"     AT 58
              SKIP
              .
        END.
        PUT ped-item.it-codigo       AT 2 FORMAT "x(8)"  
            ped-item.cod-refer       AT 12 FORMAT "x(3)" 
            ped-item.qt-pedida      AT 18 
            item.peso-bruto          AT 36 
            item.un                  AT 52 FORMAT "x(3)"
            peso                     AT 58 
            ped.

        IF LAST-OF (ped-venda.nr-pedcli) THEN DO:
            PUT "---------------"      AT 58
                SKIP
                "Total:"               AT 50
                tot-peso               AT 58
                SKIP(2)
                "-------------------------------------------------------------------------" AT 1
                SKIP(1).
            
            ASSIGN tot-peso = 0.
        END.
END.

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

