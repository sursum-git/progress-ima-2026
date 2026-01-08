/* PROGRAMA: ES001ISFRP.P                                                   **
** DATA    : 17/DEZEMBRO/2007                                               **
** AUTOR   : Anderson Fagner Dias Silva                                     **
**           Eduardo Magno Anastacio                                        **
** OBJETIVO: Instru‡Æo para Separa‡Æo e Faturamento                         **
******************************************************************************/


/* include de controle de versÆo */
{include/i-prgvrs.i ES001ISFRP 2.04.00.001}
{utp/ut-glob.i}
/* defini‡Æo das temp-tables para recebimento de parƒmetros */
DEF BUFFER empresa FOR mgcad.empresa.

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
    field fi-emb-ini                as integer
    field fi-emb-fim                as integer
    field fi-ped-ini                as CHAR
    field fi-ped-fim                as CHAR
    FIELD rs-imprime                AS LOGICAL.


DEFINE NEW SHARED temp-table tt-digita no-undo
    field ordem            as integer   format ">>>>9"
    FIELD it-codigo        LIKE ITEM.it-codigo
    FIELD desc-item         LIKE ITEM.desc-item
    index id ordem.

DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita    AS  RAW .

DEF TEMP-TABLE tt-embarque
    FIELD embarque        AS INT
    FIELD cod-emitente    AS INT 
    FIELD ped-cli         AS CHAR
    FIELD resumo          AS INT
    FIELD vendedor        AS CHAR
    FIELD endereco        AS CHAR
    FIELD bairro          AS CHAR
    FIELD cidade          AS CHAR
    FIELD estado          AS CHAR
    FIELD cpf             AS CHAR
    FIELD telefone        AS CHAR
    FIELD cond-pagto      AS INT
    FIELD desc-cond-pag   AS CHAR
    FIELD observacao      AS CHAR
    FIELD prioridade      AS INT
    FIELD nomecli         AS CHAR
    FIELD nomeabrevrep    AS CHAR
    FIELD data            AS DATE
    FIELD imprimir        AS LOGICAL
    FIELD transp          AS CHAR
    FIELD transp-red      AS CHAR.

DEF TEMP-TABLE tt-item
    FIELD embarque        AS INT
    FIELD ITEM            AS CHAR
    FIELD refer           AS CHAR
    FIELD descricao       AS CHAR
    FIELD quantidade      AS DEC
    FIELD unidade         AS CHAR
    FIELD cod-depos       AS CHAR.

DEFINE VARIABLE var-deposito-erro AS LOGICAL     NO-UNDO.

DEF VAR tipodefrete AS CHAR.
/*DEF VAR  tot-qt-geral  AS DEC FORMAT ">>>,>>>,>>9,99".
DEF VAR  tot-vl-geral  AS DEC FORMAT ">>>,>>>,>>9,99".*/

/* recebimento de parƒmetros */
def input parameter raw-param as raw no-undo.
def input parameter TABLE for tt-raw-digita.
DEF NEW GLOBAL SHARED TEMP-TABLE tt-digita-2 NO-UNDO
FIELD codigo AS INT LABEL "C¢digo"
FIELD nome-abrev AS CHAR FORMAT "x(13)" LABEL "Nome Abreviado"
FIELD rz-social  AS CHAR FORMAT "x(20)" LABEL "RazÆo Social".

    
create tt-param.
raw-transfer raw-param to tt-param.

/*FOR EACH tt-raw-digita :
    CREATE tt-digita.
    RAW-TRANSFER tt-raw-digita.raw-digita TO tt-digita.
END.*/

/**************/
/** Includes **/
/**************/

{include/tt-edit.i}
{include/pi-edit.i}

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

ASSIGN c-programa     = "ES002MFTRP":U
       c-versao       = "2.04":U
       c-revisao      = ".00.001":U
       c-empresa      = empresa.razao-social
       c-sistema      = "EMS":U.

ASSIGN c-titulo-relat = "Instru‡Æo para Separa‡Æo e Faturamento":U.              

view  frame f-cabec.
view  frame f-rodape.
run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Imprimindo *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

ASSIGN var-deposito-erro = NO. 

FOR EACH pre-fatur WHERE pre-fatur.nr-embarque >= tt-param.fi-emb-ini
                     and pre-fatur.nr-embarque <= tt-param.fi-emb-fim
                     and pre-fatur.nr-pedcli   >= tt-param.fi-ped-ini
                     and pre-fatur.nr-pedcli   <= tt-param.fi-ped-fim
                   SHARE-LOCK,
    EACH it-pre-fat WHERE it-pre-fat.nr-embarque =  pre-fatur.nr-embarque NO-LOCK,
    EACH it-dep-fat WHERE it-dep-fat.nr-embarque =  it-pre-fat.nr-embarque
                      and it-dep-fat.nr-pedcli   =  it-pre-fat.nr-pedcli
                      and it-dep-fat.nr-seq      =  it-pre-fat.nr-seq
                      and it-dep-fat.it-codigo   =  it-pre-fat.it-codigo
                      and it-dep-fat.cod-refer   =  it-pre-fat.cod-refer
                      AND it-dep-fat.nr-serlote  MATCHES  '*' + it-pre-fat.cod-refer + '*' 
                      and it-dep-fat.qt-alocada  <> 0 
                    NO-LOCK 
    BREAK by it-pre-fat.nr-embarque
          by pre-fatur.nome-abrev
          by pre-fatur.nr-pedcli
          by it-pre-fat.it-codigo
          BY it-pre-fat.cod-refer
          BY it-dep-fat.cod-depos.
    
         RUN  pi-acompanhar IN  h-acomp (INPUT pre-fatur.nr-pedcli + '-' + 
                                        STRING(pre-fatur.nr-embarque) + '-' + 
                                        it-pre-fat.nr-pedcli ).
   
         FIND ped-venda WHERE
              ped-venda.nr-pedcli = pre-fatur.nr-pedcli
              NO-LOCK NO-ERROR.

         FIND FIRST natur-oper-deposito WHERE natur-oper-deposito.nat-operacao = ped-venda.nat-oper NO-LOCK NO-ERROR.
         IF (AVAIL natur-oper-deposito AND natur-oper-deposito.cod-depos <> it-dep-fat.cod-depos) OR 
            (NOT AVAIL natur-oper-deposito AND it-dep-fat.cod-depos = "log") THEN DO:
             ASSIGN var-deposito-erro = YES.
             /*
             PUT UNFORMAT "Deposito " it-dep-fat.cod-depos " do item " it-pre-fat.it-codigo " referencia " it-pre-fat.cod-refer " nÆo confere com a natureza " ped-venda.nat-oper SKIP(1).
             */
         END.


         FIND emitente OF pre-fatur WHERE 
              emitente.nome-abrev = pre-fatur.nome-abrev 
              NO-LOCK NO-ERROR.
         FIND ITEM  WHERE
              item.it-codigo = it-pre-fat.it-codigo 
               
              NO-LOCK NO-ERROR.
         FIND repres WHERE
              repres.nome-abrev = ped-venda.no-ab-reppri
              NO-LOCK NO-ERROR.
         FIND FIRST cond-pagto WHERE 
              cond-pagto.cod-cond-pag = ped-venda.cod-cond-pag NO-LOCK NO-ERROR.
         
         IF LAST-OF(it-pre-fat.cod-refer) THEN DO: 
            
            FIND FIRST tt-embarque  WHERE 
                 tt-embarque.embarque = pre-fatur.nr-embarque
                 NO-LOCK NO-ERROR.
            
            IF NOT AVAIL tt-embarque THEN DO:
               CREATE tt-embarque.
               ASSIGN tt-embarque.embarque   = pre-fatur.nr-embarque
                      tt-embarque.ped-cli    = pre-fatur.nr-pedcli
                      tt-embarque.resumo     = pre-fatur.nr-resumo
                      tt-embarque.observacao = ped-venda.observacoes.
                      
               
               IF AVAIL cond-pagto THEN 
                  ASSIGN tt-embarque.desc-cond-pag = cond-pagto.descricao.
               IF AVAIL repres THEN 
                  ASSIGN tt-embarque.vendedor = repres.nome.
               IF AVAIL emitente THEN DO:
                  ASSIGN tt-embarque.cod-emitente = emitente.cod-emitente
                         tt-embarque.endereco     = emitente.endereco                                 
                         tt-embarque.bairro       = emitente.bairro                                   
                         tt-embarque.cidade       = emitente.cidade                                   
                         tt-embarque.estado       = emitente.estado                                   
                         tt-embarque.cpf          = emitente.cgc                                      
                         tt-embarque.nomecli      = emitente.nome-abrev                                
                         tt-embarque.telefone     = emitente.telefone[1] + " " + emitente.telefone[2].
               END.
               IF AVAIL ped-venda THEN DO:
                  ASSIGN tt-embarque.cond-pagto   = ped-venda.cod-cond-pag 
                         tt-embarque.nomeabrevrep = ped-venda.no-ab-reppri
                         tt-embarque.prioridade   = ped-venda.cod-priori
                         tt-embarque.data         = ped-venda.dt-emissao
                         tt-embarque.transp       = ped-venda.nome-transp
                         tt-embarque.transp-red   = ped-venda.nome-tr-red.
               END.
               
               IF pre-fatur.pick-impresso = NO AND tt-param.rs-imprime = YES THEN DO:
                  
                  MESSAGE "NÆo foi poss¡vel reimprimir a ISF do embarque " pre-fatur.nr-embarque "." SKIP  "A mesma nÆo foi impressa."
                          VIEW-AS ALERT-BOX INFO BUTTONS OK.
                     
                  ASSIGN tt-embarque.imprimir = NO.

               END.
               ELSE DO:
                    IF pre-fatur.pick-impresso = YES AND tt-param.rs-imprime = NO THEN DO:
                       
                       MESSAGE "NÆo foi poss¡vel imprimir a ISF do embarque " pre-fatur.nr-embarque "." SKIP "A mesma j  foi impressa."
                               VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    
                       ASSIGN tt-embarque.imprimir = NO.
                       
                    END.
                    ELSE DO:
                         IF tt-param.rs-imprime = NO THEN DO:
                            ASSIGN pre-fatur.pick-impresso = YES.
                         END.
                         ASSIGN tt-embarque.imprimir = YES. 
                    END.
               END.
            END. 
         END.
         CREATE tt-item.
         ASSIGN tt-item.ITEM       = it-pre-fat.it-codigo
                tt-item.refer      = it-pre-fat.cod-refer
                tt-item.cod-depos  = it-dep-fat.cod-depos
                tt-item.quantidade = it-dep-fat.qt-alocada
                tt-item.embarque   = pre-fatur.nr-embarque.
         IF AVAIL ITEM THEN DO:
            ASSIGN tt-item.descricao = item.desc-item
                   tt-item.unidade   = item.un.          
         
         END.
END.

/*IF var-deposito-erro = NO THEN DO:*/

    FOR EACH tt-embarque,
        EACH tt-item WHERE tt-item.embarque = tt-embarque.embarque
        BREAK BY tt-embarque.embarque
              BY tt-item.ITEM 
              BY tt-item.refer:
        RUN  pi-acompanhar IN  h-acomp (INPUT "Imprimindo:" + STRING(tt-embarque.embarque)).
           
        IF tt-embarque.imprimir = YES THEN DO:
           IF FIRST-OF(tt-embarque.embarque) THEN DO:
              FIND ped-venda WHERE ped-venda.nr-pedcli = tt-embarque.ped-cli NO-LOCK NO-ERROR.
              IF AVAIL ped-venda THEN DO:
                 /*####################################################################################################################
                   # Verifica os pedidos de venda onde a data seja depois da altera‡Æo de tipo de frete no compo ped-venda.cidade-cif #
                   ####################################################################################################################*/
                 
                 FIND ped-venda-ext WHERE
                      ped-venda-ext.cod-estabel = ped-venda.cod-estabel AND
                      ped-venda-ext.nr-pedido =  ped-venda.nr-pedido NO-LOCK NO-ERROR.
                 IF AVAIL ped-venda-ext THEN DO.
                    CASE ped-venda-ext.tp-frete.
                         WHEN 'CIF Total' THEN ASSIGN tipodefrete = "CIF TOTAL".
                         WHEN 'FOB Total' THEN ASSIGN tipodefrete = "FOB TOTAL".
                         WHEN 'CIF At‚ Redesp' THEN ASSIGN tipodefrete = "CIF AT REDESP".
                    END CASE.
                 END.
                 ELSE DO.
                     IF ped-venda.dt-useralt >= 10/20/2010 OR 
                        ped-venda.dt-emissao >= 10/20/2010 THEN DO:
                        IF ped-venda.cidade-cif <> "" THEN
                            ASSIGN tipodefrete = "CIF".
                        ELSE
                            ASSIGN tipodefrete = "FOB".
                     END.
                     ELSE DO:
                         IF ped-venda.cidade-cif = "" THEN
                            ASSIGN tipodefrete = "CIF".
                         ELSE
                            ASSIGN tipodefrete = "FOB".
                     END.
                 END.
              END.
              PUT  "Pedido Cliente: "                  AT 1         
                   tt-embarque.ped-cli                 AT 18  FORMAT "x(8)"
                   "Embarque: "                        AT 35  
                   string(tt-embarque.embarque)        AT 46  FORMAT "x(10)"
                   "Nr Resumo: "                       AT 65
                   string(tt-embarque.resumo)          AT 80  FORMAT "x(2)"
                   "Data EmissÆo: "                    AT 93
                   string(tt-embarque.data)            AT 108  FORMAT "x(11)"  
                   SKIP                               
                                                      
                   "Cliente: "                         AT 1
                   string(tt-embarque.cod-emitente)    AT 10  FORMAT "x(8)"
                   tt-embarque.nomecli                 AT 26  FORMAT "x(30)"
                   "Vendedor: "                        AT 70
                   tt-embarque.nomeabrevrep            AT 80  FORMAT "x(12)"
                   tt-embarque.vendedor                AT 93  FORMAT "x(40)"  
                   SKIP(2)                            
                                                      
                   "Endere‡o: "                        AT 1
                   tt-embarque.endereco                AT 12  FORMAT "x(35)"
                   tt-embarque.bairro                  AT 60  FORMAT "x(25)"
                   tt-embarque.cidade                  AT 87  FORMAT "x(20)"
                   tt-embarque.estado                  AT 120 FORMAT "x(2)"   
                   SKIP                               
                                                      
                   "CGC/CPF: "                         AT 1
                   tt-embarque.cpf                     AT 12  FORMAT "x(20)"
                  /* "Telefone: "                        AT 70 
                   tt-embarque.telefone                AT 83  FORMAT "x(20)" */  
                   SKIP(2)                            
                                                      
                   "Cond Pagto: "                      AT 1
                   tt-embarque.cond-pagto              AT 13  FORMAT ">>9"
                   tt-embarque.desc-cond-pag           AT 18 FORMAT "x(20)"
                   "Prioridade: "                      AT 70
                   tt-embarque.prioridade              AT 83  FORMAT "99"
                   SKIP                               
                   
                   "Frete:"                            AT 1
                   tipodefrete                         AT 8   FORMAT "x(10)"
                   "Transp:"                           AT 20
                   tt-embarque.transp                  AT 29.

              IF tt-embarque.transp-red <> "" THEN
                 PUT "Transp Red:"                     AT 70
                     tt-embarque.transp-red            AT 83.

              PUT SKIP
                  "Observa‡Æo: "                       AT 1.

              RUN pi-print-editor(INPUT tt-embarque.observacao,
                                  INPUT 132).
              FOR EACH tt-editor:
                  PUT tt-editor.conteudo AT 1 FORMAT "x(132)".
              END.

              /*
              PUT UNFORMAT SKIP
                  "Observa‡Æo: "                       AT 1
                  tt-embarque.observacao               AT 13. */
              /*IF (substr(tt-embarque.observacao,121,240)<>"") THEN DO:
                 PUT SKIP
                     substr(tt-embarque.observacao,121,240) AT 1  FORMAT "x(120)".
              END.
              IF (substr(tt-embarque.observacao,241,360)<>"") THEN DO:
                 PUT SKIP
                     substr(tt-embarque.observacao,241,360) AT 1  FORMAT "x(120)".
              END.
               */    
              PUT SKIP(3)                            
                  "Item"                              AT 1
                  "Refer"                             AT 10
                  "Descri‡Æo"                         AT 18
                  "Qt"                                AT 98
                  "Deposito"                          AT 108
                  "Un"                                AT 120
                  SKIP
                  FILL("-",132) FORMAT "x(132)"
                  SKIP(1).
           END.
           
           PUT  tt-item.ITEM           AT 1   FORMAT "x(9)"
                tt-item.refer          AT 11  FORMAT "x(4)"
                tt-item.descricao      AT 18  FORMAT "x(65)"
                tt-item.quantidade     AT 90  FORMAT ">>>,>>>,>>9.99"
                tt-item.cod-depos      AT 110 FORMAT "x(3)"
                tt-item.unidade        AT 120 FORMAT "x(2)"  
                SKIP(1).
        
           IF LAST-OF(tt-embarque.embarque) THEN DO:  
              PUT SKIP(5)
                  "---------------"  AT 12
                  "---------------"  AT 30
                  "---------------"  AT 48
                  SKIP
                  "Quantidade"       AT 15
                  "Numero"           AT 35
                  "Tipo"             AT 54
                  SKIP(5)
               
                  "---------------"  AT 12
                  "---------------"  AT 30
                  "---------------"  AT 48
                  "---------------"  AT 66
                  SKIP
                  "Separa‡Æo"        AT 15
                  "Corte"            AT 35
                  "Romaneio"         AT 52
                  "Embalagem"        AT 69.
                  PAGE.
           END.
        END.
    END.
/*END.*/

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.

