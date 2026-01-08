/* PROGRAMA: RELAT-ALGODAORP.P                                               **
** DATA    : 02/OUTUBRO/2007                                                 **
** AUTOR   :Anderson Fagner                                                  **
** OBJETIVO: RELATàRIO DE ITENS 100% ALGODÇO                                 **
******************************************************************************/

/* Programa de controle de versao e seguran»a do Datasul EMS */
{include/i-prgvrs.i relpp002rp 2.04.00.001}

/* Definicao de variaveis globais comuns a todos os programas do EMS */
{utp/ut-glob.i}

define temp-table tt-param no-undo
    field destino              as integer
    field arquivo              as char format "x(35)"
    field usuario              as char format "x(12)"
    field data-exec            as date
    field hora-exec            as integer
    field classifica           as integer
    field desc-classifica      as char format "x(40)"
    field modelo-rtf           as char format "x(35)"
    field l-habilitaRtf        as LOG
    field fi-estabel-ini       like pp-container.cod-estabel   
    field fi-estabel-fim       like pp-container.cod-estabel   
    FIELD fi-processo-ini      LIKE pp-container.nr-container
    FIELD fi-processo-fim      LIKE pp-container.nr-container
    FIELD fi-dt-compra-ini     LIKE pp-container.dt-compra
    FIELD fi-dt-compra-fim     LIKE pp-container.dt-compra
    FIELD fi-dt-chegada-ini    LIKE pp-container.dt-prev-chegada
    FIELD fi-dt-chegada-fim    LIKE pp-container.dt-prev-chegada
    FIELD fi-fornecedor-ini    LIKE pp-container.nome-ab-forn
    FIELD fi-fornecedor-fim    LIKE pp-container.nome-ab-forn
    FIELD fi-comprador-ini     LIKE pp-container.comprador 
    FIELD fi-comprador-fim     LIKE pp-container.comprador.                                                    

DEF NEW SHARED TEMP-TABLE tt-container                       
    field nr-container    like pp-container.nr-container    
    FIELD dt-compra       like pp-container.dt-compra       
    field dt-prev-chegada like pp-container.dt-prev-chegada 
    field nome-ab-forn    like pp-container.nome-ab-forn    
    FIELD comprador       like pp-container.comprador       
    FIELD cod-estabel     like pp-container.cod-estabel     
    FIELD qt-comprada     like pp-it-container.qt-pedida                            
    FIELD qt-vendida      like pp-it-container.qt-vendida                           
    FIELD qt-disponivel   like pp-it-container.qt-vendida. 

DEF NEW SHARED TEMP-TABLE tt-it-container                       
    field nr-container    like pp-container.nr-container    
    FIELD it-codigo       LIKE pp-it-container.it-codigo
    FIELD desc-item       LIKE ITEM.desc-item.


DEFINE VARIABLE var-breakby AS CHARACTER   NO-UNDO.

/* Parametros de entrada logica obrigatoria */
DEFINE NEW SHARED TEMP-TABLE tt-digita NO-UNDO
    FIELD ordem              AS INTEGER   FORMAT ">>>>9"
    FIELD nr-pedcli          AS CHAR
    FIELD nome-abrev         AS CHAR
    FIELD l-status           AS LOG.


DEF TEMP-TABLE tt-raw-digita
    FIELD raw-digita                   AS RAW.

DEF INPUT PARAMETER raw-param AS RAW NO-UNDO.
DEF INPUT PARAMETER TABLE FOR tt-raw-digita.   

CREATE tt-param.
RAW-TRANSFER raw-param TO tt-param.


DEF VAR h-acomp     AS HANDLE  NO-UNDO.


{include/i-rpvar.i}   

RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Processando..").

/* ABERTURA DO ARQUIVO DE SAÖDA (ARQUIVO/IMPRESSORA) CORREPONDE A INCLUDE CDP/CD9520.I (MAGNUS) */
{include/i-rpout.i}
{include/i-rpcab.i}

/* LOCALIZACAO DO NOME DA EMPRESA PARA COMPOR O CABECALHO PADRAO */
FIND empresa NO-LOCK WHERE empresa.ep-codigo = i-ep-codigo-usuario NO-ERROR.
IF NOT AVAIL empresa THEN
   RETURN "ADM-ERROR":U.

/* VALORIZACAO DAS OUTRAS VARIAVEIS QUE COMPOEM O CABECALHO PADRAO */
ASSIGN c-programa     = "relpp002.w":U
       c-versao       = "2.04":U
       c-revisao      = ".00.001":U
       c-empresa      = empresa.razao-social
       c-sistema      = "EMS":U.

ASSIGN c-titulo-relat = "RELATàRIO DE SALDO DO CONTAINER":U.              

VIEW FRAME f-cabec.
VIEW FRAME f-rodape.
 
FOR EACH pp-container WHERE pp-container.situacao        = 1
                        AND pp-container.cod-estabel     >= fi-estabel-ini
                        AND pp-container.cod-estabel     <= fi-estabel-fim
                        AND pp-container.nr-container    >= fi-processo-ini 
                        AND pp-container.nr-container    <= fi-processo-fim
                        AND pp-container.dt-compra       >= fi-dt-compra-ini
                        AND pp-container.dt-compra       <= fi-dt-compra-fim
                        AND pp-container.dt-prev-chegada >= fi-dt-chegada-ini 
                        AND pp-container.dt-prev-chegada <= fi-dt-chegada-fim
                        AND pp-container.comprador       >= fi-comprador-ini
                        AND pp-container.comprador       <= fi-comprador-fim
                        AND pp-container.nome-ab-forn    >= fi-fornecedor-ini
                        AND pp-container.nome-ab-forn    <= fi-fornecedor-fim
                      NO-LOCK,
    EACH pp-it-container WHERE pp-it-container.nr-container = pp-container.nr-container 
                         NO-LOCK BREAK BY pp-container.nr-container
                                       BY pp-it-container.it-codigo.
    FIND tt-container WHERE tt-container.nr-container    = pp-container.nr-container NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-container THEN DO:   
       CREATE tt-container.
       ASSIGN tt-container.nr-container    = pp-container.nr-container
              tt-container.dt-compra       = pp-container.dt-compra     
              tt-container.dt-prev-chegada = pp-container.dt-prev-chegada     
              tt-container.nome-ab-forn    = pp-container.nome-ab-forn       
              tt-container.comprador       = pp-container.comprador
              tt-container.cod-estabel     = pp-container.cod-estabel.
    END.
    ASSIGN tt-container.qt-comprada    = tt-container.qt-comprada + pp-it-container.qt-pedida
           tt-container.qt-vendida     = tt-container.qt-vendida  + pp-it-container.qt-vendida
           tt-container.qt-disponivel  = tt-container.qt-comprada - tt-container.qt-vendida.
    IF FIRST-OF (pp-it-container.it-codigo) THEN DO:
       FIND tt-it-container WHERE tt-it-container.nr-container = pp-container.nr-container  
                              AND tt-it-container.it-codigo    = pp-it-container.it-codigo NO-LOCK NO-ERROR.
       IF NOT AVAIL (tt-it-container) THEN DO:
          FIND ITEM WHERE ITEM.it-codigo = pp-it-container.it-codigo NO-LOCK NO-ERROR.
          CREATE tt-it-container.
          ASSIGN tt-it-container.nr-container = pp-container.nr-container 
                 tt-it-container.it-codigo    = pp-it-container.it-codigo 
                 tt-it-container.desc-item    = ITEM.desc-item WHEN AVAIL ITEM.
       END.
    END.
END.


CASE tt-param.classifica:
    WHEN 1 THEN 
        ASSIGN var-breakby = " BY tt-container.nr-container".
    WHEN 2 THEN 
        ASSIGN var-breakby = " BY tt-container.cod-estabel BY tt-container.nr-container".
    WHEN 3 THEN 
        ASSIGN var-breakby = " BY tt-container.qt-vendida DESC BY tt-container.nr-container".
    WHEN 4 THEN 
        ASSIGN var-breakby = " BY tt-container.qt-disponivel DESC BY tt-container.nr-container".
END CASE.

RUN esrp/relpp002imp.p VALUE(var-breakby).

/* fechamento do output do relat¢rio  */
{include/i-rpclo.i}
run pi-finalizar in h-acomp.
return "OK":U.
