/************************************************************************************************* 
programa:esbo/boDadosVendasPI.p
objetivo:Retornar dados de vendas de container a n¡vel de item
data: 07/2025
****************************************************************************************************/

DEFINE VARIABLE containerIni    AS INTEGER     NO-UNDO INIT 0.
DEFINE VARIABLE containerFim    AS INTEGER     NO-UNDO INIT 999999.
DEFINE VARIABLE dtPrevIni       AS DATE        NO-UNDO INIT 01.01.2001.
DEFINE VARIABLE dtPrevFim       AS DATE        NO-UNDO INIT 01.01.2999.
DEFINE VARIABLE dtChegadaIni    AS DATE        NO-UNDO INIT 01.01.2001 .
DEFINE VARIABLE dtChegadaFim    AS DATE        NO-UNDO INIT 01.01.2999.
DEFINE VARIABLE cListaSituacao  AS CHARACTER   NO-UNDO INIT '1,2,3'.
DEFINE VARIABLE cListaSitContainer  AS CHARACTER   NO-UNDO INIT 'Aberto,Suspenso,Fechado'.
DEFINE VARIABLE dtGeracao           AS DATETIME    NO-UNDO INIT NOW.
DEFINE VARIABLE iPrazoMedio         AS INTEGER     NO-UNDO.


DEFINE VARIABLE logDtPrev       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE logDtCheg       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE logSituacao     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE logContainer    AS LOGICAL     NO-UNDO.

DEFINE TEMP-TABLE ttSituacoes
FIELD situacao AS INT.

{esp/relpp001excel.i }


PROCEDURE iniciar:

    ASSIGN   logDtPrev        = NO
             logDtCheg        = NO
             logSituacao      = NO
             logContainer     = NO .
    
    EMPTY TEMP-TABLE ttSituacoes.
    
    


END PROCEDURE.

PROCEDURE finalizar:

     DELETE OBJECT THIS-PROCEDURE.

END PROCEDURE.


PROCEDURE setIntervalContainer: 

    DEFINE INPUT  PARAMETER pContainerIni AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pContainerFim AS INTEGER     NO-UNDO.
    
    ASSIGN containerIni = pContainerIni
           containerFim = pContainerFim.
    ASSIGN logContainer = containerIni <> 0 OR containerFim <> 999999.


END PROCEDURE.

/*
PROCEDURE setIntervalDtPrev: 

    ASSIGN logDtPrev = dtPrevIni  <> 01.01.2001 OR dtPrevFim <> 01.01.2999.

END PROCEDURE.


PROCEDURE setIntervalDtCheg:

    ASSIGN logDtCheg =  dtChegadaIni  <> 01.01.2001 OR dtChegadaFim <> 01.01.2999 .

END PROCEDURE.
*/

PROCEDURE setSituacao:    

    DEFINE INPUT  PARAMETER pSituacao AS INTEGER     NO-UNDO.
    FIND ttSituacoes
        WHERE ttSituacoes.situacao = pSituacao
        NO-ERROR.
    IF NOT AVAIL ttSituacoes THEN
    DO:
        CREATE ttSituacoes.    
        ASSIGN ttSituacoes.situacao = pSituacao.
    END.
        
        

END PROCEDURE.


PROCEDURE executar:

    FOR EACH ttSituacoes:
        FOR EACH pp-container FIELDS(nr-container situacao nome-ab-forn dt-compra dt-prev-chegada dt_prev_chegada_loja dt-recebimento)
            WHERE pp-container.situacao = ttSituacoes.situacao
            AND   pp-container.nr-container >= containerIni
            AND   pp-container.nr-container <= containerFim            
            USE-INDEX ind_situacao,
            EACH ped-venda-ext FIELDS(nr-container nr-pedido tb_preco_id) NO-LOCK
            WHERE ped-venda-ext.nr-container = pp-container.nr-container
            USE-INDEX ind_container,
            EACH ped-venda FIELDS(nr-pedido cod-emitente no-ab-reppri dt-implant ) WHERE
            ped-venda.nr-pedido = ped-venda-ext.nr-pedido NO-LOCK,
            EACH emitente FIELDS(cod-emitente nome-abrev estado)
            WHERE ped-venda.cod-emitente  = emitente.cod-emitente NO-LOCK,
            EACH mgcad.moeda OF ped-venda NO-LOCK,
            EACH ped-item  fields( nome-abrev nr-pedcli it-codigo cod-refer vl-preuni val-desconto-total vl-tot-it qt-pedida cod-sit-item) OF ped-venda NO-LOCK,
            EACH ITEM FIELDS(it-codigo desc-item) OF ped-item NO-LOCK           
            .
                
                
            CREATE  tt.
            ASSIGN  tt.nrContainer          =  pp-Container.nr-container            
                    tt.NomeFornec           =  pp-container.nome-ab-forn            
                    tt.dtCompra             =  pp-container.dt-compra               
                    tt.dtPrevChegada        =  pp-container.dt-prev-chegada         
                    tt.dtPrevChegadaLoja    =  pp-container.dt_prev_chegada_loja    
                    tt.dtRecebimento        =  pp-container.dt-recebimento          
                    tt.dtReg                =  ped-venda.dt-implant.
                    IF AVAIL pp-container THEN 
                       ASSIGN tt.situacao   =  ENTRY(pp-container.situacao,cListaSitContainer,",").
            ASSIGN 
                    tt.itCodigo      =  ped-item.it-codigo 
                    tt.descItem      =  ITEM.desc-item 
                    tt.codRefer      =  ped-item.cod-refer 
                    tt.tipoReg       =  'venda'
                    tt.quantidade    =  ped-item.qt-pedida * -1 .
            RUN retornarSitItem(ped-item.cod-sit-item,OUTPUT tt.sitPed).
            ASSIGN 
                    tt.dtHrGeracao      =  string(dtGeracao,'99/99/9999 hh:mm:ss')
                    tt.codEmitente      = ped-venda.cod-emitente
                    tt.nomeRepres       = ped-venda.no-ab-reppri
                    tt.nomeEmitente     = emitente.nome-abrev                    
                    tt.precoUnitario    = ped-item.vl-preuni
                    tt.desconto         = ped-item.val-desconto-total
                    tt.valorTotal       = ped-item.vl-tot-it
                    tt.nrPedido         = ped-venda.nr-pedido
                    tt.moeda            = IF AVAIL moeda THEN moeda.descricao ELSE ''
                    tt.precoOutlet      = 0
                    tt.idOutlet         = 0.
            //chama a api de busca do pre‡o
            RUN esapi\getPrecoPrazoItemRef.p( INPUT ped-venda-ext.tb_preco_id, //tabela de preco
                                              INPUT TODAY, //data referencia
                                              INPUT ped-item.it-codigo, //item
                                              INPUT ped-item.cod-refer, //referencia
                                              INPUT ped-venda-ext.nr-container, //container
                                              INPUT 2, //PI
                                              INPUT iPrazoMedio, //prazo medio
                                              INPUT emitente.estado, //estado do cliente
                                              //INPUT hBoPrecosItemRef, // handle da BO para melhor performance
                                              OUTPUT tt.precoReal, //pre‡o em real 
                                              OUTPUT tt.precoDolar, //pre‡o em dolar
                                              OUTPUT tt.precoId,
                                              OUTPUT tt.precoOutlet,
                                              OUTPUT tt.idOutlet).
            ASSIGN tt.valorTotal        = ped-item.vl-preuni * tt.quantidade .             
        END.
    END.

END PROCEDURE.


PROCEDURE getTT:

DEFINE OUTPUT PARAMETER TABLE FOR tt.

END PROCEDURE.

PROCEDURE retornarSitItem:

DEFINE INPUT  PARAMETER codSitItem AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER cSituacao AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cListaSituacao AS CHARACTER   NO-UNDO FORMAT 'x(100)'.


ASSIGN cListaSituacao = {diinc/i03di149.i 3}.
/*MESSAGE codSitItem SKIP
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/


ASSIGN cSituacao = ENTRY(codSitItem,cListaSituacao,",").



END PROCEDURE.

