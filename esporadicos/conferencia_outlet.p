 
{esbo/boPrecosItemRef.i}
{esbo\boMsg.i}
DEFINE VARIABLE h-bo            AS HANDLE       NO-UNDO.
DEFINE VARIABLE hBoCondPagtoPed AS HANDLE      NO-UNDO.
DEFINE VARIABLE vlReal          AS DECIMAL      NO-UNDO.
DEFINE VARIABLE vlDolar         AS DECIMAL      NO-UNDO.
DEFINE VARIABLE idPreco         AS INTEGER     NO-UNDO.
DEFINE VARIABLE tipoRetorno     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iPrazoMedio     AS INTEGER     NO-UNDO.
DEFINE VARIABLE idPrecoOutlet   AS INTEGER     NO-UNDO.
DEFINE VARIABLE dvlItem         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE h-acomp         AS HANDLE      NO-UNDO.
DEFINE VARIABLE lCorrigir       AS LOGICAL     NO-UNDO.
UPDATE lCorrigir.
RUN utp/ut-acomp.p PERSISTENT SET h-acomp.
RUN pi-inicializar IN h-acomp (INPUT "Buscando pedidos..").

RUN esbo/boPrecosItemRef.p PERSISTENT SET h-bo.
RUN esbo/boCondPagtoPed.p PERSISTENT SET hBoCondPagtoPed .
RUN iniciarBos      IN h-bo.
DEFINE TEMP-TABLE tt        LIKE ped-item
    FIELD LOG_outlet        AS LOGICAL
    FIELD LOG_outlet_pedido AS LOGICAL
    FIELD dt-implant        AS DATE
    FIELD codPrioridade     AS CHAR
    FIELD idOutlet          AS INT
    FIELD rowidPeditemExt   AS ROWID
    FIELD nrNota            AS CHAR
    FIELD dtEmisNota        AS DATE.
FOR EACH 
    nota-fiscal 
    WHERE dt-emis-nota >= 08.01.2021
    AND   dt-emis-nota <= 08.31.2021
    AND dt-cancel = ?
    USE-INDEX ch-sit-nota,
    FIRST ped-venda
    WHERE ped-venda.nr-pedido = int(nota-fiscal.nr-pedcli) NO-LOCK.
    RUN pi-acompanhar IN h-acomp("pedido:" + string(ped-venda.nr-pedido) + "- data:" + string(ped-venda.dt-implant)).
    FIND ped-venda-ext 
        WHERE ped-venda-ext.cod-estabel =  ped-venda.cod-estabel
        AND   ped-venda-ext.nr-pedido   = ped-venda.nr-pedido NO-LOCK NO-ERROR.
    
    IF nota-fiscal.cod-emitente = 33240 OR nota-fiscal.cod-emitente = 1 THEN NEXT.
    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK.   
        
        FIND ped-item 
             WHERE ped-item.nome-abrev   = it-nota-fisc.nome-ab-cli
             AND   ped-item.nr-pedcli    = it-nota-fisc.nr-pedcli
             AND   ped-item.nr-sequencia = it-nota-fisc.nr-seq-ped
             AND   ped-item.it-codigo    = it-nota-fisc.it-codigo
             AND   ped-item.cod-refer    = it-nota-fisc.cod-refer
             USE-INDEX  ch-item-ped 
             NO-LOCK NO-ERROR.
        //RUN pi-acompanhar IN h-acomp(string(ped-venda.nr-pedido) + "-" + string(ped-venda.dt-implant) + '-' + ped-item.it-codigo + '-' + ped-item.cod-refer).
        FIND ped-item-ext 
            WHERE ped-venda.cod-estabel  = ped-item-ext.cod-estabel
             AND  ped-venda.nome-abrev   = ped-item-ext.nome-abrev
             AND  ped-venda.nr-pedcli    = ped-item-ext.nr-pedcli
             AND  ped-item.nr-sequencia  = ped-item-ext.nr-sequencia
             AND  ped-item.it-codigo     = ped-item-ext.it-codigo
             AND  ped-item.cod-refer     = ped-item-ext.cod-refer  NO-LOCK NO-ERROR.
        CREATE tt.
        BUFFER-COPY ped-item TO tt.

        ASSIGN tt.LOG_outlet_pedido = IF AVAIL ped-item-ext 
                                         AND ped-item-ext.num-id-liquida-ima <> "0"  AND  ped-item-ext.num-id-liquida-ima <> '' THEN YES
                                      ELSE NO.  
        ASSIGN tt.dt-implant = ped-venda.dt-implant
               tt.codPrioridade = string(ped-venda.cod-prior)
               tt.nrNota        = nota-fiscal.nr-nota-fis
               tt.dtEmisNota    = nota-fiscal.dt-emis-nota .

        /*IF AVAIL ped-venda-ext AND ped-venda-ext.nr-container <> 0  OR ped-venda-ext.tb_preco_id <> 1 THEN DO.
           ASSIGN tt.LOG_outlet = NO
                  tt.rowidpeditemext = ROWID(ped-item-ext).
        END.*/
        /*ELSE DO:*/
            RUN limparTTPreco   IN h-bo.
            RUN limparTTMsg     IN h-bo.
            RUN setTbPreco      IN h-bo(ped-venda-ext.tb_preco_id). //1-padrao   2-rubi
            RUN setItem         IN h-bo(ped-item.it-codigo). 
            RUN setRef          IN h-bo(ped-item.cod-refer). 
            RUN setNrContainer  IN h-bo(ped-venda-ext.nr-container).
            RUN setTipoBusca    IN h-bo(IF ped-venda-ext.nr-container <> 0 THEN 2 ELSE 1 ). // 0- todos, 1- pe, 2- pi
            RUN setTipoCalc     IN hBoCondPagtoPed(1).
            RUN setRowidPedido  IN hBoCondPagtoPed(ROWID(ped-venda)).
            RUN pi-acompanhar IN h-acomp("pedido:" + string(ped-venda.nr-pedido) + "- data:" + string(ped-venda.dt-implant) + ' calculando prazo medio...').
            RUN calcularPrazoMedio IN hBoCondPagtoPed.
            RUN getPrazoMedio   IN hBoCondPagtoPed(OUTPUT iPrazoMedio).
            RUN setPrazoMedio   IN h-bo(iPrazoMedio).
            RUN setDtRefer      IN h-bo(ped-venda.dt-implant).
            //RUN pi-acompanhar IN h-acomp(' buscando pre‡o - item:' + ped-item.it-codigo + ' -ref:' + ped-item.cod-refer).
            RUN buscarPrecos    IN h-bo.
            RUN getPrecoPrazo   IN h-bo (INPUT 'outlet',
                                 OUTPUT vlReal,
                                 OUTPUT vlDolar,
                                 OUTPUT idPreco).
           ASSIGN //vlPrecoOutlet:SCREEN-VALUE =  STRING(vlReal)
                  idPrecoOutlet = idPreco.
           IF idPrecoOutlet <> 0 THEN
              ASSIGN tt.LOG_outlet = YES.
           ELSE
             ASSIGN tt.LOG_outlet = NO.   

           ASSIGN tt.idoutlet           = idPrecoOutlet
                  tt.rowidPeditemExt    = ROWID(ped-item-ext).
        /*END.*/
    END.                           
END.

RUN pi-acompanhar IN h-acomp('exportando dados e corrigindo').
OUTPUT TO c:\temp\ttConfoutlet.csv.
PUT "item;refer;pedido cliente;nome abrev;data pedido;qt.atendida;valor preori;valor preuni;tot. preori;tot. preuni;valor item;valor desconto;vl.item + desconto; outlet pedido; outlet pre‡o;prioridade;nf;dt.nf" SKIP.

FOR EACH tt:
    ASSIGN dVlItem = tt.vl-tot-it  + tt.val-desconto-total.
    EXPORT DELIMITER ";" tt.it-codigo 
                         tt.cod-refer
                         tt.nr-pedcli 
                         tt.nome-abrev 
                         tt.dt-implant 
                         tt.qt-atendida 
                         tt.vl-preori 
                         tt.vl-preuni 
                         tt.qt-atendida * tt.vl-preori 
                         tt.qt-atendida * tt.vl-preuni
                         tt.vl-tot-it 
                         tt.val-desconto-total 
                         dVlItem 
                         tt.LOG_outlet_pedido 
                         tt.LOG_outlet 
                         tt.codPrioridade
                         tt.nrNota
                        tt.dtEmisNota .
    RUN pi-acompanhar IN h-acomp( string(tt.dt-implant)  +  '-' +  tt.it-codigo + "-" + tt.cod-refer).
    IF lCorrigir THEN DO:
        FIND ped-item-ext
             WHERE rowid(ped-item-ext) = rowidpeditemext EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL ped-item-ext THEN DO:

           RUN pi-acompanhar IN h-acomp("corrigindo:" +  string(tt.dt-implant)  +  '-' +  tt.it-codigo + "-" + tt.cod-refer).
           ASSIGN ped-item-ext.num-id-liquida-ima = STRING(tt.idoutlet)
                  ped-item-ext.liquida-ima = IF tt.idoutlet > 0 THEN YES ELSE NO .
        END.
    END.    
END.

OUTPUT CLOSE.

RUN pi-finalizar IN h-acomp.

