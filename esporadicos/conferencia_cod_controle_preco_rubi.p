DEFINE TEMP-TABLE ttPedido NO-UNDO
    FIELD nrPedido              AS INT
    FIELD logOK                 AS LOGICAL
    FIELD descricao             AS CHAR FORMAT 'x(50)'
    FIELD itCodigo              AS CHAR FORMAT 'x(20)'
    FIELD codRefer              AS CHAR 
    FIELD nrSeq                 AS INT
    FIELD precoEncontrado       AS DECIMAL
    FIELD precoPedido           AS DECIMAL
    FIELD idprecoEncontrado     AS INTEGER
    FIELD idPrecoPedido         AS INTEGER
    INDEX primario IS PRIMARY  nrPedido.

DEFINE VARIABLE dVlReal     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dVlDolar    AS DECIMAL     NO-UNDO.
DEFINE VARIABLE idPreco     AS INTEGER     NO-UNDO.
DEFINE VARIABLE hBo01       AS HANDLE      NO-UNDO.
DEFINE VARIABLE iPrazoMedio AS INTEGER     NO-UNDO.

RUN esbo/boCondPagtoPed.p PERSIST SET hBo01.

OUTPUT TO c:\temp\ped-venda-rubi.txt.
FOR EACH ped-venda NO-LOCK
    WHERE ped-venda.cod-sit-ped  <> 6 
    AND   ped-venda.dt-implant >= 01.01.2020
    AND   ped-venda.dt-implant <= 01.01.2022,
    EACH ped-venda-ext NO-LOCK
    WHERE  ped-venda-ext.cod-estabel = ped-venda.cod-estabel
    AND    ped-venda-ext.nr-pedido   = ped-venda.nr-pedido
    AND    ped-venda-ext.tb_preco_id = 2
    .
    DISP ped-venda.nr-pedido . PAUSE 0.
    FOR EACH ped-item OF ped-venda NO-LOCK.
       FIND ped-item-ext
       WHERE ped-item-ext.cod-estabel      = ped-venda.cod-estabel
       AND   ped-item-ext.nome-abrev       = ped-venda.nome-abrev
       AND   ped-item-ext.nr-pedcli        = ped-venda.nr-pedcli
       AND  ped-item-ext.nr-sequencia      = ped-item.nr-sequencia
       AND ped-item-ext.it-codigo          = ped-item.it-codigo
       AND ped-item-ext.cod-refer          = ped-item.cod-refer EXCLUSIVE-LOCK NO-ERROR.
       IF NOT AVAIL ped-item-ext THEN do:
           RUN criarReg(ped-venda.nr-pedido,FALSE,"Sem PED VENDA EXT",'','',0,0,0,0).
           NEXT.
       END.
       FIND controle_preco NO-LOCK
           WHERE controle_preco.cod_controle_preco = ped-item-ext.cod_controle_preco
           NO-ERROR.
       IF NOT AVAIL controle_preco THEN DO:
          RUN criarReg(ped-venda.nr-pedido,FALSE,"O ID informado nÆo corresponde a um pre‡o cadastrado:" + string(ped-item-ext.cod_controle_preco),ped-item.it-codigo,ped-item.cod-refer,0,ped-item.vl-preori,0,ped-item-ext.cod_controle_preco).
       END.
       //busca o prazo medio
       RUN getPrazoMedioPedido IN hbo01(ROWID(ped-venda), OUTPUT iPrazoMedio).

       // buscar o pre‡o que deveria estar no banco de dados e comparar com o encontrado
       RUN esapi/getPrecoPrazoItemRef.p(
       INPUT ped-venda-ext.tb_Preco_id,
       INPUT ped-venda.dt-implant,
       INPUT ped-item.it-codigo,    
       INPUT ped-item.cod-refer,
       INPUT ped-venda-ext.nr-container,
       IF ped-venda.tp-pedido = 'pe' THEN 1 ELSE 2 ,
       INPUT iPrazoMedio,
       INPUT ped-venda.estado,
       OUTPUT dVlReal,
       OUTPUT dVlDolar,
       OUTPUT idPreco
       ).   
       IF idPreco <> ped-item-ext.cod_controle_preco THEN DO:
          RUN criarReg(ped-venda.nr-pedido,
                       FALSE,
                       "O ID do banco de dados est  diferente do ID encontrado",
                       ped-item.it-codigo,
                       ped-item.cod-refer,
                       IF ped-venda.mo-codigo = 0 THEN dvlReal ELSE dVlDolar ,
                       ped-item.vl-preori,
                       idPreco,
                       ped-item-ext.cod_controle_preco).

          ASSIGN ped-item-ext.cod_controle_preco = idPreco.
       END.
       ELSE DO:
          RUN criarReg(ped-venda.nr-pedido,
                       TRUE,
                       "ID Iguais",
                       ped-item.it-codigo,
                       ped-item.cod-refer,
                       IF ped-venda.mo-codigo = 0 THEN dvlReal ELSE dVlDolar ,
                       ped-item.vl-preori,
                       idPreco,
                       ped-item-ext.cod_controle_preco).

       END.
    END.
END.
{esp/util.i}
{esp/exportarTabelaCsv.i ttPedido}
PROCEDURE criarReg:

    DEFINE INPUT  PARAMETER pNrPedido   AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pLogOK      AS LOGICAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pDescricao  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pItcodigo   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCodRefer   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pPrecoEnc   AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pPrecoPed   AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pIdPrecoEnc AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pIdPrecoPed AS INTEGER     NO-UNDO.


    CREATE ttPedido.

    ASSIGN ttPedido.nrPedido   = pNrPedido
           ttPedido.logOK      = pLogOK
           ttPedido.descricao  = pDescricao
           ttPedido.itCodigo   = pItCodigo
           ttPedido.codRefer   = pCodRefer
           ttPedido.precoEnc   = pPrecoEnc
           ttPedido.precoPed   = pPrecoPed
           ttPedido.idPrecoEnc = pIdPrecoEnc
           ttPedido.idPrecoPed = pIdPrecoPed
           .


END PROCEDURE.
