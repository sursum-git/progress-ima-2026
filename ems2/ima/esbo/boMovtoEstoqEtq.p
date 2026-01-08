/*****************************************************************************
programa: esbo/boMovtoEstoqEtq.p
objetivo: Retornar em uma temp-table as etiquetas referentes movimentos 
de faturamento e devolu‡Æo de faturamento
******************************************************************************/

DEFINE VARIABLE cTipo               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEstab              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSerie              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iEmitente           AS INTEGER     NO-UNDO.
DEFINE VARIABLE cNrNotaFis          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNatOperacao        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE nrSeq               AS INTEGER     NO-UNDO.
DEFINE VARIABLE cTpRet              AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cEstabDevol         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSerieDevol         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iEmitenteDevol      AS INTEGER     NO-UNDO.
DEFINE VARIABLE cNrNotaFisDevol     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNatOperacaoDevol   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE nrSeqDevol          AS INTEGER     NO-UNDO.     
DEFINE VARIABLE nrTrans             AS INTEGER     NO-UNDO.


DEFINE VARIABLE rowidItemNf         AS ROWID       NO-UNDO.

DEFINE VARIABLE rRowid AS ROWID       NO-UNDO. //movtoEstoq

{esbo/boMovtoEstoqEtq.i}
 
{esp/util.i} 
 
 
PROCEDURE setRowidMovtoEstoq:

    DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
    ASSIGN rRowid = pRowid.

    FOR FIRST movto-estoq FIELDS(cod-estabel serie-docto nro-docto nat-operacao nr-sequencia) NO-LOCK
    WHERE ROWID(movto-estoq) = rRowid:        
    END.
    IF AVAIL movto-estoq THEN DO:
       RUN setParams(IF movto-estoq.esp-docto = 20 THEN 'dev' ELSE 'fat',
                     movto-estoq.cod-estabel,
                     movto-estoq.serie-docto,
                     movto-estoq.nro-docto,
                     movto-estoq.nat-operacao,
                     movto-estoq.sequen-nf,
                     movto-estoq.cod-emitente
                    ). 
      ASSIGN nrTrans = movto-estoq.nr-trans.              
        
    END.
    ELSE DO:
        RETURN 'nok'.
    END.


END PROCEDURE.

PROCEDURE setParams:

    DEFINE INPUT  PARAMETER pTipo           AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pEstab          AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pSerie          AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pNrNotaFis      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pNatOperacao    AS CHARACTER   NO-UNDO.  
    DEFINE INPUT  PARAMETER pNrSeq          AS INTEGER     NO-UNDO.  
    DEFINE INPUT  PARAMETER pEmitente       AS INTEGER     NO-UNDO.
    
    ASSIGN  cTipo         = pTipo.
    CASE  pTipo:
        WHEN 'fat' THEN DO:       
            ASSIGN
                     cEstab        = pEstab
                     cSerie        = pSerie
                     cNrNotaFis    = pNrNotaFis
                     cNatOperacao  = pNatOperacao
                     nrSeq         = pNrSeq
                     iEmitente     = pEmitente.               
        END.
        WHEN 'dev' THEN  DO:          
           ASSIGN 
                   cEstabDevol        = pEstab
                   cSerieDevol        = pSerie
                   cNrNotaFisDevol    = pNrNotaFis
                   cNatOperacaoDevol  = pNatOperacao
                   nrSeqDevol         = pNrSeq 
                   iEmitenteDevol     = pEmitente.           
                                      
        END.
    END CASE.
        
    
   

END PROCEDURE.

PROCEDURE setTipoRetorno:

    DEFINE INPUT  PARAMETER pRet AS CHARACTER   NO-UNDO.
    
    ASSIGN cTpRet = pRet.


END PROCEDURE.       

PROCEDURE getDadosNFVenda:

    EMPTY TEMP-TABLE ttContainer.
    EMPTY TEMP-TABLE ttEtq.
    
    IF cTipo = 'dev' THEN  DO:
       RUN  esapi/getSeqNfVendaDevol.p(INPUT cEstabDevol,
                                       INPUT cSerieDevol,
                                       INPUT cNrnotaFisDevol,
                                       INPUT iEmitenteDevol,
                                       INPUT cNatOperacaoDevol,
                                       INPUT nrSeqDevol,
                                       OUTPUT rowidItemNF
                                       ).     
       FOR FIRST it-nota-fisc NO-LOCK
        WHERE ROWID(it-nota-fisc) = rowidItemNF:
        ASSIGN cEstab      = it-nota-fisc.nr-nota-fis
               cSerie      = it-nota-fisc.serie
               cNrNotaFis  = it-nota-fisc.nr-nota-fis
               nrSeq       =  it-nota-fisc.nr-seq-fat .
       END.
       IF NOT AVAIL it-nota-fisc  THEN  DO:
          RETURN 'nok'.           
       END.              
    END.  
    /*
        MESSAGE          
        'tipo:' cTipo       SKIP
        'estab:' cEstab     SKIP 
        'serie:' cSerie     SKIP 
        'nf' cNrNotaFis     SKIP
        'nf' cNrNotaFisDevol     SKIP
        'nat.operacao' cNatOperacao SKIP
        'seq.' nrSeq       
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    */    
        
    
    
    
    FOR FIRST nota-fiscal FIELDS(nr-pedcli nome-abrev cod-estabel serie nr-nota-fis) NO-LOCK
        WHERE nota-fiscal.cod-estabel = cEstab
        AND   nota-fiscal.serie       = cSerie
        AND   nota-fiscal.nr-nota-fis = cNrNotaFis,
        EACH it-nota-fisc  
        WHERE it-nota-fisc.cod-estabel  = nota-fiscal.cod-estabel
        AND   it-nota-fisc.serie        = nota-fiscal.serie
        AND   it-nota-fisc.nr-nota-fis  = nota-fiscal.nr-nota-fis
        AND   it-nota-fisc.nr-seq-fat = nrSeq NO-LOCK, 
        EACH ped-item-res FIELDS(cod-estabel serie nr-nota-fis nr-sequencia) 
        WHERE ped-item-res.cod-estabel  = nota-fiscal.cod-estabel 
        AND   ped-item-res.serie        = nota-fiscal.serie 
        AND   ped-item-res.nr-nota-fis  = INT(nota-fiscal.nr-nota-fis) 
       // AND   ped-item-res.nr-seq-fat   = it-nota-fisc.nr-seq-fat 
        AND   ped-item-res.it-codigo    = it-nota-fisc.it-codigo
        AND   ped-item-res.cod-refer    = it-nota-fisc.cod-refer
        AND   ped-item-res.faturado    = YES NO-LOCK,
        EACH ped-item-rom FIELDS(cod-estabel num-etiqueta quantidade )
            WHERE ped-item-rom.cod-estabel = nota-fiscal.cod-estabel
            AND   ped-item-rom.nome-abrev  = nota-fiscal.nome-ab-cli
            AND   ped-item-rom.nr-pedcli   = nota-fiscal.nr-pedcli
            AND   ped-item-rom.nr-sequencia= ped-item-res.nr-sequencia
            NO-LOCK,                 
        EACH ob-etiqueta FIELDS(num-etiqueta cod-estabel nr-container quantidade) NO-LOCK
           WHERE ob-etiqueta.cod-estabel   = nota-fiscal.cod-estabel
           AND   ob-etiqueta.num-etiqueta  = ped-item-rom.num-etiqueta
           :   
            
             CASE cTpRet:
                WHEN 'etiqueta' THEN DO:
                    CREATE ttEtq.
                    ASSIGN ttEtq.cod-estabel  = nota-fiscal.cod-estabel
                           ttEtq.num-etiqueta = ob-etiqueta.num-etiqueta
                           ttEtq.nr-container = ob-etiqueta.nr-container
                           ttEtq.quantidade   = ob-etiqueta.quantidade .                          
                
                END.
             
                WHEN 'container' THEN DO:
                    FIND ttContainer NO-LOCK
                    WHERE ttContainer.nr-container = ob-etiqueta.nr-container  NO-ERROR.
                    IF NOT AVAIL ttContainer THEN DO:
                       CREATE ttContainer.
                       ASSIGN ttContainer.nr-container = ob-etiqueta.nr-container .                            
                    END.                    
                    ASSIGN ttContainer.quantidade = ttContainer.quantidade + ob-etiqueta.quantidade.                    
                END. 
                
                OTHERWISE 
                DO:
                    
                END.
             END CASE.                   
    END.

  


END PROCEDURE.


PROCEDURE getTtEtq:

    DEFINE OUTPUT PARAMETER TABLE FOR ttEtq.



END PROCEDURE.

PROCEDURE getTtContainer:

    DEFINE OUTPUT PARAMETER TABLE FOR ttContainer.



END PROCEDURE.


