{esapi/getSaldoTercDoctoLisa3.i}
{esapi/getNfsRetornoLisaPendLanctoErp.i}
{esp/util.i}
{esapi/integrarRetornoERp.i}


DEFINE INPUT  PARAMETER pArquivo AS CHARACTER   NO-UNDO.

DEF TEMP-TABLE tt-docum-est NO-UNDO LIKE docum-est 
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE ttDocITem NO-UNDO
    FIELD serieOrigem         AS CHAR
    FIELD nrDoctoOrigem       AS CHAR FORMAT 'x(20)'
    FIELD produtoOrigem       AS CHAR FORMAT 'x(15)'
    FIELD nrSeq               AS INT
    FIELD serieRetorno        AS CHAR
    FIELD nrDoctoRetorno      LIKE nrDoctoOrigem    
    FIELD nrSeqRetorno        LIKE nrSeq
    FIELD codReferRetorno     AS CHAR
    FIELD quantidade          AS DECIMAL
    FIELD valorUnit           AS DECIMAL
    FIELD cDtEmissao          AS CHAR
    FIELD dtEmissao           AS DATE
    INDEX ind-ret serieRetorno nrDoctoRetorno  nrSeqRetorno   
    .
    
DEF TEMP-TABLE tt-item-terc NO-UNDO
FIELD rw-saldo-terc   AS ROWID
FIELD quantidade      LIKE saldo-terc.quantidade
FIELD preco-total     LIKE componente.preco-total EXTENT 0
FIELD desconto        LIKE componente.desconto    EXTENT 0    
FIELD cod-depos       LIKE saldo-terc.cod-depos
FIELD nr-ord-prod     LIKE saldo-terc.nr-ord-prod
FIELD nat-of          LIKE item-doc-est.nat-of.

DEFINE TEMP-TABLE ttDoc
       FIELD serieRetorno        AS CHAR
       FIELD nrDoctoRetorno      LIKE nrDoctoOrigem
       FIELD vlTotal             AS DECIMAL
       FIELD pesoTotal           AS DECIMAL
       FIELD data                AS DATE
       FIELD integrado           AS LOGICAL   
       INDEX ind-ret serieRetorno nrDoctoRetorno 
       .
       
DEFINE TEMP-TABLE ttNfChave
    FIELD serie AS INT
    FIELD nota AS INT
    FIELD chave AS CHAR FORMAT 'x(60)'
    .
       
DEFINE VARIABLE pNivel          AS INTEGER     NO-UNDO INIT 4.
DEFINE VARIABLE qtCorrente      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dTotalRetornado AS DECIMAL     NO-UNDO.
    

DEFINE VARIABLE cSeriePadrao    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNatOperPadrao  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE codEmitPadrao   AS INTEGER     NO-UNDO.


RUN esapi/getVarsSaldoTercLisa.p(OUTPUT cSeriePadrao,OUTPUT cNatOperPadrao,OUTPUT codEmitPadrao).


INPUT FROM  c:\temp\nota-retorno-chave.csv.

    REPEAT:    
        CREATE ttNFChave.
        IMPORT DELIMITER ";" ttNfChave.  
        
    END.    
    

INPUT CLOSE.




INPUT FROM  VALUE(pArquivo).

    REPEAT:
    
        CREATE ttDocItem.
        IMPORT DELIMITER ";" ttDocItem.
        ASSIGN ttDocitem.dtEmissao       =  convDt4A2M2D(ttDocitem.cDtEmissao)
               ttDocitem.nrDoctoOrigem   =  string(INT(ttDocitem.nrDoctoOrigem),'9999999')
               ttDocitem.nrseq           =  ttDocitem.nrseq * 10        
               ttDocitem.nrDoctoRetorno  =  string(INT(ttDocitem.nrDoctoRetorno),'9999999')
               ttDocitem.nrseqRetorno    =  ttDocitem.nrseqRetorno * 10 
               ttDocItem.codRef          = TRIM(ttDocItem.codRef)
               .
         /*MESSAGE ttDocitem.produto  SKIP
                 ttDocitem.codRef
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    END.
    
    

INPUT CLOSE.


FOR EACH ttDocItem WHERE nrDoctoOrigem <> "0" AND nrDoctoOrigem <> " " :
    FIND ttDoc
        WHERE ttDoc.serieRetorno   = ttDocItem.serieRetorno
        AND   ttDoc.nrDoctoRetorno = ttDocItem.nrDoctoRetorno
        NO-ERROR.
   IF NOT AVAIL ttDoc THEN DO:
      CREATE  ttDoc.
      ASSIGN  ttDoc.serieRetorno   = ttDocItem.serieRetorno
              ttDoc.nrDoctoRetorno = ttDocItem.nrDoctoRetorno
              ttDoc.data           = ttDocitem.dtemissao
              .     
   END.
   ASSIGN  ttDoc.vlTotal    = ttDoc.vlTotal     + ttDocItem.quantidade  * ttDocItem.valorUnit
           ttDoc.pesoTotal  = ttDoc.pesoTotal    + ttDocItem.quantidade  
           .  

END.


OUTPUT TO value('c:\temp\retornos-incluidos' + STRING(TIME) + '.csv').
PUT "Serie Remessa;Doc.Remessa;Seq.Remesa;Serie Retorno;Doc.Retorno;Seq.Retorno;Refer;Nivel;Qt.retorno" SKIP.
 
    FOR EACH ttDoc: 
    
        EMPTY TEMP-TABLE tt-docum-est.
    
        FIND ttNfChave
            WHERE ttNfChave.serie = int(ttDoc.serieRetorno)
            AND   ttNfChave.nota  = INT(ttDoc.nrDoctoRetorno) NO-ERROR.
        CREATE tt-docum-est.
        ASSIGN 
        tt-docum-est.cod-estabel                 = '505'
        tt-docum-est.cod-emitente                = codEmitPadrao
        tt-docum-est.serie-docto                 = cSeriePadrao
        tt-docum-est.nro-docto                   = string(int(ttdoc.nrDoctoRetorno),'9999999')
        tt-docum-est.nat-operacao                = '19208i' 
        tt-docum-est.dt-emissao                  = ttDoc.data
        tt-docum-est.dt-trans                    = ttDoc.data
        tt-docum-est.cod-chave-aces-nf-eletro    = IF AVAIL ttNfChave THEN ttNfChave.chave ELSE ''
        tt-docum-est.tot-valor                   = ttDoc.vlTotal
        tt-docum-est.tot-peso                    = ttDoc.pesoTotal
        tt-docum-est.valor-mercad                = ttDoc.vlTotal
        tt-docum-est.base-icm                    = ttDoc.vlTotal  
        tt-docum-est.base-ipi                    = ttDoc.vlTotal 
        tt-docum-est.despesa-nota                = 0
        .
    
        EMPTY TEMP-TABLE tt-item-terc.
        FOR EACH ttDocITem 
            WHERE ttDocItem.serieRetorno    = ttDoc.serieRetorno
            AND   ttDocItem.nrDoctoRetorno  = ttDoc.nrDoctoRetorno
            :                        
            RUN esapi/getQtRetornadaERP.p(
            ttDocItem.serieRetorno,    
            ttDocItem.nrDoctoRetorno,    
            tt-docum-est.nat-operacao, 
            ttDocItem.nrSeqRetorno,     
            OUTPUT dTotalRetornado    
            ).
            ASSIGN qtCorrente = ttDocItem.quantidade - dTotalRetornado.         
            
            IF qtCorrente <= 0  THEN DO:
               NEXT.        
            END.
            
            RUN esapi/getSaldoTercDoctoLisa3.p(ttDocItem.nrDoctoOrigem,
                                               ttDocItem.produtoOrigem,
                                               ttDocItem.nrSeq,
                                               qtCorrente,
                                               OUTPUT TABLE ttSaldoNivel
                                               ).
            FOR EACH ttSaldoNivel
                WHERE  ttSaldoNivel.qtLimite > 0
                :                
                CREATE tt-item-terc.                                                                                                                                                    
                ASSIGN tt-item-terc.rw-saldo-terc = ttSaldoNivel.rROWID                                                                                                                   
                      tt-item-terc.quantidade     = ttSaldoNivel.qtLimite                                                                                                                              
                      tt-item-terc.preco-total    =  round(ttSaldoNivel.qtLimite * ttDocItem.valorUnit,2)  
                      tt-item-terc.desconto       = 0                                                                                                                                   
                      tt-item-terc.cod-depos      = 'ARM'   .
                
               EXPORT DELIMITER ";" 
                ttDocItem.serieOrigem 
                ttDocItem.nrDoctoOrigem
                ttDocItem.produtoOrigem
                ttDocItem.nrSeq
                ttDocItem.serieRetorno
                ttDocItem.nrDoctoRetorno
                ttDocItem.nrSeqRetorno                               
                ttDocItem.codReferRetorno            
                ttSaldoNivel.nivel 
                ttSaldoNivel.qtlimite
                .               
            END.            
        END.
        RUN esapi/cria-nota-re1001-ret.p(INPUT TABLE tt-docum-est,INPUT TABLE tt-item-terc).
        FIND docum-est NO-LOCK
        WHERE docum-est.cod-estabel  = tt-docum-est.cod-estabel                
        AND   docum-est.cod-emitente = tt-docum-est.cod-emitente               
        AND   docum-est.serie-docto  = tt-docum-est.serie-docto                
        AND   docum-est.nro-docto    = tt-docum-est.nro-docto                  
        AND   docum-est.nat-operacao = tt-docum-est.nat-operacao               
        NO-ERROR.
        IF AVAIL docum-est THEN DO:
        IF NOT CAN-FIND(FIRST item-doc-est OF docum-est) THEN DO:
           FIND CURRENT docum-est EXCLUSIVE-LOCK.
           DELETE docum-est.
           RETURN  "nok":u.
        END.
        RUN esapi/atu-nota-re1001-ret.p(INPUT TABLE tt-docum-est).        
    END.    
        
    END.
OUTPUT CLOSE.

{esp/exportarTabelaCsv3.i ttDoc " " " " "Doc-integrado"}

{esp/registrarErros.i}
