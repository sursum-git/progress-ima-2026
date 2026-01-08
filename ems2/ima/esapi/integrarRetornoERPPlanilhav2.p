{esapi/getSaldoTercDoctoLisa3.i}
{esapi/getNfsRetornoLisaPendLanctoErp.i}
{esp/util.i}
{esapi/integrarRetornoERp.i}


DEFINE INPUT  PARAMETER pArquivo AS CHARACTER   NO-UNDO.

DEF TEMP-TABLE tt-docum-est NO-UNDO LIKE docum-est 
    FIELD r-rowid AS ROWID.

DEFINE TEMP-TABLE tt-saldo-terc LIKE saldo-terc
    FIELD r-rowid AS ROWID.
    
DEFINE TEMP-TABLE ttItemTerc NO-UNDO
    FIELD serieOrigem    AS CHAR
    FIELD nrDoctoOrigem  LIKE docum-est.nro-docto
    FIELD produto        LIKE ITEM.it-codigo
    FIELD nrSeq          AS INT
    FIELD serieRetorno   AS CHAR
    FIELD nrDoctoRetorno LIKE docum-est.nro-docto
    FIELD nrSeqRetorno   AS INT
    FIELD codReferRet    AS CHAR
    FIELD nivel          AS INT
    FIELD quantidade     AS DECIMAL
    FIELD valorUnit      AS DECIMAL
    FIELD valorTotal     AS DECIMAL   
    INDEX ind-ret serieRetorno nrDoctoRetorno nrSeqRetorno
    .
    
DEFINE TEMP-TABLE ttConc NO-UNDO
    FIELD serieRetorno   AS CHAR
    FIELD nrDoctoRetorno LIKE docum-est.nro-docto
    FIELD nrSeqRetorno   AS INT
    FIELD codReferRet    AS CHAR
    FIELD qtOriginal     AS DECIMAL
    FIELD qtTerceiros    AS DECIMAL
    FIELD qtDif          AS DECIMAL
    FIELD qtLanc         AS INT
    INDEX ind-ret IS PRIMARY serieRetorno  nrDoctoRetorno  nrSeqRetorno  .
    
    

DEFINE TEMP-TABLE ttDocITem NO-UNDO
    FIELD serieOrigem         AS CHAR
    FIELD nrDoctoOrigem       AS CHAR FORMAT 'x(20)'
    FIELD produtoOrigem       AS CHAR FORMAT 'x(15)'
    FIELD nrSeq               AS INT
    FIELD serieRetorno        AS CHAR
    FIELD nrDoctoRetorno      LIKE docum-est.nro-docto
    FIELD nrSeqRetorno        AS INT
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
DEFINE VARIABLE dSaldo AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dMovto AS DECIMAL     NO-UNDO.
DEFINE VARIABLE conferenciaOK AS LOGICAL     NO-UNDO.

DEFINE TEMP-TABLE ttDoc
       FIELD serieRetorno        AS CHAR
       FIELD nrDoctoRetorno      LIKE docum-est.nro-docto
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

FOR EACH ttDocItem
    WHERE ttDocitem.serieOrigem = '':
    DELETE ttDocItem.
END.
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

FOR EACH saldo-terc NO-LOCK
    WHERE saldo-terc.cod-estabel = '505':
    CREATE tt-saldo-terc.
    BUFFER-COPY saldo-terc TO tt-saldo-terc.
    ASSIGN tt-saldo-terc.r-rowid = ROWID(saldo-terc).
    
END.

OUTPUT TO value('c:\temp\log-aloc-terceiros' + STRING(TIME) + '.csv').

 
FOR EACH ttDoc
    WHERE ttDoc.serieRetorno <> '': 

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
        BY   ttDocItem.serieRetorno BY  ttDocItem.nrDoctoRetorno BY ttDocItem.nrseqRetorno
        :                        
       /* RUN esapi/getQtRetornadaERP.p(
        ttDocItem.serieRetorno,    
        ttDocItem.nrDoctoRetorno,    
        tt-docum-est.nat-operacao, 
        ttDocItem.nrSeqRetorno,     
        OUTPUT dTotalRetornado    
        ).
        ASSIGN qtCorrente = ttDocItem.quantidade - dTotalRetornado.         
        */
        ASSIGN qtCorrente = ttDocItem.quantidade .
        IF qtCorrente <= 0  THEN DO:
           NEXT.        
        END.
        
        RUN getSaldoTerc.
        FOR EACH ttSaldoNivel
            WHERE  ttSaldoNivel.qtLimite > 0
            :                
            CREATE tt-item-terc.                                                                                                                                                    
            ASSIGN tt-item-terc.rw-saldo-terc = ttSaldoNivel.rROWID                                                                                                                   
                  tt-item-terc.quantidade     = ttSaldoNivel.qtLimite                                                                                                                              
                  tt-item-terc.preco-total    =  round(ttSaldoNivel.qtLimite * ttDocItem.valorUnit,2)  
                  tt-item-terc.desconto       = 0                                                                                                                                   
                  tt-item-terc.cod-depos      = 'ARM' .
            
         /*  EXPORT DELIMITER ";" 
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
            .   */
            CREATE ttItemTerc.
            ASSIGN 
            ttItemTerc.serieOrigem     =  ttDocItem.serieOrigem
            ttItemTerc.nrDoctoOrigem   =  ttDocItem.nrDoctoOrigem
            ttItemTerc.produto         =  ttDocItem.produtoOrigem
            ttItemTerc.nrSeq           =  ttDocItem.nrSeq
            ttItemTerc.serieRetorno    =  ttDocItem.serieRetorno
            ttItemTerc.nrDoctoRetorno  =  ttDocItem.nrDoctoRetorno
            ttItemTerc.nrSeqRetorno    =  ttDocItem.nrSeqRetorno 
            ttItemTerc.codReferRet     =  ttDocItem.codReferRetorno
            ttItemTerc.nivel           =  ttSaldoNivel.nivel 
            ttItemTerc.quantidade      =  ttSaldoNivel.qtlimite
            ttItemTerc.valorUnit       =  ttDocItem.valorUnit
            ttItemTerc.valorTotal      =   tt-item-terc.preco-total
            .            
            
            
        END.            
    END.   
    
    RUN conciliar.   
    
    
    
    IF NOT conferenciaOK THEN
    DO:
        MESSAGE 'Foram encontradas diferen‡as, favor verificar o arquivo de concilia‡Æo'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN.
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

PROCEDURE getSaldoTerc:

   ASSIGN dSaldo = qtCorrente
          dMovto = 0 .
    

    EMPTY TEMP-TABLE ttSaldoNivel.
    PUT UNFORM FILL('=',100) SKIP.
    PUT UNFORM "busca de saldos de terceiros para retorno:"  ttDocItem.nrDoctoRetorno " produto:"   ttDocItem.produtoOrigem " Seq:"  ttDocItem.nrSeqRetorno  " saldo a alocar:" dSaldo    SKIP.
    
    PUT UNFORM 'buscando o saldo diretamente pela sequencia.' SKIP.
    blsaldoterc:
    FOR FIRST tt-saldo-terc  NO-LOCK
    WHERE tt-saldo-terc.cod-emitente   = codEmitPadrao
    AND   tt-saldo-terc.nro-docto      = ttDocItem.nrDoctoOrigem
    AND   tt-saldo-terc.serie          = cSeriePadrao
    AND   tt-saldo-terc.nat-operacao   = cNatOperPadrao
    AND   tt-saldo-terc.it-codigo      = ttDocItem.produtoOrigem
    AND   tt-saldo-terc.seq            = ttDocItem.nrSeq    
    AND   tt-saldo-terc.quantidade     > 0
    :
        PUT UNFORM "Achei o registro a nivel de sequencia e o mesmo tem o saldo de:" tt-saldo-terc.quantidade " -NOTA:" tt-saldo-terc.nro-docto " -SEQ:" tt-saldo-terc.seq SKIP.
        RUN calcSaldo.
/*         IF dSaldo <= tt-saldo-terc.quantidade THEN DO:                                                                                                                   */
/*            PUT UNFORM  "a quantidade de saldo ser  zerada e o valor zera deduzido do saldo de terceiros pois o saldo corrente ‚ maior que a quantidade necessaria" SKIP. */
/*            ASSIGN dSaldo = 0                                                                                                                                             */
/*                   dMovto = dSaldo.                                                                                                                                       */
/*                   .                                                                                                                                                      */
/*         END.                                                                                                                                                             */
/*         ELSE DO:                                                                                                                                                         */
/*                                                                                                                                                                          */
/*            ASSIGN dSaldo =  dSaldo - tt-saldo-terc.quantidade                                                                                                            */
/*                   dMovto =  tt-saldo-terc.quantidade                                                                                                                     */
/*                   .                                                                                                                                                      */
/*            PUT UNFORM "a quantidade de saldo ‚ maior que o saldo do documento corrente e ser  necess rio mais de uma aloca‡Æo de terceiros" SKIP.                        */
/*                                                                                                                                                                          */
/*         END.                                                                                                                                                             */
        
         CREATE ttSaldoNivel.
         ASSIGN ttSaldoNivel.nivel       = 1
                ttSaldoNivel.qtLimite    = dMovto
                ttSaldoNivel.rRowid      = tt-saldo-terc.r-rowid
                .
         ASSIGN tt-saldo-terc.quantidade =  tt-saldo-terc.quantidade - dMovto  .   
         PUT UNFORM "quantidade do movimento:" dMovto
             " saldo de terceiro atualizado em:" tt-saldo-terc.quantidade SKIP.
    END.    
    IF dSaldo > 0 THEN DO: 
       PUT UNFORM "saldo atual maior que zero(" dSaldo "), buscando saldo por documento"  SKIP.
       FOR EACH tt-saldo-terc NO-LOCK
       WHERE tt-saldo-terc.cod-emitente   = codEmitPadrao
       AND   tt-saldo-terc.nro-docto      = ttDocItem.nrDoctoOrigem
       AND   tt-saldo-terc.serie          = cSeriePadrao
       AND   tt-saldo-terc.nat-operacao   = cNatOperPadrao
       AND   tt-saldo-terc.it-codigo      = ttDocItem.produtoOrigem    
       AND   tt-saldo-terc.quantidade     > 0
       //AND   saldo-terc.seq            = pSeq 
       .
           PUT UNFORM "achei o seguinte saldo por documento:" tt-saldo-terc.quantidade " -NOTA:" tt-saldo-terc.nro-docto " -SEQ:" tt-saldo-terc.seq SKIP. 
           RUN calcSaldo.
           IF RETURN-VALUE = 'nok' THEN DO:
              LEAVE.
               
           END.
           CREATE ttSaldoNivel.
           ASSIGN ttSaldoNivel.nivel       = 2
                  ttSaldoNivel.qtLimite    = dMovto
                  ttSaldoNivel.rRowid      = tt-saldo-terc.r-rowid
                  . 
           ASSIGN tt-saldo-terc.quantidade =  tt-saldo-terc.quantidade -  dMovto.
           PUT UNFORM "quantidade do movimento:" dMovto
             " saldo de terceiro atualizado em:" tt-saldo-terc.quantidade SKIP.
       END.        
       IF dSaldo > 0 THEN DO:
          PUT UNFORM "saldo atual maior que zero(" dSaldo "), buscando saldo por Produto " SKIP.
          FOR EACH tt-saldo-terc NO-LOCK
              WHERE tt-saldo-terc.cod-estabel    = '505'
              AND   tt-saldo-terc.it-codigo      = ttDocItem.produtoOrigem 
              AND   tt-saldo-terc.cod-emitente   = codEmitPadrao
              AND   tt-saldo-terc.quantidade     > 0
              USE-INDEX estab-item .          
              PUT UNFORM "achei o seguinte saldo por produto:" tt-saldo-terc.quantidade " -NOTA:" tt-saldo-terc.nro-docto " -SEQ:" tt-saldo-terc.seq SKIP. 
              RUN calcSaldo.
              IF RETURN-VALUE = 'nok' THEN DO:
                 LEAVE.
               
              END.
              CREATE ttSaldoNivel.
              ASSIGN ttSaldoNivel.nivel       = 3
                     ttSaldoNivel.qtLimite    = dMovto
                     ttSaldoNivel.rRowid      = tt-saldo-terc.r-rowid
                     .   
              ASSIGN tt-saldo-terc.quantidade =  tt-saldo-terc.quantidade -  dMovto.   
              PUT UNFORM "quantidade do movimento:" dMovto
             " saldo de terceiro atualizado em:" tt-saldo-terc.quantidade SKIP.
                     
          END.      
          IF dSaldo > 0 THEN  DO:
              PUT UNFORM "ATENCAO -> NAO ENCONTRADO SALDO COMPLETO NEM POR SEQUENCIA, NEM POR DOCUMENTO, NEM POR PRODUTO"   SKIP.
             /*CREATE ttSaldoNivel.
             ASSIGN ttSaldoNivel.nivel       = 4
                    ttSaldoNivel.qtLimite    = dSaldo
                    ttSaldoNivel.rRowid      = ?
                    .*/
          END.                             
       END.   
    END.   


END PROCEDURE.


PROCEDURE calcSaldo:

    IF dSaldo = 0 THEN    DO:
       PUT UNFORM "calcSaldo-> Saldo Zerado - programa retorna nok e vai para o proximo registro "  SKIP.
       RETURN 'nok'.  
           
    END.
    IF dSaldo <= tt-saldo-terc.quantidade THEN   DO:
       PUT UNFORM  "a quantidade de saldo serah zerada e o valor zera deduzido do saldo de terceiros pois o saldo corrente eh maior que a quantidade necessaria" SKIP.
       ASSIGN dMovto   = dSaldo
              dSaldo    = 0 .      
    END.
    ELSE DO:
     ASSIGN dMovto = tt-saldo-terc.quantidade
            dSaldo = dSaldo -  tt-saldo-terc.quantidade .
     PUT UNFORM "a quantidade de saldo eh maior que o saldo do documento corrente e serah necessario mais de uma alocacao de terceiros" SKIP.            
    END.
    RETURN 'ok'.
END PROCEDURE.  

PROCEDURE conciliar:


    FOR EACH ttDocItem       :   
        FIND ttConc 
            WHERE ttConc.serieRetorno       = ttDocItem.serieRetorno
            AND   ttConc.nrDoctoRetorno     = ttDocItem.nrDoctoRetorno
            AND   ttConc.nrSeqRetorno       = ttDocItem.nrSeqRetorno
            NO-ERROR  .
        IF NOT AVAIL  ttConc THEN  DO:
           CREATE ttConc.
           ASSIGN ttConc.serieRetorno       = ttDocItem.serieRetorno
                  ttConc.nrDoctoRetorno     = ttDocItem.nrDoctoRetorno
                  ttConc.nrSeqRetorno       = ttDocItem.nrSeqRetorno
                  ttConc.codReferRet        = ttDocItem.codReferRetorno
                  .            
        END.
        ASSIGN ttConc.qtOriginal = ttDocItem.quantidade.
    END.
    
    
    FOR EACH ttItemTerc        :   
        FIND ttConc 
            WHERE ttConc.serieRetorno       = ttItemTerc.serieRetorno
            AND   ttConc.nrDoctoRetorno     = ttItemTerc.nrDoctoRetorno
            AND   ttConc.nrSeqRetorno       = ttItemTerc.nrSeqRetorno
            NO-ERROR  .
        IF NOT AVAIL  ttConc THEN  DO:
           CREATE ttConc.
           ASSIGN ttConc.serieRetorno       = ttItemTerc.serieRetorno
                  ttConc.nrDoctoRetorno     = ttItemTerc.nrDoctoRetorno
                  ttConc.nrSeqRetorno       = ttItemTerc.nrSeqRetorno
                  //ttConc.codReferRet        = ttItemTerc.codReferRetorno
                  .            
        END.
        ASSIGN ttConc.qtTerceiros           = ttConc.qtTerceiros + ttItemTerc.quantidade
               ttConc.qtLanc                = ttConc.qtLanc + 1.
               .
    END.
    
    
    FOR EACH ttConc:
        ASSIGN ttConc.qtDif = ttConc.qtOriginal - ttConc.qtTerceiros .
    END.
    {esp/exportarTabelaCsv3.i ttConc " " " " "conciliacao"}
    {esp/exportarTabelaCsv3.i ttDocItem "  " " " "Terceiros-orig"}
    {esp/exportarTabelaCsv3.i ttItemTerc "  " " " "Terceiros-alocado"}
    ASSIGN conferenciaOK = NOT CAN-FIND( FIRST ttConc WHERE ttConc.qtDif > 0) .
    

/*
    ttconc
    serieRetorno  
    nrDoctoRetorno
    nrSeqRetorno  
    codReferRet   
    qtOriginal    
    qtTerceiros   
    qtDif         */
    

    



END PROCEDURE.



