{esp/util.i}
DEFINE TEMP-TABLE ttDados
    FIELD linha AS INT
    FIELD campo AS CHAR FORMAT 'x(200)' EXTENT 10 .
DEFINE TEMP-TABLE ttProduto
    FIELD linha             AS INTEGER
    FIELD produto           AS CHAR FORMAT 'x(20)'
    FIELD descricao         AS CHAR FORMAT 'x(100)'
    FIELD unidade           AS CHAR
    FIELD vlTotEntradaInf   AS DECIMAL
    FIELD vlTotEntradaCalc  AS DECIMAL
    FIELD vlTotSaidaInf     AS DECIMAL
    FIELD vlTotSaidaCalc    AS DECIMAL 
    FIELD vlSaldoInf        AS DECIMAL
    FIELD vlSaldoCalc       AS DECIMAL
    FIELD vlTotEntradaErp   AS DECIMAL
    FIELD vlTotSaidaErp     AS DECIMAL
    FIELD vlSaldoErp        AS DECIMAL
    FIELD vlDifErp          AS DECIMAL
    FIELD qtRegistros       AS INT
    INDEX primario IS PRIMARY linha
    INDEX index-prod produto .

DEFINE TEMP-TABLE ttProdRef
    FIELD linha             AS INTEGER
    FIELD produto           AS CHAR FORMAT 'x(20)'
    FIELD referencia        AS CHAR
    FIELD descricao         AS CHAR FORMAT 'x(100)'
    FIELD unidade           AS CHAR
    FIELD vlTotEntradaInf   AS DECIMAL
    FIELD vlTotEntradaCalc  AS DECIMAL
    FIELD vlTotSaidaInf     AS DECIMAL
    FIELD vlTotSaidaCalc    AS DECIMAL 
    FIELD vlSaldoInf        AS DECIMAL
    FIELD vlSaldoCalc       AS DECIMAL
    FIELD vlTotEntradaErp   AS DECIMAL
    FIELD vlTotSaidaErp     AS DECIMAL
    FIELD vlSaldoErp        AS DECIMAL
    FIELD vlDifErp          AS DECIMAL
    FIELD qtRegistros       AS INT
    INDEX primario IS PRIMARY linha
    INDEX index-prod-ref produto referencia .

DEFINE TEMP-TABLE ttLanc
    FIELD linha             AS INTEGER
    FIELD produto           AS CHAR FORMAT 'x(20)'
    FIELD referencia        AS CHAR
    FIELD descricao         AS CHAR FORMAT 'x(100)'
    FIELD unidade           AS CHAR
    FIELD documento         AS CHAR FORMAT 'x(12)'
    FIELD data              AS DATE
    FIELD vlEntrada         AS DECIMAL
    FIELD vlSaida           AS DECIMAL
    FIELD vlSaldo           AS DECIMAL
    FIELD tipo              AS CHAR
    //FIELD dtTrans           AS DATE
    FIELD vlEntradaERP      AS DECIMAL
    FIELD vlSaidaERP        AS DECIMAL
    FIELD sequencia         AS INT
    FIELD origem            AS CHAR
    FIELD cancelada         AS CHAR INIT 'NAO'
    INDEX primario  linha
    INDEX index-prod-ref-doc IS PRIMARY produto referencia documento data
    INDEX ind-tipo tipo
    INDEX ind-doc documento
     .


INPUT FROM c:\temp\lisa\kardex_lisa.csv.
REPEAT:
    CREATE ttDados.
    IMPORT DELIMITER ";" ttDados.
END.

INPUT CLOSE.
// extrai os produtos
FOR EACH ttDados
    WHERE ttDados.campo[1] = 'produto':
    CREATE ttProduto.
    ASSIGN ttProduto.produto    = replace(ttDados.campo[2],'ima','')
           ttProduto.descricao  = ttDados.campo[4]
           ttProduto.unidade    = ttDados.campo[8]
           ttProduto.linha      = ttDados.linha .
END.


// extrai as referencias dos produtos
FOR EACH ttDados
    WHERE ttDados.campo[1] = 'lote':
    FIND LAST ttproduto 
        WHERE ttproduto.linha < ttDados.linha NO-ERROR.
    IF AVAIL ttproduto THEN DO:
       FIND FIRST ttProdRef
           WHERE ttProdRef.produto      = ttProduto.produto
           AND   ttProdRef.referencia   = ttDados.campo[2]
           NO-ERROR.
       IF NOT AVAIL ttprodRef THEN DO:
          CREATE ttProdRef.
          ASSIGN ttProdRef.produto    = ttProduto.produto
                 ttProdRef.descricao  = ttProduto.descricao
                 ttProdRef.unidade    = ttProduto.unidade
                 ttProdRef.linha      = ttDados.linha 
                 ttProdRef.referencia = ttDados.campo[2].
       END.                                              
    END.                                            
END.




//busca os totais informados para o produto
FOR EACH ttDados
    WHERE ttDados.campo[1] = 'total_produto':
    FIND LAST ttproduto 
        WHERE ttproduto.linha < ttDados.linha NO-ERROR.
    IF AVAIL ttProduto THEN DO:
       ASSIGN ttProduto.vlTotEntradaInf = decimal(ttDados.campo[8])
              ttProduto.vlTotSaidaInf   = decimal(ttDados.campo[9])
              ttProduto.vlSaldoInf      = decimal(ttDados.campo[10])
              .
             
    END.
END.

//busca os totais informados para o produto
FOR EACH ttDados
    WHERE ttDados.campo[1] = 'total_lote':
    FIND LAST ttprodRef
        WHERE ttprodRef.linha < ttDados.linha NO-ERROR.
    IF AVAIL ttProdRef THEN DO:
       ASSIGN ttProdRef.vlTotEntradaInf = decimal(ttDados.campo[8])
              ttProdRef.vlTotSaidaInf   = decimal(ttDados.campo[9])
              ttProdRef.vlSaldoInf      = decimal(ttDados.campo[10])
              .
    END.
END.


//importar os dados analiticos
FOR EACH ttDados
    WHERE ttDados.campo[1] = '01'.
     FIND LAST ttprodRef
        WHERE ttprodRef.linha < ttDados.linha NO-ERROR.
     IF AVAIL ttprodRef THEN DO:
        CREATE ttLanc.
        ASSIGN ttLanc.produto       = ttProdRef.produto
               ttLanc.descricao     = ttProdRef.descricao
               ttLanc.unidade       = ttProdRef.unidade
               ttLanc.linha         = ttDados.linha 
               ttLanc.referencia    = ttProdRef.referencia
               ttLanc.data          = date(ttDados.campo[2])
               ttLanc.documento     = ttDados.campo[5]
               ttLanc.vlEntrada     = DECIMAL(ttDados.campo[8]) 
               ttLanc.vlSaida       = DECIMAL(ttDados.campo[9]) 
               ttLanc.vlSaldo       = DECIMAL(ttDados.campo[10])
               ttLanc.tipo          = IF ttDados.campo[6] = '00r' THEN 'remessa' ELSE 'retorno'
               ttLanc.origem         = 'lisa'
              
               .
     END.       
END.


//sumarizando os lancamentos por produto e produto/ref
FOR EACH ttlanc:
    FIND ttProduto
        WHERE ttProduto.produto = ttlanc.produto
        NO-ERROR.
    IF AVAIL ttproduto THEN DO:
       ASSIGN ttProduto.vlTotEntradaCalc = ttProduto.vlTotEntradaCalc + ttlanc.vlEntrada
              ttProduto.vlTotSaidaCalc   = ttProduto.vlTotSaidaCalc   + ttlanc.vlSaida
              ttProduto.qtRegistros      = ttProduto.qtRegistros + 1

              .
    END.

    FIND ttProdRef
        WHERE ttProdRef.produto     = ttlanc.produto
        AND   ttProdRef.referencia  = ttlanc.referencia NO-ERROR.
    IF AVAIL ttProdRef THEN DO:
       ASSIGN ttProdRef.vlTotEntradaCalc = ttProdRef.vlTotEntradaCalc + ttlanc.vlEntrada
              ttProdRef.vlTotSaidaCalc   = ttProdRef.vlTotSaidaCalc   + ttlanc.vlSaida
              ttProdRef.qtRegistros      = ttProdRef.qtRegistros + 1
              .
    END.                                            

END.

//saldo calculado
FOR EACH ttProduto:
    ASSIGN ttProduto.vlSaldoCalc = ttProduto.vlTotEntradaCalc - ttProduto.vlTotSaidaCalc .
END.

//saldo calculado
FOR EACH ttProdRef:
    ASSIGN ttProdRef.vlSaldoCalc = ttProdRef.vlTotEntradaCalc - ttProdRef.vlTotSaidaCalc .
END.


//inclusao das notas fiscais de remessa IMA
FOR EACH  saldo-terc NO-LOCK
    WHERE saldo-terc.cod-estabel        = '505'
    AND   saldo-terc.cod-emitente       = 38284
    AND   saldo-terc.nat-operacao       = '59207i'
    AND   saldo-terc.serie              = '2'
    AND  saldo-terc.dt-retorno         <= 05.24.2024
     :
    FIND componente OF saldo-terc NO-LOCK NO-ERROR.
    IF AVAIL componente THEN DO:
       FIND ITEM OF saldo-terc NO-LOCK NO-ERROR.
       CREATE ttLanc.
       ASSIGN ttLanc.produto       = saldo-terc.it-codigo
              ttLanc.descricao     = ITEM.desc-item
              ttLanc.unidade       = ITEM.un
              ttLanc.referencia    = saldo-terc.cod-refer
              ttLanc.data          = saldo-terc.dt-retorno
              ttLanc.documento     = saldo-terc.nro-docto
              ttLanc.vlEntrada     = componente.quantidade
              ttLanc.tipo          = 'remessa'
              ttLanc.origem        = 'ima'
              ttLanc.sequencia     = saldo-terc.sequencia.
    END.
END.

//inclusao das notas fiscais de retorno IMA
FOR EACH  saldo-terc NO-LOCK
    WHERE saldo-terc.cod-estabel        = '505'
    AND   saldo-terc.cod-emitente       = 38284
    AND   saldo-terc.nat-operacao       = '59207i'
    AND   saldo-terc.serie              = '2' :

    FOR EACH componente
        WHERE componente.serie-comp      = saldo-terc.serie-docto
        AND   componente.nro-comp        = saldo-terc.nro-docto
        AND   componente.nat-comp        = saldo-terc.nat-operacao
        AND   componente.it-codigo       = saldo-terc.it-codigo
        AND   componente.cod-refer       = saldo-terc.cod-refer
        AND   componente.seq-comp        = saldo-terc.sequencia
        AND   componente.cod-emitente    = saldo-terc.cod-emitente
        AND   componente.componente      = 2
        AND   componente.dt-retorno     <= 05.24.2024.
        CREATE ttLanc.
        ASSIGN ttLanc.produto       = saldo-terc.it-codigo
               ttLanc.descricao     = ITEM.desc-item
               ttLanc.unidade       = ITEM.un
               ttLanc.referencia    = saldo-terc.cod-refer
               ttLanc.data          = componente.dt-retorno
               ttLanc.documento     = componente.nro-docto
               ttLanc.vlSaida       = componente.quantidade
               ttLanc.tipo          = 'retorno'
               ttLanc.origem        = 'ima'
               ttLanc.sequencia     = saldo-terc.sequencia.
    END.                                      
END.

FOR EACH ttLanc
    WHERE ttLanc.origem = 'lisa' .
       ASSIGN ttLanc.vlEntrada = ttlanc.vlEntrada * - 1
              ttLanc.vlSaida   = ttlanc.vlSaida   *  -1 .
            
END.
//forcando cancelamento de notas na apura‡Æo
FOR EACH ttLanc
    WHERE int(ttLanc.documento) = 120550 
    AND   ttlanc.origem = 'lisa'.
    ASSIGN ttlanc.Cancelada = 'SIM'.
END.

//soma de totais de entrada e saida IMA por produto
FOR EACH ttProduto:
    FOR EACH ttLanc
        WHERE ttLanc.produto = ttProduto.produto.

        ASSIGN ttProduto.vlTotEntradaErp  =     ttProduto.vlTotEntradaErp + ttLanc.vlEntradaERP
               ttProduto.vlTotSaidaErp    =     ttProduto.vlTotSaidaErp   + ttLanc.vlSaidaERP
               .
    END.
END.

FOR EACH ttProduto:
    ASSIGN  ttProduto.vlSaldoErp =  ttProduto.vlTotEntradaErp - ttProduto.vlTotSaidaErp
            ttProduto.vlDifErp   = ttProduto.vlSaldoErp - ttProduto.vlSaldoCalc
            .
    
END.


//soma de totais de entrada e saida IMA por produto/referencia
FOR EACH ttProdRef:
    FOR EACH ttLanc
        WHERE ttLanc.produto        = ttProdRef.produto
        AND   ttLanc.referencia     = ttProdRef.referencia.

        ASSIGN ttProdRef.vlTotEntradaErp  =     ttProdRef.vlTotEntradaErp + ttLanc.vlEntradaERP
               ttProdRef.vlTotSaidaErp    =     ttProdRef.vlTotSaidaErp   + ttLanc.vlSaidaERP
               .
    END.
END.

FOR EACH ttProdRef:
    ASSIGN  ttProdRef.vlSaldoErp =  ttProdRef.vlTotEntradaErp - ttProdRef.vlTotSaidaErp
            ttProdRef.vlDifErp   =  ttProdRef.vlSaldoErp      - ttProdRef.vlSaldoCalc
            .
    
END.


/*CREATE ttLanc.
        ASSIGN ttLanc.produto       = ttProdRef.produto
               ttLanc.descricao     = ttProdRef.descricao
               ttLanc.unidade       = ttProdRef.unidade
               ttLanc.linha         = ttDados.linha 
               ttLanc.referencia    = ttProdRef.referencia
               ttLanc.data          = date(ttDados.campo[2])
               ttLanc.documento     = ttDados.campo[5]
               ttLanc.vlEntrada     = DECIMAL(ttDados.campo[8])
               ttLanc.vlSaida       = DECIMAL(ttDados.campo[9])
               ttLanc.vlSaldo       = DECIMAL(ttDados.campo[10])
               ttLanc.tipo          = IF ttDados.campo[6] = '00r' THEN 'remessa' ELSE 'retorno'
               ttLanc.origem         = 'lisa'
              
               .*/



/*
FOR EACH ttLanc
    WHERE ttLanc.tipo = '00R':
    ASSIGN ttLanc.vlEntradaErp = 0. 
    FOR EACH  saldo-terc NO-LOCK
        WHERE saldo-terc.cod-estabel        = '505'
        AND   saldo-terc.cod-emitente       = 38284
        AND   saldo-terc.nat-operacao       = '59207i'
        AND   saldo-terc.serie              = '2'
        AND   saldo-terc.it-Codigo          = ttLanc.produto
        AND   saldo-terc.cod-refer          = ttLanc.refer
        AND   int(saldo-terc.nro-docto)     = int(ttlanc.documento)  :
        FIND FIRST componente OF saldo-terc NO-LOCK NO-ERROR.
        IF AVAIL componente THEN DO:
           ASSIGN ttlanc.vlEntradaERP = ttlanc.vlEntradaERP + componente.quantidade
                  ttLanc.dtTrans      = componente.dt-retorno.
        END.

    END.
        
    
END.

FOR EACH ttlanc
    WHERE ttlanc.tipo = '00r'
    AND   ttLanc.vlEntrada <> vlEntradaERP.
    DISP ttLanc WITH 1 COL 1 DOWN WIDTH 550.
END.
*/


/*FOR EACH ttlanc:
    DISP ttLanc WITH 1 COL 1 DOWN WIDTH 550.
END.*/
/*
FOR EACH ttProduto
    WHERE ttProduto.vlSaldoInf <> ttProduto.vlSaldoCalc .
    DISP ttproduto WITH 1 COL WIDTH 550.
END.
*/

/*FOR EACH ttProdRef
    WHERE ttProdRef.vlSaldoInf <> ttProdRef.vlSaldoCalc .
    DISP ttprodRef WITH 1 COL WIDTH 550.        
END.*/


/*FOR EACH ttProdRef:
    DISP ttProdRef WITH 1 COL WIDTH 550.
END.*/



{esp/exportarTabelacsv3.i ttProduto " " " " "ttProduto" }
{esp/exportarTabelacsv3.i ttProdRef " " " " "ttProdRef" }
{esp/exportarTabelacsv3.i ttLanc " " " " "ttLanc" }
