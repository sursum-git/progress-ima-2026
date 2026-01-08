{esp/util.i}
DEFINE TEMP-TABLE tt
    FIELD itCodigo      AS CHAR FORMAT 'x(15)'
    FIELD codRefer      AS CHAR
    FIELD nota          AS CHAR FORMAT 'x(15)'
    FIELD quantidade    AS DECIMAL FORMAT '->>>,>>>>,>>9.99'
    FIELD qtERP         AS DECIMAL FORMAT '->>>,>>>>,>>9.99'
    FIELD qtBaixada     AS DECIMAL FORMAT '->>>,>>>>,>>9.99'
    FIELD id            AS INT
    FIELD qtRetorno     AS INT
    FIELD dtUltBaixa    AS DATE
    INDEX ind-pri IS PRIMARY itCodigo codRefer nota.
DEFINE TEMP-TABLE ttDesconsiderar
    FIELD nota AS CHAR
    INDEX ind-pri IS primary nota.
DEFINE VARIABLE iCont           AS INTEGER     NO-UNDO.
DEFINE VARIABLE dQtBaixada      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dSaldoaBaixar   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dPossivel       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE h-acomp         AS HANDLE      NO-UNDO.
DEFINE VARIABLE qtRetornos      AS INTEGER     NO-UNDO.

DEFINE TEMP-TABLE ttComp
    FIELD rowidComp     AS CHAR
    FIELD itCodigo      AS CHAR
    FIELD codRefer      AS CHAR
    FIELD notaRemessa   AS CHAR
    FIELD notaRetorno   AS CHAR
    FIELD qtOriginal AS DECIMAL
    FIELD qtNova     AS DECIMAL
    FIELD idSaldo    AS INT
    INDEX ind-pri idSaldo rowidComp.


INPUT FROM c:\temp\lanctos_ajuste_terc.csv.
    REPEAT:
        ASSIGN iCont = iCont + 1.
        CREATE tt.
        IMPORT DELIMITER ";" tt NO-ERROR.
        ASSIGN tt.id = iCont.
        

    END.
INPUT CLOSE.

/*FOR EACH tt:
    DISP tt WITH WIDTH 550.
END.*/

RUN utp/ut-acomp.p PERSIST SET  h-acomp.
RUN pi-inicializar IN h-acomp('Ajuste Saldo Terceiros').

//limpar lixos da tabela temporaria
/*FOR EACH tt
    WHERE tt.itCodigo  = 'produto' OR
          tt.itCodigo =  '' OR tt.quantidade = 0 .
    DISP tt WITH WIDTH 550 .
    DELETE tt.

END.*/


//retira os saldos de terceiros que n∆o s∆o 
FOR EACH tt:
    FIND LAST saldo-terc NO-LOCK
        WHERE saldo-terc.cod-estabel  = '505'
        //AND   saldo-terc.cod-emitente = 38284
        AND   saldo-terc.it-codigo    = tt.itCodigo
        AND   saldo-terc.cod-refer    = tt.codRefer
        AND   int(saldo-terc.nro-docto)    = int(tt.nota)
        NO-ERROR.
    IF AVAIL saldo-terc THEN DO:
       IF saldo-terc.nat-operacao <> '59207i' THEN  DO:
          CREATE ttDesconsiderar.
          ASSIGN ttDesconsiderar.nota = saldo-terc.nro-docto.
          DELETE tt.
       END.
          
          
    END.
END.


RUN pi-acompanhar IN h-acomp('corrigindo referencias').
//corrigi referencias
FOR EACH tt
    WHERE tt.quantidade <> 0:
    IF length(tt.codRefer) < 3 THEN DO:
       ASSIGN tt.codRefer = FILL('0',3 - length(tt.codRefer) ) + tt.CodRefer.
    END.
END.
OUTPUT TO c:\temp\LOG_movto.txt.

//tratando apenas saldos positivos
FOR EACH tt
    WHERE tt.quantidade > 0:
    PUT UNFORM "==========================================================================================" SKIP.
    PUT UNFORM "item:" tt.itCodigo " - ref:" tt.codRefer "- nota:" tt.nota "- qt:" tt.quantidade SKIP. 
    RUN pi-acompanhar IN h-acomp('POSITIVO - item:' + tt.itCodigo + '-ref:' + tt.codRefer + '-nota:' + tt.nota).
    FIND LAST saldo-terc NO-LOCK
        WHERE saldo-terc.cod-estabel  = '505'
        //AND   saldo-terc.cod-emitente = 38284
        AND   saldo-terc.it-codigo          = tt.itCodigo
        AND   saldo-terc.cod-refer          = tt.codRefer
        AND   int(saldo-terc.nro-docto)     = int(tt.nota)
        AND   INT(saldo-terc.nro-docto)     < 4053
        NO-ERROR.
    IF AVAIL saldo-terc THEN DO:
       RUN getNumRetsPorRemessa(ROWID(saldo-terc), OUTPUT qtRetornos).
       ASSIGN tt.qtRetorno = qtRetornos.
       PUT UNFORM "saldo de terceiros encontrado.natureza:" + saldo-terc.nat-operacao + " - cod.emitente: " + string(saldo-terc.cod-emitente) + ' - qt.retornos:' + string(qtRetornos) SKIP.
       IF qtRetornos = 0 THEN DO:
          NEXT.
       END.
       comp:
       FOR EACH componente NO-LOCK 
           WHERE componente.serie-comp      = saldo-terc.serie-docto
           AND   componente.nro-comp        = saldo-terc.nro-docto
           AND   componente.nat-comp        = saldo-terc.nat-operacao
           AND   componente.it-codigo       = saldo-terc.it-codigo
           AND   componente.cod-refer       = saldo-terc.cod-refer
           AND   componente.seq-comp        = saldo-terc.sequencia
           AND   componente.cod-emitente    = saldo-terc.cod-emitente
           AND   componente.componente      = 2
           USE-INDEX retorno BY componente.dt-retorno DESC .
           PUT UNFORM "componente - dt.retorno:" componente.dt-retorno.
           PUT UNFORM " - doc.retorno:" componente.nro-docto.
           PUT UNFORM " - serie retorno:" componente.serie-docto.
           PUT UNFORM " - qte:" componente.quantidade.
           CREATE ttComp.
           ASSIGN ttComp.rowidComp      = string(ROWID(componente))
                  ttComp.qtOriginal     = componente.quantidade
                  ttComp.itcodigo       = componente.it-codigo
                  ttComp.codRefer       = componente.cod-Refer
                  ttcomp.notaRemessa    = componente.nro-comp
                  ttcomp.notaRetorno    = componente.nro-docto
                  ttComp.idSaldo        = tt.id.
           ASSIGN tt.qtErp =  tt.qtErp + ttComp.qtOriginal.
           PUT UNFORM "qte maior que zero, vou acrescentar e sair do foreach" SKIP.
           ASSIGN ttComp.qtNova = ttComp.QtOriginal + tt.quantidade
                  tt.QtBaixada  = tt.quantidade
                  tt.dtultBaixa = componente.dt-retorno.
           LEAVE comp.
       END.
    END.
END.


//tratar apenas negativos
FOR EACH tt
    WHERE tt.quantidade < 0
   /*AND int(tt.nota) = 1893
    AND tt.itCodigo = '170163'
    AND tt.codRefer = '9'*/
    :
    PUT UNFORM "==========================================================================================" SKIP.
    PUT UNFORM "item:" tt.itCodigo " - ref:" tt.codRefer "- nota:" tt.nota "- qt:" tt.quantidade SKIP. 
    /*MESSAGE 'oi'
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    RUN pi-acompanhar IN h-acomp('item:' + tt.itCodigo + '-ref:' + tt.codRefer + '-nota:' + tt.nota).
    ASSIGN dQtbaixada = 0.
    FIND LAST saldo-terc NO-LOCK
        WHERE saldo-terc.cod-estabel  = '505'
        //AND   saldo-terc.cod-emitente = 38284
        AND   saldo-terc.it-codigo          = tt.itCodigo
        AND   saldo-terc.cod-refer          = tt.codRefer
        AND   int(saldo-terc.nro-docto)     = int(tt.nota)
        AND   INT(saldo-terc.nro-docto)     < 4053
        NO-ERROR.
    IF AVAIL saldo-terc THEN DO:
       RUN getNumRetsPorRemessa(ROWID(saldo-terc), OUTPUT qtRetornos).
       ASSIGN tt.qtRetorno = qtRetornos.
       PUT UNFORM "saldo de terceiros encontrado.natureza:" + saldo-terc.nat-operacao + " - cod.emitente: " + string(saldo-terc.cod-emitente) + ' - qt.retornos:' + string(qtRetornos) SKIP.
       IF qtRetornos = 0 THEN DO:
          NEXT.
       END.
       comp:
       FOR EACH componente NO-LOCK 
           WHERE componente.serie-comp      = saldo-terc.serie-docto
           AND   componente.nro-comp        = saldo-terc.nro-docto
           AND   componente.nat-comp        = saldo-terc.nat-operacao
           AND   componente.it-codigo       = saldo-terc.it-codigo
           AND   componente.cod-refer       = saldo-terc.cod-refer
           AND   componente.seq-comp        = saldo-terc.sequencia
           AND   componente.cod-emitente    = saldo-terc.cod-emitente
           AND   componente.componente      = 2
           USE-INDEX retorno BY componente.dt-retorno DESC .
           PUT UNFORM "componente - dt.retorno:" componente.dt-retorno.
           PUT UNFORM " - doc.retorno:" componente.nro-docto.
           PUT UNFORM " - serie retorno:" componente.serie-docto.
           PUT UNFORM " - qte:" componente.quantidade.
           CREATE ttComp.
           ASSIGN ttComp.rowidComp      = string(ROWID(componente))
                  ttComp.qtOriginal     = componente.quantidade
                  ttComp.itcodigo       = componente.it-codigo
                  ttComp.codRefer       = componente.cod-Refer
                  ttcomp.notaRemessa    = componente.nro-comp
                  ttcomp.notaRetorno    = componente.nro-docto
                  ttComp.idSaldo        = tt.id.
           ASSIGN tt.qtErp =  tt.qtErp + ttComp.qtOriginal.
           
           PUT UNFORM SKIP "qte MENOR que ZERO" SKIP.
           ASSIGN dSaldoABaixar = tt.quantidade + dQtBaixada.
           IF ABS(dSaldoABaixar)  < ttComp.qtOriginal   THEN DO:
              PUT UNFORM  "qte original:"   + STRING(tt.quantidade) + "- qte.j† baixada:"+ STRING(dQtbaixada) +  "saldo a baixar:"  
                  + STRING(dsaldoAbaixar) + " menor que a quantidade da nota de retorno:" + STRING(ttComp.qtOriginal)  SKIP.

              ASSIGN ttComp.qtNova = ttComp.QtOriginal + dSaldoAbaixar
                     dQtBaixada = dQtBaixada + abs(dSaldoABaixar).
                     .        
              PUT UNFORM "ponto 1 - data ult baixa:" tt.dtultbaixa "-dt.retorno comp.:" componente.dt-retorno SKIP.
              IF tt.dtultBaixa >  componente.dt-retorno OR tt.dtultBaixa = ? THEN
                 ASSIGN tt.dtultBaixa  = componente.dt-retorno.
              LEAVE comp.
           END.
           ELSE DO:
              PUT UNFORM  "qte original:"   + STRING(tt.quantidade) + "- qte.j† baixada:"+ STRING(dQtbaixada) +  "saldo a baixar:"  
                  + STRING(dSaldoaBaixar) + " MAIOR que a quantidade da nota de retorno:" + STRING(ttComp.qtOriginal)  SKIP.

              ASSIGN  ttComp.qtNova = 0
                      dQtBaixada =  dQtBaixada + ttComp.qtOriginal 
                      .

              PUT UNFORM  "quantidade do retorno setada como 0 e total de quantidade baixada igual a: " STRING(dQtBaixada)  SKIP.
              PUT UNFORM "ponto 2 - data ult baixa:" tt.dtultbaixa "-dt.retorno comp.:" componente.dt-retorno SKIP.
              IF tt.dtultBaixa >  componente.dt-retorno  OR tt.dtultBaixa = ? THEN
                 ASSIGN tt.dtultBaixa  = componente.dt-retorno.
           END. 
       END.
    END.
    ASSIGN tt.qtBaixada = dQtBaixada.

END.
OUTPUT CLOSE.

{esp/exportarTabelaCsv3.i  tt " " " " "ttNovoSaldo"}
{esp/exportarTabelaCsv3.i  ttComp " " " " "ttComp"}
{esp/exportarTabelaCsv3.i  ttDesconsiderar " " " " "ttDesconsiderar"}
RUN pi-finalizar IN h-acomp.

PROCEDURE getNumRetsPorRemessa:

    DEFINE INPUT  PARAMETER pRowid AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER icont  AS INTEGER     NO-UNDO.
    DEFINE BUFFER bf     FOR saldo-terc.
    DEFINE BUFFER bfComp FOR componente.
    FIND bf NO-LOCK
        WHERE rowid(bf)  = pRowid NO-ERROR.
    IF AVAIL bf THEN DO:
       
       FOR EACH bfComp 
           WHERE bfComp.serie-comp      = bf.serie-docto
           AND   bfComp.nro-comp        = bf.nro-docto
           AND   bfComp.nat-comp        = bf.nat-operacao
           AND   bfComp.it-codigo       = bf.it-codigo
           AND   bfComp.cod-refer       = bf.cod-refer
           AND   bfComp.seq-comp        = bf.sequencia
           AND   bfComp.cod-emitente    = bf.cod-emitente
           AND   bfComp.componente      = 2 // retorno
           NO-LOCK : 
           ASSIGN iCont = iCont + 1.
       END.
    END.
/*
    MESSAGE  'serie:' bf.serie-docto     SKIP 
                'docto:' bf.nro-docto       SKIP
                'nat.oper.:' bf.nat-operacao    SKIP
                'it.codigo:' bf.it-codigo       SKIP
                'refer.:' bf.cod-refer       SKIP
                'seq.' bf.sequencia       SKIP
                'emit.:' bf.cod-emitente   SKIP
                'qt.retornos:' iCont 
           VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
*/

END PROCEDURE.


