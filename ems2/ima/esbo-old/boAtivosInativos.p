{esbo/boAtivosInativos.i}
{esbo/esbo_fatur.i}
DEFINE VARIABLE iRepresIni  AS INTEGER     NO-UNDO INIT 0.
DEFINE VARIABLE iRepresFim  AS INTEGER     NO-UNDO INIT 999999.
DEFINE VARIABLE iSeqTT      AS INTEGER     NO-UNDO.
DEFINE VARIABLE dtRefer     AS DATE        NO-UNDO INIT TODAY.
DEFINE VARIABLE iSitAtuIni  AS INTEGER     NO-UNDO INIT 0.
DEFINE VARIABLE iSitAtuFim  AS INTEGER     NO-UNDO INIT 9999.

PROCEDURE setIntervalRepres:
   DEFINE INPUT  PARAMETER pRepresIni AS INTEGER     NO-UNDO.
   DEFINE INPUT  PARAMETER pRepresFim AS INTEGER     NO-UNDO.

   ASSIGN iRepresIni = pRepresIni 
          iRepresFim = pRepresFim.

END PROCEDURE.



PROCEDURE sincrRepresSitCli:

DEFINE INPUT  PARAMETER pDtRefer   AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER codRepres AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER codSitCli AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER qtSitCli  AS DECIMAL     NO-UNDO.

FIND FIRST repres_sit_cli
    WHERE repres_sit_cli.dt_referencia = pdtRefer
    AND   repres_sit_cli.cod_repres = codRepres
    AND   repres_sit_cli.cod_sit_cli = codSitCli
    EXCLUSIVE-LOCK NO-ERROR.
IF NOT AVAIL repres_sit_cli THEN DO:
    CREATE repres_sit_cli.
    ASSIGN repres_sit_cli.dt_referencia = pdtRefer
           repres_sit_cli.cod_repres = codRepres 
           repres_sit_cli.cod_sit_cli = codSitCli
           repres_sit_cli.repres_sit_cli_id = NEXT-VALUE(seq_repres_sit_cli).
END.
   ASSIGN repres_sit_cli.qt_sit_cli = qtSitCli. 




END PROCEDURE.

PROCEDURE calcInicioProxMes.


    DEFINE OUTPUT PARAMETER  dtInicioProxMes AS DATE        NO-UNDO.
    DEFINE VARIABLE iMes            AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iAno            AS INTEGER     NO-UNDO.
    ASSIGN iMes = MONTH(dtrefer) + 1
           iAno = YEAR(dtrefer).
    IF iMes = 13 THEN 
        ASSIGN iAno = iAno + 1
               iMes  = 1.
    
    ASSIGN dtInicioProxMes = DATE(iMes , 1, iAno).
    
END PROCEDURE.

PROCEDURE criarTT:

    DEFINE INPUT  PARAMETER pdtRefer   AS DATE       NO-UNDO.
    DEFINE INPUT  PARAMETER codRepres AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER codSitCli AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER qtSitCli  AS DECIMAL     NO-UNDO.


    FIND FIRST tt-represcli
        WHERE tt-represcli.dt_referencia = pdtRefer
        AND   tt-represcli.cod_repres = codRepres
        AND   tt-represcli.cod_sit_cli = codSitCli
        NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-represcli THEN DO:
        CREATE tt-represcli.
        ASSIGN tt-represcli.dt_referencia = pdtRefer
               tt-represcli.cod_repres = codRepres 
               tt-represcli.cod_sit_cli = codSitCli
               tt-represcli.repres_sit_cli_id = iSeqTT
               iSeqTT = iSeqTT + 1.
    END.
    ASSIGN tt-represcli.qt_sit_cli =  tt-represcli.qt_sit_cli  + qtSitCli. 



END PROCEDURE.



PROCEDURE  buscarClientesRepres.
    DEFINE VARIABLE qtNf AS INTEGER     NO-UNDO.
    DEFINE VARIABLE vlFat AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dtInicioProxMes AS DATE        NO-UNDO.
    DEFINE VARIABLE dtFimMesAtual   AS DATE        NO-UNDO.

    RUN  calcInicioProxMes(OUTPUT dtInicioProxMes).
    ASSIGN dtFimMesAtual = dtInicioProxMes - 1.
    /*MESSAGE dtFimMesAtual SKIP
            dtInicioProxMes
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

    EMPTY TEMP-TABLE tt-represcli.

    FOR EACH repres WHERE
             repres.ind-situacao = 1 AND
             repres.cod-rep >= iRepresIni AND
             repres.cod-rep <= iRepresFim:
        RUN buscarFaturamento(repres.cod-rep, OUTPUT qtNf, OUTPUT vlFat).
        RUN criarTt(dtRefer, repres.cod-rep, 98, qtNf).
        RUN criarTt(dtRefer, repres.cod-rep, 99, vlFat).

        FOR EACH emitente
            WHERE emitente.cod-rep =  repres.cod-rep 
            AND emitente.cod-rep <> 1 NO-LOCK.
            FIND FIRST ext-emitente 
             WHERE ext-emitente.cod-emitente =  emitente.cod-emitente NO-LOCK NO-ERROR.
            RUN criarTt(TODAY, repres.cod-rep, ext-emitente.situacao,1 ).  
        END.
        
    END.
    
    FOR EACH tt-represcli
        WHERE tt-RepresCli.cod_sit_cli >= iSitAtuIni
        AND   tt-RepresCli.cod_sit_cli <= iSitAtuFim :
       /* MESSAGE 'dt.refer:' tt-represcli.dt_referencia SKIP
                'dtfimmesatual:' dtFimMesAtual SKIP
                'dtinicioproxmes:' dtInicioProxMes SKIP
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.  */
        RUN sincrRepresSitCli(tt-represcli.dt_referencia, tt-represcli.cod_rep, tt-represcli.cod_sit_cli,tt-represcli.qt_sit_cli).
        /*RUN sincrRepresSitCli(dtFimMesAtual, tt-represcli.cod_rep, tt-represcli.cod_sit_cli,tt-represcli.qt_sit_cli).
        RUN sincrRepresSitCli(dtInicioProxMes, tt-represcli.cod_rep, tt-represcli.cod_sit_cli,tt-represcli.qt_sit_cli).*/
        /*MESSAGE 'dt.refer:' tt-represcli.dt_referencia SKIP
                'dt.fimmes atual:' dtFimMesAtual              SKIP
                'dtinicioproxmes:' dtInicioProxMes            SKIP
                'rep:' tt-represcli.cod_rep SKIP
                'sit.cli;' tt-RepresCli.cod_sit_cli SKIP
                'qt.' tt-represcli.qt_sit_cli SKIP

            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

    END.
    
    //copia quantidade do £ltimo dia para o ultimo dia do mes e para o primeiro dia do pr¢ximo mˆs.
    OUTPUT TO value('p:\LOG_sit_cli_ult_dia_mes' + STRING(TIME) + '.csv').
    PUT "data;repres;situacao;qt" SKIP.
    FOR EACH repres_sit_cli
        WHERE repres_sit_cli.dt_referencia > TODAY:
        DELETE repres_sit_cli.
    END.
    FOR EACH repres_sit_cli
    WHERE repres_sit_cli.dt_referencia = dtRefer NO-LOCK.
         RUN sincrRepresSitCli(dtFimMesAtual, repres_sit_cli.cod_rep, repres_sit_cli.cod_sit_cli,repres_sit_cli.qt_sit_cli).
         RUN sincrRepresSitCli(dtInicioProxMes, repres_sit_cli.cod_rep, repres_sit_cli.cod_sit_cli,repres_sit_cli.qt_sit_cli).
         PUT UNFORM dtFimMesAtual ';' repres_sit_cli.cod_rep ';'   repres_sit_cli.cod_sit_cli ';' repres_sit_cli.qt_sit_cli SKIP.
    END.
    OUTPUT CLOSE.


END PROCEDURE.

PROCEDURE setDtRefer:
    DEFINE INPUT  PARAMETER  pRefer AS DATE        NO-UNDO.
    ASSIGN dtRefer = pRefer.

END PROCEDURE.

PROCEDURE setInterValAtuSitCli:
    DEFINE INPUT  PARAMETER pSitAtuIni AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pSitAtuFim AS INTEGER     NO-UNDO.
    ASSIGN iSitAtuIni = pSitAtuIni
           iSitAtuFim = pSitAtuFim.


END PROCEDURE.

PROCEDURE buscarFaturamento.
          
          DEFINE INPUT  PARAMETER repres    AS INTEGER     NO-UNDO.
          DEFINE OUTPUT PARAMETER iCont     AS INTEGER     NO-UNDO.
          DEFINE OUTPUT PARAMETER dSoma     AS DECIMAL     NO-UNDO.
          DEFINE VARIABLE dtIni             AS DATE        NO-UNDO.
          DEFINE VARIABLE dtFim             AS DATE        NO-UNDO.
          DEFINE VARIABLE hBoFatur          AS HANDLE      NO-UNDO.
          RUN calcInicioProxMes(OUTPUT dtfim).
          ASSIGN dtfim = dtfim - 1.
          /*MESSAGE dtfim
              VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

          ASSIGN dtIni = DATE(MONTH(dtRefer),01,YEAR(dtRefer)).

          
          EMPTY TEMP-TABLE tt-fatur.
          RUN esbo/esbo_fatur.p PERSISTENT SET hBoFatur.
          RUN iniciarBos IN hBoFatur.
          RUN setIntervalDtEmisNota IN hBoFatur (dtIni,DtFim).
          RUN setIntervalRepres IN hBoFatur (INPUT repres, INPUT repres).
          RUN buscarFaturados IN hBoFatur.
          RUN buscarDevolucao IN hBoFatur.
          RUN retornarTtFatur IN hBoFatur(OUTPUT TABLE tt-fatur).
          OUTPUT TO value('p:\log_boAtivosInativos\tt-fatur_' + STRING(repres) + "_" + STRING(time) + '.CSV') NO-CONVERT.
          PUT "data de referencia:" dtRefer SKIP.
          PUT "Data;NF;ITEM;Desc;Vl.ITem;Acumulado" SKIP.
          FOR EACH tt-fatur
              WHERE LOOKUP(tt-fatur.tipo, 'fat,dev') > 0
              BREAK BY tt-fatur.nr_nf. 
              
              IF FIRST-OF (tt-fatur.nr_nf) THEN DO:
                 IF tt-fatur.tipo = "fat" THEN
                    ASSIGN iCont = iCont + 1.
              END.
               
              ASSIGN dSoma = dSoma + tt-fatur.vl_tot_it_nf.

              EXPORT DELIMITER ";" tt-fatur.dt_emis  
                   tt-fatur.nr_nf     COLUMN-LABEL 'NOTA'
                   tt-fatur.cod_item  COLUMN-LABEL 'ITEM'
                   tt-fatur.DESC_item COLUMN-LABEL 'descricao'
                   tt-fatur.vl_tot_it_nf  COLUMN-LABEL 'Vl.Item' FORMAT '->>>,>>9.99'
                   dSoma COLUMN-LABEL 'Acumulado' FORMAT '->>>,>>9.99' .

              /*IF tt-fatur.tipo = "fat" THEN
                  ASSIGN dSoma = dSoma + tt-fatur.vl_tot_it_nf.
              IF tt-fatur.tipo = "dev" THEN
                 dSoma = dSoma - tt-fatur.vl_tot_it_nf.*/
          END.
          OUTPUT CLOSE.
          RUN finalizarBos IN hBoFatur.

END PROCEDURE.


