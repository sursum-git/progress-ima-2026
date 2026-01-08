
/*
16/02/2022 - tadeu -  acrecimo de possibilitade de desconsiderar conta corrente
*/
{esbo\bo_ctbl_cx.i}
{utp/ut-glob.i}

DEFINE VARIABLE cbaseOutra          AS CHARACTER   NO-UNDO FORMAT 'x(15)' .
DEFINE VARIABLE cbaseAtual          AS CHARACTER   NO-UNDO FORMAT 'x(15)' .
DEFINE VARIABLE cempresaAtual       AS CHARACTER   NO-UNDO INIT '5'.
DEFINE VARIABLE arquivoSaida        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAmbiente           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cConexao            AS CHARACTER   NO-UNDO FORMAT 'x(200)'.
DEFINE VARIABLE digitoAmbiente      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE servidorAmbiente    AS CHARACTER   NO-UNDO FORMAT 'x(50)'.
DEFINE VARIABLE dbems5              AS CHARACTER   NO-UNDO FORMAT 'x(10)'.
DEFINE VARIABLE cBaseDesconectar    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPortaDesconectar   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE lMensagem AS LOGICAL     NO-UNDO INIT NO.

DEFINE VARIABLE dValor              AS DECIMAL     NO-UNDO.
DEFINE VARIABLE valorInd            AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dIndice             AS DECIMAL     NO-UNDO.

DEFINE VARIABLE hLog                AS HANDLE      NO-UNDO.
DEFINE VARIABLE i                   AS INTEGER     NO-UNDO.


DEFINE VARIABLE iCont               AS INTEGER     NO-UNDO.
DEFINE VARIABLE cContaContabil      AS CHARACTER   NO-UNDO FORMAT 'x(30)'.
DEFINE VARIABLE cListaTpFluxoDescon AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cNivel AS CHARACTER   NO-UNDO INIT 'ctbl'.

DEFINE BUFFER bf_movto_tit_ap FOR movto_tit_ap.

DEFINE VARIABLE dat_transacao_ini AS DATE        NO-UNDO.
DEFINE VARIABLE dat_transacao_fim AS DATE        NO-UNDO.

DEFINE VARIABLE cEstabIni AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEstabFim AS CHARACTER   NO-UNDO INIT 'zzzz'.

DEFINE VARIABLE ctaCorrenIni AS CHARACTER   NO-UNDO INIT ''.
DEFINE VARIABLE ctaCorrenFim AS CHARACTER   NO-UNDO INIT 'zzzzzzzzzz'.

DEFINE VARIABLE iSeqIni AS INTEGER     NO-UNDO INIT 0.
DEFINE VARIABLE iSeqFim AS INTEGER     NO-UNDO INIT 99999.

DEFINE TEMP-TABLE ttCtaCorrenDescon
    FIELD ctaCorren AS CHAR FORMAT 'X(50)' .


PROCEDURE setIntervalEstab:

    DEFINE INPUT  PARAMETER pEstabIni AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pEstabFim AS CHARACTER   NO-UNDO.

    ASSIGN cEstabIni    = pEstabIni
           cEstabFim    = pEstabFim .


END PROCEDURE.


PROCEDURE setDataTransacao:

DEFINE INPUT  PARAMETER dtIni AS DATE        NO-UNDO.
DEFINE INPUT  PARAMETER dtFim AS DATE        NO-UNDO.

ASSIGN dat_transacao_ini = dtIni
       dat_transacao_fim = dtFim.


END PROCEDURE.

PROCEDURE setlistaTpFluxoDescons:
    DEFINE INPUT  PARAMETER pLista AS CHARACTER   NO-UNDO.
    ASSIGN cListaTpFluxoDescon = pLista.
END PROCEDURE.

PROCEDURE setNivel:
    DEFINE INPUT  PARAMETER pNivel AS CHARACTER   NO-UNDO.
    ASSIGN cNivel = pNivel.



END PROCEDURE.

PROCEDURE setCtaCorren:
    DEFINE INPUT  PARAMETER pConta AS CHARACTER   NO-UNDO.
    ASSIGN ctaCorrenIni = pConta
           ctaCorrenFim = pConta .

END PROCEDURE.

PROCEDURE setCtaCorrenDesconsiderar:
    DEFINE INPUT  PARAMETER pCta AS CHARACTER   NO-UNDO.

    CREATE ttCtaCorrenDescon.
    ASSIGN ttCtaCorrenDescon.ctaCorren = pCta .




END PROCEDURE.

PROCEDURE setSeq:
    DEFINE INPUT  PARAMETER pSeq AS INTEGER     NO-UNDO.
    ASSIGN iSeqIni = pSeq
           iSeqFim = pSeq .

END PROCEDURE.


PROCEDURE getSinalCtaCorrente:

DEFINE OUTPUT PARAMETER iSinal AS INTEGER     NO-UNDO.
ASSIGN iSinal = IF movto_cta_corren.ind_fluxo_movto_cta_corren = 'sai' THEN
                   -1 ELSE 1 . 

END.

PROCEDURE buscarRegistros:
   
DEFINE VARIABLE LOG_achou AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iSinal    AS INTEGER     NO-UNDO.


/*MESSAGE 'dt.ini:' dat_transacao_ini SKIP
        'dt.fim:' dat_transacao_fim SKIP
        'estab.ini.' cEstabIni      SKIP
        'estab.fim.' cEstabFim      SKIP

    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/



OUTPUT TO c:\temp\movto_cta_corren.txt.
ASSIGN lMensagem = no. //c-seg-usuario = 'super' .
/*MESSAGE cestabini SKIP
        cestabfim
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
IF lMensagem THEN
   MESSAGE 'oi'
       VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
FOR EACH  movto_cta_corren 
    WHERE movto_cta_corren.dat_transacao >= dat_transacao_ini
    AND   movto_cta_corren.dat_transacao <= dat_transacao_fim
    AND   movto_cta_corren.cod_cta_corren >= ctaCorrenIni
    AND   movto_cta_corren.cod_cta_corren <= ctaCorrenFim
    AND   movto_cta_corren.num_seq_movto_cta_corren >= iSeqIni
    AND   movto_cta_corren.num_seq_movto_cta_corren <= iSeqFim
    AND   ind_tip_movto_cta_corren = 're' NO-LOCK: 
    FIND FIRST cta_corren OF movto_cta_corren
        WHERE cta_corren.cod_estab >= cEstabIni
        AND   cta_corren.cod_estab <= cEstabFim NO-LOCK NO-ERROR.
    IF NOT AVAIL cta_Corren  THEN NEXT.
    FIND ttCtaCorrenDescon
        WHERE ttCtaCorrenDescon.ctaCorren =  cta_corren.cod_cta_corren
        NO-LOCK NO-ERROR.
    IF AVAIL ttCtaCorrenDescon THEN NEXT.

    FIND FIRST estabelecimento  WHERE
        estabelecimento.cod_estab =  cta_corren.cod_estab NO-LOCK NO-ERROR.


    //IF movto_cta_corren.cod_estab <> '' AND   movto_cta_corren.cod_estab >=  cEstabIni AND  movto_cta_corren.cod_estab <= cEstabFim THEN NEXT.


    /*MESSAGE  'cta.corre:.'  movto_cta_corren.cod_cta_corren SKIP
             'seq:' movto_cta_corren.num_seq_movto_cta_corren SKIP
            'cod_mod.' movto_cta_corren.cod_modul_dtsul
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/


   /* IF movto_cta_corren.cod_cta_corren = '5178-0'  THEN DO:
      MESSAGE 'seq:' movto_cta_corren.num_seq_movto_cta_corren SKIP
            'cod_mod.' movto_cta_corren.cod_modul_dtsul
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

    END.*/


    /*IF movto_cta_corren.cod_cta_corren = '5178-0' 
      AND  movto_cta_corren.dat_transacao = 05.04.2021 
      AND movto_cta_corren.num_seq_movto_cta_corren = 42 
      THEN
      ASSIGN lMensagem = YES.
    ELSE
      ASSIGN lMensagem = NO.*/
    
    //FIND FIRST cta_corren OF movto_cta_corren NO-LOCK NO-ERROR.
    
    IF movto_cta_corren.cod_tip_trans_cx <> movto_cta_corren.cod_modul_dtsul 
        AND movto_cta_corren.cod_modul_dtsul <> 'cmg' THEN DO:
        /*MESSAGE 'entrei na condicao'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
        RUN buscarCtblPadraoTransCaixa(ROWID(movto_cta_corren),OUTPUT LOG_achou).
    END.
    ELSE DO:
        CASE movto_cta_corren.cod_modul_dtsul:
            WHEN 'acr' THEN DO:
                RUN buscarCtblACR(ROWID(movto_cta_corren), OUTPUT LOG_achou).
            END.               
            WHEN 'apb' THEN
                RUN buscarCtblAPB(ROWID(movto_cta_corren),OUTPUT LOG_achou).
            WHEN 'cmg' THEN
                RUN buscarCtblCMG(ROWID(movto_cta_corren),OUTPUT LOG_achou).
        END CASE.  
    END.
    EXPORT DELIMITER "|"
    movto_cta_corren.cod_modul_dtsul                                                           
    movto_cta_corren.cod_estab                                                                     
    movto_cta_corren.dat_transacao                                                             
    movto_cta_corren.cod_tip_trans_cx                                                          
    movto_cta_corren.val_movto_cta_corren                                                      
    movto_cta_corren.cod_cta_corren                                                            
    replace(replace(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),CHR(10),";")     
    movto_cta_corren.num_id_movto_cta_corren                                                   
    movto_cta_corren.num_seq_movto_cta_corren SKIP.

    IF LOG_achou = NO THEN DO:
       CREATE ttSemCtbl.
       ASSIGN ttSemCtbl.origem            =   movto_cta_corren.cod_modul_dtsul  
              ttSemCtbl.cod_estab         =   movto_cta_corren.cod_estab
              ttSemCtbl.data              =   movto_cta_corren.dat_transacao
              ttSemCtbl.nro_docto         =   movto_cta_corren.cod_tip_trans_cx
              ttSemCtbl.valor             =   movto_cta_corren.val_movto_cta_corren
              ttSemCtbl.conta_corrente    =   movto_cta_corren.cod_cta_corren 
              ttSemCtbl.historico         =   replace(replace(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),CHR(10),";") 
              ttSemCtbl.id_movto_corren   =   movto_cta_corren.num_id_movto_cta_corren
              ttSemCtbl.sequencia         =   movto_cta_corren.num_seq_movto_cta_corren.
    END.
END.
OUTPUT CLOSE.
RUN preencherCtas.
RUN exportarCtasDesconsideradas.


END PROCEDURE.


PROCEDURE buscarCtblPadraoTransCaixa:

    DEFINE INPUT  PARAMETER rRowid      AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER lAchou      AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE iSinal AS INTEGER     NO-UNDO.
    FIND movto_cta_corren NO-LOCK
        WHERE rowid(movto_cta_corren) = rRowid NO-ERROR.
    RUN getSinalCtaCorrente(OUTPUT iSinal).
    IF AVAIL movto_cta_corren THEN DO:
       IF cNivel = 'fluxo' THEN DO:
           FIND tip_trans_cx OF movto_cta_corren NO-LOCK NO-ERROR.
           FIND tip_fluxo_financ
               WHERE tip_fluxo_financ.cod_tip_fluxo_financ = tip_trans_cx.cod_tip_fluxo_financ_saida
               NO-LOCK NO-ERROR.
           RUN criarTTFluxo(movto_cta_corren.cod_modul_dtsul,            
                estabelecimento.cod_empresa,
                cta_corren.cod_estab,
                movto_cta_corren.cod_tip_trans_cx ,
                tip_trans_cx.des_tip_trans_cx, 
                movto_cta_corren.dat_movto_cta_corren,
                cod_docto_movto_cta_bco,
                '',             
                '',           
                '',           
                movto_cta_corren.val_movto_cta_corren * iSinal,            
                tip_trans_cx.cod_tip_fluxo_financ_saida ,      
                tip_fluxo_financ.des_tip_fluxo_financ ,      
                '',
                            //tit_acr.des_observacao,         
                movto_cta_corren.dat_transacao ,             
                movto_cta_corren.num_seq_movto_cta_corren,         
                movto_cta_corren.cod_cta_corren,    
                movto_cta_corren.num_id_movto_cta_corren,
                movto_cta_corren.dat_transacao
                   ).
       END.                                             
       /*nivel igual a contabilizaá∆o n∆o implementado*/
    END.
    ELSE DO:
       ASSIGN lAchou = NO.
    END.
    

END PROCEDURE.

PROCEDURE exportarCtasDesconsideradas.
OUTPUT TO c:\temp\cta_desconsideradas.txt.
PUT "conta Corrente Desconsiderada" SKIP.
FOR EACH ttCtaCorrenDescon :
    PUT UNFORM ttCtaCorrenDescon.ctaCorren SKIP.
END.


OUTPUT CLOSE.

END PROCEDURE.

PROCEDURE retornarLancsSemCtbl:
                              
DEFINE OUTPUT PARAMETER TABLE FOR ttSemCtbl.

END PROCEDURE.

PROCEDURE exportarLancsSemCtbl:

DEFINE INPUT  PARAMETER pArquivo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE logBranco AS LOGICAL     NO-UNDO INIT YES.

OUTPUT TO VALUE(pArquivo).
FOR EACH ttSemCtbl:
    ASSIGN logBranco = NO.
    EXPORT DELIMITER "|" ttSemCtbl.
END.
IF logBranco THEN
   PUT "Todos os Lanáamentos foram Contabilizados corretamente|||||||||" SKIP.
OUTPUT CLOSE.

END PROCEDURE.

PROCEDURE buscarCtblCMG:

DEFINE INPUT  PARAMETER rMovtoCtaCorren AS ROWID  NO-UNDO.
DEFINE OUTPUT PARAMETER LOG_achou       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cOrigem                 AS CHARACTER   NO-UNDO INIT 'Caixa e Bancos'.
DEFINE VARIABLE iSinal                  AS INTEGER     NO-UNDO.
FIND movto_cta_corren
     WHERE ROWID(movto_cta_corren) = rMovtoCtaCorren
     NO-LOCK NO-ERROR.
RUN getSinalCtaCorrente(OUTPUT iSinal).


ASSIGN LOG_achou  = NO.

/*FIND FIRST cta_corren OF movto_cta_corren
    NO-LOCK NO-ERROR.
FIND FIRST estabelecimento 
    WHERE estabelecimento.cod_estab =  cta_corren.cod_Estab
    NO-LOCK NO-ERROR.
  */


IF cNivel = 'fluxo' THEN DO:
   FIND first tip_trans_cx OF movto_cta_corren NO-LOCK NO-ERROR.
       
   FOR EACH rat_financ_cmg OF movto_cta_corren :
       FIND tip_fluxo_financ OF rat_financ_cmg NO-LOCK NO-ERROR.
       IF LOOKUP(rat_financ_cmg.cod_tip_fluxo_financ ,cListaTpFluxoDescon) > 0 THEN NEXT.      
       ASSIGN LOG_achou = YES.
       RUN criarTTFluxo(cOrigem,            
           estabelecimento.cod_empresa,
           cta_corren.cod_estab,
           movto_cta_corren.cod_tip_trans_cx ,
           tip_trans_cx.des_tip_trans_cx, 
           movto_cta_corren.dat_movto_cta_corren,
           cod_docto_movto_cta_bco,
           '',             
           '',           
           '',           
           rat_financ_cmg.val_movto_cta_corren * iSinal,            
           rat_financ_cmg.cod_tip_fluxo_financ ,      
           tip_fluxo_financ.des_tip_fluxo_financ ,      
           tit_acr.des_observacao,         
           movto_cta_corren.dat_transacao ,             
           movto_cta_corren.num_seq_movto_cta_corren,         
           movto_cta_corren.cod_cta_corren,    
           movto_cta_corren.num_id_movto_cta_corren,
           movto_cta_corren.dat_transacao ).

   END.
END.



IF cNivel = 'ctbl' THEN DO:
   FOR EACH aprop_ctbl_cmg OF movto_cta_corren
    /*WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_cmg.ind_natur_lancto_ctbl = 'cr')
    OR  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'sai' AND aprop_ctbl_cmg.ind_natur_lancto_ctbl = 'db') NO-LOCK */ :
       ASSIGN LOG_achou = YES.
       RUN criarTT(aprop_ctbl_cmg.ind_natur_lancto_ctbl,
                   aprop_ctbl_cmg.cod_empresa,aprop_ctbl_cmg.cod_estab,
                   movto_cta_corren.cod_tip_trans_cx,
                   IF AVAIL tip_trans_cx THEN tip_trans_cx.des_tip_trans_cx  ELSE '',
                   movto_cta_corren.dat_transacao,
                   aprop_ctbl_cmg.cod_cta_ctbl,
                   aprop_ctbl_cmg.val_movto_cta_corren,
                   movto_cta_corren.cod_cta_corren,
                   cOrigem,
                   movto_cta_corren.cod_modul_dtsul,
                   movto_cta_corren.ind_fluxo_movto_cta_corren,
                   aprop_ctbl_cmg.cod_ccusto,                                                               
                   cod_docto_movto_cta_bco,
                   '',
                   '',
                   '',
                   '',
                   movto_cta_corren.num_seq_movto_cta_corren,
                   movto_cta_corren.num_id_movto_cta_corren,
                   '',                                                                                    
                   replace(replace(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),CHR(10),";"),
                   ''
                 ) .
    END.
END.    

END PROCEDURE.



PROCEDURE buscarCtblACR:

DEFINE INPUT  PARAMETER rMovtoCtaCorren AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER LOG_achou       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cOrigem AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iSinal  AS INTEGER     NO-UNDO.
DEFINE VARIABLE lRateio AS LOGICAL     NO-UNDO.
ASSIGN cOrigem = 'Contas a Receber'.
FIND movto_cta_corren
     WHERE ROWID(movto_cta_corren) = rMovtoCtaCorren
     NO-LOCK NO-ERROR.
RUN getSinalCtaCorrente(OUTPUT iSinal).
ASSIGN LOG_achou = NO.

FOR EACH movto_tit_acr OF movto_cta_corren NO-LOCK:
    FIND FIRST tit_acr OF movto_tit_acr NO-LOCK NO-ERROR.                                                                                                               
    FIND FIRST emitente WHERE                                                                                                                                           
         tit_acr.cdn_cliente =  emitente.cod-emitente NO-LOCK NO-ERROR. 
    FIND FIRST ext-emitente OF emitente
        NO-LOCK NO-ERROR.
    IF AVAIL ext-emitente THEN
       FIND FIRST ramo-ativ
            WHERE ramo-ativ.cod-ramo-ativ = ext-emitente.cod-ramo-ativ NO-LOCK NO-ERROR.

    /*FIND histor_movto_tit_acr 
        WHERE histor_movto_tit_acr.num_id_movto_tit_acr = movto_tit_acr.num_id_movto_tit_acr NO-LOCK NO-ERROR.*/
    /*Fluxo de Caixa*/

    IF cNivel = 'fluxo' THEN DO:
       ASSIGN lRateio = NO.
       FOR EACH rat_movto_tit_acr OF movto_tit_acr NO-LOCK.
           ASSIGN lRateio = YES.
           FIND FIRST tip_fluxo_financ OF rat_movto_tit_acr NO-LOCK NO-ERROR.
           RUN criarTTFluxo(cOrigem,            
                            tit_acr.cod_empresa,
                            tit_acr.cod_estab,
                            IF AVAIL tit_acr THEN string(tit_acr.cdn_cliente) ELSE  '0'  ,
                            IF AVAIL emitente THEN emitente.nome-emit ELSE '', 
                            movto_cta_corren.dat_movto_cta_corren,
                            tit_acr.cod_tit_acr,
                            tit_acr.cod_ser ,             
                            tit_acr.cod_parcela,           
                            tit_acr.cod_espec_docto,           
                           /* (val_movto_tit_acr.val_liquidac_tit_acr + val_movto_tit_acr.val_juros 
                             + val_movto_tit_acr.val_multa_tit_acr - val_movto_tit_acr.val_abat_tit_acr 
                             - val_movto_tit_acr.val_desconto ) * iSinal, */
                            rat_movto_tit_acr.val_aprop_ctbl * iSinal ,
                            rat_movto_tit_acr.cod_tip_fluxo_financ ,      
                            tip_fluxo_financ.des_tip_fluxo_financ ,      
                           '',// IF AVAIL histor_movto_tit_acr THEN histor_movto_tit_acr.des_text_histor ELSE '',         
                            movto_cta_corren.dat_transacao ,             
                            movto_cta_corren.num_seq_movto_cta_corren,         
                            movto_cta_corren.cod_cta_corren,    
                            movto_cta_corren.num_id_movto_cta_corren,
                            tit_acr.dat_emis ).

       END.
       IF lRateio = NO THEN DO:
          FOR EACH val_movto_tit_acr OF movto_tit_acr NO-LOCK .
               FIND FIRST tip_fluxo_financ OF val_movto_tit_acr NO-LOCK NO-ERROR.
               RUN criarTTFluxo(cOrigem,            
                                tit_acr.cod_empresa,
                                tit_acr.cod_estab,
                                IF AVAIL tit_acr THEN string(tit_acr.cdn_cliente) ELSE  '0'  ,
                                IF AVAIL emitente THEN emitente.nome-emit ELSE '', 
                                movto_cta_corren.dat_movto_cta_corren,
                                tit_acr.cod_tit_acr,
                                tit_acr.cod_ser ,             
                                tit_acr.cod_parcela,           
                                tit_acr.cod_espec_docto,           
                               (val_movto_tit_acr.val_liquidac_tit_acr + val_movto_tit_acr.val_juros 
                                 + val_movto_tit_acr.val_multa_tit_acr 
                                 - val_movto_tit_acr.val_abat_tit_acr 
                                 //- val_movto_tit_acr.val_desconto 
                                ) * iSinal, 
                                //val_movto_tit_acr.val_liquidac_tit_acr ,
                                val_movto_tit_acr.cod_tip_fluxo_financ ,      
                                tip_fluxo_financ.des_tip_fluxo_financ ,      
                               '',// IF AVAIL histor_movto_tit_acr THEN histor_movto_tit_acr.des_text_histor ELSE '',         
                                movto_cta_corren.dat_transacao ,             
                                movto_cta_corren.num_seq_movto_cta_corren,         
                                movto_cta_corren.cod_cta_corren,    
                                movto_cta_corren.num_id_movto_cta_corren,
                                 tit_acr.dat_emis ).
   
          END.
       END.
    END.
       
       
    IF cNivel = 'ctbl' THEN
       FOR EACH aprop_ctbl_acr OF movto_tit_acr                                                                                                                               
           /*WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent'   AND aprop_ctbl_acr.ind_natur_lancto_ctbl = 'cr')                                                       
           OR  (movto_cta_corren.ind_fluxo_movto_cta_corren   = 'sai'   AND aprop_ctbl_acr.ind_natur_lancto_ctbl = 'db')*/  NO-LOCK :   
           ASSIGN LOG_achou = YES.
           RUN criarTT( 
                        aprop_ctbl_acr.ind_natur_lancto_ctbl , 
                        aprop_ctbl_acr.cod_empresa , 
                        aprop_ctbl_acr.cod_estab , 
                        IF AVAIL tit_acr THEN string(tit_acr.cdn_cliente) ELSE  '0'  ,
                        IF AVAIL emitente THEN emitente.nome-emit ELSE '', 
                        movto_cta_corren.dat_transacao , 
                        aprop_ctbl_acr.cod_cta_ctbl  , 
                        aprop_ctbl_acr.val_aprop_ctbl ,
                        movto_cta_corren.cod_cta_corren, 
                        cOrigem, 
                        movto_cta_corren.cod_modul_dtsul , 
                        movto_cta_corren.ind_fluxo_movto_cta_corren ,
                        aprop_ctbl_acr.cod_ccusto , 
                        tit_acr.cod_tit_acr , 
                        tit_acr.Cod_ser_docto , 
                        tit_acr.cod_parcela  , 
                        tit_acr.cod_espec_docto ,
                        movto_tit_acr.cod_refer, 
                        movto_cta_corren.num_seq_movto_cta_corren , 
                        movto_cta_corren.num_id_movto_cta_corren , 
                        IF AVAIL ramo-ativ THEN string(ramo-ativ.cod-ramo-ativ) + '-' + ramo-ativ.descricao ELSE '' , 
                        replace(replace(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),CHR(10),";") , 
                        movto_tit_acr.ind_trans_acr_abrev
                      ).
       END.
END.

    
END PROCEDURE.

PROCEDURE buscarCtblAPB:

DEFINE INPUT  PARAMETER rMovtoCtaCorren AS ROWID       NO-UNDO.
DEFINE OUTPUT PARAMETER LOG_achou       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cOrigem                 AS CHARACTER   NO-UNDO INIT 'Contas a Pagar - PEF'.


DEFINE VARIABLE iSinal                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE lRateio                 AS LOGICAL     NO-UNDO.

FIND movto_cta_corren
     WHERE ROWID(movto_cta_corren) = rMovtoCtaCorren
     NO-LOCK NO-ERROR.
RUN getSinalCtaCorrente(OUTPUT iSinal).
ASSIGN LOG_achou = NO.
FOR EACH movto_tit_ap OF movto_cta_corren NO-LOCK:
    FIND estabelecimento OF movto_tit_ap NO-LOCK NO-ERROR.
    FIND FIRST tit_ap OF movto_tit_ap NO-LOCK NO-ERROR. 
    FIND FIRST EMS5.espec_docto OF tit_ap NO-LOCK NO-ERROR.
    IF AVAIL tit_ap THEN
       FIND FIRST EMS5.fornecedor                                                                                                                                              
       WHERE tit_ap.cdn_fornecedor = fornecedor.cdn_fornecedor NO-LOCK NO-ERROR.
       IF AVAIL fornecedor THEN
          FIND FIRST grp_fornec where
                grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec NO-LOCK NO-ERROR.
    /*MESSAGE '2- dentro movto tit ap'
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
    IF NOT AVAIL tit_Ap  THEN DO:
        /*MESSAGE '3- sem titulo'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
        IF lMensagem THEN
        MESSAGE 'nao achou tit ap'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        /*caso n∆o exista o titulo, faz o tratamento para PEF*/
        FOR EACH ems5.antecip_pef_pend OF movto_tit_ap  NO-LOCK:
            FIND FIRST ems5.fornecedor OF ems5.antecip_pef_pend NO-LOCK NO-ERROR.
            
            IF AVAIL fornecedor THEN
            FIND FIRST ems5.grp_fornec where
                  ems5.grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec NO-LOCK NO-ERROR.
            FOR EACH ems5.aprop_ctbl_pend_ap OF antecip_pef_pend NO-LOCK:
                FIND FIRST tip_fluxo_financ OF aprop_ctbl_pend_ap NO-LOCK NO-ERROR.
                ASSIGN LOG_achou = YES.
                RUN criarTT(
                  'db', 
                  estabelecimento.cod_empresa, 
                  estabelecimento.cod_estab, 
                  string(ems5.antecip_pef_pend.cdn_fornecedor),
                  IF AVAIL fornecedor THEN fornecedor.nom_pessoa  ELSE '', 
                  movto_cta_corren.dat_transacao, 
                  aprop_ctbl_pend_ap.cod_cta_ctbl ,
                  // se o valor de apropriaá∆o for maior que o valor do  moviment, coloca o valor do movimento de caixa
                  IF aprop_ctbl_pend_ap.val_aprop_ctbl > movto_cta_corren.val_movto_cta_corren  THEN movto_cta_corren.val_movto_cta_corren  * iSinal ELSE  aprop_ctbl_pend_ap.val_aprop_ctbl  ,            
                  movto_cta_corren.cod_cta_corren , 
                  cOrigem, 
                  movto_cta_corren.cod_modul_dtsul,
                  movto_cta_corren.ind_fluxo_movto_cta_corren, 
                  aprop_ctbl_pend_ap.cod_ccusto,
                  antecip_pef_pend.cod_tit_ap, 
                  antecip_pef_pend.cod_ser_docto,         
                  antecip_pef_pend.cod_parcela, 
                  antecip_pef_pend.cod_espec_docto, 
                  antecip_pef_pend.cod_refer, 
                  movto_cta_corren.num_seq_movto_cta_corren,       
                  movto_cta_corren.num_id_movto_cta_corren, 
                  IF AVAIL ems5.grp_fornec THEN  string(fornecedor.cod_grp_fornec) + "-" + ems5.grp_fornec.des_grp_fornec  ELSE '',
                  REPLACE(replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),CHR(10),";"),    
                    movto_tit_ap.ind_trans_ap_abrev 
                ).
                RUN retornarCtaPortador(antecip_pef_pend.cod_portador, antecip_pef_pend.cod_finalid_econ, OUTPUT cContaContabil).
                RUN criarTT
                  ('CR', 
                    estabelecimento.cod_empresa, 
                    estabelecimento.cod_estab, 
                    string(ems5.antecip_pef_pend.cdn_fornecedor),
                    IF AVAIL fornecedor THEN fornecedor.nom_pessoa  ELSE '', 
                    movto_cta_corren.dat_transacao, 
                    cContaContabil ,
                    // se o valor de apropriaá∆o for maior que o valor do  moviment, coloca o valor do movimento de caixa
                    IF aprop_ctbl_pend_ap.val_aprop_ctbl > movto_cta_corren.val_movto_cta_corren  THEN movto_cta_corren.val_movto_cta_corren  * iSinal ELSE  aprop_ctbl_pend_ap.val_aprop_ctbl  ,            
                    movto_cta_corren.cod_cta_corren , 
                    cOrigem, 
                    movto_cta_corren.cod_modul_dtsul,
                    movto_cta_corren.ind_fluxo_movto_cta_corren, 
                    '',
                    antecip_pef_pend.cod_tit_ap, 
                    antecip_pef_pend.cod_ser_docto,         
                    antecip_pef_pend.cod_parcela, 
                    antecip_pef_pend.cod_espec_docto, 
                    antecip_pef_pend.cod_refer, 
                    movto_cta_corren.num_seq_movto_cta_corren,       
                    movto_cta_corren.num_id_movto_cta_corren, 
                    IF AVAIL ems5.grp_fornec THEN  string(fornecedor.cod_grp_fornec) + "-" + ems5.grp_fornec.des_grp_fornec  ELSE '',
                    REPLACE(replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),CHR(10),";"),    
                    movto_tit_ap.ind_trans_ap_abrev ).
                RUN criarTTFluxo(cOrigem,            
                                estabelecimento.cod_empresa ,  //IF AVAIL tit_ap THEN tit_ap.cod_empresa ELSE '',
                                estabelecimento.cod_estab ,   //movto_cta_corren.cod_estab,
                                string(ems5.antecip_pef_pend.cdn_fornecedor),
                                IF AVAIL fornecedor THEN fornecedor.nom_pessoa  ELSE '', 
                                movto_cta_corren.dat_movto_cta_corren,
                                IF AVAIL tit_ap THEN tit_ap.cod_tit_ap ELSE '',
                                IF AVAIL tit_ap THEN tit_ap.cod_ser ELSE '',             
                                IF AVAIL tit_ap THEN tit_ap.cod_parcela ELSE '',           
                                IF AVAIL tit_ap THEN tit_ap.cod_espec_docto ELSE '',   
                                // se o valor de apropriaá∆o for maior que o valor do  moviment, coloca o valor do movimento de caixa
                                IF aprop_ctbl_pend_ap.val_aprop_ctbl > movto_cta_corren.val_movto_cta_corren  THEN movto_cta_corren.val_movto_cta_corren  * iSinal ELSE  aprop_ctbl_pend_ap.val_aprop_ctbl  ,            
                                aprop_ctbl_pend_ap.cod_tip_fluxo_financ ,      
                                tip_fluxo_financ.des_tip_fluxo_financ ,      
                                '',         
                                movto_cta_corren.dat_transacao ,             
                                movto_cta_corren.num_seq_movto_cta_corren,         
                                movto_cta_corren.cod_cta_corren,    
                                movto_cta_corren.num_id_movto_cta_corren,
                                antecip_pef_pend.dat_emis_docto ).
                
            END.
         END.
         /** criaá∆o do registro de credito a partir do de debito **/
       
    END.
    ELSE DO:
       /* MESSAGE 'com titulo'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
       /**************************************************************************************************
          verifica a diferenca de valor entre a a baixa e a implantacao e cria indice proporcional                                                     
           para tratar valores retirados ou acrescidos ap¢s a implantacao do titulo
        **************************************************************************************************/      
        IF lMensagem THEN
        MESSAGE 'achou titulo'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        FIND FIRST bf_movto_tit_ap OF tit_ap                                                                                                         
            WHERE bf_movto_tit_ap.ind_trans_ap_abrev = 'IMPL'                                                                                        
            OR    bf_movto_tit_ap.ind_trans_ap_abrev  = 'SBND'  NO-LOCK NO-ERROR.                                                                   

          
        ASSIGN  valorInd = movto_tit_ap.val_movto_ap + movto_tit_ap.val_juros - movto_tit_ap.val_desconto.
        IF bf_movto_tit_ap.val_movto_ap <> valorInd THEN DO:                 
           ASSIGN dIndice = bf_movto_tit_ap.val_movto_ap / valorInd.         
        END.                                                                                                                                         
        ELSE                                                                                                                                         
           ASSIGN dIndice = 1. 

        /*MESSAGE 'existe o tit ap' SKIP
                dindice
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/


        /*********************************************************************/
      /*MESSAGE 'nivel' SKIP
               cNivel SKIP
          'titulo'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      IF cNivel = 'fluxo' THEN DO:
         FIND  aprop_ctbl_ap OF movto_tit_ap NO-LOCK NO-ERROR.
         FIND espec_docto OF tit_ap NO-LOCK NO-ERROR.
         IF espec_docto.ind_tip_espec_docto = 'antecipaá∆o' THEN DO:
            FOR EACH val_tit_ap OF tit_ap 
               WHERE val_tit_ap.cod_finalid_econ = 'corrente' NO-LOCK.
               FIND FIRST tip_fluxo_financ OF val_tit_ap NO-LOCK NO-ERROR.
               RUN criarTTFluxo(cOrigem,            
                       tit_ap.cod_empresa,
                       tit_ap.cod_estab,
                       IF AVAIL tit_ap THEN string(tit_ap.cdn_fornec) ELSE  '0'  ,
                       IF AVAIL fornecedor THEN fornecedor.nom_pessoa ELSE '', 
                       movto_cta_corren.dat_movto_cta_corren,
                       tit_ap.cod_tit_ap,
                       tit_ap.cod_ser,             
                       tit_ap.cod_parcela,           
                       tit_ap.cod_espec_docto,           
                       aprop_ctbl_ap.val_aprop_ctbl, // movto_cta_corren.val_movto_cta_corren  * iSinal, //val_tit_ap.val_origin_tit_ap * iSinal,            
                       val_tit_ap.cod_tip_fluxo_financ ,      
                       tip_fluxo_financ.des_tip_fluxo_financ ,      
                       '',         
                       movto_cta_corren.dat_transacao ,             
                       movto_cta_corren.num_seq_movto_cta_corren,         
                       movto_cta_corren.cod_cta_corren,    
                       movto_cta_corren.num_id_movto_cta_corren,
                       tit_ap.dat_emis ).

            END.
         END.
         ELSE DO:
             /*MESSAGE 'dentro do fluxo, n∆o Ç antecipaá∆o'
                 VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
             ASSIGN lRateio = NO.
             FOR EACH rat_movto_tit_ap OF movto_tit_ap NO-LOCK:
                FIND FIRST tip_fluxo_financ OF rat_movto_tit_ap NO-LOCK NO-ERROR.
                ASSIGN lRateio = YES. 
                RUN criarTTFluxo(cOrigem,            
                                tit_ap.cod_empresa,
                                tit_ap.cod_estab,
                                IF AVAIL tit_ap THEN string(tit_ap.cdn_fornec) ELSE  '0'  ,
                                IF AVAIL fornecedor THEN fornecedor.nom_pessoa ELSE '', 
                                movto_cta_corren.dat_movto_cta_corren,
                                tit_ap.cod_tit_ap,
                                tit_ap.cod_ser,             
                                tit_ap.cod_parcela,           
                                tit_ap.cod_espec_docto,           
                                rat_movto_tit_ap.val_aprop_ctbl * iSinal,            
                                rat_movto_tit_ap.cod_tip_fluxo_financ ,      
                                tip_fluxo_financ.des_tip_fluxo_financ ,      
                                '',         
                                movto_cta_corren.dat_transacao ,             
                                movto_cta_corren.num_seq_movto_cta_corren,         
                                movto_cta_corren.cod_cta_corren,    
                                movto_cta_corren.num_id_movto_cta_corren,
                                tit_ap.dat_emis ).

             END.
             IF lRateio = NO THEN DO:
                FOR EACH val_tit_ap OF tit_ap NO-LOCK . 
                    FIND tip_fluxo_financ OF val_tit_ap NO-LOCK NO-ERROR.
                    RUN criarTTFluxo(cOrigem,            
                                tit_ap.cod_empresa,
                                tit_ap.cod_estab,
                                IF AVAIL tit_ap THEN string(tit_ap.cdn_fornec) ELSE  '0',
                                IF AVAIL fornecedor THEN fornecedor.nom_pessoa ELSE '', 
                                movto_cta_corren.dat_movto_cta_corren,
                                tit_ap.cod_tit_ap,
                                tit_ap.cod_ser,             
                                tit_ap.cod_parcela,           
                                tit_ap.cod_espec_docto,           
                                val_tit_ap.val_pagto_tit_ap  * iSinal,            
                                val_tit_ap.cod_tip_fluxo_financ ,      
                                IF AVAIL tip_fluxo_financ THEN tip_fluxo_financ.des_tip_fluxo_financ ELSE '' ,      
                                '',         
                                movto_cta_corren.dat_transacao ,             
                                movto_cta_corren.num_seq_movto_cta_corren,         
                                movto_cta_corren.cod_cta_corren,    
                                movto_cta_corren.num_id_movto_cta_corren,
                                tit_ap.dat_emis ).
                    
                END.
             END.
             /*
            FOR EACH val_movto_ap OF movto_tit_ap NO-LOCK:
                FIND FIRST tip_fluxo_financ OF val_movto_ap NO-LOCK NO-ERROR.
                
                RUN criarTTFluxo(cOrigem,            
                                tit_ap.cod_empresa,
                                tit_ap.cod_estab,
                                IF AVAIL tit_ap THEN string(tit_ap.cdn_fornec) ELSE  '0'  ,
                                IF AVAIL fornecedor THEN fornecedor.nom_pessoa ELSE '', 
                                movto_cta_corren.dat_movto_cta_corren,
                                tit_ap.cod_tit_ap,
                                tit_ap.cod_ser,             
                                tit_ap.cod_parcela,           
                                tit_ap.cod_espec_docto,           
                                (val_movto_ap.val_pagto_tit_ap  + val_movto_ap.val_multa_tit_ap + val_movto_ap.val_juros 
                                - val_movto_ap.val_desconto - val_movto_ap.val_abat_tit_ap) * iSinal,            
                                val_movto_ap.cod_tip_fluxo_financ ,      
                                tip_fluxo_financ.des_tip_fluxo_financ ,      
                                '',         
                                movto_cta_corren.dat_transacao ,             
                                movto_cta_corren.num_seq_movto_cta_corren,         
                                movto_cta_corren.cod_cta_corren,    
                                movto_cta_corren.num_id_movto_cta_corren ).

            END.*/
         END.
      END.
      IF cNivel = 'ctbl' THEN DO:
         FOR EACH ems5.aprop_ctbl_ap OF movto_tit_ap NO-LOCK                                                                                                                   
                /*WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'cr')                                                  
                 OR  (movto_cta_corren.ind_fluxo_movto_cta_corren   = 'sai' AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'db') */ :
          /* MESSAGE 'aprop ctbl'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
               FIND tit_ap OF movto_tit_ap NO-LOCK NO-ERROR.
               /*MESSAGE 'achei titulo:' 
                        tit_ap.cod_tit_ap
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
               IF aprop_ctbl_ap.cod_indic_econ = 'dolar' THEN DO:
                   FIND FIRST ems5.val_aprop_ctbl_ap OF aprop_ctbl_ap
                         WHERE val_aprop_ctbl_ap.cod_finalid_econ = 'corrente' NO-LOCK NO-ERROR.
                   IF AVAIL val_aprop_ctbl_ap THEN 
                      ASSIGN dvalor = val_aprop_ctbl_ap.val_aprop_ctbl / dIndice.
                   ELSE
                      ASSIGN dvalor = aprop_ctbl_ap.val_aprop_ctbl / dIndice.

               END.
               ELSE DO:
                   ASSIGN dvalor = aprop_ctbl_ap.val_aprop_ctbl / dIndice.
               END.
               ASSIGN icont = icont + 1.
               ASSIGN LOG_achou = YES.
               /*MESSAGE 'vou criar a tt'
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
               
               RUN criarTT(
                         aprop_ctbl_ap.ind_natur_lancto_ctbl, 
                         aprop_ctbl_ap.cod_empresa, 
                         aprop_ctbl_ap.cod_estab, 
                         string(tit_ap.cdn_fornecedor), 
                         IF AVAIL fornecedor THEN ems5.fornecedor.nom_pessoa  ELSE '', 
                         movto_cta_corren.dat_movto_cta_corren,aprop_ctbl_ap.cod_cta_ctbl,
                         dValor, 
                         movto_cta_corren.cod_cta_corren,
                         'Contas a Pagar - Titulo', 
                         'APB', 
                         movto_cta_corren.ind_fluxo_movto_cta_corren, 
                         aprop_ctbl_ap.cod_ccusto,
                         tit_ap.cod_tit_ap,tit_ap.cod_ser_docto, 
                         tit_ap.cod_parcela, 
                         tit_ap.cod_espec_docto, 
                         movto_tit_ap.cod_refer, 
                         movto_cta_corren.num_seq_movto_cta_corren,
                         movto_cta_corren.num_id_movto_cta_corren, 
                         IF AVAIL ems5.grp_fornec THEN  string(fornecedor.cod_grp_fornec) + "-" + ems5.grp_fornec.des_grp_fornec  ELSE '',
                         replace(replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),CHR(10),";"),
                         movto_tit_ap.ind_trans_ap_abrev 
                         ). 

         END.
      END.
    END.
END.



END PROCEDURE.


PROCEDURE criarTT:

    DEFINE INPUT  PARAMETER pPartida        AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCodEmpresa     AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCodEstab       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCodEmitente    AS CHAR        NO-UNDO.
    DEFINE INPUT  PARAMETER pDescEmitente   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pData           AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER pContaContabil  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pValor          AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER pContaCorrente  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pOrigem         AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCodModulo      AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pTipo           AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pCC             AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pnroDocto       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pSerie          AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pParcela        AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pEspecie        AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER PCodRefer       AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pSequencia      AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pIdMovtoCorren  AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER pGrupoEmitente  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pHistorico      AS CHARACTER   NO-UNDO FORMAT 'x(500)'.
    DEFINE INPUT  PARAMETER pTransacao      AS CHARACTER   NO-UNDO.


    CREATE tt.             
    ASSIGN                 
    tt.partida           = pPartida        
    tt.cod_modulo        = pCodModulo  
    tt.cod_empresa       = pCodEmpresa            
    tt.cod_estab         = pCodEstab    
    tt.cod_emitente      = pCodEmitente   
    tt.desc_emitente     = pDescEmitente           
    tt.data              = pData  
    tt.conta_contabil    = pContaContabil           
    tt.valor             = pValor  
    tt.conta_corrente    = pContaCorrente         
    tt.origem            = pOrigem     
    tt.tipo              = pTipo           
    tt.cc                = pCC             
    tt.nro_docto         = pnroDocto       
    tt.serie             = pSerie          
    tt.parcela           = pParcela        
    tt.especie           = pEspecie        
    tt.cod_refer         = PCodRefer       
    tt.sequencia         = pSequencia      
    tt.id_movto_corren   = pIdMovtoCorren  
    tt.grupo_emitente    = pGrupoEmitente  
    tt.historico         = pHistorico      
    tt.transacao         = pTransacao  .




END PROCEDURE.

PROCEDURE criarTTFluxo:
    DEFINE INPUT  PARAMETER  pOrigem                 AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER  pCodempresa             AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER  pCodEstab               AS CHAR        NO-UNDO.
    DEFINE INPUT  PARAMETER  pCodEmitente            AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER  pDescEmitente           AS CHAR        NO-UNDO.
    DEFINE INPUT  PARAMETER  pData                   AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER  pNroDocto               AS CHAR        NO-UNDO.
    DEFINE INPUT  PARAMETER  pSerie                  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER  pParcela                AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER  pEspecie                AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER  pValor                  AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER  pCodTipoFluxo           AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER  pDescTipoFluxo          AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER  pHistorico              AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER  pDia                    AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER  pSequencia              AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER  pContaCorrente          AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER  pIdMovtoCorrente        AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER  pDtEmissao              AS DATE        NO-UNDO.

    IF pHistorico = '' THEN DO:
        IF AVAIL movto_cta_corren THEN
        ASSIGN pHistorico = replace(replace(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),CHR(10),";") .
    END.
      
    CREATE ttFluxo.             
    ASSIGN     
    ttFluxo.origem            =  pOrigem
    ttFluxo.cod_empresa       =  pCodEmpresa            
    ttFluxo.cod_estab         =  pCodEstab    
    ttFluxo.cod_emitente      =  pCodEmitente   
    ttFluxo.desc_emitente     =  pDescEmitente           
    ttFluxo.data              =  pData  
    ttFluxo.valor             =  pValor  
    ttFluxo.contaCorrente     =  pContaCorrente         
    ttFluxo.origem            =  pOrigem     
    ttfluxo.nro_docto         =  pnroDocto       
    ttFluxo.serie             =  pSerie          
    ttFluxo.parcela           =  pParcela        
    ttFluxo.especie           =  pEspecie        
    ttFluxo.sequencia         =  pSequencia      
    ttFluxo.idMovtoCorrente   =  pIdMovtoCorrente  
    ttFluxo.historico         =  pHistorico
    ttFluxo.codTipoFluxo      =  pCodTipoFluxo
    ttFluxo.descTipoFluxo     =  pDescTipoFluxo
    ttFluxo.dtEmissao         =  pDtEmissao .

END PROCEDURE.


PROCEDURE retornarCtaPortador:

/*------------------------------------------------------------------------------
  Retorna a conta cont†bil conforme o Portador e finalidade economica passada
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcodPortador AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER pcodFinalidEcon AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cContaContabil  AS CHARACTER   NO-UNDO.
FIND FIRST portad_finalid_econ NO-LOCK
    WHERE portad_finalid_econ.cod_portador     = pCodPortador
    AND   portad_finalid_econ.cod_finalid_econ = pCodFinalidEcon
    NO-ERROR.
IF AVAIL portad_finalid_econ THEN DO:
    FIND FIRST cta_corren_cta_ctbl
        WHERE cta_corren_cta_ctbl.cod_cta_corren = portad_finalid_econ.cod_cta_corren
        AND  cta_corren_cta_ctbl.dat_inic_valid <= TODAY
        AND  cta_corren_cta_ctbl.dat_fim_valid >= TODAY
        NO-LOCK NO-ERROR.
    IF AVAIL cta_corren_cta_ctbl THEN DO:
       ASSIGN cContaContabil = cta_corren_cta_ctbl.cod_cta_ctbl.
    END.
END.


END PROCEDURE.


PROCEDURE preencherCtas:

FOR EACH tt BREAK BY tt.conta_contabil:
    FIND FIRST ems5.cta_ctbl
          WHERE cta_ctbl.cod_cta_ctbl = tt.conta_contabil NO-LOCK NO-ERROR.
    ASSIGN tt.DESC_conta = IF AVAIL  cta_ctbl THEN cta_ctbl.DES_tit_ctbl ELSE 'Conta n∆o Encontrada'.
END.  

END PROCEDURE.    

PROCEDURE retornarRegistros:
DEFINE INPUT PARAMETER  TABLE FOR tt.

END PROCEDURE.

PROCEDURE getRegsFluxo:
DEFINE INPUT PARAMETER  TABLE FOR ttfluxo.
END PROCEDURE.

PROCEDURE exportarRegistros:
DEFINE INPUT  PARAMETER pArquivo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE logBranco AS LOGICAL     NO-UNDO INIT YES.
OUTPUT TO VALUE(pArquivo).
FOR EACH tt:
    ASSIGN logBranco = NO.
    EXPORT DELIMITER "|" tt EXCEPT tt.rowidnota 
        tt.cod_param_desemb          
        tt.cod_param_desemb_cCusto   
        tt.LOG_desconsiderar         
        tt.classificacao
        tt.ccusto_gerencial
        tt.grupo
        tt.base.
END.
IF logBranco THEN
   PUT "N∆o Existem Registros" SKIP.
OUTPUT CLOSE.
END PROCEDURE.


PROCEDURE exportarRegsFluxo:
DEFINE INPUT  PARAMETER pArquivo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE logBranco AS LOGICAL     NO-UNDO INIT YES.
OUTPUT TO VALUE(pArquivo).
FOR EACH ttFluxo:
    ASSIGN logBranco = NO.
    EXPORT DELIMITER "|" ttFluxo.
END.
IF logBranco THEN
   PUT "N∆o Existem Registros" SKIP.
OUTPUT CLOSE.
END PROCEDURE.


PROCEDURE limparTTs:
    EMPTY TEMP-TABLE ttSemCtbl .
    EMPTY TEMP-TABLE tt.
    EMPTY TEMP-TABLE ttFluxo.
    EMPTY TEMP-TABLE ttCtaCorrenDescon .
    /*MESSAGE 'oi'
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/


END PROCEDURE.


