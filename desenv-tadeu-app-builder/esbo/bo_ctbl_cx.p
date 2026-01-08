/*********************************************************************************************************
programa: bo_ctbl_cx.p
descriá∆o: Programa para extrair dados de movimento de caixa e bancos atrelados as suas origens de outros
modulos(ACR,APB)
16/02/2022 - tadeu -  acrecimo de possibilitade de desconsiderar conta corrente
17/04/2025 - tadeu - tsp02 - acrescimo de geraá∆o de log pela BO de mensagem.
********************************************************************************************************/

{esbo\bo_ctbl_cx.i}
{utp/ut-glob.i}

define variable cbaseOutra          as character   no-undo format 'x(15)' .
define variable cbaseAtual          as character   no-undo format 'x(15)' .
define variable cempresaAtual       as character   no-undo init '5'.
define variable arquivoSaida        as character   no-undo.
define variable cAmbiente           as character   no-undo.
define variable cConexao            as character   no-undo format 'x(200)'.
define variable digitoAmbiente      as character   no-undo.
define variable servidorAmbiente    as character   no-undo format 'x(50)'.
define variable dbems5              as character   no-undo format 'x(10)'.
define variable cBaseDesconectar    as character   no-undo.
define variable cPortaDesconectar   as character   no-undo.

define variable lMensagem as logical     no-undo init no.

define variable dValor              as decimal     no-undo.
define variable valorInd            as decimal     no-undo.
define variable dIndice             as decimal     no-undo.

define variable hLog                as handle      no-undo.
define variable i                   as integer     no-undo.


define variable iCont               as integer     no-undo.
define variable cContaContabil      as character   no-undo format 'x(30)'.
define variable cListaTpFluxoDescon as character   no-undo.

define variable cNivel as character   no-undo init 'ctbl'.

define buffer bf_movto_tit_ap for movto_tit_ap.

define variable dat_transacao_ini as date        no-undo.
define variable dat_transacao_fim as date        no-undo.

define variable cEstabIni as character   no-undo.
define variable cEstabFim as character   no-undo init 'zzzz'.

define variable ctaCorrenIni as character   no-undo init ''.
define variable ctaCorrenFim as character   no-undo init 'zzzzzzzzzz'.

define variable iSeqIni as integer     no-undo init 0.
define variable iSeqFim as integer     no-undo init 99999.

//tsp02
define variable hBoMsg as handle      no-undo.

define temp-table ttCtaCorrenDescon
    field ctaCorren as char format 'X(50)' .


procedure setMostrarMsg:

    define input  parameter pL as logical     no-undo.
    assign lMensagem = pL .

end procedure.

procedure setIntervalEstab:

    define input  parameter pEstabIni as character   no-undo.
    define input  parameter pEstabFim as character   no-undo.

    assign cEstabIni    = pEstabIni
           cEstabFim    = pEstabFim .


end procedure.


procedure setDataTransacao:

define input  parameter dtIni as date        no-undo.
define input  parameter dtFim as date        no-undo.

assign dat_transacao_ini = dtIni
       dat_transacao_fim = dtFim.


end procedure.

procedure setlistaTpFluxoDescons:
    define input  parameter pLista as character   no-undo.
    assign cListaTpFluxoDescon = pLista.
end procedure.

procedure setNivel:
    define input  parameter pNivel as character   no-undo.
    assign cNivel = pNivel.



end procedure.

procedure setCtaCorren:
    define input  parameter pConta as character   no-undo.
    assign ctaCorrenIni = pConta
           ctaCorrenFim = pConta .

end procedure.

procedure setCtaCorrenDesconsiderar:
    define input  parameter pCta as character   no-undo.

    create ttCtaCorrenDescon.
    assign ttCtaCorrenDescon.ctaCorren = pCta .




end procedure.

procedure setSeq:
    define input  parameter pSeq as integer     no-undo.
    assign iSeqIni = pSeq
           iSeqFim = pSeq .

end procedure.


procedure getSinalCtaCorrente:

define output parameter iSinal as integer     no-undo.
assign iSinal = if movto_cta_corren.ind_fluxo_movto_cta_corren = 'sai' then
                   -1 else 1 . 

end.

procedure buscarRegistros:
   
define variable LOG_achou as logical     no-undo.
define variable iSinal    as integer     no-undo.


/*MESSAGE 'dt.ini:' dat_transacao_ini SKIP
        'dt.fim:' dat_transacao_fim SKIP
        'estab.ini.' cEstabIni      SKIP
        'estab.fim.' cEstabFim      SKIP

    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/



output TO c:\temp\movto_cta_corren.txt.
    //ASSIGN lMensagem = no. //c-seg-usuario = 'super' .
    /*MESSAGE cestabini SKIP
            cestabfim
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
        
    run esbo/boMsg.p persist set hBoMsg.   
        
    if lMensagem then
       message 'oi'
           view-as alert-box information buttons ok.
    for each  movto_cta_corren 
        where movto_cta_corren.dat_transacao >= dat_transacao_ini
        and   movto_cta_corren.dat_transacao <= dat_transacao_fim
        and   movto_cta_corren.cod_cta_corren >= ctaCorrenIni
        and   movto_cta_corren.cod_cta_corren <= ctaCorrenFim
        and   movto_cta_corren.num_seq_movto_cta_corren >= iSeqIni
        and   movto_cta_corren.num_seq_movto_cta_corren <= iSeqFim
        and   ind_tip_movto_cta_corren = 're' no-lock: 


        run sincrTtFluxoFechamento(movto_cta_corren.num_id_movto_cta_corren,'movto',  movto_cta_corren.val_movto_cta_corren).

        find first cta_corren of movto_cta_corren
            where cta_corren.cod_estab >= cEstabIni
            and   cta_corren.cod_estab <= cEstabFim no-lock no-error.
        if not avail cta_Corren  then next.
        find ttCtaCorrenDescon
            where ttCtaCorrenDescon.ctaCorren =  cta_corren.cod_cta_corren
            no-lock no-error.
        if avail ttCtaCorrenDescon then next.

        find first estabelecimento  where
            estabelecimento.cod_estab =  cta_corren.cod_estab no-lock no-error.
        
        if movto_cta_corren.cod_tip_trans_cx <> movto_cta_corren.cod_modul_dtsul 
            and movto_cta_corren.cod_modul_dtsul <> 'cmg' then do:
            /*MESSAGE 'entrei na condicao'
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
            run buscarCtblPadraoTransCaixa(rowid(movto_cta_corren),output LOG_achou).
        end.
        else do:
            case movto_cta_corren.cod_modul_dtsul:
                when 'acr' then do:
                    run buscarCtblACR(rowid(movto_cta_corren), output LOG_achou).
                end.               
                when 'apb' then
                    run buscarCtblAPB(rowid(movto_cta_corren),output LOG_achou).
                when 'cmg' then
                    run buscarCtblCMG(rowid(movto_cta_corren),output LOG_achou).
            end case.  
        end.
        export delimiter "|"
        movto_cta_corren.cod_modul_dtsul                                                           
        movto_cta_corren.cod_estab                                                                     
        movto_cta_corren.dat_transacao                                                             
        movto_cta_corren.cod_tip_trans_cx                                                          
        movto_cta_corren.val_movto_cta_corren                                                      
        movto_cta_corren.cod_cta_corren                                                            
        replace(replace(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),chr(10),";")     
        movto_cta_corren.num_id_movto_cta_corren                                                   
        movto_cta_corren.num_seq_movto_cta_corren skip.

        if LOG_achou = no then do:
           create ttSemCtbl.
           assign ttSemCtbl.origem            =   movto_cta_corren.cod_modul_dtsul  
                  ttSemCtbl.cod_estab         =   movto_cta_corren.cod_estab
                  ttSemCtbl.data              =   movto_cta_corren.dat_transacao
                  ttSemCtbl.nro_docto         =   movto_cta_corren.cod_tip_trans_cx
                  ttSemCtbl.valor             =   movto_cta_corren.val_movto_cta_corren
                  ttSemCtbl.conta_corrente    =   movto_cta_corren.cod_cta_corren 
                  ttSemCtbl.historico         =   replace(replace(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),chr(10),";") 
                  ttSemCtbl.id_movto_corren   =   movto_cta_corren.num_id_movto_cta_corren
                  ttSemCtbl.sequencia         =   movto_cta_corren.num_seq_movto_cta_corren.
        end.
    end.
    output CLOSE.
    run preencherCtas.
    run exportarCtasDesconsideradas.
    if valid-handle(hBoMsg) then do:
       delete object hBoMsg no-error.
        
    end.
     

end procedure.


procedure buscarCtblPadraoTransCaixa:

    define input  parameter rRowid      as rowid       no-undo.
    define output parameter lAchou      as logical     no-undo.
    define variable iSinal as integer     no-undo.
    find movto_cta_corren no-lock
        where rowid(movto_cta_corren) = rRowid no-error.
    run getSinalCtaCorrente(output iSinal).
    if avail movto_cta_corren then do:
       if cNivel = 'fluxo' then do:
           find tip_trans_cx of movto_cta_corren no-lock no-error.
           find tip_fluxo_financ
               where tip_fluxo_financ.cod_tip_fluxo_financ = tip_trans_cx.cod_tip_fluxo_financ_saida
               no-lock no-error.
           run criarTTFluxo(movto_cta_corren.cod_modul_dtsul,            
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
       end.                                             
       /*nivel igual a contabilizaá∆o n∆o implementado*/
    end.
    else do:
       assign lAchou = no.
    end.
    

end procedure.

procedure exportarCtasDesconsideradas.
output TO c:\temp\cta_desconsideradas.txt.
put "conta Corrente Desconsiderada" skip.
for each ttCtaCorrenDescon :
    put unform ttCtaCorrenDescon.ctaCorren skip.
end.


output CLOSE.

end procedure.

procedure retornarLancsSemCtbl:
                              
define output parameter TABLE for ttSemCtbl.

end procedure.

procedure exportarLancsSemCtbl:

define input  parameter pArquivo as character   no-undo.
define variable logBranco as logical     no-undo init yes.

output TO VALUE(pArquivo).
for each ttSemCtbl:
    assign logBranco = no.
    export delimiter "|" ttSemCtbl.
end.
if logBranco then
   put "Todos os Lanáamentos foram Contabilizados corretamente|||||||||" skip.
output CLOSE.

end procedure.

procedure buscarCtblCMG:

define input  parameter rMovtoCtaCorren as rowid  no-undo.
define output parameter LOG_achou       as logical     no-undo.
define variable cOrigem                 as character   no-undo init 'Caixa e Bancos'.
define variable iSinal                  as integer     no-undo.
find movto_cta_corren
     where rowid(movto_cta_corren) = rMovtoCtaCorren
     no-lock no-error.
run getSinalCtaCorrente(output iSinal).


assign LOG_achou  = no.

/*FIND FIRST cta_corren OF movto_cta_corren
    NO-LOCK NO-ERROR.
FIND FIRST estabelecimento 
    WHERE estabelecimento.cod_estab =  cta_corren.cod_Estab
    NO-LOCK NO-ERROR.
  */


if cNivel = 'fluxo' then do:
   find first tip_trans_cx of movto_cta_corren no-lock no-error.
       
   for each rat_financ_cmg of movto_cta_corren :
       find tip_fluxo_financ of rat_financ_cmg no-lock no-error.
       if lookup(rat_financ_cmg.cod_tip_fluxo_financ ,cListaTpFluxoDescon) > 0 then next.      
       assign LOG_achou = yes.
       run criarTTFluxo(cOrigem,            
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

   end.
end.



if cNivel = 'ctbl' then do:
   for each aprop_ctbl_cmg of movto_cta_corren
    /*WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_cmg.ind_natur_lancto_ctbl = 'cr')
    OR  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'sai' AND aprop_ctbl_cmg.ind_natur_lancto_ctbl = 'db') NO-LOCK */ :
       assign LOG_achou = yes.
       run criarTT(aprop_ctbl_cmg.ind_natur_lancto_ctbl,
                   aprop_ctbl_cmg.cod_empresa,aprop_ctbl_cmg.cod_estab,
                   movto_cta_corren.cod_tip_trans_cx,
                   if avail tip_trans_cx then tip_trans_cx.des_tip_trans_cx  else '',
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
                   replace(replace(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),chr(10),";"),
                   ''
                 ) .
    end.
end.    

end procedure.



procedure buscarCtblACR:

define input  parameter rMovtoCtaCorren as rowid       no-undo.
define output parameter LOG_achou       as logical     no-undo.
define variable cOrigem as character   no-undo.
define variable iSinal  as integer     no-undo.
define variable lRateio as logical     no-undo.
assign cOrigem = 'Contas a Receber'.
find movto_cta_corren
     where rowid(movto_cta_corren) = rMovtoCtaCorren
     no-lock no-error.
run getSinalCtaCorrente(output iSinal).
assign LOG_achou = no.

for each movto_tit_acr of movto_cta_corren no-lock:
    find first tit_acr of movto_tit_acr no-lock no-error.                                                                                                               
    find first emitente where                                                                                                                                           
         tit_acr.cdn_cliente =  emitente.cod-emitente no-lock no-error. 
    find first ext-emitente of emitente
        no-lock no-error.
    if avail ext-emitente then
       find first ramo-ativ
            where ramo-ativ.cod-ramo-ativ = ext-emitente.cod-ramo-ativ no-lock no-error.

    /*FIND histor_movto_tit_acr 
        WHERE histor_movto_tit_acr.num_id_movto_tit_acr = movto_tit_acr.num_id_movto_tit_acr NO-LOCK NO-ERROR.*/
    /*Fluxo de Caixa*/

    if cNivel = 'fluxo' then do:
       assign lRateio = no.
       for each rat_movto_tit_acr of movto_tit_acr no-lock.
           assign lRateio = yes.
           find first tip_fluxo_financ of rat_movto_tit_acr no-lock no-error.
           run criarTTFluxo(cOrigem,            
                            tit_acr.cod_empresa,
                            tit_acr.cod_estab,
                            if avail tit_acr then string(tit_acr.cdn_cliente) else  '0'  ,
                            if avail emitente then emitente.nome-emit else '', 
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
                           '', // IF AVAIL histor_movto_tit_acr THEN histor_movto_tit_acr.des_text_histor ELSE '',         
                            movto_cta_corren.dat_transacao ,             
                            movto_cta_corren.num_seq_movto_cta_corren,         
                            movto_cta_corren.cod_cta_corren,    
                            movto_cta_corren.num_id_movto_cta_corren,
                            tit_acr.dat_emis ).

       end.
       if lRateio = no then do:
          for each val_movto_tit_acr of movto_tit_acr no-lock .
               find first tip_fluxo_financ of val_movto_tit_acr no-lock no-error.
               run criarTTFluxo(cOrigem,            
                                tit_acr.cod_empresa,
                                tit_acr.cod_estab,
                                if avail tit_acr then string(tit_acr.cdn_cliente) else  '0'  ,
                                if avail emitente then emitente.nome-emit else '', 
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
                               '', // IF AVAIL histor_movto_tit_acr THEN histor_movto_tit_acr.des_text_histor ELSE '',         
                                movto_cta_corren.dat_transacao ,             
                                movto_cta_corren.num_seq_movto_cta_corren,         
                                movto_cta_corren.cod_cta_corren,    
                                movto_cta_corren.num_id_movto_cta_corren,
                                 tit_acr.dat_emis ).
   
          end.
       end.
    end.
       
       
    if cNivel = 'ctbl' then
       for each aprop_ctbl_acr of movto_tit_acr                                                                                                                               
           /*WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent'   AND aprop_ctbl_acr.ind_natur_lancto_ctbl = 'cr')                                                       
           OR  (movto_cta_corren.ind_fluxo_movto_cta_corren   = 'sai'   AND aprop_ctbl_acr.ind_natur_lancto_ctbl = 'db')*/  no-lock :   
           assign LOG_achou = yes.
           run criarTT( 
                        aprop_ctbl_acr.ind_natur_lancto_ctbl , 
                        aprop_ctbl_acr.cod_empresa , 
                        aprop_ctbl_acr.cod_estab , 
                        if avail tit_acr then string(tit_acr.cdn_cliente) else  '0'  ,
                        if avail emitente then emitente.nome-emit else '', 
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
                        if avail ramo-ativ then string(ramo-ativ.cod-ramo-ativ) + '-' + ramo-ativ.descricao else '' , 
                        replace(replace(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),chr(10),";") , 
                        movto_tit_acr.ind_trans_acr_abrev
                      ).
       end.
end.

    
end procedure.

procedure buscarCtblAPB:

define input  parameter rMovtoCtaCorren as rowid       no-undo.
define output parameter LOG_achou       as logical     no-undo.
define variable cOrigem                 as character   no-undo init 'Contas a Pagar - PEF'.


define variable iSinal                  as integer     no-undo.
define variable lRateio                 as logical     no-undo.
define variable lValAp                  as logical     no-undo.
define variable dConv                   as decimal     no-undo.
define variable dValorAprop             as decimal     no-undo.

find movto_cta_corren
     where rowid(movto_cta_corren) = rMovtoCtaCorren
     no-lock no-error.
run getSinalCtaCorrente(output iSinal).
assign LOG_achou = no.
for each movto_tit_ap of movto_cta_corren no-lock:
    find estabelecimento of movto_tit_ap no-lock no-error.
    find first tit_ap of movto_tit_ap no-lock no-error. 
    find first EMS5.espec_docto of tit_ap no-lock no-error.
    if avail tit_ap then
       find first EMS5.fornecedor                                                                                                                                              
       where tit_ap.cdn_fornecedor = fornecedor.cdn_fornecedor no-lock no-error.
       if avail fornecedor then
          find first grp_fornec where
                grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec no-lock no-error.
    
    if not avail tit_Ap  then do:
        if lMensagem then
        message 'nao achou tit ap'
            view-as alert-box info buttons ok.
        /*caso n∆o exista o titulo, faz o tratamento para PEF*/
        for each ems5.antecip_pef_pend of movto_tit_ap  no-lock:
            find first ems5.fornecedor of ems5.antecip_pef_pend no-lock no-error.
            if avail fornecedor then
            find first ems5.grp_fornec where
                  ems5.grp_fornec.cod_grp_fornec = fornecedor.cod_grp_fornec no-lock no-error.
            for each ems5.aprop_ctbl_pend_ap of antecip_pef_pend no-lock:
                find first tip_fluxo_financ of aprop_ctbl_pend_ap no-lock no-error.
                assign LOG_achou = yes.
                run criarTT(
                  'db', 
                  estabelecimento.cod_empresa, 
                  estabelecimento.cod_estab, 
                  string(ems5.antecip_pef_pend.cdn_fornecedor),
                  if avail fornecedor then fornecedor.nom_pessoa  else '', 
                  movto_cta_corren.dat_transacao, 
                  aprop_ctbl_pend_ap.cod_cta_ctbl ,
                  aprop_ctbl_pend_ap.val_aprop_ctbl,
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
                  if avail ems5.grp_fornec then  string(fornecedor.cod_grp_fornec) + "-" + ems5.grp_fornec.des_grp_fornec  else '',
                  replace(replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),chr(10),";"),    
                    movto_tit_ap.ind_trans_ap_abrev 
                ).
                run retornarCtaPortador(antecip_pef_pend.cod_portador, antecip_pef_pend.cod_finalid_econ, output cContaContabil).
                run criarTT
                  ('CR', 
                    estabelecimento.cod_empresa, 
                    estabelecimento.cod_estab, 
                    string(ems5.antecip_pef_pend.cdn_fornecedor),
                    if avail fornecedor then fornecedor.nom_pessoa  else '', 
                    movto_cta_corren.dat_transacao, 
                    cContaContabil ,
                    aprop_ctbl_pend_ap.val_aprop_ctbl   , 
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
                    if avail ems5.grp_fornec then  string(fornecedor.cod_grp_fornec) + "-" + ems5.grp_fornec.des_grp_fornec  else '',
                    replace(replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),chr(10),";"),    
                    movto_tit_ap.ind_trans_ap_abrev ).
                run criarTTFluxo(cOrigem,            
                                estabelecimento.cod_empresa ,  //IF AVAIL tit_ap THEN tit_ap.cod_empresa ELSE '',
                                estabelecimento.cod_estab ,   //movto_cta_corren.cod_estab,
                                string(ems5.antecip_pef_pend.cdn_fornecedor),
                                if avail fornecedor then fornecedor.nom_pessoa  else '', 
                                movto_cta_corren.dat_movto_cta_corren,
                                if avail tit_ap then tit_ap.cod_tit_ap else '',
                                if avail tit_ap then tit_ap.cod_ser else '',             
                                if avail tit_ap then tit_ap.cod_parcela else '',           
                                if avail tit_ap then tit_ap.cod_espec_docto else '',           
                                aprop_ctbl_pend_ap.val_aprop_ctbl * iSinal ,            
                                aprop_ctbl_pend_ap.cod_tip_fluxo_financ ,      
                                tip_fluxo_financ.des_tip_fluxo_financ ,      
                                '',         
                                movto_cta_corren.dat_transacao ,             
                                movto_cta_corren.num_seq_movto_cta_corren,         
                                movto_cta_corren.cod_cta_corren,    
                                movto_cta_corren.num_id_movto_cta_corren,
                                antecip_pef_pend.dat_emis_docto ).
                
            end.
         end.
         /** criaá∆o do registro de credito a partir do de debito **/
       
    end.
    else do:
       find espec_docto of tit_ap no-lock no-error.
       run getIndConvMonet(rowid(tit_ap),output dConv).
       /**************************************************************************************************
          verifica a diferenca de valor entre a a baixa e a implantacao e cria indice proporcional                                                     
           para tratar valores retirados ou acrescidos ap¢s a implantacao do titulo
        **************************************************************************************************/      
        if lMensagem then
        message 'achou titulo'
            view-as alert-box info buttons ok.
        find first bf_movto_tit_ap of tit_ap                                                                                                         
            where bf_movto_tit_ap.ind_trans_ap_abrev = 'IMPL'                                                                                        
            or    bf_movto_tit_ap.ind_trans_ap_abrev  = 'SBND'  no-lock no-error.                                                                   

          
        assign  valorInd = movto_tit_ap.val_movto_ap + movto_tit_ap.val_juros - movto_tit_ap.val_desconto.
        if bf_movto_tit_ap.val_movto_ap <> valorInd then do:                 
           assign dIndice = bf_movto_tit_ap.val_movto_ap / valorInd.         
        end.                                                                                                                                         
        else                                                                                                                                         
           assign dIndice = 1. 

        

        /*********************************************************************/
      /*MESSAGE 'nivel' SKIP
               cNivel SKIP
          'titulo'
          VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      if cNivel = 'fluxo' then do:
         find espec_docto of tit_ap no-lock no-error.
         if espec_docto.ind_tip_espec_docto = 'antecipaá∆o' then do:
            

            for each rat_movto_tit_ap of movto_tit_ap no-lock:
               find first tip_fluxo_financ of  rat_movto_tit_ap no-lock no-error.
               run criarTTFluxo(cOrigem,            
                       tit_ap.cod_empresa,
                       tit_ap.cod_estab,
                       string(tit_ap.cdn_fornec),
                       if avail fornecedor then fornecedor.nom_pessoa else '', 
                       movto_cta_corren.dat_movto_cta_corren,
                       tit_ap.cod_tit_ap,
                       tit_ap.cod_ser,             
                       tit_ap.cod_parcela,           
                       tit_ap.cod_espec_docto,           
                       rat_movto_tit_ap.val_aprop_ctbl * iSinal / dConv,
                       //movto_cta_corren.val_movto_cta_corren * iSinal, //val_tit_ap.val_origin_tit_ap * iSinal,            
                       rat_movto_tit_ap.cod_tip_fluxo_financ ,      
                       tip_fluxo_financ.des_tip_fluxo_financ ,      
                       '',         
                       movto_cta_corren.dat_transacao ,             
                       movto_cta_corren.num_seq_movto_cta_corren,         
                       movto_cta_corren.cod_cta_corren,    
                       movto_cta_corren.num_id_movto_cta_corren,
                       tit_ap.dat_emis ).
            end.
         end.
         else do:
             if lMensagem then
                message 'dentro do fluxo, n∆o Ç antecipaá∆o '
                 view-as alert-box information buttons ok.
             assign lRateio = no.
             
             for each rat_movto_tit_ap of movto_tit_ap no-lock:
                find first tip_fluxo_financ of rat_movto_tit_ap no-lock no-error.
                assign lRateio = yes. 
                run criarTTFluxo(cOrigem,            
                                tit_ap.cod_empresa,
                                tit_ap.cod_estab,
                                if avail tit_ap then string(tit_ap.cdn_fornec) else  '0'  ,
                                if avail fornecedor then fornecedor.nom_pessoa else '', 
                                movto_cta_corren.dat_movto_cta_corren,
                                tit_ap.cod_tit_ap,
                                tit_ap.cod_ser,             
                                tit_ap.cod_parcela,           
                                tit_ap.cod_espec_docto,           
                                rat_movto_tit_ap.val_aprop_ctbl * iSinal / dConv,            
                                rat_movto_tit_ap.cod_tip_fluxo_financ ,      
                                tip_fluxo_financ.des_tip_fluxo_financ ,      
                                '',         
                                movto_cta_corren.dat_transacao ,             
                                movto_cta_corren.num_seq_movto_cta_corren,         
                                movto_cta_corren.cod_cta_corren,    
                                movto_cta_corren.num_id_movto_cta_corren,
                                tit_ap.dat_emis ).

             end.
             if lRateio = no then do:
                
            
                assign lValAp = no.         
                for each val_movto_ap of movto_tit_ap no-lock:
                    find first tip_fluxo_financ of val_movto_ap no-lock no-error.
/*                    if tit_ap.cod_tit_ap = '000019' then  do:                                                              */
/*                       message     (val_movto_ap.val_pagto_tit_ap  + val_movto_ap.val_multa_tit_ap + val_movto_ap.val_juros*/
/*                                    - val_movto_ap.val_desconto - val_movto_ap.val_abat_tit_ap)                            */
/*                        view-as alert-box information buttons ok.                                                          */
/*                    end.                                                                                                   */
                    run criarTTFluxo(cOrigem,            
                                    tit_ap.cod_empresa,
                                    tit_ap.cod_estab,
                                    if avail tit_ap then string(tit_ap.cdn_fornec) else  '0'  ,
                                    if avail fornecedor then fornecedor.nom_pessoa else '', 
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
                    assign lValAp = yes.
                end.          
                
                if lValAp = no  then do:
                    for each val_tit_ap of tit_ap no-lock . 
                        find tip_fluxo_financ of val_tit_ap no-lock no-error.
                        if lMensagem then
                            message 'sem rateio' skip
                              'valor movto ap:' movto_tit_ap.val_movto_ap skip
                              'val pgto ap:' val_tit_ap.val_pagto_tit_ap skip
                              'val movto cx:' movto_cta_corren.val_movto_cta_corren skip
                               movto_tit_ap.num_id_movto_tit_ap
                           view-as alert-box information buttons ok.

                        if espec_docto.ind_tip_espec_docto = 'imposto retido' then
                           assign dValorAprop = movto_tit_ap.val_movto_ap * iSinal .
                        else
                           assign dValorAprop = if val_tit_ap.val_pagto_tit_ap > movto_cta_corren.val_movto_cta_corren 
                                               then movto_cta_corren.val_movto_cta_corren * iSinal 
                                               else val_tit_ap.val_pagto_tit_ap  * iSinal.

                        run criarTTFluxo(cOrigem,            
                                    tit_ap.cod_empresa,
                                    tit_ap.cod_estab,
                                    if avail tit_ap then string(tit_ap.cdn_fornec) else  '0',
                                    if avail fornecedor then fornecedor.nom_pessoa else '', 
                                    movto_cta_corren.dat_movto_cta_corren,
                                    tit_ap.cod_tit_ap,
                                    tit_ap.cod_ser,             
                                    tit_ap.cod_parcela,           
                                    tit_ap.cod_espec_docto,           
                                    dValorAprop ,            
                                    val_tit_ap.cod_tip_fluxo_financ ,      
                                    if avail tip_fluxo_financ then tip_fluxo_financ.des_tip_fluxo_financ else '' ,      
                                    '',         
                                    movto_cta_corren.dat_transacao ,             
                                    movto_cta_corren.num_seq_movto_cta_corren,         
                                    movto_cta_corren.cod_cta_corren,    
                                    movto_cta_corren.num_id_movto_cta_corren,
                                    tit_ap.dat_emis ).
                        
                    end.                    
                end.                
             end.              
         end.
      end.
      if cNivel = 'ctbl' then do:
         for each ems5.aprop_ctbl_ap of movto_tit_ap no-lock                                                                                                                   
                /*WHERE  (movto_cta_corren.ind_fluxo_movto_cta_corren = 'ent' AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'cr')                                                  
                 OR  (movto_cta_corren.ind_fluxo_movto_cta_corren   = 'sai' AND aprop_ctbl_ap.ind_natur_lancto_ctbl = 'db') */ :
          /* MESSAGE 'aprop ctbl'
               VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
               find tit_ap of movto_tit_ap no-lock no-error.
               /*MESSAGE 'achei titulo:' 
                        tit_ap.cod_tit_ap
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
               if aprop_ctbl_ap.cod_indic_econ = 'dolar' then do:
                   find first ems5.val_aprop_ctbl_ap of aprop_ctbl_ap
                         where val_aprop_ctbl_ap.cod_finalid_econ = 'corrente' no-lock no-error.
                   if avail val_aprop_ctbl_ap then 
                      assign dvalor = val_aprop_ctbl_ap.val_aprop_ctbl / dIndice.
                   else
                      assign dvalor = aprop_ctbl_ap.val_aprop_ctbl / dIndice.

               end.
               else do:
                   assign dvalor = aprop_ctbl_ap.val_aprop_ctbl / dIndice.
               end.
               assign icont = icont + 1.
               assign LOG_achou = yes.
               /*MESSAGE 'vou criar a tt'
                   VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
               
               run criarTT(
                         aprop_ctbl_ap.ind_natur_lancto_ctbl, 
                         aprop_ctbl_ap.cod_empresa, 
                         aprop_ctbl_ap.cod_estab, 
                         string(tit_ap.cdn_fornecedor), 
                         if avail fornecedor then ems5.fornecedor.nom_pessoa  else '', 
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
                         if avail ems5.grp_fornec then  string(fornecedor.cod_grp_fornec) + "-" + ems5.grp_fornec.des_grp_fornec  else '',
                         replace(replace(ems5.movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),chr(10),";"),
                         movto_tit_ap.ind_trans_ap_abrev 
                         ). 

         end.
      end.
    end.
end.



end procedure.


procedure criarTT:

    define input  parameter pPartida        as character   no-undo.
    define input  parameter pCodEmpresa     as character   no-undo.
    define input  parameter pCodEstab       as character   no-undo.
    define input  parameter pCodEmitente    as char        no-undo.
    define input  parameter pDescEmitente   as character   no-undo.
    define input  parameter pData           as date        no-undo.
    define input  parameter pContaContabil  as character   no-undo.
    define input  parameter pValor          as decimal     no-undo.
    define input  parameter pContaCorrente  as character   no-undo.
    define input  parameter pOrigem         as character   no-undo.
    define input  parameter pCodModulo      as character   no-undo.
    define input  parameter pTipo           as character   no-undo.
    define input  parameter pCC             as character   no-undo.
    define input  parameter pnroDocto       as character   no-undo.
    define input  parameter pSerie          as character   no-undo.
    define input  parameter pParcela        as character   no-undo.
    define input  parameter pEspecie        as character   no-undo.
    define input  parameter PCodRefer       as character   no-undo.
    define input  parameter pSequencia      as integer     no-undo.
    define input  parameter pIdMovtoCorren  as integer     no-undo.
    define input  parameter pGrupoEmitente  as character   no-undo.
    define input  parameter pHistorico      as character   no-undo format 'x(500)'.
    define input  parameter pTransacao      as character   no-undo.

    run sincrTtFluxoFechamento(movto_cta_corren.num_id_movto_cta_corren,'aprop_ctbl',pvalor).
    create tt.             
    assign                 
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




end procedure.

procedure criarTTFluxo:
    define input  parameter  pOrigem                 as character   no-undo.
    define input  parameter  pCodempresa             as character   no-undo.
    define input  parameter  pCodEstab               as char        no-undo.
    define input  parameter  pCodEmitente            as character   no-undo.
    define input  parameter  pDescEmitente           as char        no-undo.
    define input  parameter  pData                   as date        no-undo.
    define input  parameter  pNroDocto               as char        no-undo.
    define input  parameter  pSerie                  as character   no-undo.
    define input  parameter  pParcela                as character   no-undo.
    define input  parameter  pEspecie                as character   no-undo.
    define input  parameter  pValor                  as decimal     no-undo.
    define input  parameter  pCodTipoFluxo           as character   no-undo.
    define input  parameter  pDescTipoFluxo          as character   no-undo.
    define input  parameter  pHistorico              as character   no-undo.
    define input  parameter  pDia                    as date        no-undo.
    define input  parameter  pSequencia              as integer     no-undo.
    define input  parameter  pContaCorrente          as character   no-undo.
    define input  parameter  pIdMovtoCorrente        as integer     no-undo.
    define input  parameter  pDtEmissao              as date        no-undo.

    if pHistorico = '' then do:
        if avail movto_cta_corren then
        assign pHistorico = replace(replace(movto_cta_corren.des_histor_movto_cta_corren,chr(13),";"),chr(10),";") .
    end.
      
    run sincrTtFluxoFechamento(movto_cta_corren.num_id_movto_cta_corren,'aprop_fluxo',pValor).
    
    create ttFluxo.             
    assign     
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

end procedure.


procedure retornarCtaPortador:

/*------------------------------------------------------------------------------
  Retorna a conta cont†bil conforme o Portador e finalidade economica passada
------------------------------------------------------------------------------*/
define input  parameter pcodPortador as character   no-undo.
define input  parameter pcodFinalidEcon as character   no-undo.
define output parameter cContaContabil  as character   no-undo.
find first portad_finalid_econ no-lock
    where portad_finalid_econ.cod_portador     = pCodPortador
    and   portad_finalid_econ.cod_finalid_econ = pCodFinalidEcon
    no-error.
if avail portad_finalid_econ then do:
    find first cta_corren_cta_ctbl
        where cta_corren_cta_ctbl.cod_cta_corren = portad_finalid_econ.cod_cta_corren
        and  cta_corren_cta_ctbl.dat_inic_valid <= today
        and  cta_corren_cta_ctbl.dat_fim_valid >= today
        no-lock no-error.
    if avail cta_corren_cta_ctbl then do:
       assign cContaContabil = cta_corren_cta_ctbl.cod_cta_ctbl.
    end.
end.


end procedure.


procedure preencherCtas:

for each tt break by tt.conta_contabil:
    find first ems5.cta_ctbl
          where cta_ctbl.cod_cta_ctbl = tt.conta_contabil no-lock no-error.
    assign tt.DESC_conta = if avail  cta_ctbl then cta_ctbl.DES_tit_ctbl else 'Conta n∆o Encontrada'.
end.  

end procedure.    

procedure retornarRegistros:
define input parameter  TABLE for tt.

end procedure.

procedure getRegsFluxo:
define input parameter  TABLE for ttfluxo.
end procedure.

procedure exportarRegistros:
define input  parameter pArquivo as character   no-undo.
define variable logBranco as logical     no-undo init yes.
output TO VALUE(pArquivo).
for each tt:
    assign logBranco = no.
    export delimiter "|" tt except tt.rowidnota 
        tt.cod_param_desemb          
        tt.cod_param_desemb_cCusto   
        tt.LOG_desconsiderar         
        tt.classificacao
        tt.ccusto_gerencial
        tt.grupo
        tt.base.
end.
if logBranco then
   put "N∆o Existem Registros" skip.
output CLOSE.
end procedure.


procedure exportarRegsFluxo:
define input  parameter pArquivo as character   no-undo.
define variable logBranco as logical     no-undo init yes.
output TO VALUE(pArquivo).
for each ttFluxo:
    assign logBranco = no.
    export delimiter "|" ttFluxo.
end.
if logBranco then
   put "N∆o Existem Registros" skip.
output CLOSE.
end procedure.


procedure limparTTs:
    empty temp-table ttSemCtbl .
    empty temp-table tt.
    empty temp-table ttFluxo.
    empty temp-table ttFluxoFechamento.
    empty temp-table ttCtaCorrenDescon .


end procedure.

procedure getIndConvMonet:
    define input  parameter pRowid as rowid       no-undo.
    define buffer bf for tit_ap.
    define output parameter dConv as decimal format '999,9999999999'  init 1   no-undo.

    find tit_ap
        where rowid(tit_ap) = pRowid no-lock no-error.
    if avail tit_ap then do:
       if tit_ap.cod_indic_econ <> 'real' then do:
          find val_tit_ap of tit_ap
              where cod_finalid_econ = 'corrente' no-lock no-error.
          if avail val_tit_ap then do:
             assign dConv = val_tit_ap.val_cotac_indic_econ .
          end.                                               
       end.                                                  
    end.
end procedure.

procedure getTTFluxoFechamento:

    define output parameter TABLE for ttfluxoFechamento.

end procedure.

procedure exportarLancsFechamento:
    define input  parameter pArquivo as character   no-undo.
    define variable logBranco as logical     no-undo init yes.
    define variable dValotAbs as decimal     no-undo.
    output TO VALUE(pArquivo).
    put "ID|Vl.Movto|Vl.Aprop.Fluxo|Vl.Aprop.Ctbl|Ct.Corren|Seq|Data|DIF" skip.
    for each ttFluxoFechamento
        //WHERE ROUND(ttFluxoFechamento.vl_movto,2) <> ABSOLUTE(round(ttFluxoFechamento.vl_aprop_fluxo,2))
        :
        find movto_cta_corren no-lock
            where movto_cta_corren.num_id_movto_cta_corren = ttFluxoFechamento.id_movto_corren no-error.
        find ttCtaCorrenDescon
            where ttCtaCorrenDescon.ctaCorren = movto_cta_corren.cod_cta_corren
            no-lock no-error.
        if avail ttCtaCorrenDescon then next.
        assign logBranco = no.
        put ttFluxoFechamento.id_movto_corren "|"
            ttFluxoFechamento.vl_movto "|"
            ttFluxoFechamento.vl_movto "|"
            ttFluxoFechamento.vl_aprop_ctbl "|"
            movto_cta_corren.cod_cta_corren "|"
            movto_cta_corren.num_seq_movto_cta_corren "|"
            movto_cta_corren.dat_movto_cta_corren "|" 
            ttFluxoFechamento.vl_movto - ABSOLUTE(ttFluxoFechamento.vl_movto)
            skip .

            
    end.
    if logBranco then
       put "N∆o Existem Registros" skip.
    output CLOSE.
end procedure.
