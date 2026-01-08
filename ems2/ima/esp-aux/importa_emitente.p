DEF TEMP-TABLE tt-aux LIKE mgcad.emitente.
DEF TEMP-TABLE tt-cont-emit LIKE mgcad.cont-emit.

DEF BUFFER empresa FOR mgadm.empresa.

DEFINE VAR h-api               AS HANDLE NO-UNDO.
DEFINE VAR i-ep-codigo-empresa LIKE empresa.ep-codigo.
DEFINE VAR c-arquivo-saida     AS CHAR FORMAT "x(20)" INITIAL "c:\temp\api006.lst". 
DEFINE VAR c-arquivo-integra   AS CHAR FORMAT "x(20)" INITIAL "c:\temp\integra.lst". 
DEFINE VAR l-erro              AS LOGICAL FORMAT "yes/no" INITIAL NO.

DEF INPUT PARAMETER TABLE FOR tt-aux.
DEF INPUT PARAMETER TABLE FOR tt-cont-emit.

FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST empresa WHERE 
           empresa.ep-codigo = param-global.empresa-prin NO-LOCK NO-ERROR.

ASSIGN i-ep-codigo-empresa = empresa.ep-codigo.  

{esp-aux/cdapi366b.i}
RUN cdp/cdapi366b.p PERSISTENT SET h-api.

FOR EACH tt-emitente.
    DELETE tt-emitente.
END.

FOR EACH tt-aux.
    CREATE tt-emitente.
    BUFFER-COPY tt-aux TO tt-emitente.

    FOR EACH tt_emitente_integr_new.
        DELETE tt_emitente_integr_new.
    END.

    FIND tt_emitente_integr_new WHERE
         tt_emitente_integr_new.Cod_emitente =	tt-emitente.Cod-emitente
         NO-ERROR.
    IF NOT AVAIL tt_emitente_integr_new THEN DO.
       CREATE tt_emitente_integr_new.
       ASSIGN tt_emitente_integr_new.Cod_emitente	        =	tt-emitente.Cod-emitente.
    END.

    ASSIGN tt_emitente_integr_new.Cod_versao_integracao	    =	01
           tt_emitente_integr_new.Identific                 =	tt-emitente.Identific
           tt_emitente_integr_new.Nome_abrev                =	tt-emitente.Nome-abrev
           tt_emitente_integr_new.Nome_matriz               =	tt-emitente.Nome-matriz
           tt_emitente_integr_new.Natureza                  =	tt-emitente.Natureza
           tt_emitente_integr_new.Cgc                       =	tt-emitente.Cgc
           tt_emitente_integr_new.Cod_portador	            =	tt-emitente.portador
           tt_emitente_integr_new.Modalidade                =	tt-emitente.Modalidade
           tt_emitente_integr_new.Conta_corren	            =	tt-emitente.Conta-corren
           tt_emitente_integr_new.Agencia	                =	tt-emitente.Agencia
           tt_emitente_integr_new.Cod_banco	                =	tt-emitente.Cod-banco
           tt_emitente_integr_new.Forn_exp	                =	tt-emitente.Forn-exp
           tt_emitente_integr_new.Data_implant	            =	tt-emitente.Data-implant
           tt_emitente_integr_new.Cod_gr_cli	            =	tt-emitente.Cod-gr-cli
           tt_emitente_integr_new.Cod_gr_forn	            =	tt-emitente.Cod-gr-forn
           tt_emitente_integr_new.Ins_estadual	            =	tt-emitente.Ins-estadual
           tt_emitente_integr_new.Ins_municipal	            =	tt-emitente.Ins-municipal
           tt_emitente_integr_new.Estado	                =	tt-emitente.Estado
           tt_emitente_integr_new.Endereco	                =	tt-emitente.Endereco
           tt_emitente_integr_new.Endereco2	                =	tt-emitente.Endereco2
           tt_emitente_integr_new.Bairro	                =	tt-emitente.Bairro
           tt_emitente_integr_new.Cep                    	=	tt-emitente.Cep
           tt_emitente_integr_new.Cod_pais	                =	tt-emitente.pais
           tt_emitente_integr_new.Nome_mic_reg	            =	tt-emitente.Nome-mic-reg
           tt_emitente_integr_new.Nom_cidade             	=	tt-emitente.cidade
           tt_emitente_integr_new.Caixa_postal	            =	tt-emitente.Caixa-postal
           tt_emitente_integr_new.Telefax                	=	tt-emitente.Telefax
           tt_emitente_integr_new.Ramal_fax	                =	tt-emitente.Ramal-fax
           tt_emitente_integr_new.Telex	                    =	tt-emitente.Telex
           tt_emitente_integr_new.Telefone[1]	            =	tt-emitente.Telefone[1]
           tt_emitente_integr_new.Telefone[2]	            =	tt-emitente.Telefone[2]
           tt_emitente_integr_new.Ramal[1]	                =	tt-emitente.Ramal[1]
           tt_emitente_integr_new.Ramal[2]	                =	tt-emitente.Ramal[2]
           tt_emitente_integr_new.Telef_modem	            =	tt-emitente.Telef-modem
           tt_emitente_integr_new.Ramal_modem	            =	tt-emitente.Ramal-modem
           tt_emitente_integr_new.Zip_code            	    =	tt-emitente.Zip-code
           tt_emitente_integr_new.Tp_pagto	                =	tt-emitente.Tp-pagto
           tt_emitente_integr_new.Emite_bloq	            =	tt-emitente.Emite-bloq
           tt_emitente_integr_new.Ins_banc[1]            	=	tt-emitente.Ins-banc[1]
           tt_emitente_integr_new.Ins_banc[2]            	=	tt-emitente.Ins-banc[2]
           tt_emitente_integr_new.Ven_sabado	            =	tt-emitente.Ven-sabado
           tt_emitente_integr_new.Ven_Domingo	            =	tt-emitente.Ven-Domingo
           tt_emitente_integr_new.Ven_feriado         	    =	tt-emitente.Ven-feriado
           tt_emitente_integr_new.E_mail	                =	tt-emitente.E-mail
           tt_emitente_integr_new.End_cobranca	            =	tt-emitente.End-cobranca
           tt_emitente_integr_new.Cod_rep             	    =	tt-emitente.Cod-rep
           tt_emitente_integr_new.Observacoes	            =	tt-emitente.Observacoes
           tt_emitente_integr_new.Nome_emit	                =	tt-emitente.Nome-emit
           tt_emitente_integr_new.Endereco_cob	            =	tt-emitente.Endereco-cob
           tt_emitente_integr_new.Bairro_cob	            =	tt-emitente.Bairro-cob
           tt_emitente_integr_new.Cidade_cob	            =	tt-emitente.Cidade-cob
           tt_emitente_integr_new.Estado_cob	            =	tt-emitente.Estado-cob
           tt_emitente_integr_new.Cep_cob             	    =	tt-emitente.Cep-cob
           tt_emitente_integr_new.Cgc_cob	                =	tt-emitente.Cgc-cob
           tt_emitente_integr_new.Cx_post_cob	            =	tt-emitente.Cx-post-cob
           tt_emitente_integr_new.Zip_cob_code	            =	tt-emitente.Zip-cob-code
           tt_emitente_integr_new.Ins_est_cob	            =	tt-emitente.Ins-est-cob
           tt_emitente_integr_new.Pais_cob	                =	tt-emitente.Pais-cob
           tt_emitente_integr_new.Gera_ad	                =	tt-emitente.Gera-ad
           tt_emitente_integr_new.Port_prefer	            =	tt-emitente.Port-prefer
           tt_emitente_integr_new.Mod_prefer	            =	tt-emitente.Mod-prefer
           tt_emitente_integr_new.Ep_codigo	                =	i-ep-codigo-empresa
           tt_emitente_integr_new.Agente_retencao	        =	tt-emitente.Agente-retencao
           tt_emitente_integr_new.Ramo_atividade      	    =	tt-emitente.atividade
           tt_emitente_integr_new.Recebe_inf_sci	        =	tt-emitente.Recebe-inf-sci
           tt_emitente_integr_new.Vencto_dia_nao_util	    =	tt-emitente.Vencto-dia-nao-util
           tt_emitente_integr_new.Tp_desp_padrao	        =	tt-emitente.Tp-desp-padrao
           tt_emitente_integr_new.Bonificacao         	    =	tt-emitente.Bonificacao
           tt_emitente_integr_new.Ind_rendiment	            =	tt-emitente.Ind-rendiment
           tt_emitente_integr_new.Dias_comp	                =	tt-emitente.Dias-comp
           tt_emitente_integr_new.Rendto_tribut	            =	tt-emitente.Rend-tribut
           tt_emitente_integr_new.Home_page	                =	tt-emitente.Home-page
           tt_emitente_integr_new.Utiliza_verba       	    =	tt-emitente.Utiliza-verba
           tt_emitente_integr_new.Percent_verba	            =	tt-emitente.Percent-verba
           tt_emitente_integr_new.Valor_minimo	            =	tt-emitente.Valor-minimo
           tt_emitente_integr_new.Dias_atraso	            =	tt-emitente.nr-Dias-atraso
           tt_emitente_integr_new.Tp_rec_padrao	            =	tt-emitente.Tp-rec-padrao
           tt_emitente_integr_new.Calcula_multa	            =	tt-emitente.Calcula-multa
           tt_emitente_integr_new.Ender_text	            =	""
           tt_emitente_integr_new.Ender_cobr_text	        =	"".

    IF tt-emitente.flag-pag = 1 THEN
       ASSIGN tt_emitente_integr_new.Flag_pag	        =	NO.
    ELSE
       ASSIGN tt_emitente_integr_new.Flag_pag	        =	YES.

    ASSIGN tt_emitente_integr_new.Num_tip_operac      	=	1.
    
    FOR EACH tt-cont-emit WHERE 
             tt-cont-emit.cod-emitente = tt-emitente.cod-emitente NO-LOCK:

        FIND tt_cont_emit_integr_new WHERE 
             tt_cont_emit_integr_new.cod_emitente = tt-cont-emit.cod-emitente NO-ERROR.
        IF NOT AVAIL tt_cont_emit_integr_new THEN
           CREATE tt_cont_emit_integr_new.

        ASSIGN tt_cont_emit_integr_new.area         =  tt-cont-emit.area 
               tt_cont_emit_integr_new.des_cargo    =  tt-cont-emit.cargo 
               tt_cont_emit_integr_new.cod_emitente =  tt-cont-emit.cod-emitente 
               tt_cont_emit_integr_new.e_mail       =  tt-cont-emit.e-mail 
               tt_cont_emit_integr_new.nome         =  tt-cont-emit.nome 
               tt_cont_emit_integr_new.observacao   =  tt-cont-emit.observacao 
               tt_cont_emit_integr_new.ramal        =  tt-cont-emit.ramal 
               tt_cont_emit_integr_new.ramal_fax    =  tt-cont-emit.ramal-fax 
               tt_cont_emit_integr_new.sequencia    =  tt-cont-emit.sequencia 
               tt_cont_emit_integr_new.telefax      =  tt-cont-emit.telefax   
               tt_cont_emit_integr_new.telefone     =  tt-cont-emit.telefone.
    END.

    RUN execute_evoluida_7 IN h-api (INPUT        TABLE tt_emitente_integr_new,
                                     INPUT        TABLE tt_cont_emit_integr_new,
                                     INPUT-OUTPUT TABLE tt_retorno_clien_fornec,
                                     INPUT        i-ep-codigo-empresa,
                                     INPUT-OUTPUT c-arquivo-saida,
                                     INPUT        TABLE tt_cta_emitente).

    ASSIGN l-erro = NO.
    OUTPUT TO c:\temp\erro-cli.txt APPEND.
    FOR EACH tt_retorno_clien_fornec:
        PUT tt-emitente.cod-emitente
            tt_retorno_clien_fornec.ttv_des_mensagem
            SKIP.

        ASSIGN l-erro = YES.
    END.
    OUTPUT CLOSE.

    IF l-erro = NO  THEN DO:
       FIND FIRST tt-cont-emit OF tt-emitente NO-LOCK NO-ERROR.
       IF AVAIL tt-cont-emit THEN DO.
          FOR EACH tt-cont-emit OF tt-emitente.
              RUN cdp/cd1608.p (INPUT tt-cont-emit.cod-emitente,
                                INPUT tt-cont-emit.cod-emitente,
                                INPUT tt-emitente.identific,
                                INPUT YES,
                                INPUT 1,
                                INPUT tt-cont-emit.sequencia,
                                INPUT c-arquivo-integra,
                                INPUT "Arquivo":U,
                                INPUT tt-cont-emit.nome). 
           END.
       END.
       ELSE DO.
          RUN cdp/cd1608.p (INPUT tt-emitente.cod-emitente,
                            INPUT tt-emitente.cod-emitente,
                            INPUT tt-emitente.identific,
                            INPUT YES,
                            INPUT 1,
                            INPUT 0,
                            INPUT c-arquivo-integra,
                            INPUT "Arquivo":U,
                            INPUT "").
       END.
    END.

    FOR EACH tt_emitente_integr_new.
        DELETE tt_emitente_integr_new.
    END.

    FOR EACH tt_cont_emit_integr_new.
        DELETE tt_cont_emit_integr_new.
    END.
    FOR EACH tt_retorno_clien_fornec.
        DELETE tt_retorno_clien_fornec.
    END.
END.

DELETE PROCEDURE h-api.

/* fim de programa */
