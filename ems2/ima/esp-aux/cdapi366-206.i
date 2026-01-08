
/* ************************************************************************ */
/*      CDAPI366.i  - Include da CDAPI366                                   */
/*      Dataminas  - Agosto 2002                                            */
/* ************************************************************************ */

DEFINE TEMP-TABLE tt-emitente NO-UNDO LIKE mgadm.emitente.

DEFINE TEMP-TABLE tt_emitente_integr_new NO-UNDO  
        field	Cod_versao_integracao	as	Integer	    format	'999' initial	0
        field	Cod_emitente        	as	Integer	    format	'>>>>>>9'	initial	0
        field	Identific	            as	Integer	    format	'>9'	initial	0
        field	Nome_abrev	            as	Character	format	'x(12)' 
        field	Nome_matriz	            as	Character	format	'x(12)'	
        field	Natureza	            as	Integer	    format	'>9'	initial	0
        field	Cgc	                    as	Character	format	'x(19)'	
        field	Cod_portador	        as	Integer	    format	'>>>>9'	initial	0
        field	Modalidade	            as	Integer	    format	'9'	initial	0
        field	Conta_corren	        as	Character	format	'x(20)'	
        field	Agencia	                as	Character	format	'x(08)'	
        field	Cod_banco	            as	Integer	    format	'999'	initial	0
        field	Forn_exp	            as	Logical	    format	'Sim/NÆo'	initial	No
        field	Data_implant	        as	Date	    format	'99/99/9999'	initial	?
        field	Cod_gr_cli	            as	Integer	    format	'>9'	initial	0
        field	Cod_gr_forn	            as	Integer	    format	'>9'	initial	0
        field	Ins_estadual	        as	Character	format	'x(19)'	
        field	Ins_municipal	        as	Character	format	'x(19)'	
        field	Estado	                as	Character	format	'x(04)'	
        field	Endereco	            as	Character	format	'x(40)'	
        field	Endereco2	            as	Character	format	'x(40)'	
        field	Bairro	                as	Character	format	'x(30)'	
        field	Cep	                    as	Character	format	'x(12)'	
        field	Cod_pais	            as	Character	format	'x(20)'	
        field	Nome_mic_reg	        as	Character	format	'x(12)'	
        field	Nom_cidade	            as	Character	format	'x(25)'	
        field	Caixa_postal	        as	Character	format	'x(10)'	
        field	Telefax	                as	Character	format	'x(15)'	
        field	Ramal_fax	            as	Character	format	'x(05)'	
        field	Telex	                as	Character	format	'x(15)'	
        field	Telefone	            as	Character	format	'x(15)'    extent 2	
        field	Ramal	                as	Character	format	'x(05)'    extent 2	
        field	Telef_modem	            as	Character	format	'x(15)'
        field	Ramal_modem	            as	Character	format	'x(05)'
        field	Zip_code	            as	Character	format	'x(12)'
        field	Tp_pagto	            as	Integer	    format	'99'	initial	0
        field	Emite_bloq	            as	Logical	    format	'Sim/NÆo'	initial	No
        field	Ins_banc	            as	Integer	    format	'>>9'     extent 2	initial	0
        field	Ven_sabado	            as	Integer	    format	'9'	initial	0
        field	Ven_Domingo	            as	Integer	    format	'9'	initial	0
        field	Ven_feriado	            as	Integer	    format	'9'	initial	0
        field	E_mail	                as	Character	format	'x(40)'	
        field	End_cobranca	        as	Integer 	format	'>>>>>9'	initial	0
        field	Cod_rep	                as	Integer	    format	'>>>>9'	    initial	0
        field	Observacoes	            as	Character	format	'x(2000)'	
        field	Nome_emit	            as	Character	format	'x(40)'		
        field	Endereco_cob	        as	Character	format	'x(40)'		
        field	Bairro_cob	            as	Character	format	'x(30)'	
        field	Cidade_cob	            as	Character	format	'x(25)'	
        field	Estado_cob	            as	Character	format	'x(04)'	
        field	Cep_cob	                as	Character	format	'x(12)'	
        field	Cgc_cob	                as	Character	format	'x(19)'	
        field	Cx_post_cob	            as	Character	format	'x(10)'		
        field	Zip_cob_code	        as	Character	format	'x(12)'		
        field	Ins_est_cob	            as	Character	format	'x(19)'		
        field	Pais_cob	            as	Character	format	'x(20)'		
        field	Gera_ad	                as	Logical	format	'Sim/NÆo'	initial	No
        field	Port_prefer	            as	Integer	format	'>>>>9'	initial	0
        field	Mod_prefer	            as	Integer	format	'9'	initial	0
        field	Ep_codigo	            as	Integer	format	'>>9'	initial	0
        field	Ep_codigo_principal	    as	Integer	format	'>>9'	initial	0
        field	Num_tip_operac	        as	Integer	format	'9'	initial	0
        field	Agente_retencao	        as	Logical	format	"Sim/NÆo"	initial	No
        field	Ramo_atividade	        as	Character	format	"x(08)"	initial	''
        field	Recebe_inf_sci	        as	Logical	format	"Sim/NÆo"	initial	No
        field	Vencto_dia_nao_util	    as	Logical	format	'Sim/NÆo'	initial	No
        field	Tp_desp_padrao	        as	Integer	format	'99'	initial	0
        field	Bonificacao	            as	Decimal	format	">>9.99"	initial	0
        field	Ind_rendiment	        as	Logical	format	'Sim/NÆo'	initial	No
        field	Dias_comp	            as	Integer	format	">>9"	initial	0
        field	Rendto_tribut	        as	Integer	format	'999'	initial	0
        field	Home_page	            as	Character	format	'x(40)'	initial	''
        field	Utiliza_verba	        as	Logical	format	"Sim/NÆo"	initial	No
        field	Percent_verba	        as	Decimal	format	">>>9.99"	initial	0
        field	Valor_minimo	        as	Decimal	format	">>,>>>,>>>,>>9.99"	initial	0
        field	Dias_atraso	            as	integer	format	"999"	initial	0
        field	Tp_rec_padrao	        as	integer	format	">>9"	initial	0
        field	Calcula_multa	        as	Logical	format	"Sim/NÆo"	initial	No
        field	Flag_pag	            as	Logical	format	"Sim/NÆo"	initial	No
        field	Ender_text	            as	Character	format	'x(2000)'	initial	''
        field	Ender_cobr_text	        as	Character	format	'x(2000)'	initial	''
     index codigo is primary UNIQUE cod_emitente ascending.

DEFINE TEMP-TABLE tt_cont_emit_integr NO-UNDO 
       FIELD cod_versao_integracao AS INTEGER FORMAT "999"
       FIELD cod_emitente          AS INTEGER FORMAT ">>>>>>9" 
       FIELD sequencia             AS INTEGER FORMAT ">>9"
       FIELD nome                  AS CHAR FORMAT "x(40)" 
       FIELD des_cargo             AS CHAR FORMAT "x(20)"
       FIELD area                  AS CHAR FORMAT "x(18)"
       FIELD telefone              AS CHAR FORMAT "x(15)"
       FIELD ramal                 AS CHAR FORMAT "x(5)"
       FIELD telefax               AS CHAR FORMAT "x(15)"
       FIELD ramal-fax             AS CHAR FORMAT "x(5)"
       FIELD e_mail                AS CHAR FORMAT "x(25)"
       FIELD observacao            AS CHAR FORMAT "x(2000)"
       FIELD ep_cod_principal      AS INTEGER FORMAT ">>9"
       FIELD num_tip_operc         AS INTEGER FORMAT "9".
                                    

DEFINE TEMP-TABLE tt_retorno_clien_fornec NO-UNDO
       FIELD Ttv_cod_parameters                AS CHAR FORMAT "x(256)"
       FIELD Ttv_num_mensagem                  AS INTEGER FORMAT ">>>>,>>9"
       FIELD Ttv_des_mensagem                  AS CHARACTER FORMAT "X(52)"
       FIELD Ttv_des_ajuda                     AS CHARACTER FORMAT "X(256)"
       FIELD Ttv_cod_parameters_clien          AS CHARACTER FORMAT "x(2000)"
       FIELD Ttv_cod_parameters_fornec         AS CHARACTER FORMAT "x(2000)"
       FIELD Ttv_log_envdo                     AS LOGICAL FORMAT "Sim/Nao" INITIAL No
       FIELD Ttv_cod_parameters_clien_financ   AS CHARACTER FORMAT "x(2000)"
       FIELD Ttv_cod_parameters_fornec_financ  AS CHARACTER FORMAT "x(2000)"
       FIELD ttv_cod_parameters_pessoa_fisic   AS CHARACTER FORMAT "x(2000)"
       FIELD Ttv_cod_parameters_pessoa_jurid   AS CHARACTER FORMAT "x(2000)"
       FIELD Ttv_cod_parameters_estrut_clien   AS CHARACTER FORMAT "x(2000)"
       FIELD Ttv_cod_parameters_estrut_fornec  AS CHARACTER FORMAT "x(2000)"
       FIELD Ttv_cod_parameters_contat         AS CHARACTER FORMAT "x(2000)"
       FIELD Ttv_cod_parameters_repres         AS CHARACTER FORMAT "x(2000)"
       FIELD Ttv_cod_parameters_ender_entreg   AS CHARACTER FORMAT "x(2000)"
       FIELD Ttv_cod_parameters_pessoa_ativid  AS CHARACTER FORMAT "x(2000)"
       FIELD Ttv_cod_parameters_ramo_negoc     AS CHARACTER FORMAT "x(2000)"
       FIELD Ttv_cod_parameters_porte_pessoa   AS CHARACTER FORMAT "x(2000)" 
       FIELD Ttv_cod_parameters_idiom_pessoa   AS CHARACTER FORMAT "x(2000)"
       FIELD Ttv_cod_parameters_clas_contat    AS CHARACTER FORMAT "x(2000)"
       FIELD Ttv_cod_parameters_idiom_contat   AS CHARACTER FORMAT "x(2000)"
       FIELD Ttv_cod_parameters_telef          AS CHARACTER FORMAT "x(2000)"
       FIELD Ttv_cod_parameters_telef_pessoa   AS CHARACTER FORMAT "x(2000)"
       FIELD Ttv_cod_parameters_histor_clien   AS CHARACTER FORMAT "x(2000)"
       FIELD ttv_cod_parameters_histor_fornec  AS CHARACTER FORMAT "x(2000)".
