DEFINE VARIABLE iCodigoFunc AS INTEGER     NO-UNDO.

PROCEDURE setcodFunc:
    DEFINE INPUT  PARAMETER  iCod AS INTEGER     NO-UNDO.
    ASSIGN iCodigoFunc = iCod.
END PROCEDURE.

PROCEDURE getDadosFunc:
   FIND funcionario
       WHERE funcionario.cdn_funcionario = iCodigoFunc
       NO-LOCK NO-ERROR.
   IF AVAIL funcionario THEN DO:
       FIND rh_pessoa_fisic OF funcionario
           NO-LOCK NO-ERROR.
       IF AVAIL rh_pessoa_fisic THEN DO:


       END.



   END.



END.






/*
rela‡äes funcionarios

funcionario:
  x adc_ferias_13o OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x afast_pacien OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x alter_sit_benefic_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  x alter_jorn_trab OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  acidte_ambntal OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  analis_nec_trein OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  analis_prob_desemp OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  analis_prob_desemp_pessoa OF funcionario (cdn_empresa,cdn_estab,cdn_funcion
  anot_geral_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  apont_hora_aula OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  apont_hora_aula_histor OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  apont_hora_aula_mestre OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  apont_hora_aula_retroat OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  aprvdor_substto OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  aso_risco_ambntal OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x ativid_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x ativid_perf_profis OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x atstdo_aso OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x atstdo_medic OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  autoriz_hora_extra_compens OF funcionario (cdn_empresa,cdn_estab,cdn_funcio
  x aux_calc_dissidio_retroat OF funcionario (cdn_empresa,cdn_estab,cdn_funcion
  x aux_calc_irf OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x aux_calc_palim OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x aux_calc_rescis OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x aux_exec_rat_taref OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x aux_gerac_rais_meio_magnet OF funcionario (cdn_empresa,cdn_estab,cdn_funcio
  x aux_gerac_rat_taref OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  avaliac_epi_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  avaliac_integr OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  avpes_emitid OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  avpes_meta_indual OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  avpes_selec_avaldor OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  avpes_selec_avaliac OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  bco_hrs_compens_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x benefic_calc_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x benefic_calc_movto OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x benefic_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x bnfciar_palim OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x bnfciar_palim_det OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x bnfciar_seguro OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  brigdsta OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x calc_turno_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  candidato_cipa OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  candidato_elimda OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  candidato_inter OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  cart_vacina_depend OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x cat OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x cat_perf_profis OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  clien_depto_secao_sub_secao OF funcionario (cdn_empresa,cdn_estab,cdn_funci
  compon_cipa OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  compon_sesmt OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  compos_ambien_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  consmedic OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  consmedic_acidtrab OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x contrat_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x control_cheq_salario OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  control_proces_reclaman OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  x control_seq_calc OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  convid_reuniao_cipa OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  convoc_exam_medic OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  con_acidtrab_natur_lesao OF funcionario (cdn_empresa,cdn_estab,cdn_funciona
  cx_sugest_segur OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  defcncia_pacien OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  demand_ambntal OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  Z depend_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  desenv_habcomp_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  despes_provis_ccusto OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  det_calend_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  Z det_rescis OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  Z det_rescis_normal OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x dias_trabdo_benefic OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  Z dirf_anual_meio_magnet OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  efp_par_marcac_ptoelet OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  emprest_func_ccusto OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  emprest_func_localid OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  emprest_interv_jorn OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  emprest_turno_turma_trab OF funcionario (cdn_empresa,cdn_estab,cdn_funciona
  encam_ext OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  encam_ext_exam OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  encam_inter OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  entrev_desligto_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  epi_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  epi_liber_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  eqpto_perf_profis OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  erro_import_mpe OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  estabil_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  estab_transf_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  estoq_benefic_det OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  exam_pacien OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  exam_perf_profis OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  excec_benefic_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  excec_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  excec_func_afastdo_ppr OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  excec_func_motiv OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  faixa_param_calc_ppr_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcion
  Z ficha_acompto_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  ficha_medic OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  fisc_tar_agric OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  fonte_gerador_medid_func OF funcionario (cdn_empresa,cdn_estab,cdn_funciona
  formul_calc_bnfciar_palim OF funcionario (cdn_empresa,cdn_estab,cdn_funcion
  formul_calc_colab OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  formul_calc_depend_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  funcao_func_cipa OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  funcao_func_sind OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_acidte_trab OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_aux_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x func_calc OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x func_calc_compl OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x func_cargo_salario OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x func_categ_ptoelet OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x func_categ_sal OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x func_ccusto OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_cenar_prh OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_cpromis OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_curso_prh OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x func_demit OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_desenv_prh OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_diverg_ptoelet OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_docto_quiosq OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_emprest_financ OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_emprest_financ_det OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  func_estab_unid_negoc OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario
  x func_event_liq_neg OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_expos_agent_nociv OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  func_fpas OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_habcomp OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_habcomp_essencial OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  func_item_reqdo OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_lin_vale_transp OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_localid OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_lotac_interv_refei OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  func_nao_convoc OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_neces_trein OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_pagto_pasep OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_pagto_pis OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_pca OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_perspect OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_plano_habcomp OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_prev_movimen_sal OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario
  func_ptoelet OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_rat_ppr OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_reinteg OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_sat OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_sem_ferias_coletiv OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  Z func_sind_estab OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_substdo_req OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_sucess_potenc OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_sugest_prom OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_tip_mdo OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_tomador_serv OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  func_tomador_serv_ppp OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario
  func_trab_extra_ptoelet OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  func_treindo_cipa OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x func_turno_trab OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x func_unid_lotac_plano OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario
  func_vacina OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x grp_calc_colab OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x grp_event_movto OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  habilid_profis_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x habilit_ferias OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x habilit_ferias_simul OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x habilit_rescis OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x histor_alter_dirf OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x histor_ambien_trab_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  x histor_benefic OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  histor_candidato_inter OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  histor_candidato_pre_selec OF funcionario (cdn_empresa,cdn_estab,cdn_funcio
  histor_cargo OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  histor_comptcia_nao_recolhid OF funcionario (cdn_empresa,cdn_estab,cdn_func
  histor_contrat_func_temp OF funcionario (cdn_empresa,cdn_estab,cdn_funciona
  histor_contrib_sindic_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcio
  Z histor_det_remun_var_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcion
  histor_fasb_cmcac_fp OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  Z histor_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  histor_func_habcomp OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  histor_pagto_palim OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  histor_pagto_palim_diss OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  histor_remun_var_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario
  histor_req_candidato_etap OF funcionario (cdn_empresa,cdn_estab,cdn_funcion
  Z histor_sal_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  Z histor_solicit_adiant_13o OF funcionario (cdn_empresa,cdn_estab,cdn_funcion
  histor_tit_pagto OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  histor_utiliz_media_adc OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  histor_val_calc_palim OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario
  X histor_variac_sal_ferias OF funcionario (cdn_empresa,cdn_estab,cdn_funciona
  X histor_variac_sal_fp OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x histor_variac_sal_rescis OF funcionario (cdn_empresa,cdn_estab,cdn_funciona
  horar_jorn_trab_ptoelet OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  indcao_carreira OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  indcao_func_orcto OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  indcao_pos_suces OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  inform_cadast_fgts_re OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario
  x inform_dirf OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x inform_rais OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  integr_pend_dados OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  interhrou_reg_import OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  interv_movel_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x irf_rendto OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x irf_rendto_mestre OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  item_analis_exam_medic OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  jorn_altern_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  laudo_aposent_especial OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  liber_folha_pagto OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  liber_previa_salario OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  licenc_ambntal OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  local_dest_residuo OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  lote_movto_infor_fp OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  marcac_atraso_aces OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  marcac_ptoelet OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  marcac_pto_taref OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  matr_curso_assincr OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  mdcacao_pacien OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  movimen_quadro_desligto OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  x movimen_quadro_pessoal_trans OF funcionario (cdn_empresa,cdn_estab,cdn_func
  movimen_quadro_propost OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  movto_agric_det OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x movto_benefic OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x movto_benefic_cart OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x movto_calcul_colab OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x movto_calcul_colab_det OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  Z movto_calcul_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  Z movto_calcul_normal OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x movto_calc_retroat_desligto OF funcionario (cdn_empresa,cdn_estab,cdn_funci
  movto_colab_transito_fp OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  x movto_ferias_calcul OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x movto_fp_control_parc OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario
  movto_func_tomador_serv OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  movto_histor_ratdo_contrat OF funcionario (cdn_empresa,cdn_estab,cdn_funcio
  x movto_histor_rat_ccusto OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  movto_histor_rat_tomador OF funcionario (cdn_empresa,cdn_estab,cdn_funciona
  x movto_infor_fp OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x movto_integr_benefic_fp OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  movto_integr_taref OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  movto_interm_fasb_cmcac OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  movto_mpe_refeit OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x movto_pendcia_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  movto_ppr OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  movto_premio_taref OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  movto_ptoelet OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  movto_reserva_refeit OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  movto_turma_educnal OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x movto_vale_transp OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  normsegur_entreg_recibo OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  objorg OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  objorg_acao OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  objorg_acao_anom OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  objorg_pessoa_aces OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  objorg_sel_avaldor OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  objorg_sel_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  obs_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  oportun_fut OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  pagto_prestdor_serv OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  parte_atgdo OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  participan_prog_trein OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario
  partic_reuniao_brigincen OF funcionario (cdn_empresa,cdn_estab,cdn_funciona
  partic_reuniao_cipa OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  par_marcac_ptoelet OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  Z perc_rat_ccusto OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  period_aqst_ferias OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  permis_func_area_negoc OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  permis_func_usuar OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  plano_organ OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  prefer_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  prefer_func_ativid OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  prefer_func_regiao_definit OF funcionario (cdn_empresa,cdn_estab,cdn_funcio
  prefer_func_regiao_tmp OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  prev_func_dispon_funcao OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  proces_inss_especial OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  proces_inss_especial_cabec OF funcionario (cdn_empresa,cdn_estab,cdn_funcio
  produt_inter_avaldor OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  produt_inter_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  produt_inter_rh OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  programac_avaliac_medic OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  programac_pcmso_elimda OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  propost_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  propost_func_cargo OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  propost_liber_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  propost_projec_sal OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  ptoelet_excec_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  ptoelet_marcac OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  ptoelet_par_marcac OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  ptoelet_regra_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  quest_medic_pacien OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  Z rat_arq_movto_benefic OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario
  recta_medic OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  regra_permis_area_negoc OF funcionario (cdn_empresa,cdn_estab,cdn_funcionar
  regra_permis_estrut_reporte OF funcionario (cdn_empresa,cdn_estab,cdn_funci
  regra_permis_lotac OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  regra_permis_subor OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  regra_permis_usuar OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x reg_det_ferias OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  reg_det_ferias_simul OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x reg_ferias_normal OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x remun_var_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  restdo_exam_audtvo OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  restdo_exam_audtvo_aux OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  restdo_exam_espirom OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  restdo_exam_medic OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  restdo_exam_otolog OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  restdo_exam_raiox_1 OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  restdo_exam_visao OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  restdo_sub_item_exam OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  restric_trab_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  risco_perf_profis OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  ruido_nao_ocupac_exam OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario
  x sdo_fgts_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  selec_estado OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  seq_aux_calc_inss OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  seq_aux_calc_irf OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  serv_social_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x sit_afast_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  sit_calc_ptoelet_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario
  x sit_calc_rescis OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  x sit_calc_taref OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  sit_dia_ptoelet_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  sped_admis_prelim OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  sped_agendto_transf OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  sped_agent_risco_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario
  sped_epi_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  sped_exam_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  sped_func_reinteg OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  sped_ident_pos_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  sped_movto_msg OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  sped_pagto_inform OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  sped_proces_utiliz OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  sped_remun_multiplo_vinc OF funcionario (cdn_empresa,cdn_estab,cdn_funciona
  sped_remun_recibo OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  sped_sit_afast_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  sugest_matr_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  suplen_cipa OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  tar_concdo_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  time_trab_compon OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  trajet_prof_entid_educnal OF funcionario (cdn_empresa,cdn_estab,cdn_funcion
  transpdor_rural OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  trein_cipa_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  usuar_aplicat_rh OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  usuar_control_aces OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  vacina_aplic_func OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  vale_transp_ant OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  workflow_objorg OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  workflow_objorg_acao OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  sped_movto_rat_tomador OF funcionario (cdn_empresa,cdn_estab,cdn_funcionari
  sped_agendto_interv OF funcionario (cdn_empresa,cdn_estab,cdn_funcionario)
  funcionario OF aux_calc_irf (cdn_empresa,cdn_estab,cdn_funcionario)
  funcionario OF aux_exec_rat_taref (cdn_empresa,cdn_estab,cdn_funcionario,co
  funcionario OF aux_param_slr (cdn_empresa)
  funcionario OF cargo (cdn_cargo_basic,cdn_niv_cargo)
  funcionario OF cargo_basic (cdn_cargo_basic)
  funcionario OF cargo_turno_trab (cdn_cargo_basic,cdn_niv_cargo,cdn_turno_tr
  funcionario OF categ_habilit_calc_fp (cdn_empresa,cdn_estab,cdn_categ_sal,i
  funcionario OF categ_ptoelet (cdn_clas_func,cdn_empresa,cdn_estab,cdn_categ
  funcionario OF categ_sal (cdn_empresa,cdn_estab,cdn_categ_sal)
  funcionario OF clas_func (cdn_clas_func)
  funcionario OF clien_depto_secao_sub_secao (cdn_empresa,cdn_clien_rh,cdn_es
  funcionario OF competit_regiao_sal (cdn_cargo_basic,cdn_niv_cargo,cdn_regia
  funcionario OF compl_pessoa_fisic (num_pessoa_fisic)
  funcionario OF coorden_trein (num_pessoa_fisic)
  funcionario OF entrev_desligto_func (cdn_empresa,cdn_estab,cdn_funcionario)
  funcionario OF estab_localid (cdn_empresa,cdn_estab,cod_pais,cdn_localidade
  funcionario OF event_integr (cdn_empresa,cdn_estab,cdn_categ_sal,idi_tip_fu
  funcionario OF forma_pagto (cod_forma_pagto)
  funcionario OF fornec (cdn_empresa,cdn_fornecedor)
  funcionario OF func_aux_func (cdn_empresa,cdn_estab,cdn_funcionario)
  funcionario OF func_calc (cdn_empresa,cdn_estab,cdn_funcionario)
  funcionario OF func_calc_compl (cdn_empresa,cdn_estab,cdn_funcionario)
  funcionario OF func_cargo_salario (cdn_empresa,cdn_estab,cdn_funcionario)
  funcionario OF func_demit (cdn_empresa,cdn_estab,cdn_funcionario)
  funcionario OF func_ptoelet (cdn_empresa,cdn_estab,cdn_funcionario)
  funcionario OF habilit_rescis (cdn_empresa,cdn_estab,cdn_funcionario)
  funcionario OF histor_lotac_tomador_serv (cdn_empresa,cdn_clien_rh,cod_unid
  funcionario OF hrs_extra_projec_sal (cdn_empresa,cdn_estab,cdn_categ_sal)
  funcionario OF indic_mdo_mestre (cdn_empresa,cdn_estab,cdn_plano_lotac,cod_
  funcionario OF instrut_trein (num_pessoa_fisic)
  funcionario OF localidade (cod_pais,cdn_localidade)
  funcionario OF local_marcac_ptoelet (cdn_empresa,cdn_local_marcac_cartao_pt
  funcionario OF lotac_cargo (cdn_empresa,cdn_estab,cdn_cargo_basic,cdn_niv_c
  funcionario OF min_candidato_cargo (cdn_cargo_basic)
  funcionario OF niv_cargo (cdn_niv_cargo)
  funcionario OF orig_mdo_estab (cdn_empresa,cdn_estab,idi_orig_contratac_fun
  funcionario OF param_adm_trein (cdn_empresa)
  funcionario OF param_alerta_proces (cdn_empresa,cdn_estab)
  funcionario OF param_calc_categ_sal (cdn_empresa,cdn_estab,cdn_categ_sal)
  funcionario OF param_calc_empres_rh (cdn_empresa)
  funcionario OF param_calc_rh_estab (cdn_empresa,cdn_estab)
  funcionario OF param_control_aces (cdn_empresa)
  funcionario OF param_control_atraso (cdn_clas_func,cdn_empresa,cdn_estab,cd
  funcionario OF param_ctbz_unid_negoc (cdn_empresa,cod_unid_negoc)
  funcionario OF param_desenv_pessoal (cdn_empresa)
  funcionario OF param_empres_jsl (cdn_empresa)
  funcionario OF param_empres_rh (cdn_empresa)
  funcionario OF param_empres_tma (cdn_empresa)
  funcionario OF param_folha_educnal (cdn_empresa)
  funcionario OF param_gerencto_diretriz (cdn_empresa)
  funcionario OF param_gestao_ambntal (cdn_empresa)
  funcionario OF param_integr (cdn_empresa)
  funcionario OF param_integr_ext (cdn_empresa)
  funcionario OF param_medic_segur (cdn_empresa)
  funcionario OF param_modul_agric (cdn_empresa)
  funcionario OF param_proces_trab (cdn_empresa,cdn_estab)
  funcionario OF param_slr (cdn_empresa)
  funcionario OF param_slr_cargo (cdn_empresa,cdn_cargo_basic,cdn_niv_cargo)
  funcionario OF param_tip_guia (cdn_empresa,cdn_estab,cdn_fpas)
  funcionario OF pendcia_orig_mdo_estab (cdn_empresa,cdn_estab,idi_orig_contr
  funcionario OF pessoa_ext_trein (num_pessoa_fisic)
  funcionario OF plano_lotac (cdn_plano_lotac)
  funcionario OF plano_lotac_estab (cdn_empresa,cdn_estab,cdn_plano_lotac,dat
  funcionario OF portad (cdn_empresa,cod_portador)
  funcionario OF pos_desemp_matriz (cdn_empresa,cdn_estab,cdn_cargo_basic,cdn
  funcionario OF prestdor_serv (cdn_empresa,cdn_prestdor_serv)
  funcionario OF programac_calc_ptoelet (cdn_empresa,cdn_estab,cdn_turno_trab
  funcionario OF ptoelet_param (cdn_empresa,dat_inic_valid)
  funcionario OF rais_estab (cdn_empresa,cdn_estab,cdn_clien_rh)
  funcionario OF regiao_sal (cdn_regiao_sal)
  funcionario OF regiao_sal_estab (cdn_regiao_sal,cdn_empresa,cdn_estab)
  funcionario OF regra_liq_neg (cdn_empresa,cdn_estab,cdn_sindicato,cdn_cargo
  funcionario OF relacto_cod_retenc_estab (cdn_empresa,cdn_estab,cdn_retenc_i
  funcionario OF retenc_irf (cdn_retenc_irf)
  funcionario OF rh_ccusto (cdn_empresa,cod_rh_ccusto)
  funcionario OF rh_clien (cdn_empresa,cdn_clien_rh)
  funcionario OF rh_estab (cdn_empresa,cdn_estab)
  funcionario OF rh_estab_compl (cdn_empresa,cdn_estab)
  funcionario OF rh_estab_unid_negoc (cdn_empresa,cdn_estab,cod_unid_negoc)
  funcionario OF rh_pais (cod_pais)
  funcionario OF rh_pessoa_fisic (num_pessoa_fisic)
  funcionario OF rh_unid_federac (cod_pais,cod_unid_federac_rh)
  funcionario OF selec_estado (cdn_empresa,cdn_estab,cdn_funcionario)
  funcionario OF sindicato (cdn_sindicato)
  funcionario OF sind_cargo_orcto (cdn_cargo_basic,cdn_niv_cargo,cdn_empresa,
  funcionario OF sind_estab (cdn_empresa,cdn_estab,cdn_sindicato,dat_inic_val
  funcionario OF sind_tab_sal (cdn_empresa,cdn_tab_sal,cdn_sindicato)
  funcionario OF sped_admis_prelim (cdn_empresa,dat_admis_func,cod_id_feder)
  funcionario OF sped_agendto_transf (cdn_empresa,cdn_estab,cdn_funcionario,d
  funcionario OF sped_cargo_basic (cdn_cargo_basic)
  funcionario OF sped_estab_requis_epi (cdn_empresa,cdn_estab,dat_inic_valid)
  funcionario OF sped_param_tss (cdn_empresa,cdn_estab)
  funcionario OF sped_rh_estab (cdn_empresa,cdn_estab)
  funcionario OF sped_turno_trab (cdn_turno_trab)
  funcionario OF sped_unid_lotac_estab (cdn_empresa,cdn_estab,cdn_plano_lotac
  funcionario OF sped_vers_layout (cdn_empresa,cdn_estab,dat_inic_valid)
  funcionario OF tab_sal (cdn_empresa,cdn_tab_sal)
  funcionario OF tip_contrat (cdn_tip_contrat_func)
  funcionario OF tip_mdo (cod_tip_mdo)
  funcionario OF tip_mdo_despes (cdn_empresa,cod_tip_mdo)
  funcionario OF turma_trab (cdn_turno_trab,cdn_turma_trab)
  funcionario OF turno_trab (cdn_turno_trab)
  funcionario OF turno_trab_categ_sal (cdn_turno_trab,cdn_empresa,cdn_estab,c
  funcionario OF unid_lotac (cod_unid_lotac)
  funcionario OF unid_lotac_plano (cdn_plano_lotac,cod_unid_lotac)
  funcionario OF unid_negoc (cod_unid_negoc)
  funcionario OF vinc_empregat (cdn_vinc_empregat)
  funcionario OF param_adm_trein_aux (cdn_empresa)
  funcionario OF sped_param_taf (cdn_empresa,cdn_estab)
  funcionario OF sped_prestdor (cdn_empresa,cdn_prestdor_serv)
  
  
  
  
rela‡äes pessoa fisica  
rh_pessoa_fisic:
  advog_proces_trab OF rh_pessoa_fisic (num_pessoa_fisic)
  acidado_acidte_ambntal OF rh_pessoa_fisic (num_pessoa_fisic)
  analis_nec_trein OF rh_pessoa_fisic (num_pessoa_fisic)
  analis_prob_desemp OF rh_pessoa_fisic (num_pessoa_fisic)
  analis_prob_desemp_pessoa OF rh_pessoa_fisic (num_pessoa_fisic)
  ativid_perf_profis OF rh_pessoa_fisic (num_pessoa_fisic)
  atstdo_aso OF rh_pessoa_fisic (num_pessoa_fisic)
  avpes_emitid OF rh_pessoa_fisic (num_pessoa_fisic)
  avpes_meta_indual OF rh_pessoa_fisic (num_pessoa_fisic)
  bnfciar_palim OF rh_pessoa_fisic (num_pessoa_fisic)
  bolsista OF rh_pessoa_fisic (num_pessoa_fisic)
  candidato_elimda OF rh_pessoa_fisic (num_pessoa_fisic)
  candidato_ext OF rh_pessoa_fisic (num_pessoa_fisic)
  cat OF rh_pessoa_fisic (num_pessoa_fisic)
  cat_perf_profis OF rh_pessoa_fisic (num_pessoa_fisic)
  compl_pessoa_fisic OF rh_pessoa_fisic (num_pessoa_fisic)
  compon_cipa OF rh_pessoa_fisic (num_pessoa_fisic)
  conhecto_experien_pessoa OF rh_pessoa_fisic (num_pessoa_fisic)
  contrat_func OF rh_pessoa_fisic (num_pessoa_fisic)
  control_proces_reclaman OF rh_pessoa_fisic (num_pessoa_fisic)
  control_proces_testem OF rh_pessoa_fisic (num_pessoa_fisic)
  coorden_trein OF rh_pessoa_fisic (num_pessoa_fisic)
  cx_sugest_segur OF rh_pessoa_fisic (num_pessoa_fisic)
  demand_ambntal OF rh_pessoa_fisic (num_pessoa_fisic)
  depend_func OF rh_pessoa_fisic (num_pessoa_fisic)
  empres_ant_pessoa_fisic OF rh_pessoa_fisic (num_pessoa_fisic)
  eqpto_perf_profis OF rh_pessoa_fisic (num_pessoa_fisic)
  equipe_ambntal OF rh_pessoa_fisic (num_pessoa_fisic)
  estrut_reporte_pos_pessoa OF rh_pessoa_fisic (num_pessoa_fisic)
  exam_perf_profis OF rh_pessoa_fisic (num_pessoa_fisic)
  ficha_invent_trein OF rh_pessoa_fisic (num_pessoa_fisic)
  ficha_medic OF rh_pessoa_fisic (num_pessoa_fisic)
  funcionario OF rh_pessoa_fisic (num_pessoa_fisic)
  func_cargo_salario OF rh_pessoa_fisic (num_pessoa_fisic)
  func_neces_trein OF rh_pessoa_fisic (num_pessoa_fisic)
  func_perf_profis OF rh_pessoa_fisic (num_pessoa_fisic)
  func_respons_laudo OF rh_pessoa_fisic (num_pessoa_fisic)
  grp_mail_func OF rh_pessoa_fisic (num_pessoa_fisic)
  habcomp_pessoa_fisic OF rh_pessoa_fisic (num_pessoa_fisic)
  histor_req_candidato_etap OF rh_pessoa_fisic (num_pessoa_fisic)
  idiom_pessoa_fisic OF rh_pessoa_fisic (num_pessoa_fisic)
  instrut_trein OF rh_pessoa_fisic (num_pessoa_fisic)
  integr_novo_func OF rh_pessoa_fisic (num_pessoa_fisic)
  lista_inscr OF rh_pessoa_fisic (num_pessoa_fisic)
  matr_curso_assincr OF rh_pessoa_fisic (num_pessoa_fisic)
  objorg OF rh_pessoa_fisic (num_pessoa_fisic)
  objorg_acao OF rh_pessoa_fisic (num_pessoa_fisic)
  objorg_acao_anom OF rh_pessoa_fisic (num_pessoa_fisic)
  objorg_pessoa_aces OF rh_pessoa_fisic (num_pessoa_fisic)
  participan_prog_trein OF rh_pessoa_fisic (num_pessoa_fisic)
  pessoa_ext_trein OF rh_pessoa_fisic (num_pessoa_fisic)
  pessoa_fisic_docto_quiosq OF rh_pessoa_fisic (num_pessoa_fisic)
  plano_habcomp_pessoa_fisic OF rh_pessoa_fisic (num_pessoa_fisic)
  plano_organ OF rh_pessoa_fisic (num_pessoa_fisic)
  prestdor_serv_compon OF rh_pessoa_fisic (num_pessoa_fisic)
  req_etap_sel OF rh_pessoa_fisic (num_pessoa_fisic)
  rh_estab OF rh_pessoa_fisic (num_pessoa_fisic)
  risco_perf_profis OF rh_pessoa_fisic (num_pessoa_fisic)
  seq_aux_calc_inss OF rh_pessoa_fisic (num_pessoa_fisic)
  seq_aux_calc_irf OF rh_pessoa_fisic (num_pessoa_fisic)
  sped_bnfciar_prestdor_serv OF rh_pessoa_fisic (num_pessoa_fisic)
  sped_event_perdco OF rh_pessoa_fisic (num_pessoa_fisic)
  terc_cartao_pto OF rh_pessoa_fisic (num_pessoa_fisic)
  test_psicolog_pessoa OF rh_pessoa_fisic (num_pessoa_fisic)
  treindo_turma_trein OF rh_pessoa_fisic (num_pessoa_fisic)
  usuar_aplicat_rh OF rh_pessoa_fisic (num_pessoa_fisic)
  workflow_objorg OF rh_pessoa_fisic (num_pessoa_fisic)
  workflow_objorg_acao OF rh_pessoa_fisic (num_pessoa_fisic)
  participan_prog_trein_aux OF rh_pessoa_fisic (num_pessoa_fisic)
  rh_pessoa_fisic OF compl_pessoa_fisic (num_pessoa_fisic)
  rh_pessoa_fisic OF coorden_trein (num_pessoa_fisic)
  rh_pessoa_fisic OF grau_instruc (cdn_grau_instruc)
  rh_pessoa_fisic OF instrut_trein (num_pessoa_fisic)
  rh_pessoa_fisic OF pessoa_ext_trein (num_pessoa_fisic)
  rh_pessoa_fisic OF rh_pais (cod_pais)
  rh_pessoa_fisic OF rh_unid_federac (cod_pais,cod_unid_federac_rh)
  
  
*/
