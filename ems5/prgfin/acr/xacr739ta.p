/*****************************************************************************
** Copyright DATASUL S.A. (1994)
** Todos os Direitos Reservados.
** 
** Este fonte e de propriedade exclusiva da DATASUL, sua reproducao
** parcial ou total por qualquer meio, so' podera ser feita mediante
** autorizacao expressa.
**
** Programa..............: dlg_tit_acr_destinac
** Descricao.............: Seleá∆o Manual T°tulos para Destinaá∆o
** Versao................:  1.00.00.011
** Procedimento..........: tar_gerar_destinac_cobr
** Nome Externo..........: prgfin/acr/acr739ta.p
** Data Geracao..........: 26/05/2005 - 11:31:52
** Criado por............: Alexsandra
** Criado em.............: 18/10/1996 15:21:37
** Alterado por..........: bre18732
** Alterado em...........: 03/06/2004 09:52:27
** Gerado por............: FUT1147_2
*****************************************************************************/

def var c-versao-prg as char initial " 1.00.00.011":U no-undo.

{include/i_dbinst.i}
{include/i_dbtype.i}
{include/i_fcldef.i}


/******************************* Private-Data *******************************/
assign this-procedure:private-data = "HLP=37":U.
/*************************************  *************************************/

&if "{&emsfin_dbinst}" <> "yes" &then
run pi_messages (input "show",
                 input 5884,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", 
                                    "EMSFIN")) /*msg_5884*/.
&elseif "{&emsfin_version}" < "5.01" &then
run pi_messages (input "show",
                 input 5009,
                 input substitute ("&1~&2~&3~&4~&5~&6~&7~&8~&9", 
                                    "DLG_TIT_ACR_DESTINAC","~~EMSFIN", "~~{~&emsfin_version}", "~~5.01")) /*msg_5009*/.
&else

/********************* Temporary Table Definition Begin *********************/

def shared temp-table tt_rpt_tit_acr_destinac        
    field ttv_rec_tit_acr                  as recid format ">>>>>>9"
    field ttv_rec_movto_tit_acr            as recid format ">>>>>>9"
    field tta_cod_estab                    as character format "x(3)" label "Estabelecimento" column-label "Estab"
    field tta_num_id_tit_acr               as integer format "9999999999" initial 0 label "Token Cta Receber" column-label "Token Cta Receber"
    field tta_cod_espec_docto              as character format "x(3)" label "EspÇcie Documento" column-label "EspÇcie"
    field tta_cod_ser_docto                as character format "x(3)" label "SÇrie Documento" column-label "SÇrie"
    field tta_cod_parcela                  as character format "x(02)" label "Parcela" column-label "Parc"
    field tta_cdn_cliente                  as Integer format ">>>,>>>,>>9" initial 0 label "Cliente" column-label "Cliente"
    field tta_dat_emis_docto               as date format "99/99/9999" initial today label "Data  Emiss∆o" column-label "Dt Emiss∆o"
    field tta_dat_vencto_tit_acr           as date format "99/99/9999" initial ? label "Vencimento" column-label "Vencimento"
    field tta_val_sdo_tit_acr              as decimal format ">>>,>>>,>>9.99" decimals 2 initial 0 label "Saldo T°tulo" column-label "Saldo T°tulo"
    field tta_cod_portador                 as character format "x(5)" label "Portador" column-label "Portador"
    field tta_cod_cart_bcia                as character format "x(3)" label "Carteira" column-label "Carteira"
    field tta_cod_tit_acr                  as character format "x(10)" label "T°tulo" column-label "T°tulo"
    field tta_ind_trans_acr                as character format "X(29)" initial "Implantaá∆o" label "Transaá∆o" column-label "Transaá∆o"
    field ttv_log_selec_tit_acr            as logical format "Sim/N∆o" initial no label "Envia" column-label "Envia"
    field tta_num_dwb_order                as integer format ">>>>,>>9" initial 0 label "Ordem" column-label "Ordem"
    field ttv_des_obs_campo                as character format "x(30)" label "Observaá‰es" column-label "Observaá‰es"
    field ttv_log_elimina                  as logical format "Sim/N∆o" initial no label "Elimina"
    field ttv_cod_cep                      as character format "x(20)" label "CEP" column-label "CEP"
    field tta_cod_portad_prefer            as character format "x(5)" label "Portador Preferenc" column-label "Port Preferenc"
    field ttv_cod_order                    as character format "x(40)"
    field tta_cod_empresa                  as character format "x(3)" label "Empresa" column-label "Empresa"
    field tta_ind_ender_cobr               as character format "X(15)" initial "Cliente" label "Endereáo Cobranáa" column-label "Endereáo Cobranáa"
    field tta_cdn_repres                   as Integer format ">>>,>>9" initial 0 label "Representante" column-label "Representante"
    field tta_nom_abrev_contat             as character format "x(15)" label "Abreviado Contato" column-label "Abreviado Contato"
    field tta_nom_cidade                   as character format "x(32)" label "Cidade" column-label "Cidade"
    index tt_rpt_cliente_vencto_cod        is primary
          tta_cdn_cliente                  ascending
          tta_dat_vencto_tit_acr           ascending
          tta_cod_tit_acr                  ascending
          tta_cod_parcela                  ascending
    index tt_rpt_rec_tit_acr              
          ttv_rec_tit_acr                  ascending
    .



/********************** Temporary Table Definition End **********************/

/************************ Parameter Definition Begin ************************/

def output param p_cod_return
    as character
    format "x(40)"
    no-undo.
def Input param p_ind_classif_destinac
    as character
    format "X(100)"
    no-undo.


/************************* Parameter Definition End *************************/

/************************* Variable Definition Begin ************************/

def new global shared var v_cod_aplicat_dtsul_corren
    as character
    format "x(3)":U
    no-undo.
def new global shared var v_cod_ccusto_corren
    as character
    format "x(11)":U
    label "Centro Custo"
    column-label "Centro Custo"
    no-undo.
def new global shared var v_cod_dwb_user
    as character
    format "x(21)":U
    label "Usu†rio"
    column-label "Usu†rio"
    no-undo.
def new global shared var v_cod_empres_usuar
    as character
    format "x(3)":U
    label "Empresa"
    column-label "Empresa"
    no-undo.
def new global shared var v_cod_estab_usuar
    as character
    format "x(3)":U
    label "Estabelecimento"
    column-label "Estab"
    no-undo.
def new global shared var v_cod_funcao_negoc_empres
    as character
    format "x(50)":U
    no-undo.
def new global shared var v_cod_grp_usuar_lst
    as character
    format "x(3)":U
    label "Grupo Usu†rios"
    column-label "Grupo"
    no-undo.
def new global shared var v_cod_idiom_usuar
    as character
    format "x(8)":U
    label "Idioma"
    column-label "Idioma"
    no-undo.
def new global shared var v_cod_modul_dtsul_corren
    as character
    format "x(3)":U
    label "M¢dulo Corrente"
    column-label "M¢dulo Corrente"
    no-undo.
def new global shared var v_cod_modul_dtsul_empres
    as character
    format "x(100)":U
    no-undo.
def new global shared var v_cod_pais_empres_usuar
    as character
    format "x(3)":U
    label "Pa°s Empresa Usu†rio"
    column-label "Pa°s"
    no-undo.
def new global shared var v_cod_plano_ccusto_corren
    as character
    format "x(8)":U
    label "Plano CCusto"
    column-label "Plano CCusto"
    no-undo.
def new global shared var v_cod_unid_negoc_usuar
    as character
    format "x(3)":U
    view-as combo-box
    list-items ""
    inner-lines 5
    bgcolor 15 font 2
    label "Unidade Neg¢cio"
    column-label "Unid Neg¢cio"
    no-undo.
def new global shared var v_cod_usuar_corren
    as character
    format "x(12)":U
    label "Usu†rio Corrente"
    column-label "Usu†rio Corrente"
    no-undo.
def new global shared var v_cod_usuar_corren_criptog
    as character
    format "x(16)":U
    no-undo.
def var v_ind_classif_destinac_aux
    as character
    format "X(200)":U
    view-as radio-set Vertical
    radio-buttons "Estab/Portador/Carteira/Dt Emiss∆o/Dt Vencto/T°tulo/Parcela", "Estab/Portador/Carteira/Dt Emiss∆o/Dt Vencto/T°tulo/Parcela","Estab/Portador/Carteira/Cliente/Dt Emiss∆o/Dt Vencto/T°tulo/Parcela", "Estab/Portador/Carteira/Cliente/Dt Emiss∆o/Dt Vencto/T°tulo/Parcela", "Estab/Portador/Carteira/Dt Emiss∆o/T°tulo/Parcela","Estab/Portador/Carteira/Dt Emiss∆o/T°tulo/Parcela", "Estab/Portador/Carteira/Cliente/Dt Emiss∆o/T°tulo/Parcela", "Estab/Portador/Carteira/Cliente/Dt Emiss∆o/T°tulo/Parcela", "Cliente/Dt Vencto/T°tulo/Parcela", "Cliente/Dt Vencto/T°tulo/Parcela"
     /*l_vencto_tit_parc*/ /*l_vencto_tit_parc*/ /*l_vencto_clien_tit_parc*/ /*l_emis_tit_parcela*/ /*l_emis_tit_parcela*/ /*l_emis_tit_parcela*/ /*l_emis_clien_tit_parc*/ /*l_emis_clien_tit_parc*/ /*l_clien_classif*/ /*l_clien_classif*/
    bgcolor 8 
    no-undo.
def var v_log_method
    as logical
    format "Sim/N∆o"
    initial yes
    no-undo.
def var v_log_save
    as logical
    format "Sim/N∆o"
    initial no
    view-as toggle-box
    no-undo.
def var v_log_save_ok
    as logical
    format "Sim/N∆o"
    initial no
    no-undo.
def var v_nom_prog_appc
    as character
    format "x(50)":U
    label "Programa APPC"
    column-label "Programa APPC"
    no-undo.
def var v_nom_prog_dpc
    as character
    format "x(50)":U
    label "Programa Dpc"
    column-label "Programa Dpc"
    no-undo.
def var v_nom_prog_upc
    as character
    format "X(50)":U
    label "Programa UPC"
    column-label "Programa UPC"
    no-undo.
def var v_nom_table_epc
    as character
    format "x(30)":U
    no-undo.
def var v_nom_title_aux
    as character
    format "x(60)":U
    no-undo.
def var v_num_count
    as integer
    format ">>>>,>>9":U
    no-undo.
def new global shared var v_num_ped_exec_corren
    as integer
    format ">>>>9":U
    no-undo.
def var v_rec_log
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_rec_table
    as recid
    format ">>>>>>9":U
    initial ?
    no-undo.
def var v_rec_table_epc
    as recid
    format ">>>>>>9":U
    no-undo.
def new global shared var v_rec_tit_acr
    as recid
    format ">>>>>>9":U
    no-undo.
def var v_val_tot_selec
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Total Selecionado"
    column-label "Total Selecionado"
    no-undo.
def var v_val_tot_tit_acr
    as decimal
    format "->>,>>>,>>>,>>9.99":U
    decimals 2
    label "Total T°tulos"
    column-label "Total"
    no-undo.
def var v_wgh_focus
    as widget-handle
    format ">>>>>>9":U
    no-undo.
def var v_wgh_frame_epc
    as widget-handle
    format ">>>>>>9":U
    no-undo.


/************************** Variable Definition End *************************/

/*************************** Menu Definition Begin **************************/

.

def menu      m_help                menubar
    menu-item mi_conteudo           label "&Conte£do"
    menu-item mi_sobre              label "&Sobre".



/**************************** Menu Definition End ***************************/

/************************** Query Definition Begin **************************/

def query qr_dlg_tit_acr_destinac_cobr
    for tt_rpt_tit_acr_destinac,
        cliente
    scrolling.


/*************************** Query Definition End ***************************/

/************************** Browse Definition Begin *************************/

def browse br_dlg_tit_acr_destinac_cobr query qr_dlg_tit_acr_destinac_cobr display 
    tt_rpt_tit_acr_destinac.tta_cod_portador
    width-chars 05.00
        column-label "Port"
    tt_rpt_tit_acr_destinac.tta_cod_cart_bcia
    width-chars 03.00
        column-label "Cart"
    tt_rpt_tit_acr_destinac.tta_cod_estab
    width-chars 03.00
        column-label "Est"
    tt_rpt_tit_acr_destinac.tta_cod_espec_docto
    width-chars 03.00
        column-label "Esp"
    tt_rpt_tit_acr_destinac.tta_cod_ser_docto
    width-chars 03.00
        column-label "Ser"
    tt_rpt_tit_acr_destinac.tta_cod_tit_acr
    width-chars 10.00
        column-label "T°tulo"
    tt_rpt_tit_acr_destinac.tta_cod_parcela
    width-chars 02.00
        column-label "/P"
    tt_rpt_tit_acr_destinac.tta_cdn_cliente
    width-chars 09.00
        column-label "Cliente"
    cliente.nom_abrev
    width-chars 15.00
        column-label "Nome Abreviado"
    tt_rpt_tit_acr_destinac.tta_dat_emis_docto
    width-chars 10.00
        column-label "Emiss∆o"
    tt_rpt_tit_acr_destinac.tta_dat_vencto_tit_acr
    width-chars 10.00
        column-label "Vencto"
    tt_rpt_tit_acr_destinac.tta_val_sdo_tit_acr
    width-chars 12.00
        column-label "Saldo"
    with no-box separators multiple 
         size 85.72 by 10.38
         font 1
         bgcolor 15.


/*************************** Browse Definition End **************************/

/************************ Rectangle Definition Begin ************************/

def rectangle rt_001
    size 1 by 1
    edge-pixels 2.
def rectangle rt_cxcf
    size 1 by 1
    fgcolor 1 edge-pixels 2.


/************************* Rectangle Definition End *************************/

/************************** Button Definition Begin *************************/

def button bt_can
    label "Cancela"
    tooltip "Cancela"
    size 1 by 1
    auto-endkey.
def button bt_deseleciona_todos_a_sel
    label "Nenhum"
    tooltip "Desmarca Todas Ocorràncias"
    size 1 by 1.
def button bt_hel2
    label "Ajuda"
    tooltip "Ajuda"
    size 1 by 1.
def button bt_ok
    label "OK"
    tooltip "OK"
    size 1 by 1
    auto-go.
def button bt_seleciona_todos_a_selecionar
    label "Todos"
    tooltip "Marca Todas Ocorràncias"
    size 1 by 1.
/****************************** Function Button *****************************/


/*************************** Button Definition End **************************/

/************************** Frame Definition Begin **************************/

def frame f_dlg_03_tit_acr_destinac
    rt_001
         at row 01.42 col 02.00
    " Classificaá∆o " view-as text
         at row 01.12 col 04.00 bgcolor 8 
    rt_cxcf
         at row 18.17 col 02.00 bgcolor 7 
    v_ind_classif_destinac_aux
         at row 01.88 col 04.14 no-label
         view-as radio-set Vertical
         radio-buttons "Estab/Portador/Carteira/Dt Emiss∆o/Dt Vencto/T°tulo/Parcela", "Estab/Portador/Carteira/Dt Emiss∆o/Dt Vencto/T°tulo/Parcela","Estab/Portador/Carteira/Cliente/Dt Emiss∆o/Dt Vencto/T°tulo/Parcela", "Estab/Portador/Carteira/Cliente/Dt Emiss∆o/Dt Vencto/T°tulo/Parcela", "Estab/Portador/Carteira/Dt Emiss∆o/T°tulo/Parcela","Estab/Portador/Carteira/Dt Emiss∆o/T°tulo/Parcela", "Estab/Portador/Carteira/Cliente/Dt Emiss∆o/T°tulo/Parcela", "Estab/Portador/Carteira/Cliente/Dt Emiss∆o/T°tulo/Parcela", "Cliente/Dt Vencto/T°tulo/Parcela", "Cliente/Dt Vencto/T°tulo/Parcela"
          /*l_vencto_tit_parc*/ /*l_vencto_tit_parc*/ /*l_vencto_clien_tit_parc*/ /*l_emis_tit_parcela*/ /*l_emis_tit_parcela*/ /*l_emis_tit_parcela*/ /*l_emis_clien_tit_parc*/ /*l_emis_clien_tit_parc*/ /*l_clien_classif*/ /*l_clien_classif*/
         bgcolor 8 
    br_dlg_tit_acr_destinac_cobr
         at row 06.38 col 02.00
    bt_seleciona_todos_a_selecionar
         at row 17.00 col 03.00 font ?
         help "Marca Todas Ocorràncias"
    bt_deseleciona_todos_a_sel
         at row 17.00 col 14.00 font ?
         help "Desmarca Todas Ocorràncias"
    v_val_tot_selec
         at row 17.00 col 66.57 colon-aligned label "Total Selecionado"
         help "Total Selecionado"
         view-as fill-in
         size-chars 19.14 by .88
         fgcolor ? bgcolor 15 font 2
    bt_ok
         at row 18.38 col 03.00 font ?
         help "OK"
    bt_can
         at row 18.38 col 14.00 font ?
         help "Cancela"
    bt_hel2
         at row 18.38 col 76.57 font ?
         help "Ajuda"
    with 1 down side-labels no-validate keep-tab-order three-d
         size-char 89.00 by 20.00 default-button bt_ok
         view-as dialog-box
         font 1 fgcolor ? bgcolor 8
         title "T°tulo Contas a Receber".
    /* adjust size of objects in this frame */
    assign bt_can:width-chars                           in frame f_dlg_03_tit_acr_destinac = 10.00
           bt_can:height-chars                          in frame f_dlg_03_tit_acr_destinac = 01.00
           bt_deseleciona_todos_a_sel:width-chars       in frame f_dlg_03_tit_acr_destinac = 10.00
           bt_deseleciona_todos_a_sel:height-chars      in frame f_dlg_03_tit_acr_destinac = 01.00
           bt_hel2:width-chars                          in frame f_dlg_03_tit_acr_destinac = 10.00
           bt_hel2:height-chars                         in frame f_dlg_03_tit_acr_destinac = 01.00
           bt_ok:width-chars                            in frame f_dlg_03_tit_acr_destinac = 10.00
           bt_ok:height-chars                           in frame f_dlg_03_tit_acr_destinac = 01.00
           bt_seleciona_todos_a_selecionar:width-chars  in frame f_dlg_03_tit_acr_destinac = 10.00
           bt_seleciona_todos_a_selecionar:height-chars in frame f_dlg_03_tit_acr_destinac = 01.00
           rt_001:width-chars                           in frame f_dlg_03_tit_acr_destinac = 85.72
           rt_001:height-chars                          in frame f_dlg_03_tit_acr_destinac = 04.83
           rt_cxcf:width-chars                          in frame f_dlg_03_tit_acr_destinac = 85.57
           rt_cxcf:height-chars                         in frame f_dlg_03_tit_acr_destinac = 01.42.
    /* set private-data for the help system */
    assign v_ind_classif_destinac_aux:private-data      in frame f_dlg_03_tit_acr_destinac = "HLP=000020520":U
           br_dlg_tit_acr_destinac_cobr:private-data    in frame f_dlg_03_tit_acr_destinac = "HLP=000020520":U
           bt_seleciona_todos_a_selecionar:private-data in frame f_dlg_03_tit_acr_destinac = "HLP=000013366":U
           bt_deseleciona_todos_a_sel:private-data      in frame f_dlg_03_tit_acr_destinac = "HLP=000013362":U
           v_val_tot_selec:private-data                 in frame f_dlg_03_tit_acr_destinac = "HLP=000025001":U
           bt_ok:private-data                           in frame f_dlg_03_tit_acr_destinac = "HLP=000010721":U
           bt_can:private-data                          in frame f_dlg_03_tit_acr_destinac = "HLP=000011050":U
           bt_hel2:private-data                         in frame f_dlg_03_tit_acr_destinac = "HLP=000011326":U
           frame f_dlg_03_tit_acr_destinac:private-data                                    = "HLP=000020520".



{include/i_fclfrm.i f_dlg_03_tit_acr_destinac }
/*************************** Frame Definition End ***************************/

/*********************** User Interface Trigger Begin ***********************/


ON MOUSE-EXTEND-CLICK OF br_dlg_tit_acr_destinac_cobr IN FRAME f_dlg_03_tit_acr_destinac
DO:

    /************************* Variable Definition Begin ************************/

    def var v_num_row_a
        as integer
        format ">>>,>>9":U
        no-undo.


    /************************** Variable Definition End *************************/

    assign v_val_tot_selec = 0.
    cheq_devol:
    do v_num_row_a = 1 to browse br_dlg_tit_acr_destinac_cobr:num-selected-rows:
       assign v_log_method = browse br_dlg_tit_acr_destinac_cobr:fetch-selected-row(v_num_row_a).
       if  avail tt_rpt_tit_acr_destinac
       then do:
           assign v_val_tot_selec = v_val_tot_selec + tt_rpt_tit_acr_destinac.tta_val_sdo_tit_acr.
       end /* if */.
    end /* do cheq_devol */.
    display v_val_tot_selec
            with frame f_dlg_03_tit_acr_destinac.

END. /* ON MOUSE-EXTEND-CLICK OF br_dlg_tit_acr_destinac_cobr IN FRAME f_dlg_03_tit_acr_destinac */

ON MOUSE-SELECT-CLICK OF br_dlg_tit_acr_destinac_cobr IN FRAME f_dlg_03_tit_acr_destinac
DO:

    /************************* Variable Definition Begin ************************/

    def var v_num_row_a                      as integer         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    apply "mouse-extend-click" to self.

END. /* ON MOUSE-SELECT-CLICK OF br_dlg_tit_acr_destinac_cobr IN FRAME f_dlg_03_tit_acr_destinac */

ON " " OF br_dlg_tit_acr_destinac_cobr IN FRAME f_dlg_03_tit_acr_destinac
DO:

    apply "mouse-extend-click" to self.
END. /* ON " " OF br_dlg_tit_acr_destinac_cobr IN FRAME f_dlg_03_tit_acr_destinac */

ON CHOOSE OF bt_deseleciona_todos_a_sel IN FRAME f_dlg_03_tit_acr_destinac
DO:

    if  br_dlg_tit_acr_destinac_cobr:num-selected-rows > 0
    then do:
        assign v_log_method = browse br_dlg_tit_acr_destinac_cobr:deselect-rows().
    end /* if */.

    for each tt_rpt_tit_acr_destinac no-lock:
        assign v_val_tot_selec = 0.
    end.
    display v_val_tot_selec
            with frame f_dlg_03_tit_acr_destinac.
END. /* ON CHOOSE OF bt_deseleciona_todos_a_sel IN FRAME f_dlg_03_tit_acr_destinac */

ON CHOOSE OF bt_hel2 IN FRAME f_dlg_03_tit_acr_destinac
DO:


    /* Begin_Include: i_context_help_frame */
    run prgtec/men/men900za.py (Input self:frame,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.


    /* End_Include: i_context_help_frame */

END. /* ON CHOOSE OF bt_hel2 IN FRAME f_dlg_03_tit_acr_destinac */

ON CHOOSE OF bt_ok IN FRAME f_dlg_03_tit_acr_destinac
DO:

    get first qr_dlg_tit_acr_destinac_cobr no-lock.
    if  avail tt_rpt_tit_acr_destinac
    then do:
        reposition qr_dlg_tit_acr_destinac_cobr to recid recid(tt_rpt_tit_acr_destinac).
        block:
        do v_num_count = 1 to br_dlg_tit_acr_destinac_cobr:num-selected-rows:
           assign v_log_method=br_dlg_tit_acr_destinac_cobr:fetch-selected-row(v_num_count).
           assign tt_rpt_tit_acr_destinac.ttv_log_elimina        = no.
        end /* do block */.
    end /* if */.

    /* Elimina da TabTemp os t°tulos que n∆o foram selecionados. */
    block:
    for each tt_rpt_tit_acr_destinac where tt_rpt_tit_acr_destinac.ttv_log_elimina = yes and
       tt_rpt_tit_acr_destinac.ttv_log_selec_tit_acr = yes:
       delete tt_rpt_tit_acr_destinac.
    end /* for block */.

    assign p_cod_return = "OK" /*l_ok*/ .
END. /* ON CHOOSE OF bt_ok IN FRAME f_dlg_03_tit_acr_destinac */

ON CHOOSE OF bt_seleciona_todos_a_selecionar IN FRAME f_dlg_03_tit_acr_destinac
DO:

    /************************* Variable Definition Begin ************************/

    def var v_num_count                      as integer         no-undo. /*local*/
    def var v_num_lin                        as integer         no-undo. /*local*/


    /************************** Variable Definition End *************************/

    assign v_log_method = session:set-wait-state('general').

    assign v_num_lin =  br_dlg_tit_acr_destinac_cobr:num-iterations.
    apply 'home' to  br_dlg_tit_acr_destinac_cobr in frame f_dlg_03_tit_acr_destinac.
    do  v_num_count = 1 to v_num_lin:
        if  br_dlg_tit_acr_destinac_cobr:is-row-selected(v_num_lin) then leave.
        if  br_dlg_tit_acr_destinac_cobr:select-row(v_num_count) then.
        if  v_num_count mod v_num_lin = 0 then do:
            apply 'page-down' to br_dlg_tit_acr_destinac_cobr in frame f_dlg_03_tit_acr_destinac.
            assign v_num_count = 0.
        end.  
    end.  

    assign v_log_method = session:set-wait-state('').
    assign v_val_tot_selec = 0.
    for each tt_rpt_tit_acr_destinac no-lock:
        assign v_val_tot_selec = v_val_tot_selec + tt_rpt_tit_acr_destinac.tta_val_sdo_tit_acr.
    end.
    display v_val_tot_selec
            with frame f_dlg_03_tit_acr_destinac.

END. /* ON CHOOSE OF bt_seleciona_todos_a_selecionar IN FRAME f_dlg_03_tit_acr_destinac */

ON VALUE-CHANGED OF v_ind_classif_destinac_aux IN FRAME f_dlg_03_tit_acr_destinac
DO:

    assign p_ind_classif_destinac = input frame f_dlg_03_tit_acr_destinac v_ind_classif_destinac_aux
           v_val_tot_selec = 0.
    display v_val_tot_selec
            with frame f_dlg_03_tit_acr_destinac.
    run pi_open_query_tit_acr_destinac.


END. /* ON VALUE-CHANGED OF v_ind_classif_destinac_aux IN FRAME f_dlg_03_tit_acr_destinac */


/************************ User Interface Trigger End ************************/

/**************************** Frame Trigger Begin ***************************/


ON HELP OF FRAME f_dlg_03_tit_acr_destinac ANYWHERE
DO:


    /* Begin_Include: i_context_help */
    run prgtec/men/men900za.py (Input self:handle,
                                Input this-procedure:handle) /*prg_fnc_chamar_help_context*/.
    /* End_Include: i_context_help */

END. /* ON HELP OF FRAME f_dlg_03_tit_acr_destinac */

ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_03_tit_acr_destinac ANYWHERE
DO:

    /************************* Variable Definition Begin ************************/

    def var v_wgh_frame
        as widget-handle
        format ">>>>>>9":U
        no-undo.


    /************************** Variable Definition End *************************/


    /* Begin_Include: i_right_mouse_down_dialog_box */
    if  (self:type <> "DIALOG-BOX" /*l_dialog_box*/ )
    and (self:type <> "FRAME" /*l_frame*/      )
    and (self:type <> "text" /*l_text*/       )
    and (self:type <> "IMAGE" /*l_image*/      )
    and (self:type <> "RECTANGLE" /*l_rectangle*/  )
    then do:

        assign v_wgh_frame = self:parent.

        if  self:type        = "fill-in" /*l_fillin*/ 
        and v_wgh_frame:type = "Browse" /*l_browse*/  then
            return no-apply.

        if  valid-handle(self:popup-menu) = yes then
            return no-apply.

        assign v_wgh_frame = self:frame.

        if  (v_wgh_frame:type <> "DIALOG-BOX" /*l_dialog_box*/ ) and (v_wgh_frame:frame <> ?)
        then do:
               assign v_wgh_frame     = v_wgh_frame:frame.
        end /* if */.
        assign v_nom_title_aux    = v_wgh_frame:title
               v_wgh_frame:title  = self:help.
    end /* if */.
    /* End_Include: i_right_mouse_down_dialog_box */

END. /* ON RIGHT-MOUSE-DOWN OF FRAME f_dlg_03_tit_acr_destinac */

ON RIGHT-MOUSE-UP OF FRAME f_dlg_03_tit_acr_destinac ANYWHERE
DO:

    /************************* Variable Definition Begin ************************/

    def var v_wgh_frame
        as widget-handle
        format ">>>>>>9":U
        no-undo.


    /************************** Variable Definition End *************************/


    /* Begin_Include: i_right_mouse_up_dialog_box */
    if  (self:type <> "DIALOG-BOX" /*l_dialog_box*/ )
    and (self:type <> "FRAME" /*l_frame*/      )
    and (self:type <> "text" /*l_text*/       )
    and (self:type <> "IMAGE" /*l_image*/      )
    and (self:type <> "RECTANGLE" /*l_rectangle*/  )
    then do:

        assign v_wgh_frame = self:parent.

        if  self:type        = "fill-in" /*l_fillin*/ 
        and v_wgh_frame:type = "Browse" /*l_browse*/  then
            return no-apply.

        if  valid-handle(self:popup-menu) = yes then
            return no-apply.

        assign v_wgh_frame        = self:frame.
        if  (v_wgh_frame:type <> "DIALOG-BOX" /*l_dialog_box*/ ) and (v_wgh_frame:frame <> ?)
        then do:
               assign v_wgh_frame     = v_wgh_frame:frame.
        end /* if */.
        assign v_wgh_frame:title  = v_nom_title_aux.
    end /* if */.

    /* End_Include: i_right_mouse_up_dialog_box */

END. /* ON RIGHT-MOUSE-UP OF FRAME f_dlg_03_tit_acr_destinac */

ON WINDOW-CLOSE OF FRAME f_dlg_03_tit_acr_destinac
DO:

    apply "end-error" to self.

END. /* ON WINDOW-CLOSE OF FRAME f_dlg_03_tit_acr_destinac */


/***************************** Frame Trigger End ****************************/

/**************************** Menu Trigger Begin ****************************/


ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help
DO:


        apply "choose" to bt_hel2 in frame f_dlg_03_tit_acr_destinac.





END. /* ON CHOOSE OF MENU-ITEM mi_conteudo IN MENU m_help */

ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help
DO:

    /************************* Variable Definition Begin ************************/

    def var v_cod_release
        as character
        format "x(12)":U
        no-undo.
    def var v_nom_prog
        as character
        format "x(8)":U
        no-undo.
    def var v_nom_prog_ext
        as character
        format "x(8)":U
        label "Nome Externo"
        no-undo.


    /************************** Variable Definition End *************************/


        assign v_nom_prog     = substring(frame f_dlg_03_tit_acr_destinac:title, 1, max(1, length(frame f_dlg_03_tit_acr_destinac:title) - 10)).
        if  v_nom_prog = ? then
            assign v_nom_prog = "".

        assign v_nom_prog     = v_nom_prog
                              + chr(10)
                              + "dlg_tit_acr_destinac":U.




    assign v_nom_prog_ext = "prgfin/acr/acr739ta.p":U
           v_cod_release  = trim(" 1.00.00.011":U).
    run prgtec/btb/btb901zb.p (Input v_nom_prog,
                               Input v_nom_prog_ext,
                               Input v_cod_release) /*prg_fnc_about*/.
END. /* ON CHOOSE OF MENU-ITEM mi_sobre IN MENU m_help */


/***************************** Menu Trigger End *****************************/


/****************************** Main Code Begin *****************************/


/* Begin_Include: i_version_extract */
def new global shared var v_cod_arq
    as char  
    format 'x(60)'
    no-undo.
def new global shared var v_cod_tip_prog
    as character
    format 'x(8)'
    no-undo.

def stream s-arq.

if  v_cod_arq <> '' and v_cod_arq <> ?
then do:
    run pi_version_extract ('dlg_tit_acr_destinac':U, 'prgfin/acr/acr739ta.p':U, '1.00.00.011':U, 'pro':U).
end /* if */.
/* End_Include: i_version_extract */

if  search("prgtec/btb/btb906za.r") = ? and search("prgtec/btb/btb906za.py") = ? then do:
    if  v_cod_dwb_user begins 'es_' then
        return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgtec/btb/btb906za.py".
    else do:
        message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgtec/btb/btb906za.py"
               view-as alert-box error buttons ok.
        stop.
    end.
end.
else
    run prgtec/btb/btb906za.py /*prg_fnc_verify_controls*/.

/* Begin_Include: i_verify_security */
if  search("prgtec/men/men901za.r") = ? and search("prgtec/men/men901za.py") = ? then do:
    if  v_cod_dwb_user begins 'es_' then
        return "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  + "prgtec/men/men901za.py".
    else do:
        message "Programa execut†vel n∆o foi encontrado:" /*l_programa_nao_encontrado*/  "prgtec/men/men901za.py"
               view-as alert-box error buttons ok.
        return.
    end.
end.
else
    run prgtec/men/men901za.py (Input 'dlg_tit_acr_destinac') /*prg_fnc_verify_security*/.
if  return-value = "2014"
then do:
    /* Programa a ser executado n∆o Ç um programa v†lido Datasul ! */
    run pi_messages (input "show",
                     input 2014,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'dlg_tit_acr_destinac')) /*msg_2014*/.
    return.
end /* if */.
if  return-value = "2012"
then do:
    /* Usu†rio sem permiss∆o para acessar o programa. */
    run pi_messages (input "show",
                     input 2012,
                     input substitute ("&1~~&2~~&3~~&4~~&5~~&6~~&7~~&8~~&9",
                                       'dlg_tit_acr_destinac')) /*msg_2012*/.
    return.
end /* if */.
/* End_Include: i_verify_security */



/* Begin_Include: i_log_exec_prog_dtsul_ini */
assign v_rec_log = ?.

if can-find(prog_dtsul
       where prog_dtsul.cod_prog_dtsul = 'dlg_tit_acr_destinac' 
         and prog_dtsul.log_gera_log_exec = yes) then do transaction:
    create log_exec_prog_dtsul.
    assign log_exec_prog_dtsul.cod_prog_dtsul           = 'dlg_tit_acr_destinac'
           log_exec_prog_dtsul.cod_usuario              = v_cod_usuar_corren
           log_exec_prog_dtsul.dat_inic_exec_prog_dtsul = today
           log_exec_prog_dtsul.hra_inic_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").
    assign v_rec_log = recid(log_exec_prog_dtsul).
    release log_exec_prog_dtsul no-error.
end.


/* End_Include: i_log_exec_prog_dtsul_ini */


/* Begin_Include: i_verify_program_epc */
&if '{&emsbas_version}' > '1.00' &then
assign v_rec_table_epc = ?
       v_wgh_frame_epc = ?.

find prog_dtsul
    where prog_dtsul.cod_prog_dtsul = "dlg_tit_acr_destinac":U
    no-lock no-error.
if  avail prog_dtsul then do:
    if  prog_dtsul.nom_prog_upc <> ''
    and prog_dtsul.nom_prog_upc <> ? then
        assign v_nom_prog_upc = prog_dtsul.nom_prog_upc.
    if  prog_dtsul.nom_prog_appc <> ''
    and prog_dtsul.nom_prog_appc <> ? then
        assign v_nom_prog_appc = prog_dtsul.nom_prog_appc.
&if '{&emsbas_version}' > '5.00' &then
    if  prog_dtsul.nom_prog_dpc <> ''
    and prog_dtsul.nom_prog_dpc <> ? then
        assign v_nom_prog_dpc = prog_dtsul.nom_prog_dpc.
&endif
end.


assign v_wgh_frame_epc = frame f_dlg_03_tit_acr_destinac:handle.



assign v_nom_table_epc = 'tit_acr':U
       v_rec_table_epc = recid(tit_acr).

&endif

/* End_Include: i_verify_program_epc */


/* ix_p00_dlg_tit_acr_destinac */

/* redefiniá‰es do frame */

/* Begin_Include: i_std_dialog_box */
/* tratamento do titulo e vers∆o */
assign frame f_dlg_03_tit_acr_destinac:title = frame f_dlg_03_tit_acr_destinac:title
                            + chr(32)
                            + chr(40)
                            + trim(" 1.00.00.011":U)
                            + chr(41).
/* menu pop-up de ajuda e sobre */
assign menu m_help:popup-only = yes
       bt_hel2:popup-menu in frame f_dlg_03_tit_acr_destinac = menu m_help:handle.


/* End_Include: i_std_dialog_box */


pause 0 before-hide.
view frame f_dlg_03_tit_acr_destinac.

/* Begin_Include: i_exec_program_epc */
&if '{&emsbas_version}' > '1.00' &then
if  v_nom_prog_upc <> '' then
do:
    assign v_rec_table_epc = recid(tit_acr).
    run value(v_nom_prog_upc) (input 'INITIALIZE',
                               input 'viewer',
                               input this-procedure,
                               input v_wgh_frame_epc,
                               input v_nom_table_epc,
                               input v_rec_table_epc).
    if  'no' = 'yes'
    and return-value = 'NOK' then
        undo, retry.
end.

if  v_nom_prog_appc <> '' then
do:
    assign v_rec_table_epc = recid(tit_acr).
    run value(v_nom_prog_appc) (input 'INITIALIZE',
                                input 'viewer',
                                input this-procedure,
                                input v_wgh_frame_epc,
                                input v_nom_table_epc,
                                input v_rec_table_epc).
    if  'no' = 'yes'
    and return-value = 'NOK' then
        undo, retry.
end.

&if '{&emsbas_version}' > '5.00' &then
if  v_nom_prog_dpc <> '' then
do:
    assign v_rec_table_epc = recid(tit_acr).
    run value(v_nom_prog_dpc) (input 'INITIALIZE',
                                input 'viewer',
                                input this-procedure,
                                input v_wgh_frame_epc,
                                input v_nom_table_epc,
                                input v_rec_table_epc).
    if  'no' = 'yes'
    and return-value = 'NOK' then
        undo, retry.
end.
&endif
&endif
/* End_Include: i_exec_program_epc */


main_block:
do on error undo main_block, retry main_block:
    /* ix_p10_dlg_tit_acr_destinac */
    assign v_log_save        = yes
           v_log_save_ok     = no.




    find tit_acr exclusive-lock no-error.

/* ix_p15_dlg_tit_acr_destinac */

    assign v_rec_table = recid(tit_acr).




    if  not retry
    then do:
        display with frame f_dlg_03_tit_acr_destinac.
        display with frame f_dlg_03_tit_acr_destinac.

        /* Begin_Include: i_exec_program_epc */
        &if '{&emsbas_version}' > '1.00' &then
        if  v_nom_prog_upc <> '' then
        do:
            assign v_rec_table_epc = recid(tit_acr).
            run value(v_nom_prog_upc) (input 'DISPLAY',
                                       input 'viewer',
                                       input this-procedure,
                                       input v_wgh_frame_epc,
                                       input v_nom_table_epc,
                                       input v_rec_table_epc).
            if  'no' = 'yes'
            and return-value = 'NOK' then
                undo, retry.
        end.

        if  v_nom_prog_appc <> '' then
        do:
            assign v_rec_table_epc = recid(tit_acr).
            run value(v_nom_prog_appc) (input 'DISPLAY',
                                        input 'viewer',
                                        input this-procedure,
                                        input v_wgh_frame_epc,
                                        input v_nom_table_epc,
                                        input v_rec_table_epc).
            if  'no' = 'yes'
            and return-value = 'NOK' then
                undo, retry.
        end.

        &if '{&emsbas_version}' > '5.00' &then
        if  v_nom_prog_dpc <> '' then
        do:
            assign v_rec_table_epc = recid(tit_acr).
            run value(v_nom_prog_dpc) (input 'DISPLAY',
                                        input 'viewer',
                                        input this-procedure,
                                        input v_wgh_frame_epc,
                                        input v_nom_table_epc,
                                        input v_rec_table_epc).
            if  'no' = 'yes'
            and return-value = 'NOK' then
                undo, retry.
        end.
        &endif
        &endif
        /* End_Include: i_exec_program_epc */

    end /* if */.


    enable bt_ok
           bt_can
           bt_hel2
           bt_seleciona_todos_a_selecionar
           bt_deseleciona_todos_a_sel
           v_ind_classif_destinac_aux
           with frame f_dlg_03_tit_acr_destinac.

    /* Begin_Include: i_exec_program_epc */
    &if '{&emsbas_version}' > '1.00' &then
    if  v_nom_prog_upc <> '' then
    do:
        assign v_rec_table_epc = recid(tit_acr).
        run value(v_nom_prog_upc) (input 'ENABLE',
                                   input 'viewer',
                                   input this-procedure,
                                   input v_wgh_frame_epc,
                                   input v_nom_table_epc,
                                   input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.

    if  v_nom_prog_appc <> '' then
    do:
        assign v_rec_table_epc = recid(tit_acr).
        run value(v_nom_prog_appc) (input 'ENABLE',
                                    input 'viewer',
                                    input this-procedure,
                                    input v_wgh_frame_epc,
                                    input v_nom_table_epc,
                                    input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.

    &if '{&emsbas_version}' > '5.00' &then
    if  v_nom_prog_dpc <> '' then
    do:
        assign v_rec_table_epc = recid(tit_acr).
        run value(v_nom_prog_dpc) (input 'ENABLE',
                                    input 'viewer',
                                    input this-procedure,
                                    input v_wgh_frame_epc,
                                    input v_nom_table_epc,
                                    input v_rec_table_epc).
        if  'no' = 'yes'
        and return-value = 'NOK' then
            undo, retry.
    end.
    &endif
    &endif
    /* End_Include: i_exec_program_epc */


    /* Begin_Include: ix_p20_dlg_tit_acr_destinac */
    enable br_dlg_tit_acr_destinac_cobr
           with frame f_dlg_03_tit_acr_destinac.
    run pi_open_query_tit_acr_destinac /*pi_open_query_tit_acr_destinac*/.
    apply "choose" to bt_seleciona_todos_a_selecionar in frame f_dlg_03_tit_acr_destinac.

    get first qr_dlg_tit_acr_destinac_cobr no-lock.
    if  avail tt_rpt_tit_acr_destinac
    then do:
       reposition qr_dlg_tit_acr_destinac_cobr to recid recid(tt_rpt_tit_acr_destinac).
    end /* if */.

    assign p_cod_return = "NOK" /*l_nok*/ .
    /* End_Include: ix_p20_dlg_tit_acr_destinac */

    wait_block:
    repeat on endkey undo main_block, leave main_block while v_log_save_ok = no:
        if  valid-handle(v_wgh_focus)
        then do:
            wait-for go of frame f_dlg_03_tit_acr_destinac focus v_wgh_focus.
        end /* if */.
        else do:
            wait-for go of frame f_dlg_03_tit_acr_destinac.
        end /* else */.
        /* ix_p25_dlg_tit_acr_destinac */
        if  v_log_save = yes
        then do:
            save_block:
            do on error undo save_block, leave save_block:
               /* ix_p26_dlg_tit_acr_destinac */
               run pi_save_fields /*pi_save_fields*/.
               run pi_save_extra_fields /*pi_save_extra_fields*/.

               /* Begin_Include: i_exec_program_epc */
               &if '{&emsbas_version}' > '1.00' &then
               if  v_nom_prog_upc <> '' then
               do:
                   assign v_rec_table_epc = recid(tit_acr).
                   run value(v_nom_prog_upc) (input 'ASSIGN',
                                              input 'viewer',
                                              input this-procedure,
                                              input v_wgh_frame_epc,
                                              input v_nom_table_epc,
                                              input v_rec_table_epc).
                   if  'yes' = 'yes'
                   and return-value = 'NOK' then
                       undo, retry.
               end.

               if  v_nom_prog_appc <> '' then
               do:
                   assign v_rec_table_epc = recid(tit_acr).
                   run value(v_nom_prog_appc) (input 'ASSIGN',
                                               input 'viewer',
                                               input this-procedure,
                                               input v_wgh_frame_epc,
                                               input v_nom_table_epc,
                                               input v_rec_table_epc).
                   if  'yes' = 'yes'
                   and return-value = 'NOK' then
                       undo, retry.
               end.

               &if '{&emsbas_version}' > '5.00' &then
               if  v_nom_prog_dpc <> '' then
               do:
                   assign v_rec_table_epc = recid(tit_acr).
                   run value(v_nom_prog_dpc) (input 'ASSIGN',
                                               input 'viewer',
                                               input this-procedure,
                                               input v_wgh_frame_epc,
                                               input v_nom_table_epc,
                                               input v_rec_table_epc).
                   if  'yes' = 'yes'
                   and return-value = 'NOK' then
                       undo, retry.
               end.
               &endif
               &endif
               /* End_Include: i_exec_program_epc */

               /* ix_p27_dlg_tit_acr_destinac */
               run pi_vld_tit_acr_destinac /*pi_vld_tit_acr_destinac*/.

               /* Begin_Include: i_exec_program_epc */
               &if '{&emsbas_version}' > '1.00' &then
               if  v_nom_prog_upc <> '' then
               do:
                   assign v_rec_table_epc = recid(tit_acr).
                   run value(v_nom_prog_upc) (input 'VALIDATE',
                                              input 'viewer',
                                              input this-procedure,
                                              input v_wgh_frame_epc,
                                              input v_nom_table_epc,
                                              input v_rec_table_epc).
                   if  'yes' = 'yes'
                   and return-value = 'NOK' then
                       undo, retry.
               end.

               if  v_nom_prog_appc <> '' then
               do:
                   assign v_rec_table_epc = recid(tit_acr).
                   run value(v_nom_prog_appc) (input 'VALIDATE',
                                               input 'viewer',
                                               input this-procedure,
                                               input v_wgh_frame_epc,
                                               input v_nom_table_epc,
                                               input v_rec_table_epc).
                   if  'yes' = 'yes'
                   and return-value = 'NOK' then
                       undo, retry.
               end.

               &if '{&emsbas_version}' > '5.00' &then
               if  v_nom_prog_dpc <> '' then
               do:
                   assign v_rec_table_epc = recid(tit_acr).
                   run value(v_nom_prog_dpc) (input 'VALIDATE',
                                               input 'viewer',
                                               input this-procedure,
                                               input v_wgh_frame_epc,
                                               input v_nom_table_epc,
                                               input v_rec_table_epc).
                   if  'yes' = 'yes'
                   and return-value = 'NOK' then
                       undo, retry.
               end.
               &endif
               &endif
               /* End_Include: i_exec_program_epc */

               /* ix_p30_dlg_tit_acr_destinac */
               assign v_log_save_ok = yes.
            end /* do save_block */.
        end /* if */.
        else do:
            assign v_log_save_ok = yes.
        end /* else */.
        /* ix_p40_dlg_tit_acr_destinac */
    end /* repeat wait_block */.
end /* do main_block */.

/* ix_p50_dlg_tit_acr_destinac */
hide frame f_dlg_03_tit_acr_destinac.

/* Begin_Include: i_log_exec_prog_dtsul_fim */
if v_rec_log <> ? then do transaction:
    find log_exec_prog_dtsul where recid(log_exec_prog_dtsul) = v_rec_log exclusive-lock no-error.
    if  avail log_exec_prog_dtsul
    then do:
        assign log_exec_prog_dtsul.dat_fim_exec_prog_dtsul = today
               log_exec_prog_dtsul.hra_fim_exec_prog_dtsul = replace(string(time,"hh:mm:ss" /*l_hh:mm:ss*/ ),":","").
    end /* if */.
    release log_exec_prog_dtsul.
end.

/* End_Include: i_log_exec_prog_dtsul_fim */



/******************************* Main Code End ******************************/

/************************* Internal Procedure Begin *************************/

/*****************************************************************************
** Procedure Interna.....: pi_save_fields
** Descricao.............: pi_save_fields
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: bre18856
** Alterado em...........: 18/04/2001 14:10:53
*****************************************************************************/
PROCEDURE pi_save_fields:

    assign_block:
    do on error undo assign_block, return error:
        do with frame f_dlg_03_tit_acr_destinac:
        end.
        assign .
        assign v_wgh_focus = ?.
    end /* do assign_block */.

    /* Foráar a criaá∆o do registro com bases Oracle */
    if recid(tit_acr) <> ?  then.
END PROCEDURE. /* pi_save_fields */
/*****************************************************************************
** Procedure Interna.....: pi_save_extra_fields
** Descricao.............: pi_save_extra_fields
** Criado por............: 
** Criado em.............: // 
** Alterado por..........: 
** Alterado em...........: 17/02/1995 17:29:39
*****************************************************************************/
PROCEDURE pi_save_extra_fields:

    assign_block:
    do on error undo assign_block, return error:
        do with frame f_dlg_03_tit_acr_destinac:
        end.
        assign .
        assign v_wgh_focus = ?.
    end /* do assign_block */.
END PROCEDURE. /* pi_save_extra_fields */
/*****************************************************************************
** Procedure Interna.....: pi_vld_tit_acr_destinac
** Descricao.............: pi_vld_tit_acr_destinac
** Criado por............: Alexsandra
** Criado em.............: 18/10/1996 14:32:12
** Alterado por..........: Alexsandra
** Alterado em...........: 22/10/1996 16:45:53
*****************************************************************************/
PROCEDURE pi_vld_tit_acr_destinac:

    /* ESTA PI FOI INTENCIONALMENTE GERADA EM BRANCO */
END PROCEDURE. /* pi_vld_tit_acr_destinac */
/*****************************************************************************
** Procedure Interna.....: pi_open_query_tit_acr_destinac
** Descricao.............: pi_open_query_tit_acr_destinac
** Criado por............: Alexsandra
** Criado em.............: 18/10/1996 16:51:59
** Alterado por..........: bre18732_2
** Alterado em...........: 14/07/2004 14:37:04
*****************************************************************************/
PROCEDURE pi_open_query_tit_acr_destinac:

    if  p_ind_classif_destinac = "Estab/Portador/Carteira/Dt Emiss∆o/Dt Vencto/T°tulo/Parcela" /*l_vencto_tit_parc*/  then do:
        open query qr_dlg_tit_acr_destinac_cobr for
          each tt_rpt_tit_acr_destinac no-lock
          where tt_rpt_tit_acr_destinac.ttv_log_selec_tit_acr = yes,
          first cliente no-lock 
          where cliente.cod_empresa = tt_rpt_tit_acr_destinac.tta_cod_empresa
            and cliente.cdn_cliente = tt_rpt_tit_acr_destinac.tta_cdn_cliente
          by tt_rpt_tit_acr_destinac.tta_cod_estab
          by tt_rpt_tit_acr_destinac.tta_cod_portador
          by tt_rpt_tit_acr_destinac.tta_cod_cart_bcia
          by tt_rpt_tit_acr_destinac.tta_dat_emis_docto
          by tt_rpt_tit_acr_destinac.tta_dat_vencto_tit_acr
          by tt_rpt_tit_acr_destinac.tta_cod_tit_acr
          by tt_rpt_tit_acr_destinac.tta_cod_parcela.
    end.
    else do:    
        if  p_ind_classif_destinac = "Estab/Portador/Carteira/Dt Emiss∆o/T°tulo/Parcela" /*l_emis_tit_parcela*/  then do:
            open query qr_dlg_tit_acr_destinac_cobr for
              each tt_rpt_tit_acr_destinac no-lock
              where tt_rpt_tit_acr_destinac.ttv_log_selec_tit_acr = yes,
              first cliente no-lock 
              where cliente.cod_empresa = tt_rpt_tit_acr_destinac.tta_cod_empresa
                and cliente.cdn_cliente = tt_rpt_tit_acr_destinac.tta_cdn_cliente
              by tt_rpt_tit_acr_destinac.tta_cod_estab
              by tt_rpt_tit_acr_destinac.tta_cod_portador
              by tt_rpt_tit_acr_destinac.tta_cod_cart_bcia
              by tt_rpt_tit_acr_destinac.tta_dat_emis_docto
              by tt_rpt_tit_acr_destinac.tta_cod_tit_acr
              by tt_rpt_tit_acr_destinac.tta_cod_parcela.
        end.
        else do:
             if p_ind_classif_destinac = "Estab/Portador/Carteira/Cliente/Dt Emiss∆o/Dt Vencto/T°tulo/Parcela" /*l_vencto_clien_tit_parc*/  then do:
                open query qr_dlg_tit_acr_destinac_cobr for
                  each tt_rpt_tit_acr_destinac no-lock
                  where tt_rpt_tit_acr_destinac.ttv_log_selec_tit_acr = yes,
                  first cliente no-lock 
                  where cliente.cod_empresa = tt_rpt_tit_acr_destinac.tta_cod_empresa
                    and cliente.cdn_cliente = tt_rpt_tit_acr_destinac.tta_cdn_cliente
                  by tt_rpt_tit_acr_destinac.tta_cod_estab
                  by tt_rpt_tit_acr_destinac.tta_cod_portador
                  by tt_rpt_tit_acr_destinac.tta_cod_cart_bcia
                  by tt_rpt_tit_acr_destinac.tta_cdn_cliente
                  by tt_rpt_tit_acr_destinac.tta_dat_emis_docto
                  by tt_rpt_tit_acr_destinac.tta_dat_vencto_tit_acr
                  by tt_rpt_tit_acr_destinac.tta_cod_tit_acr
                  by tt_rpt_tit_acr_destinac.tta_cod_parcela.
             end.
             else do:
                  if p_ind_classif_destinac = "Estab/Portador/Carteira/Cliente/Dt Emiss∆o/T°tulo/Parcela" /*l_emis_clien_tit_parc*/  then do:
                     open query qr_dlg_tit_acr_destinac_cobr for
                      each tt_rpt_tit_acr_destinac no-lock
                      where tt_rpt_tit_acr_destinac.ttv_log_selec_tit_acr = yes,
                      first cliente no-lock 
                      where cliente.cod_empresa = tt_rpt_tit_acr_destinac.tta_cod_empresa
                        and cliente.cdn_cliente = tt_rpt_tit_acr_destinac.tta_cdn_cliente
                      by tt_rpt_tit_acr_destinac.tta_cod_estab
                      by tt_rpt_tit_acr_destinac.tta_cod_portador
                      by tt_rpt_tit_acr_destinac.tta_cod_cart_bcia
                      by tt_rpt_tit_acr_destinac.tta_cdn_cliente
                      by tt_rpt_tit_acr_destinac.tta_dat_emis_docto
                      by tt_rpt_tit_acr_destinac.tta_cod_tit_acr
                      by tt_rpt_tit_acr_destinac.tta_cod_parcela.
                  end.
                  else do:
                      if p_ind_classif_destinac = "Cliente/Dt Vencto/T°tulo/Parcela" /*l_clien_classif*/  then do:
                         open query qr_dlg_tit_acr_destinac_cobr for
                          each tt_rpt_tit_acr_destinac no-lock
                          where tt_rpt_tit_acr_destinac.ttv_log_selec_tit_acr = yes,
                          first cliente no-lock 
                          where cliente.cod_empresa = tt_rpt_tit_acr_destinac.tta_cod_empresa
                            and cliente.cdn_cliente = tt_rpt_tit_acr_destinac.tta_cdn_cliente
                          by tt_rpt_tit_acr_destinac.tta_cdn_cliente
                          by tt_rpt_tit_acr_destinac.tta_dat_vencto_tit_acr
                          by tt_rpt_tit_acr_destinac.tta_cod_tit_acr
                          by tt_rpt_tit_acr_destinac.tta_cod_parcela.
                      end.
                  end.
             end.
        end.
    end.
END PROCEDURE. /* pi_open_query_tit_acr_destinac */
/*****************************************************************************
** Procedure Interna.....: pi_version_extract
** Descricao.............: pi_version_extract
** Criado por............: jaison
** Criado em.............: 31/07/1998 09:33:22
** Alterado por..........: tech14013
** Alterado em...........: 05/01/2005 19:27:44
*****************************************************************************/
PROCEDURE pi_version_extract:

    /************************ Parameter Definition Begin ************************/

    def Input param p_cod_program
        as character
        format "x(08)"
        no-undo.
    def Input param p_cod_program_ext
        as character
        format "x(8)"
        no-undo.
    def Input param p_cod_version
        as character
        format "x(8)"
        no-undo.
    def Input param p_cod_program_type
        as character
        format "x(8)"
        no-undo.


    /************************* Parameter Definition End *************************/

    /************************* Variable Definition Begin ************************/

    def var v_cod_event_dic
        as character
        format "x(20)":U
        label "Evento"
        column-label "Evento"
        no-undo.
    def var v_cod_tabela
        as character
        format "x(28)":U
        label "Tabela"
        column-label "Tabela"
        no-undo.


    /************************** Variable Definition End *************************/

    if  can-do(v_cod_tip_prog, p_cod_program_type)
    then do:
        if p_cod_program_type = 'dic' then 
           assign p_cod_program_ext = replace(p_cod_program_ext, 'database/', '').

        output stream s-arq to value(v_cod_arq) append.

        put stream s-arq unformatted
            p_cod_program            at 1 
            p_cod_program_ext        at 43 
            p_cod_version            at 69 
            today                    at 84 
            string(time, 'HH:MM:SS') at 94 skip.

        if  p_cod_program_type = 'pro' then do:
            &if '{&emsbas_version}' > '1.00' &then
            find emsbas.prog_dtsul 
                where emsbas.prog_dtsul.cod_prog_dtsul = p_cod_program 
                no-lock no-error.
            if  avail emsbas.prog_dtsul
            then do:
                &if '{&emsbas_version}' > '5.00' &then
                    if  emsbas.prog_dtsul.nom_prog_dpc <> '' then
                        put stream s-arq 'DPC : ' at 5 emsbas.prog_dtsul.nom_prog_dpc  at 15 skip.
                &endif
                if  emsbas.prog_dtsul.nom_prog_appc <> '' then
                    put stream s-arq 'APPC: ' at 5 emsbas.prog_dtsul.nom_prog_appc at 15 skip.
                if  emsbas.prog_dtsul.nom_prog_upc <> '' then
                    put stream s-arq 'UPC : ' at 5 emsbas.prog_dtsul.nom_prog_upc  at 15 skip.
            end /* if */.
            &endif
        end.

        if  p_cod_program_type = 'dic' then do:
            &if '{&emsbas_version}' > '1.00' &then
            assign v_cod_event_dic = ENTRY(1,p_cod_program ,'/':U)
                   v_cod_tabela    = ENTRY(2,p_cod_program ,'/':U). /* FO 1100.980 */
            find emsbas.tab_dic_dtsul 
                where emsbas.tab_dic_dtsul.cod_tab_dic_dtsul = v_cod_tabela 
                no-lock no-error.
            if  avail emsbas.tab_dic_dtsul
            then do:
                &if '{&emsbas_version}' > '5.00' &then
                    if  emsbas.tab_dic_dtsul.nom_prog_dpc_gat_delete <> '' and v_cod_event_dic = 'Delete':U then
                        put stream s-arq 'DPC-DELETE : ' at 5 emsbas.tab_dic_dtsul.nom_prog_dpc_gat_delete  at 25 skip.
                &endif
                if  emsbas.tab_dic_dtsul.nom_prog_appc_gat_delete <> '' and v_cod_event_dic = 'Delete':U then
                    put stream s-arq 'APPC-DELETE: ' at 5 emsbas.tab_dic_dtsul.nom_prog_appc_gat_delete at 25 skip.
                if  emsbas.tab_dic_dtsul.nom_prog_upc_gat_delete <> '' and v_cod_event_dic = 'Delete':U then
                    put stream s-arq 'UPC-DELETE : ' at 5 emsbas.tab_dic_dtsul.nom_prog_upc_gat_delete  at 25 skip.
                &if '{&emsbas_version}' > '5.00' &then
                    if  emsbas.tab_dic_dtsul.nom_prog_dpc_gat_write <> '' and v_cod_event_dic = 'Write':U then
                        put stream s-arq 'DPC-WRITE : ' at 5 emsbas.tab_dic_dtsul.nom_prog_dpc_gat_write  at 25 skip.
                &endif
                if  emsbas.tab_dic_dtsul.nom_prog_appc_gat_write <> '' and v_cod_event_dic = 'Write':U then
                    put stream s-arq 'APPC-WRITE: ' at 5 emsbas.tab_dic_dtsul.nom_prog_appc_gat_write at 25 skip.
                if  emsbas.tab_dic_dtsul.nom_prog_upc_gat_write <> '' and v_cod_event_dic = 'Write':U  then
                    put stream s-arq 'UPC-WRITE : ' at 5 emsbas.tab_dic_dtsul.nom_prog_upc_gat_write  at 25 skip.
            end /* if */.
            &endif
        end.

        output stream s-arq close.
    end /* if */.

END PROCEDURE. /* pi_version_extract */


/************************** Internal Procedure End **************************/

/************************* External Procedure Begin *************************/



/************************** External Procedure End **************************/
&endif

/*************************************  *************************************/
/*****************************************************************************
**  Procedure Interna: pi_messages
**  Descricao........: Mostra Mensagem com Ajuda
*****************************************************************************/
PROCEDURE pi_messages:

    def input param c_action    as char    no-undo.
    def input param i_msg       as integer no-undo.
    def input param c_param     as char    no-undo.

    def var c_prg_msg           as char    no-undo.

    assign c_prg_msg = "messages/":U
                     + string(trunc(i_msg / 1000,0),"99":U)
                     + "/msg":U
                     + string(i_msg, "99999":U).

    if search(c_prg_msg + ".r":U) = ? and search(c_prg_msg + ".p":U) = ? then do:
        message "Mensagem nr. " i_msg "!!!":U skip
                "Programa Mensagem" c_prg_msg "n∆o encontrado."
                view-as alert-box error.
        return error.
    end.

    run value(c_prg_msg + ".p":U) (input c_action, input c_param).
    return return-value.
END PROCEDURE.  /* pi_messages */
/***********************  End of dlg_tit_acr_destinac ***********************/
