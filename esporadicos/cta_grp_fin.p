FOR EACH CTA_GRP_CLIEN
WHERE COD_ESPEC = 'DP'
:
    DISP CTA_GRP_CLIEN.COD_GRP_CLIEN COD_FINALID ind_finalid_ctbl cod_cta_ctbl   WITH  WIDTH 550.
END.


  /*
=========================================================================
============================= Table: cta_grp_clien ======================

              Table Flags: "f" = frozen, "s" = a SQL table


Table                            Table Field Index Table
Name                             Flags Count Count Label
-------------------------------- ----- ----- ----- ------------------------
cta_grp_clien                             27     7 Conta Grupo de Cliente

    Dump Name: fin459
  Description: Conta Grupo de Cliente
 Storage Area: Schema Area

       Trigger Event Trigger Procedure    Overridable? Check CRC?
       ------------- -------------------- ------------ ----------
       Create        database/tgfin/tcp/t yes          no
       Delete        database/tgfin/tdp/t yes          no
       Write         database/tgfin/twp/t yes          no


============================= FIELD SUMMARY =============================
============================= Table: cta_grp_clien ======================

Flags: <c>ase sensitive, <i>ndex component, <m>andatory, <v>iew component

Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 cod_empresa                      char        im
   20 cod_espec_docto                  char        im
   30 cod_grp_clien                    char        im
   40 cod_finalid_econ                 char        im
   50 ind_tip_espec_docto              char        im
   60 ind_finalid_ctbl                 char        im
   70 ind_control_ctbl                 char        m
   80 num_seq_cta_grp_clien            inte        im
   90 dat_inic_valid                   date        im
  100 dat_fim_valid                    date        m
  110 cod_plano_cta_ctbl               char        im
  120 cod_cta_ctbl                     char        im
  130 cod_cta_ctbl_ativ                char        m
  140 cod_cta_ctbl_passiv              char        m
  150 cod_livre_1                      char
  160 cod_plano_ccusto                 char        m
  170 cod_ccusto                       char        m
  180 cod_livre_2                      char
  190 dat_livre_1                      date
  200 dat_livre_2                      date
  210 log_livre_1                      logi
  220 log_livre_2                      logi
  230 num_livre_1                      inte
  240 num_livre_2                      inte
  250 val_livre_1                      deci-4
  260 val_livre_2                      deci-4
  270 cdd_version                      deci-0

Field Name                       Format
-------------------------------- -----------------------------
cod_empresa                      x(3)
cod_espec_docto                  x(3)
cod_grp_clien                    x(4)
cod_finalid_econ                 x(10)
ind_tip_espec_docto              X(17)
ind_finalid_ctbl                 X(30)
ind_control_ctbl                 X(18)
num_seq_cta_grp_clien            >>9
dat_inic_valid                   99/99/9999
dat_fim_valid                    99/99/9999
cod_plano_cta_ctbl               x(8)
cod_cta_ctbl                     x(20)
cod_cta_ctbl_ativ                x(20)
cod_cta_ctbl_passiv              x(20)
cod_livre_1                      x(100)
cod_plano_ccusto                 x(8)
cod_ccusto                       x(11)
cod_livre_2                      x(100)
dat_livre_1                      99/99/9999
dat_livre_2                      99/99/9999
log_livre_1                      Sim/NÆo
log_livre_2                      Sim/NÆo
num_livre_1                      >>>>>9
num_livre_2                      >>>>>9
val_livre_1                      >>>,>>>,>>9.9999
val_livre_2                      >>>,>>>,>>9.9999
cdd_version                      >>>,>>>,>>>,>>9

Field Name                       Initial
-------------------------------- -----------------------------
cod_empresa
cod_espec_docto
cod_grp_clien
cod_finalid_econ
ind_tip_espec_docto
ind_finalid_ctbl
ind_control_ctbl
num_seq_cta_grp_clien            0
dat_inic_valid                   01/01/0001
dat_fim_valid                    12/31/9999
cod_plano_cta_ctbl
cod_cta_ctbl
cod_cta_ctbl_ativ
cod_cta_ctbl_passiv
cod_livre_1
cod_plano_ccusto
cod_ccusto
cod_livre_2
dat_livre_1                      ?
dat_livre_2                      ?
log_livre_1                      no
log_livre_2                      no
num_livre_1                      0
num_livre_2                      0
val_livre_1                      0
val_livre_2                      0
cdd_version                      0

Field Name                     Label                  Column Label
------------------------------ ---------------------- ----------------------
cod_empresa                    Empresa                Empresa
cod_espec_docto                Esp‚cie Documento      Esp‚cie
cod_grp_clien                  Grupo Cliente          Grupo Cliente
cod_finalid_econ               Finalidade             Finalidade
ind_tip_espec_docto            Tipo Esp‚cie           Tipo Esp‚cie
ind_finalid_ctbl               Finalidade Cont bil    Finalidade Cont bil
ind_control_ctbl               Controle Cont bil      Controle Cont bil
num_seq_cta_grp_clien          Sequˆncia              Sequˆncia
dat_inic_valid                 In¡cio Validade        Inic Validade
dat_fim_valid                  Fim Validade           Fim Validade
cod_plano_cta_ctbl             Plano Contas           Plano Contas
cod_cta_ctbl                   Conta Cont bil         Conta Cont bil
cod_cta_ctbl_ativ              Cta Ctbl Credora       Cta Ctbl Credora
cod_cta_ctbl_passiv            Cta Ctbl Devedora      Cta Ctbl Devedora
cod_livre_1                    Livre 1                Livre 1
cod_plano_ccusto               Plano Centros Custo    Plano Centros Custo
cod_ccusto                     Centro Custo           Centro Custo
cod_livre_2                    Livre 2                Livre 2
dat_livre_1                    Livre 1                Livre 1
dat_livre_2                    Livre 2                Livre 2
log_livre_1                    Livre 1                Livre 1
log_livre_2                    Livre 2                Livre 2
num_livre_1                    Livre 1                Livre 1
num_livre_2                    Livre 2                Livre 2
val_livre_1                    Livre 1                Livre 1
val_livre_2                    Livre 2                Livre 2
cdd_version                    VersÆo                 VersÆo


============================= INDEX SUMMARY =============================
============================= Table: cta_grp_clien ======================

Flags: <p>rimary, <u>nique, <w>ord, <a>bbreviated, <i>nactive, + asc, - desc

Flags Index Name                       Cnt Field Name
----- -------------------------------- --- ---------------------------------
      ctgrpcln_cta_ctbl_integr           2 + cod_plano_cta_ctbl
                                           + cod_cta_ctbl

      ctgrpcln_espec_docto               1 + cod_espec_docto

      ctgrpcln_finalid_ctbl              2 + cod_empresa
                                           + ind_finalid_ctbl

      ctgrpcln_finalid_econ              1 + cod_finalid_econ

      ctgrpcln_gr_clien                  1 + cod_grp_clien

pu    ctgrpcln_id                        7 + cod_empresa
                                           + ind_tip_espec_docto
                                           + cod_espec_docto
                                           + cod_grp_clien
                                           + cod_finalid_econ
                                           + ind_finalid_ctbl
                                           + num_seq_cta_grp_clien

      ctgrpcln_inicio_validade           7 + cod_empresa
                                           + cod_espec_docto
                                           + ind_tip_espec_docto
                                           + cod_grp_clien
                                           + cod_finalid_econ
                                           + ind_finalid_ctbl
                                           + dat_inic_valid

** Index Name: ctgrpcln_cta_ctbl_integr
 Storage Area: Schema Area
** Index Name: ctgrpcln_espec_docto
 Storage Area: Schema Area
** Index Name: ctgrpcln_finalid_ctbl
 Storage Area: Schema Area
** Index Name: ctgrpcln_finalid_econ
 Storage Area: Schema Area
** Index Name: ctgrpcln_gr_clien
 Storage Area: Schema Area
** Index Name: ctgrpcln_id
 Storage Area: Schema Area
** Index Name: ctgrpcln_inicio_validade
 Storage Area: Schema Area


============================= FIELD DETAILS =============================
============================= Table: cta_grp_clien ======================

** Field Name: cod_empresa
  Description: C¢digo Empresa
         Help: C¢digo Empresa

** Field Name: cod_espec_docto
  Description: C¢digo Esp‚cie Documento
         Help: C¢digo Esp‚cie Documento

** Field Name: cod_grp_clien
  Description: C¢digo Grupo Cliente
         Help: C¢digo Grupo Cliente

** Field Name: cod_finalid_econ
  Description: C¢digo Finalidade Econ“mica
         Help: C¢digo Finalidade Econ“mica

** Field Name: ind_tip_espec_docto
  Description: Tipo Esp‚cie Documento
         Help: Tipo Esp‚cie Documento
      Val-Msg: FV: Valor nÆo contido na lista de valores poss¡veis !
      Val-Exp: can-do("Nenhum,Normal,Antecipa‡Æo,PrevisÆo,ProvisÆo,Cheques
               Recebidos,Imposto Retido,Imposto Taxado,Material
               Recebido,Nota de Cr‚dito,Nota de D‚bito,Nota Fiscal,Aviso
               D‚bito,Nota Promiss¢ria", cta_grp_clien.ind_tip_espec_docto)

** Field Name: ind_finalid_ctbl
  Description: Indicador Finalidade Cont bil
         Help: Indicador Finalidade Cont bil

** Field Name: ind_control_ctbl
  Description: Controle Cont bil
         Help: Controle Cont bil
      Val-Msg: FV: Valor nÆo contido na lista de valores poss¡veis !
      Val-Exp: can-do("Controla Todas FE,Controla na FE,Nao Controla",
               cta_grp_clien.ind_control_ctbl)

** Field Name: num_seq_cta_grp_clien
  Description: Sequˆncia da Conta Cont bil do Grupo de Cliente
         Help: Sequˆncia da Conta Cont bil do Grupo de Cliente

** Field Name: dat_inic_valid
  Description: Data In¡cio Validade
         Help: Data In¡cio Validade

** Field Name: dat_fim_valid
  Description: Data Fim Validade
         Help: Data Fim Validade

** Field Name: cod_plano_cta_ctbl
  Description: C¢digo Plano Contas
         Help: C¢digo Plano Contas

** Field Name: cod_cta_ctbl
  Description: C¢digo Conta Cont bil
         Help: C¢digo Conta Cont bil

** Field Name: cod_cta_ctbl_ativ
  Description: Conta Cont bil de Contra Partida Ativo
         Help: Conta Cont bil de Contra Partida Ativo

** Field Name: cod_cta_ctbl_passiv
  Description: Conta Contabil de Contra Partida Passivo
         Help: Conta Contabil de Contra Partida Passivo

** Field Name: cod_livre_1
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: cod_plano_ccusto
  Description: C¢digo Plano Centros Custo
         Help: C¢digo Plano Centros Custo

** Field Name: cod_ccusto
  Description: C¢digo Centro Custo
         Help: C¢digo Centro Custo

** Field Name: cod_livre_2
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: dat_livre_1
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: dat_livre_2
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: log_livre_1
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: log_livre_2
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: num_livre_1
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: num_livre_2
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: val_livre_1
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: val_livre_2
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: cdd_version
  Description: C¢digo da VersÆo do Registro
         Help: C¢digo da VersÆo do Registro


*/
