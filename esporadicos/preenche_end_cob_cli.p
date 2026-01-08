FOR EACH pessoa_jurid
  WHERE pessoa_jurid.num_pessoa_jurid_cobr = 0
  /*  AND num_pessoa_jurid =  55371*/
   AND nom_ender_cobr        <> nom_endereco  
  /*AND nom_ender_cobr = ''*/ :
  DISP pessoa_jurid.num_pessoa_jurid  . PAUSE 0 .
  /*ASSIGN nom_ender_cobr         = nom_endereco   
         nom_bairro_cobr        = nom_bairro
         nom_cidad_cobr         = nom_cidade     
         nom_condad_cobr        = nom_condado        
         cod_pais_cobr          = cod_pais          
         cod_unid_federac_cobr  = cod_unid_federac  
         cod_cep_cobr           = cod_cep           
         cod_cx_post_cobr       = cod_cx_post .*/
END.

  /*

=========================================================================
============================= Table: pessoa_jurid =======================

              Table Flags: "f" = frozen, "s" = a SQL table


Table                            Table Field Index Table
Name                             Flags Count Count Label
-------------------------------- ----- ----- ----- ------------------------
pessoa_jurid                              80    11 Pessoa Jur°dica

    Dump Name: uni183
  Description: Pessoa Juridica
 Storage Area: dados

       Trigger Event Trigger Procedure    Overridable? Check CRC?
       ------------- -------------------- ------------ ----------
       create        database/tguni/tcp/t yes          no
       delete        database/tguni/tdp/t yes          no
       write         database/tguni/twp/t yes          no


============================= FIELD SUMMARY =============================
============================= Table: pessoa_jurid =======================

Flags: <c>ase sensitive, <i>ndex component, <m>andatory, <v>iew component

Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 num_pessoa_jurid                 inte        im
   20 nom_pessoa                       char        im
   30 cod_id_feder                     char        i
   40 cod_id_estad_jurid               char
   50 cod_id_munic_jurid               char
   60 cod_id_previd_social             char        i
   70 log_fins_lucrat                  logi        m
   80 num_pessoa_jurid_matriz          inte        im
   90 nom_endereco                     char        m
  100 nom_ender_compl                  char        m
  110 nom_bairro                       char        m
  120 nom_cidade                       char        m
  130 nom_condado                      char
  140 cod_pais                         char        im
  150 cod_unid_federac                 char        im
  160 cod_cep                          char        m
  170 cod_cx_post                      char        m
  180 cod_telefone                     char        m
  190 cod_fax                          char        m
  200 cod_ramal_fax                    char        m
  210 cod_telex                        char        m
  220 cod_modem                        char        m
  230 cod_ramal_modem                  char        m
  240 cod_e_mail                       char        m
  250 cod_e_mail_cobr                  char        m
  260 num_pessoa_jurid_cobr            inte        im
  270 nom_ender_cobr                   char        m
  280 nom_ender_compl_cobr             char        m
  290 nom_bairro_cobr                  char        m
  300 nom_cidad_cobr                   char        m
  310 nom_condad_cobr                  char        m
  320 cod_unid_federac_cobr            char        im
  330 cod_pais_cobr                    char        im
  340 cod_cep_cobr                     char        m
  350 cod_cx_post_cobr                 char        m
  360 num_pessoa_jurid_pagto           inte        im
  370 nom_ender_pagto                  char        m
  380 nom_ender_compl_pagto            char        m
  390 nom_bairro_pagto                 char        m
  400 nom_cidad_pagto                  char        m
  410 nom_condad_pagto                 char        m
  420 cod_pais_pagto                   char        im
  430 cod_unid_federac_pagto           char        im
  440 cod_cep_pagto                    char        m
  450 cod_cx_post_pagto                char        m
  460 des_anot_tab                     char        m
  470 ind_tip_pessoa_jurid             char        m
  480 ind_tip_capit_pessoa_jurid       char        m
  490 cod_usuar_ult_atualiz            char        m
  500 dat_ult_atualiz                  date
  510 hra_ult_atualiz                  char        m
  520 cod_imagem                       char
  530 log_ems_20_atlzdo                logi        m
  540 cod_livre_1                      char
  550 log_livre_1                      logi
  560 num_livre_1                      inte
  570 val_livre_1                      deci-4
  580 dat_livre_1                      date
  590 nom_home_page                    char
  600 nom_ender_text                   char
  610 nom_ender_cobr_text              char
  620 nom_ender_pagto_text             char
  630 log_envio_bco_histor             logi        m
  640 cod_livre_2                      char
  650 dat_livre_2                      date
  660 log_livre_2                      logi
  670 num_livre_2                      inte
  680 val_livre_2                      deci-4
  690 ind_natur_pessoa_jurid           char
  700 nom_fantasia                     char
  710 cod_sub_regiao_vendas            char
  720 cdd_version                      deci-0
  730 log_replic_pessoa_hcm            logi
  740 log_replic_pessoa_crm            logi
  750 log_replic_pessoa_gps            logi
  760 cod_fax_2                        char
  770 cod_ramal_fax_2                  char
  780 cod_telef_2                      char
  790 cod_ramal_2                      char
  800 ind_tip_matriz                   char

Field Name                       Format
-------------------------------- -----------------------------
num_pessoa_jurid                 >>>,>>>,>>9
nom_pessoa                       x(40)
cod_id_feder                     x(20)
cod_id_estad_jurid               x(20)
cod_id_munic_jurid               x(20)
cod_id_previd_social             x(20)
log_fins_lucrat                  Sim/N∆o
num_pessoa_jurid_matriz          >>>,>>>,>>9
nom_endereco                     x(40)
nom_ender_compl                  x(10)
nom_bairro                       x(20)
nom_cidade                       x(32)
nom_condado                      x(32)
cod_pais                         x(3)
cod_unid_federac                 x(3)
cod_cep                          x(20)
cod_cx_post                      x(20)
cod_telefone                     x(20)
cod_fax                          x(20)
cod_ramal_fax                    x(07)
cod_telex                        x(7)
cod_modem                        x(20)
cod_ramal_modem                  x(07)
cod_e_mail                       x(40)
cod_e_mail_cobr                  x(40)
num_pessoa_jurid_cobr            >>>,>>>,>>9
nom_ender_cobr                   x(40)
nom_ender_compl_cobr             x(10)
nom_bairro_cobr                  x(20)
nom_cidad_cobr                   x(32)
nom_condad_cobr                  x(32)
cod_unid_federac_cobr            x(3)
cod_pais_cobr                    x(3)
cod_cep_cobr                     x(20)
cod_cx_post_cobr                 x(20)
num_pessoa_jurid_pagto           >>>,>>>,>>9
nom_ender_pagto                  x(40)
nom_ender_compl_pagto            x(10)
nom_bairro_pagto                 x(20)
nom_cidad_pagto                  x(32)
nom_condad_pagto                 x(32)
cod_pais_pagto                   x(3)
cod_unid_federac_pagto           x(3)
cod_cep_pagto                    x(20)
cod_cx_post_pagto                x(20)
des_anot_tab                     x(2000)
ind_tip_pessoa_jurid             X(08)
ind_tip_capit_pessoa_jurid       X(13)
cod_usuar_ult_atualiz            x(12)
dat_ult_atualiz                  99/99/9999
hra_ult_atualiz                  99:99:99
cod_imagem                       x(30)
log_ems_20_atlzdo                Sim/N∆o
cod_livre_1                      x(100)
log_livre_1                      Sim/N∆o
num_livre_1                      >>>>>9
val_livre_1                      >>>,>>>,>>9.9999
dat_livre_1                      99/99/9999
nom_home_page                    x(40)
nom_ender_text                   x(2000)
nom_ender_cobr_text              x(2000)
nom_ender_pagto_text             x(2000)
log_envio_bco_histor             Sim/N∆o
cod_livre_2                      x(100)
dat_livre_2                      99/99/9999
log_livre_2                      Sim/N∆o
num_livre_2                      >>>>>9
val_livre_2                      >>>,>>>,>>9.9999
ind_natur_pessoa_jurid           X(20)
nom_fantasia                     x(60)
cod_sub_regiao_vendas            x(12)
cdd_version                      >>>,>>>,>>>,>>9
log_replic_pessoa_hcm            Sim/N∆o
log_replic_pessoa_crm            Sim/N∆o
log_replic_pessoa_gps            Sim/N∆o
cod_fax_2                        x(10)
cod_ramal_fax_2                  x(7)
cod_telef_2                      x(20)
cod_ramal_2                      x(7)
ind_tip_matriz                   x(20)

Field Name                       Initial
-------------------------------- -----------------------------
num_pessoa_jurid                 0
nom_pessoa
cod_id_feder
cod_id_estad_jurid
cod_id_munic_jurid
cod_id_previd_social
log_fins_lucrat                  yes
num_pessoa_jurid_matriz          0
nom_endereco
nom_ender_compl
nom_bairro
nom_cidade
nom_condado
cod_pais
cod_unid_federac
cod_cep
cod_cx_post
cod_telefone
cod_fax
cod_ramal_fax
cod_telex
cod_modem
cod_ramal_modem
cod_e_mail
cod_e_mail_cobr
num_pessoa_jurid_cobr            0
nom_ender_cobr
nom_ender_compl_cobr
nom_bairro_cobr
nom_cidad_cobr
nom_condad_cobr
cod_unid_federac_cobr
cod_pais_cobr
cod_cep_cobr
cod_cx_post_cobr
num_pessoa_jurid_pagto           0
nom_ender_pagto
nom_ender_compl_pagto
nom_bairro_pagto
nom_cidad_pagto
nom_condad_pagto
cod_pais_pagto
cod_unid_federac_pagto
cod_cep_pagto
cod_cx_post_pagto
des_anot_tab
ind_tip_pessoa_jurid
ind_tip_capit_pessoa_jurid
cod_usuar_ult_atualiz
dat_ult_atualiz                  ?
hra_ult_atualiz
cod_imagem
log_ems_20_atlzdo                no
cod_livre_1
log_livre_1                      no
num_livre_1                      0
val_livre_1                      0
dat_livre_1                      today
nom_home_page
nom_ender_text
nom_ender_cobr_text
nom_ender_pagto_text
log_envio_bco_histor             no
cod_livre_2
dat_livre_2                      ?
log_livre_2                      no
num_livre_2                      0
val_livre_2                      0
ind_natur_pessoa_jurid           Nacional
nom_fantasia
cod_sub_regiao_vendas
cdd_version                      0
log_replic_pessoa_hcm            yes
log_replic_pessoa_crm            yes
log_replic_pessoa_gps            yes
cod_fax_2
cod_ramal_fax_2
cod_telef_2
cod_ramal_2
ind_tip_matriz                   F°sica

Field Name                     Label                  Column Label
------------------------------ ---------------------- ----------------------
num_pessoa_jurid               Pessoa Jur°dica        Pessoa Jur°dica
nom_pessoa                     Nome                   Nome
cod_id_feder                   ID Federal             ID Federal
cod_id_estad_jurid             ID Estadual            ID Estadual
cod_id_munic_jurid             ID Municipal           ID Municipal
cod_id_previd_social           Id Previdància         Id Previdància
log_fins_lucrat                Fins Lucrativos        Fins Lucrativos
num_pessoa_jurid_matriz        Matriz                 Matriz
nom_endereco                   Endereáo               Endereáo
nom_ender_compl                Complemento            Complemento
nom_bairro                     Bairro                 Bairro
nom_cidade                     Cidade                 Cidade
nom_condado                    Condado                Condado
cod_pais                       Pa°s                   Pa°s
cod_unid_federac               Unidade Federaá∆o      UF
cod_cep                        CEP                    CEP
cod_cx_post                    Caixa Postal           Caixa Postal
cod_telefone                   Telefone               Telefone
cod_fax                        FAX                    FAX
cod_ramal_fax                  Ramal Fax              Ramal Fax
cod_telex                      TELEX                  TELEX
cod_modem                      Modem                  Modem
cod_ramal_modem                Ramal Modem            Ramal Modem
cod_e_mail                     Internet E-Mail        Internet E-Mail
cod_e_mail_cobr                E-Mail Cobranáa        E-Mail Cobranáa
num_pessoa_jurid_cobr          Pessoa Jur°dica Cobr   Pessoa Jur°dica Cobr
nom_ender_cobr                 Endereáo Cobranáa      Endereáo Cobranáa
nom_ender_compl_cobr           Complemento            Complemento
nom_bairro_cobr                Bairro Cobranáa        Bairro Cobranáa
nom_cidad_cobr                 Cidade Cobranáa        Cidade Cobranáa
nom_condad_cobr                Condado Cobranáa       Condado Cobranáa
cod_unid_federac_cobr          Unidade Federaá∆o      Unidade Federaá∆o
cod_pais_cobr                  Pa°s Cobranáa          Pa°s Cobranáa
cod_cep_cobr                   CEP Cobranáa           CEP Cobranáa
cod_cx_post_cobr               Caixa Postal Cobraná   Caixa Postal Cobraná
num_pessoa_jurid_pagto         Pessoa Jurid Pagto     Pessoa Jurid Pagto
nom_ender_pagto                Endereáo Pagamento     Endereáo Pagamento
nom_ender_compl_pagto          Complemento            Complemento
nom_bairro_pagto               Bairro Pagamento       Bairro Pagamento
nom_cidad_pagto                Cidade Pagamento       Cidade Pagamento
nom_condad_pagto               Condado Pagamento      Condado Pagamento
cod_pais_pagto                 Pa°s Pagamento         Pa°s Pagamento
cod_unid_federac_pagto         Unidade Federaá∆o      Unidade Federaá∆o
cod_cep_pagto                  CEP Pagamento          CEP Pagamento
cod_cx_post_pagto              Caixa Postal Pagamen   Caixa Postal Pagamen
des_anot_tab                   Anotaá∆o Tabela        Anotaá∆o Tabela
ind_tip_pessoa_jurid           Tipo Pessoa            Tipo Pessoa
ind_tip_capit_pessoa_jurid     Tipo Capital           Tipo Capital
cod_usuar_ult_atualiz          Usu†rio Ult Atualiz    Usu†rio Ult Atualiz
dat_ult_atualiz                Èltima Atualizaá∆o     Data Ult Atualiz
hra_ult_atualiz                Hora Èltima Atualiz    Hora Ult Atualiz
cod_imagem                     Imagem                 Imagem
log_ems_20_atlzdo              2.0 Atualizado         2.0 Atualizado
cod_livre_1                    Livre 1                Livre 1
log_livre_1                    Livre 1                Livre 1
num_livre_1                    Livre 1                Livre 1
val_livre_1                    Livre 1                Livre 1
dat_livre_1                    Livre 1                Livre 1
nom_home_page                  Home Page              Home Page
nom_ender_text                 Endereco Compl.        Endereco Compl.
nom_ender_cobr_text            End Cobranca Compl     End Cobranca Compl
nom_ender_pagto_text           End Pagto Compl.       End Pagto Compl.
log_envio_bco_histor           Envio Banco Historic   Envio Banco Historic
cod_livre_2                    Livre 2                Livre 2
dat_livre_2                    Livre 2                Livre 2
log_livre_2                    Livre 2                Livre 2
num_livre_2                    Livre 2                Livre 2
val_livre_2                    Livre 2                Livre 2
ind_natur_pessoa_jurid         Natureza Pessoa        Nat Pessoa
nom_fantasia                   Nome Fantasia          Nome Fantasia
cod_sub_regiao_vendas          Microrregi∆o Vendas    Microrregi∆o Vendas
cdd_version                    Vers∆o                 Vers∆o
log_replic_pessoa_hcm          Replica Pessoa HCM     Replic P HCM
log_replic_pessoa_crm          Replica Pessoa CRM     Replic P CRM
log_replic_pessoa_gps          Replica Pessoa GPS     Replic P GPS
cod_fax_2                      FAX 2                  FAX 2
cod_ramal_fax_2                Ramal Fax 2            Ramal Fax 2
cod_telef_2                    Telefone 2             Telefone 2
cod_ramal_2                    Ramal 2                Ramal 2
ind_tip_matriz                 Tipo Matriz            Tipo Matriz


============================= INDEX SUMMARY =============================
============================= Table: pessoa_jurid =======================

Flags: <p>rimary, <u>nique, <w>ord, <a>bbreviated, <i>nactive, + asc, - desc

Flags Index Name                       Cnt Field Name
----- -------------------------------- --- ---------------------------------
      pssjrda_cobranca                   1 + num_pessoa_jurid_cobr

pu    pssjrda_id                         1 + num_pessoa_jurid

      pssjrda_id_feder_jurid             2 + cod_pais
                                           + cod_id_feder

      pssjrda_id_previd_social           2 + cod_pais
                                           + cod_id_previd_social

      pssjrda_matriz                     1 + num_pessoa_jurid_matriz

w     pssjrda_nom_pessoa_word            1 + nom_pessoa

      pssjrda_pagto                      1 + num_pessoa_jurid_pagto

      pssjrda_razao_social               1 + nom_pessoa

      pssjrda_unid_federac               2 + cod_pais
                                           + cod_unid_federac

      pssjrda_unid_feder_cobr            2 + cod_pais_cobr
                                           + cod_unid_federac_cobr

      pssjrda_unid_feder_pagto           2 + cod_pais_pagto
                                           + cod_unid_federac_pagto

** Index Name: pssjrda_cobranca
 Storage Area: indices
** Index Name: pssjrda_id
 Storage Area: indices
** Index Name: pssjrda_id_feder_jurid
 Storage Area: indices
** Index Name: pssjrda_id_previd_social
 Storage Area: indices
** Index Name: pssjrda_matriz
 Storage Area: indices
** Index Name: pssjrda_nom_pessoa_word
 Storage Area: indices
** Index Name: pssjrda_pagto
 Storage Area: indices
** Index Name: pssjrda_razao_social
 Storage Area: indices
** Index Name: pssjrda_unid_federac
 Storage Area: indices
** Index Name: pssjrda_unid_feder_cobr
 Storage Area: indices
** Index Name: pssjrda_unid_feder_pagto
 Storage Area: indices


============================= FIELD DETAILS =============================
============================= Table: pessoa_jurid =======================

** Field Name: num_pessoa_jurid
  Description: N£mero Pessoa Jur°dica
         Help: N£mero Pessoa Jur°dica

** Field Name: nom_pessoa
  Description: Nome Pessoa
         Help: Nome Pessoa
      Val-Msg: FV: Nome deve ser informado(a) !
      Val-Exp: pessoa_jurid.nom_pessoa <> ""

** Field Name: cod_id_feder
  Description: C¢digo ID Federal
         Help: C¢digo ID Federal

** Field Name: cod_id_estad_jurid
  Description: C¢digo ID Estadual Pessoa Jur°dica
         Help: C¢digo ID Estadual Pessoa Jur°dica

** Field Name: cod_id_munic_jurid
  Description: C¢digo ID Municipal Pessoa Jur°dica
         Help: C¢digo ID Municipal Pessoa Jur°dica

** Field Name: cod_id_previd_social
  Description: C¢digo ID Previdància Social
         Help: C¢digo ID Previdància Social

** Field Name: log_fins_lucrat
  Description: Fins Lucrativos
         Help: Fins Lucrativos

** Field Name: num_pessoa_jurid_matriz
  Description: N£mero Pessoa Jur°dica Matriz
         Help: N£mero Pessoa Jur°dica Matriz
      Val-Msg: FV: Matriz deve ser maior que Zero !
      Val-Exp: pessoa_jurid.num_pessoa_jurid_matriz > 0

** Field Name: nom_endereco
  Description: Endereáo
         Help: Endereáo
      Val-Msg: FV: Endereáo deve ser informado(a) !
      Val-Exp: pessoa_jurid.nom_endereco <> ""

** Field Name: nom_ender_compl
  Description: Complemento Endereáo
         Help: Complemento Endereáo

** Field Name: nom_bairro
  Description: Bairro
         Help: Bairro

** Field Name: nom_cidade
  Description: Nome Cidade
         Help: Nome Cidade
      Val-Msg: FV: Cidade deve ser informado(a) !
      Val-Exp: pessoa_jurid.nom_cidade <> ""

** Field Name: nom_condado
  Description: Nome Condado
         Help: Nome Condado

** Field Name: cod_pais
  Description: C¢digo Pa°s
         Help: C¢digo Pa°s

** Field Name: cod_unid_federac
  Description: C¢digo Unidade Federaá∆o
         Help: C¢digo Unidade Federaá∆o

** Field Name: cod_cep
  Description: C¢digo Endereáamento Postal
         Help: C¢digo Endereáamento Postal

** Field Name: cod_cx_post
  Description: Caixa Postal
         Help: Caixa Postal

** Field Name: cod_telefone
  Description: Telefone
         Help: Telefone

** Field Name: cod_fax
  Description: N£mero Fac-S°mile
         Help: N£mero Fac-S°mile

** Field Name: cod_ramal_fax
  Description: Ramal Fac-S°mile
         Help: Ramal Fac-S°mile

** Field Name: cod_telex
  Description: C¢digo Endereáo Telegr†fico
         Help: C¢digo Endereáo Telegr†fico

** Field Name: cod_modem
  Description: Modem
         Help: Modem

** Field Name: cod_ramal_modem
  Description: Ramal Modem
         Help: Ramal Modem

** Field Name: cod_e_mail
  Description: C¢digo Internet E-Mail
         Help: C¢digo Internet E-Mail

** Field Name: cod_e_mail_cobr
  Description: C¢digo E-Mail de Cobranáa
         Help: C¢digo E-Mail de Cobranáa

** Field Name: num_pessoa_jurid_cobr
  Description: N£mero Pessoa Jur°dica Cobranáa
         Help: N£mero Pessoa Jur°dica Cobranáa

** Field Name: nom_ender_cobr
  Description: Endereáo Cobranáa
         Help: Endereáo Cobranáa

** Field Name: nom_ender_compl_cobr
  Description: Endereáo Complemento Cobranáa
         Help: Endereáo Complemento Cobranáa

** Field Name: nom_bairro_cobr
  Description: Bairro Cobranáa
         Help: Bairro Cobranáa

** Field Name: nom_cidad_cobr
  Description: Nome Cidade Cobranáa
         Help: Nome Cidade Cobranáa

** Field Name: nom_condad_cobr
  Description: Nome Condado Cobranáa
         Help: Nome Condado Cobranáa

** Field Name: cod_unid_federac_cobr
  Description: C¢digo Unidade Federaá∆o Cobranáa
         Help: C¢digo Unidade Federaá∆o Cobranáa

** Field Name: cod_pais_cobr
  Description: C¢digo Pa°s Cobranáa
         Help: C¢digo Pa°s Cobranáa

** Field Name: cod_cep_cobr
  Description: C¢digo Endereáamento Postal
         Help: C¢digo Endereáamento Postal

** Field Name: cod_cx_post_cobr
  Description: Caixa Postal Cobranáa
         Help: Caixa Postal Cobranáa

** Field Name: num_pessoa_jurid_pagto
  Description: N£mero Pessoa Jur°dica Pagamento
         Help: N£mero Pessoa Jur°dica Pagamento

** Field Name: nom_ender_pagto
  Description: Endereáo Pagamento
         Help: Endereáo Pagamento

** Field Name: nom_ender_compl_pagto
  Description: Endereáo Complemento Pagamento
         Help: Endereáo Complemento Pagamento

** Field Name: nom_bairro_pagto
  Description: Nome Bairro Pagamento
         Help: Nome Bairro Pagamento

** Field Name: nom_cidad_pagto
  Description: Nome Cidade Pagamento
         Help: Nome Cidade Pagamento

** Field Name: nom_condad_pagto
  Description: Nome Condado Pagamento
         Help: Nome Condado Pagamento

** Field Name: cod_pais_pagto
  Description: C¢digo Pa°s Pagamento
         Help: C¢digo Pa°s Pagamento

** Field Name: cod_unid_federac_pagto
  Description: C¢digo Unidade Federaá∆o Pagamento
         Help: C¢digo Unidade Federaá∆o Pagamento

** Field Name: cod_cep_pagto
  Description: C¢digo CEP Pagamento
         Help: C¢digo CEP Pagamento

** Field Name: cod_cx_post_pagto
  Description: C¢digo Caixa Postal Pagamento
         Help: C¢digo Caixa Postal Pagamento

** Field Name: des_anot_tab
  Description: Descriá∆o Anotaá∆o Tabela
         Help: Descriá∆o Anotaá∆o Tabela

** Field Name: ind_tip_pessoa_jurid
  Description: Indicador Tipo Pessoa Jur°dica
         Help: Indicador Tipo Pessoa Jur°dica
      Val-Msg: FV: Valor n∆o contido na lista de valores poss°veis !
      Val-Exp: can-do("Privada,P£blica,Mista",
               pessoa_jurid.ind_tip_pessoa_jurid)

** Field Name: ind_tip_capit_pessoa_jurid
  Description: Indicador Tipo Capital Pessoa Jur°dica
         Help: Indicador Tipo Capital Pessoa Jur°dica
      Val-Msg: FV: Valor n∆o contido na lista de valores poss°veis !
      Val-Exp: can-do("Nacional,Multinacional,Municipal,Estadual,Federal",
               pessoa_jurid.ind_tip_capit_pessoa_jurid)

** Field Name: cod_usuar_ult_atualiz
  Description: C¢digo Usu†rio Èltima Atualizaá∆o
         Help: C¢digo Usu†rio Èltima Atualizaá∆o

** Field Name: dat_ult_atualiz
  Description: Data Èltima Atualizaá∆o
         Help: Data Èltima Atualizaá∆o

** Field Name: hra_ult_atualiz
  Description: Hora Èltima Atualizaá∆o
         Help: Hora Èltima Atualizaá∆o

** Field Name: cod_imagem
  Description: C¢digo Imagem
         Help: C¢digo Imagem

** Field Name: log_ems_20_atlzdo
  Description: L¢gico Ems 2.0 Atualizado
         Help: L¢gico Ems 2.0 Atualizado

** Field Name: cod_livre_1
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: log_livre_1
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: num_livre_1
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: val_livre_1
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: dat_livre_1
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: nom_home_page
  Description: Nome Home Page
         Help: Nome Home Page

** Field Name: nom_ender_text
  Description: Nome Endereco Texto
         Help: Nome Endereco Texto

** Field Name: nom_ender_cobr_text
  Description: Nome Endereco Cobranca Texto
         Help: Nome Endereco Cobranca Texto

** Field Name: nom_ender_pagto_text
  Description: Nome Endereco Pagamento Texto
         Help: Nome Endereco Pagamento Texto

** Field Name: log_envio_bco_histor
  Description: L¢gico Envio Banco Historico
         Help: L¢gico Envio Banco Historico

** Field Name: cod_livre_2
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: dat_livre_2
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: log_livre_2
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: num_livre_2
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: val_livre_2
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: ind_natur_pessoa_jurid
  Description: Indicar a Natureza da Pessoa Jur°dica
         Help: Indicar a Natureza da Pessoa Jur°dica
      Val-Msg: FV: Valor n∆o contido na lista de valores poss°veis !
      Val-Exp: can-do("Nacional,Estrangeiro,Trading",
               pessoa_jurid.ind_natur_pessoa_jurid)

** Field Name: nom_fantasia
  Description: Nome Fantasia da Pessoa Jur°dica
         Help: Nome Fantasia da Pessoa Jur°dica

** Field Name: cod_sub_regiao_vendas
  Description: C¢digo Micro Regi∆o Vendas
         Help: C¢digo Micro Regi∆o Vendas

** Field Name: cdd_version
  Description: C¢digo da Vers∆o do Registro
         Help: C¢digo da Vers∆o do Registro

** Field Name: log_replic_pessoa_hcm
  Description: L¢gico Replica Pessoa HCM
         Help: L¢gico Replica Pessoa HCM

** Field Name: log_replic_pessoa_crm
  Description: L¢gico Replica Pessoa CRM
         Help: L¢gico Replica Pessoa CRM

** Field Name: log_replic_pessoa_gps
  Description: L¢gico Replica Pessoa GPS
         Help: L¢gico Replica Pessoa GPS

** Field Name: cod_fax_2
  Description: C¢digo Fax 2
         Help: C¢digo Fax 2

** Field Name: cod_ramal_fax_2
  Description: C¢digo Ramal Fax 2
         Help: C¢digo Ramal Fax 2

** Field Name: cod_telef_2
  Description: Telefone 2
         Help: Telefone 2

** Field Name: cod_ramal_2
  Description: Ramal 2
         Help: Ramal 2

** Field Name: ind_tip_matriz
  Description: Tipo Matriz
         Help: Tipo Matriz


*/
