FOR EACH ITEM_lancto_ctbl
    WHERE cod_empresa = '100'
    AND cod_cta_ctbl = '41400007'
    AND cod_ccusto = '',
    EACH lancto_ctbl OF ITEM_lancto_ctbl
    WHERE lancto_ctbl.dat_lancto_ctbl >= 01.01.2015
    AND lancto_ctbl.dat_lancto_ctbl <= 12.31.2015:
    DISP lancto_ctbl WITH 1 COL WIDTH 550.
    DISP ITEM_lancto_ctbl  WITH 1 COL WIDTH 550.
END.

/*

=========================================================================
============================= Table: item_lancto_ctbl ===================

              Table Flags: "f" = frozen, "s" = a SQL table


Table                            Table Field Index Table
Name                             Flags Count Count Label
-------------------------------- ----- ----- ----- ------------------------
item_lancto_ctbl                          38    17 Item Lanáamento Cont†bil

    Dump Name: fin160
  Description: Item de Lanáamento Cont†bil
 Storage Area: dados

       Trigger Event Trigger Procedure    Overridable? Check CRC?
       ------------- -------------------- ------------ ----------
       create        database/tgfin/tcp/t yes          no
       delete        database/tgfin/tdp/t yes          no
       write         database/tgfin/twp/t yes          no


============================= FIELD SUMMARY =============================
============================= Table: item_lancto_ctbl ===================

Flags: <c>ase sensitive, <i>ndex component, <m>andatory, <v>iew component

Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 num_lote_ctbl                    inte        im
   20 num_lancto_ctbl                  inte        im
   30 num_seq_lancto_ctbl              inte        im
   40 ind_natur_lancto_ctbl            char        m
   50 cod_empresa                      char        im
   60 cod_plano_cta_ctbl               char        im
   70 cod_cta_ctbl                     char        im
   80 cod_plano_ccusto                 char        im
   90 cod_ccusto                       char        i
  100 cod_estab                        char        im
  110 cod_unid_negoc                   char        i
  120 cod_proj_financ                  char        im
  130 cod_histor_padr                  char        i
  140 cod_espec_docto                  char        i
  150 dat_docto                        date
  160 des_docto                        char
  170 cod_imagem                       char        i
  180 cod_indic_econ                   char        im
  190 dat_lancto_ctbl                  date        im
  200 qtd_unid_lancto_ctbl             deci-2      m
  210 val_lancto_ctbl                  deci-2      im
  220 des_histor_lancto_ctbl           char        im
  230 num_seq_lancto_ctbl_cpart        inte        m
  240 cod_tip_lancto_ctbl              char        im
  250 cod_cenar_ctbl                   char        m
  260 ind_sit_lancto_ctbl              char        im
  270 log_lancto_apurac_restdo         logi        m
  280 cod_livre_1                      char
  290 val_seq_entry_number             deci-0      i
  300 cod_livre_2                      char
  310 dat_livre_1                      date
  320 dat_livre_2                      date
  330 log_livre_1                      logi
  340 log_livre_2                      logi
  350 num_livre_1                      inte
  360 num_livre_2                      inte
  370 val_livre_1                      deci-4
  380 val_livre_2                      deci-4

Field Name                       Format
-------------------------------- -----------------------------
num_lote_ctbl                    >>>,>>>,>>9
num_lancto_ctbl                  >>,>>>,>>9
num_seq_lancto_ctbl              >>>>9
ind_natur_lancto_ctbl            X(02)
cod_empresa                      x(3)
cod_plano_cta_ctbl               x(8)
cod_cta_ctbl                     x(20)
cod_plano_ccusto                 x(8)
cod_ccusto                       x(11)
cod_estab                        x(5)
cod_unid_negoc                   x(3)
cod_proj_financ                  x(20)
cod_histor_padr                  x(8)
cod_espec_docto                  x(3)
dat_docto                        99/99/9999
des_docto                        x(25)
cod_imagem                       x(30)
cod_indic_econ                   x(8)
dat_lancto_ctbl                  99/99/9999
qtd_unid_lancto_ctbl             >>,>>>,>>9.99
val_lancto_ctbl                  >>>>>,>>>,>>9.99
des_histor_lancto_ctbl           x(2000)
num_seq_lancto_ctbl_cpart        >>>9
cod_tip_lancto_ctbl              x(8)
cod_cenar_ctbl                   x(8)
ind_sit_lancto_ctbl              X(4)
log_lancto_apurac_restdo         Sim/N∆o
cod_livre_1                      x(100)
val_seq_entry_number             >>>,>>>,>>>,>>9
cod_livre_2                      x(100)
dat_livre_1                      99/99/9999
dat_livre_2                      99/99/9999
log_livre_1                      Sim/N∆o
log_livre_2                      Sim/N∆o
num_livre_1                      >>>>>9
num_livre_2                      >>>>>9
val_livre_1                      >>>,>>>,>>9.9999
val_livre_2                      >>>,>>>,>>9.9999

Field Name                       Initial
-------------------------------- -----------------------------
num_lote_ctbl                    0
num_lancto_ctbl                  0
num_seq_lancto_ctbl              0
ind_natur_lancto_ctbl            DB
cod_empresa
cod_plano_cta_ctbl
cod_cta_ctbl
cod_plano_ccusto
cod_ccusto
cod_estab
cod_unid_negoc
cod_proj_financ
cod_histor_padr
cod_espec_docto
dat_docto                        ?
des_docto
cod_imagem
cod_indic_econ
dat_lancto_ctbl                  today
qtd_unid_lancto_ctbl             0
val_lancto_ctbl                  0
des_histor_lancto_ctbl
num_seq_lancto_ctbl_cpart        0
cod_tip_lancto_ctbl
cod_cenar_ctbl
ind_sit_lancto_ctbl              Pend
log_lancto_apurac_restdo         no
cod_livre_1
val_seq_entry_number             0
cod_livre_2
dat_livre_1                      ?
dat_livre_2                      ?
log_livre_1                      no
log_livre_2                      no
num_livre_1                      0
num_livre_2                      0
val_livre_1                      0
val_livre_2                      0

Field Name                     Label                  Column Label
------------------------------ ---------------------- ----------------------
num_lote_ctbl                  Lote Cont†bil          Lote Cont†bil
num_lancto_ctbl                Lanáamento Cont†bil    Lanáamento Cont†bil
num_seq_lancto_ctbl            Sequància Lanáto       Sequància Lanáto
ind_natur_lancto_ctbl          Natureza               Natureza
cod_empresa                    Empresa                Empresa
cod_plano_cta_ctbl             Plano Contas           Plano Contas
cod_cta_ctbl                   Conta Cont†bil         Conta Cont†bil
cod_plano_ccusto               Plano Centros Custo    Plano Centros Custo
cod_ccusto                     Centro Custo           Centro Custo
cod_estab                      Estabelecimento        Estab
cod_unid_negoc                 Unid Neg¢cio           Un Neg
cod_proj_financ                Projeto                Projeto
cod_histor_padr                Hist¢rico Padr∆o       Hist¢rico Padr∆o
cod_espec_docto                EspÇcie Documento      EspÇcie
dat_docto                      Data Documento         Data Documento
des_docto                      N£mero Documento       N£mero Documento
cod_imagem                     Imagem                 Imagem
cod_indic_econ                 Moeda                  Moeda
dat_lancto_ctbl                Data Lanáamento        Data Lanáto
qtd_unid_lancto_ctbl           Quantidade             Quantidade
val_lancto_ctbl                Valor Lanáamento       Valor Lanáamento
des_histor_lancto_ctbl         Hist¢rico Cont†bil     Hist¢rico Cont†bil
num_seq_lancto_ctbl_cpart      Sequància CPartida     Sequància CP
cod_tip_lancto_ctbl            Tipo Lanáamento        Tipo Lanáto
cod_cenar_ctbl                 Cen†rio Cont†bil       Cen†rio Cont†bil
ind_sit_lancto_ctbl            Situaá∆o Lanáamento    Situaá∆o Lanáto
log_lancto_apurac_restdo       Lanáamento Apuraá∆o    Lancto Apuraá∆o
cod_livre_1                    Livre 1                Livre 1
val_seq_entry_number           Entry Number           Entry Number
cod_livre_2                    Livre 2                Livre 2
dat_livre_1                    Livre 1                Livre 1
dat_livre_2                    Livre 2                Livre 2
log_livre_1                    Livre 1                Livre 1
log_livre_2                    Livre 2                Livre 2
num_livre_1                    Livre 1                Livre 1
num_livre_2                    Livre 2                Livre 2
val_livre_1                    Livre 1                Livre 1
val_livre_2                    Livre 2                Livre 2


============================= INDEX SUMMARY =============================
============================= Table: item_lancto_ctbl ===================

Flags: <p>rimary, <u>nique, <w>ord, <a>bbreviated, <i>nactive, + asc, - desc

Flags Index Name                       Cnt Field Name
----- -------------------------------- --- ---------------------------------
      tmlnctcb_ccusto                    3 + cod_empresa
                                           + cod_plano_ccusto
                                           + cod_ccusto

      tmlnctcb_cta_ctbl                  2 + cod_plano_cta_ctbl
                                           + cod_cta_ctbl

      tmlnctcb_data_lancto               2 + cod_empresa
                                           + dat_lancto_ctbl

w     tmlnctcb_des_histor                1 + des_histor_lancto_ctbl

      tmlnctcb_entry_number              1 + val_seq_entry_number

      tmlnctcb_espec_docto               1 + cod_espec_docto

      tmlnctcb_estab                     5 + cod_estab
                                           + cod_plano_cta_ctbl
                                           + cod_cta_ctbl
                                           + ind_sit_lancto_ctbl
                                           + dat_lancto_ctbl

      tmlnctcb_histor_padr               1 + cod_histor_padr

pu    tmlnctcb_id                        3 + num_lote_ctbl
                                           + num_lancto_ctbl
                                           + num_seq_lancto_ctbl

      tmlnctcb_imagem                    1 + cod_imagem

      tmlnctcb_indic_econ                1 + cod_indic_econ

      tmlnctcb_movto_ctbl                9 + cod_empresa
                                           + cod_plano_cta_ctbl
                                           + cod_cta_ctbl
                                           + cod_plano_ccusto
                                           + cod_ccusto
                                           + cod_estab
                                           + cod_unid_negoc
                                           + cod_proj_financ
                                           + dat_lancto_ctbl

      tmlnctcb_pessoa                    1 + cod_empresa

      tmlnctcb_proj_financ               1 + cod_proj_financ

      tmlnctcb_tplnctct                  1 + cod_tip_lancto_ctbl

      tmlnctcb_unid_negoc                1 + cod_unid_negoc

      tmlnctcb_valor                     2 + cod_empresa
                                           + val_lancto_ctbl

** Index Name: tmlnctcb_ccusto
 Storage Area: indices
** Index Name: tmlnctcb_cta_ctbl
 Storage Area: indices
** Index Name: tmlnctcb_data_lancto
 Storage Area: indices
** Index Name: tmlnctcb_des_histor
 Storage Area: indices
** Index Name: tmlnctcb_entry_number
 Storage Area: indices
** Index Name: tmlnctcb_espec_docto
 Storage Area: indices
** Index Name: tmlnctcb_estab
 Storage Area: indices
** Index Name: tmlnctcb_histor_padr
 Storage Area: indices
** Index Name: tmlnctcb_id
 Storage Area: indices
** Index Name: tmlnctcb_imagem
 Storage Area: indices
** Index Name: tmlnctcb_indic_econ
 Storage Area: indices
** Index Name: tmlnctcb_movto_ctbl
 Storage Area: indices
** Index Name: tmlnctcb_pessoa
 Storage Area: indices
** Index Name: tmlnctcb_proj_financ
 Storage Area: indices
** Index Name: tmlnctcb_tplnctct
 Storage Area: indices
** Index Name: tmlnctcb_unid_negoc
 Storage Area: indices
** Index Name: tmlnctcb_valor
 Storage Area: indices


============================= FIELD DETAILS =============================
============================= Table: item_lancto_ctbl ===================

** Field Name: num_lote_ctbl
  Description: N£mero Lote Cont†bil
         Help: N£mero Lote Cont†bil

** Field Name: num_lancto_ctbl
  Description: N£mero Lanáamento Cont†bil
         Help: N£mero Lanáamento Cont†bil

** Field Name: num_seq_lancto_ctbl
  Description: N£mero SeqÅància Lanáamento
         Help: N£mero SeqÅància Lanáamento

** Field Name: ind_natur_lancto_ctbl
  Description: Indicador Natureza Lanáamento Cont†bil
         Help: Indicador Natureza Lanáamento Cont†bil
      Val-Msg: FV: Valor n∆o contido na lista de valores poss°veis !
      Val-Exp: can-do("DB,CR", item_lancto_ctbl.ind_natur_lancto_ctbl)

** Field Name: cod_empresa
  Description: C¢digo Empresa
         Help: C¢digo Empresa

** Field Name: cod_plano_cta_ctbl
  Description: C¢digo Plano Contas
         Help: C¢digo Plano Contas

** Field Name: cod_cta_ctbl
  Description: C¢digo Conta Cont†bil
         Help: C¢digo Conta Cont†bil

** Field Name: cod_plano_ccusto
  Description: C¢digo Plano Centros Custo
         Help: C¢digo Plano Centros Custo

** Field Name: cod_ccusto
  Description: C¢digo Centro Custo
         Help: C¢digo Centro Custo

** Field Name: cod_estab
  Description: C¢digo Estabelecimento
         Help: C¢digo Estabelecimento

** Field Name: cod_unid_negoc
  Description: C¢digo Unidade Neg¢cio
         Help: C¢digo Unidade Neg¢cio

** Field Name: cod_proj_financ
  Description: C¢digo Projeto
         Help: C¢digo Projeto

** Field Name: cod_histor_padr
  Description: C¢digo Hist¢rico Padr∆o
         Help: C¢digo Hist¢rico Padr∆o

** Field Name: cod_espec_docto
  Description: C¢digo EspÇcie Documento
         Help: C¢digo EspÇcie Documento

** Field Name: dat_docto
  Description: Data Documento
         Help: Data Documento

** Field Name: des_docto
  Description: Descriá∆o N£mero Documento
         Help: Descriá∆o N£mero Documento

** Field Name: cod_imagem
  Description: C¢digo Imagem
         Help: C¢digo Imagem

** Field Name: cod_indic_econ
  Description: C¢digo Indicador Econìmico
         Help: C¢digo Indicador Econìmico

** Field Name: dat_lancto_ctbl
  Description: Data Lanáamento Cont†bil
         Help: Data Lanáamento Cont†bil
      Val-Msg: FV: Data Lanáamento deve ser preenchido !
      Val-Exp: item_lancto_ctbl.dat_lancto_ctbl <> ?

** Field Name: qtd_unid_lancto_ctbl
  Description: Quantidade Unidades Lanáamento Cont†bil
         Help: Quantidade Unidades Lanáamento Cont†bil

** Field Name: val_lancto_ctbl
  Description: Valor Lanáamento Cont†bil
         Help: Valor Lanáamento Cont†bil

** Field Name: des_histor_lancto_ctbl
  Description: Descriá∆o Hist¢rico Lanáamento Cont†bil
         Help: Descriá∆o Hist¢rico Lanáamento Cont†bil

** Field Name: num_seq_lancto_ctbl_cpart
  Description: N£mero Sequància Lanáamento Cont†bil Contra partida
         Help: N£mero Sequància Lanáamento Cont†bil Contra partida

** Field Name: cod_tip_lancto_ctbl
  Description: C¢digo Tipo Lanáamento Cont†bil
         Help: C¢digo Tipo Lanáamento Cont†bil

** Field Name: cod_cenar_ctbl
  Description: C¢digo Cen†rio Cont†bil
         Help: C¢digo Cen†rio Cont†bil

** Field Name: ind_sit_lancto_ctbl
  Description: Indicador Situaá∆o Lanáamento Cont†bil
         Help: Indicador Situaá∆o Lanáamento Cont†bil
      Val-Msg: FV: Valor n∆o contido na lista de valores poss°veis !
      Val-Exp: can-do("Pend,Conf,Ctbz",
               item_lancto_ctbl.ind_sit_lancto_ctbl)

** Field Name: log_lancto_apurac_restdo
  Description: Lanáamento Apuraá∆o Resultados
         Help: Lanáamento Apuraá∆o Resultados

** Field Name: cod_livre_1
  Description: Atributo livre para uso interno
         Help: Atributo livre para uso interno

** Field Name: val_seq_entry_number
  Description: Seq. Entry Number
         Help: Seq. Entry Number

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


*/
