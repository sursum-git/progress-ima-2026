DEFINE VARIABLE iSinal AS INTEGER     NO-UNDO.
DEFINE VARIABLE cDescricao AS CHARACTER   NO-UNDO FORMAT 'x(300)'.
DEFINE VARIABLE dValor     AS DECIMAL     NO-UNDO FORMAT '>>>>>,>>>,>>9.99'.
DEFINE VARIABLE iCont AS INTEGER     NO-UNDO.
OUTPUT TO c:\temp\ITEM_lancto_ctbl501.txt.
FOR EACH ITEM_lancto_ctbl 
    WHERE ITEM_lancto_ctbl.cod_empresa = '500'
    AND   ITEM_lancto_ctbl.dat_lancto_ctbl >= 01.01.2017
    AND   ITEM_lancto_ctbl.dat_lancto_ctbl <= 08.31.2017
    /*AND   ITEM_lancto_ctbl.cod_cta_ctbl = '19999999'   21347/2/3*/
   /* AND   ITEM_lancto_ctbl.num_lote_ctbl = 21347
    AND   ITEM_lancto_ctbl.num_lancto_ctbl = 2
    AND   ITEM_lancto_ctbl.num_seq_lancto_ctbl = 3*/
    AND ITEM_lancto_ctbl.ind_sit_lancto_ctbl <> ''
    //AND ITEM_lancto_ctbl.cod_indic_econ =  'real' 
    AND ITEM_lancto_ctbl.cod_cta_ctbl <> '19999999'
    BY    ITEM_lancto_ctbl.cod_cta_ctbl BY ITEM_lancto_ctbl.cod_ccusto BY ITEM_lancto_ctbl.dat_lancto_ctbl .
    //DISP ITEM_lancto_ctbl WITH 1 COL WIDTH 550.
    
       // DISP ITEM_lancto_ctbl WITH 1 COL WIDTH 550.
    IF ITEM_lancto_ctbl.cod_indic_econ <> 'real'  THEN DO:
       FIND aprop_lancto_ctbl OF ITEM_lancto_ctbl
       WHERE aprop_lancto_ctbl.cod_finalid_econ = 'corrente' NO-LOCK NO-ERROR.
       IF AVAIL aprop_lancto_ctbl  THEN
          ASSIGN dValor = aprop_lancto_ctbl.val_lancto_ctbl.
       ELSE
          ASSIGN dValor = ITEM_lancto_ctbl.val_lancto_ctbl.
    END.
    ELSE
       ASSIGN dValor = ITEM_lancto_ctbl.val_lancto_ctbl.

    /*DISP ITEM_lancto_ctbl WITH 1 COL WIDTH 550.
    DISP aprop_lancto_ctbl WITH 1 COL WIDTH 550.*/
    /*21560|1|18*/

     IF ind_natur_lancto_ctbl = 'db' THEN 
       ASSIGN iSinal = 1.
    ELSE
      ASSIGN iSinal = -1.
    FIND cta_ctbl OF ITEM_lancto_ctbl
        NO-LOCK NO-ERROR.

    FIND ems5.ccusto OF ITEM_lancto_ctbl 
        NO-LOCK NO-ERROR.

   /*REPEAT iCont = 1 TO LENGTH(ITEM_lancto_ctbl.des_histor_lancto_ctbl):
       
       PUT ASC(substr(ITEM_lancto_ctbl.des_histor_lancto_ctbl,iCont,1)) "->" substr(ITEM_lancto_ctbl.des_histor_lancto_ctbl,iCont,1) SKIP.
   END.*/


    ASSIGN cDescricao = REPLACE(ITEM_lancto_ctbl.des_histor_lancto_ctbl,CHR(10), "*").

    /*PUT 'apos substitui‡Æo' SKIP.
    REPEAT iCont = 1 TO LENGTH(cDescricao):
       PUT ASC( cDescricao) "->" substr(cdescricao,iCont,1) SKIP.
    END.
    MESSAGE cDescricao
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

    /*ASSIGN cDescricao = REPLACE(ITEM_lancto_ctbl.des_histor_lancto_ctbl,CHR(13), "*").    */
    
    EXPORT DELIMITER "|"
    ITEM_lancto_ctbl.num_lote_ctbl
    ITEM_lancto_ctbl.num_lancto_ctbl
    ITEM_lancto_ctbl.num_seq_lancto_ctbl
    ITEM_lancto_ctbl.ind_natur_lancto_ctbl
    ITEM_lancto_ctbl.cod_empresa
    ITEM_lancto_ctbl.cod_estab
    ITEM_lancto_ctbl.cod_cta_ctbl
    cta_ctbl.des_tit_ctbl
    ITEM_lancto_ctbl.cod_ccusto
    IF AVAIL ccusto THEN ccusto.des_tit_ctbl ELSE ''
    ITEM_lancto_ctbl.cod_unid_negoc
    //ITEM_lancto_ctbl.cod_histor_padr
    //ITEM_lancto_ctbl.cod_indic_econ
    ITEM_lancto_ctbl.dat_lancto_ctbl
    cDescricao
    ITEM_lancto_ctbl.num_seq_lancto_ctbl_cpart
    //ITEM_lancto_ctbl.cod_tip_lancto_ctbl
    //ITEM_lancto_ctbl.cod_cenar_ctbl
    //
    //log_lancto_apurac_restdo
    dValor
    dValor * iSinal
    ITEM_lancto_ctbl.ind_sit_lancto_ctbl.


END.
OUTPUT CLOSE.

