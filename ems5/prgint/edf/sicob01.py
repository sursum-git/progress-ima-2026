/*****************************************************************************
** Copyright DATASUL S.A. (1994)
** Programa..............: 
** Descricao.............: Acerta Valor de Juros e Valor Recebido
** Nome Externo..........: prgint/edf/edfleo01.py
** Data Geracao..........: 18/08/2010
** Criado por............: Amarildo (Consultoria Framework)
*****************************************************************************/

 

/********************* Temporary Table Definition Begin *********************/
def temp-table tt_param_program_formul no-undo
   field tta_cdn_segment_edi              as Integer format ">>>>>9" initial 0 label "Segmento" column-label "Segmento"
   field tta_cdn_element_edi              as Integer format ">>>>>9" initial 0 label "Elemento" column-label "Elemento"
   field tta_des_label_utiliz_formul_edi  as character format "x(10)" label "Label Utiliz Formula" column-label "Label Utiliz Formula"
   field ttv_des_contdo                   as character format "x(100)" label "Conteudo" column-label "Conteudo"
   index tt_param_program_formul_id       is primary
         tta_cdn_segment_edi              ascending
         tta_cdn_element_edi              ascending.
/********************** Temporary Table Definition End **********************/

 

/************************ Parameter Definition Begin ************************/
/* Elemento do mapa de destino n*/
/* ********************** */
def Input param p_cdn_mapa_edi
    as Integer
    format ">>>>>9"
    no-undo.

def Input param p_cdn_segment_edi
    as Integer
    format ">>>>>9"
    no-undo.

def Input param p_cdn_element_edi
    as Integer
    format ">>>>>9"
    no-undo.

def Input param table 
    for tt_param_program_formul.

/************************* Parameter Definition End *************************/

 

/****************************** Main Code Begin *****************************/
/* Elemento do mapa de origem */
/* **********************/

find tt_param_program_formul
     where tt_param_program_formul.tta_cdn_segment_edi = 292
     and tt_param_program_formul.tta_cdn_element_edi   = 4423 
     no-error.

if available tt_param_program_formul 
    then do:
    if int(tt_param_program_formul.ttv_des_contdo) <> 0
       then return "1".
       else return "0".
end.

return "".        
/******************************* Main Code End ******************************/

