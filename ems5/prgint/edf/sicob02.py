/*****************************************************************************
** Copyright DATASUL S.A. (1994)
** Programa..............: 
** Descricao.............: Acerta Valor de Juros e Valor Recebido
** Nome Externo..........: prgint/edf/sicob02.py
** Data Geracao..........: 17/09/2018
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
    
def var teste as char.
def var totTam as integer.
def var posIfem as integer.

def var vNossoNum as char.
def var vParc as char.
def var vRetorno as char.
/************************* Parameter Definition End *************************/

 

/****************************** Main Code Begin *****************************/
/* Elemento do mapa de origem */
/* **********************/

find tt_param_program_formul
     where tt_param_program_formul.tta_cdn_segment_edi = 292
     and tt_param_program_formul.tta_cdn_element_edi   = 3887 
     no-error.
     assign vParc = tt_param_program_formul.ttv_des_contdo.     

find tt_param_program_formul
     where tt_param_program_formul.tta_cdn_segment_edi = 292
     and tt_param_program_formul.tta_cdn_element_edi   = 3743 
     no-error.

if available tt_param_program_formul 
    then do:
   assign teste = tt_param_program_formul.ttv_des_contdo.
   assign teste = trim(teste).  
   
/*   assign totTam = length(teste).     
   assign posIfem = index(teste,"-"). 
   if posIfem > 0 then
       assign vNossoNum = substring(teste,(posIfem + 1),(totTam - posIfem)).
   else
       assign vNossoNum = teste. */

       assign vNossoNum = REPLACE(teste,'-', '').

   assign vRetorno = string(int(vNossoNum),"9999999999")+ string(int(vParc),"99") + "014     ".


/*message "teste" teste skip
	"totTam" totTam skip
	"posIfem" posIfem skip
	"vNossoNum" vNossoNum skip
	"vRetorno" vRetorno view-as alert-box error buttons ok.*/
   
   return vRetorno.
end.


return "".        
/******************************* Main Code End ******************************/

