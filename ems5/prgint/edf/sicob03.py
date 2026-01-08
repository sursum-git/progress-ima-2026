/*****************************************************************************
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
define var dia as int.
define var mes as int.
define var ano as int.
define var data as date.

//return string(TODAY  + 1).        
/******************************* Main Code End ******************************/
output to c:\temp\edi.txt.
/*for each  tt_param_program_formul .
   export delimiter "|" tt_param_program_formul .
end.*/

disp p_cdn_segment_edi  p_cdn_element_edi .

find first tt_param_program_formul
   where tt_param_program_formul.tta_cdn_segment_edi = 292
   and   tt_param_program_formul.tta_cdn_element_edi =  3606
   no-error.
if avail tt_param_program_formul  then do:
   assign dia = int(substr(tt_param_program_formul.ttv_des_contdo,1,2))
          mes = int(substr(tt_param_program_formul.ttv_des_contdo,3,2))
          ano = int(substr(tt_param_program_formul.ttv_des_contdo,5,4))
          data = date(mes, dia, ano) + 1.
end.                                     
output close.
return string(data,'99999999').
