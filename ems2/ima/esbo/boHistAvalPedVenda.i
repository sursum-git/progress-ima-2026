DEFINE TEMP-TABLE ttHistAvalPedVenda LIKE hist_aval_ped_venda 
       FIELD descCodTipoAprov AS CHAR FORMAT 'x(50)'
       FIELD descIndSituacao  AS CHAR FORMAT 'x(50)'
       FIELD dtHrSubstituicao AS DATETIME .



/*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 hist_aval_ped_venda_id           inte        i
   20 nr_pedido                        inte        i
   30 dt_hr_aval                       datetm
   40 cod_usuario                      char
   50 cod_tipo_aprov                   inte        i
   60 ind_situacao                     inte        i
   70 cod_estab                        char        i
   80 dt_hr_registro                   datetm
   90 hist_aval_ped_venda_relac_id     inte
  100 desc_motivo                      char
  110 ped_web_id                       inte        i
*/

