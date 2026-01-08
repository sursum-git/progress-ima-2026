DEFINE TEMP-TABLE tt
    FIELD It_codigo LIKE ITEM.it-codigo
    FIELD vl_unit AS DECIMAL FORMAT '->>>,>>>,>>9.99'.

INPUT FROM c:\temp\saldos_itens.csv.

REPEAT:
    CREATE tt.
    IMPORT DELIMITER ";" tt.

END.

INPUT CLOSE.
FOR EACH tt WHERE tt.vl_unit  <> 0:
    CREATE ITEM_custos.
    ASSIGN ITEM_custos.ITEM_custo_id = NEXT-VALUE(seq_item_custos)
           ITEM_custos.dt_hr_custo = NOW
           ITEM_custos.vl_unit_novo = tt.vl_unit
           ITEM_custos.dt_referencia = TODAY
           ITEM_custos.cod_tipo_custo = 1
           ITEM_custos.it_codigo = tt.IT_codigo.
END.


/*Order Field Name                       Data Type   Flags
----- -------------------------------- ----------- -----
   10 item_custo_id                    inte        i
   20 dt_hr_custo                      datetm
   30 vl_unit_novo                     deci-2
   40 dt_referencia                    date        i
   50 caminho_arquivo                  char
   60 cod_tipo_custo                   inte        i
   70 memoria_calculo                  char
   80 it_codigo                        char        i
   90 qt_estoque                       deci-2
  100 qt_nf_mae                        deci-2
  110 qt_etiq_container                deci-2
  120 qt_etiq_branco                   deci-2
  130 qt_etiq_outros_container         deci-2
  140 vl_unit_anterior                 deci-2
  150 cod_situacao                     inte
  160 cod_usuario                      char
  170 qt_estoque_selecionado           deci-2
  180 it_codigo_media                  char
  190 qt_estoque_media                 deci-2
  200 vl_unit_media                    deci-2
  210 vl_unit_container                deci-2
  220 qt_container                     deci-2
 */
