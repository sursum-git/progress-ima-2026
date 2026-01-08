DEF TEMP-TABLE tt
    FIELD itCodigo AS CHAR FORMAT 'X(20)'
    FIELD valor    AS DECIMAL
    FIELD dtRefer  AS DATE.

INPUT FROM u:\custo_itens.csv.
REPEAT:
    CREATE tt.
    IMPORT DELIMITER ";" tt.
END.
INPUT CLOSE.

FOR EACH tt.
    CREATE ITEM_custos.
    ASSIGN item_custos.ITEM_custo_id    = NEXT-VALUE(seq_item_custos)
           ITEM_custos.dt_hr_custo      = NOW
           item_custos.vl_unit_novo     = tt.valor
           ITEM_custos.dt_referencia    = tt.dtRefer
           ITEM_custos.cod_tipo_custo   = 1
           ITEM_custos.it_codigo        = tt.itCodigo
           ITEM_custos.cod_situacao     = 2.     
END.

/*


Order Field Name                       Data Type   Flags
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

*/
