DEFINE VARIABLE dMedia AS DECIMAL     NO-UNDO.
OUTPUT TO c:\temp\ITEM_custos.txt .
FOR EACH ITEM_custos
WHERE cod_tipo_custo = 3 :
    
    ASSIGN dMedia = ((qt_estoque * vl_unit_anterior) + 
        (qt_estoque_media * vl_unit_media)) / (qt_estoque + qt_estoque_media) .
    IF ROUND(dmedia,2) <> vl_unit_novo THEN
    DISP ITEM_custo_id ITEM_custos.it_codigo ITEM_custos.it_codigo_media
    qt_estoque vl_unit_anterior
    qt_estoque_media
    vl_unit_media
    dMedia COLUMN-LABEL "media calc."
    vl_unit_novo     
    WITH  WIDTH 550.

    //ASSIGN cod_usuario = 'ana.flavia'.
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
  150 cod_situacao                     inte
  160 cod_usuario                      char
  170 qt_estoque_selecionado           deci-2
  180 it_codigo_media                  char
  190 qt_estoque_media                 deci-2
  200 vl_unit_media                    deci-2
  210 vl_unit_container                deci-2
  220 qt_container                     deci-2




*/
