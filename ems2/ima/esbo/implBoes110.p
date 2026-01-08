{esbo/boEs110.i ttRegra}
{method/dbotterr.i}
DEFINE VARIABLE hBo AS HANDLE      NO-UNDO.

RUN esbo/boEs110.p PERSIST SET hBo.
RUN openqueryStatic IN hBo('main').

CREATE ttRegra.
ASSIGN ttRegra.cod_estab                = '505'
       ttRegra.uf                       = 'sc'
       ttRegra.dt_hr_ini                = NOW - 5000000000
       ttRegra.dt_hr_fim                = NOW + 5000000000
       ttRegra.vl_minimo_pedido         = 1501
       ttRegra.vl_maximo_pedido         = 3000
       ttRegra.perc_min_preco_tb        = 0
       ttRegra.per_max_preco_tb        = 999
       ttRegra.cod_tipo_frete           = 1
       ttRegra.num_tipo_cobranca_frete  = 1       
       .
       
RUN emptyRowErrors IN hBo.
RUN setRecord   IN hBo(TABLE ttRegra).
RUN createRecord IN hBo.
RUN getRowErrors IN hBo(OUTPUT TABLE rowErrors).
IF CAN-FIND(FIRST ROWErrors) THEN
DO:
  FOR EACH rowErrors:
      MESSAGE rowErrors.errorDESCRIPTION
          VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  END.
    
END.



       
