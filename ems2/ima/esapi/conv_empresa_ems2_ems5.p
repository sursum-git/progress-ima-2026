DEFINE INPUT  PARAMETER cEmpresaEntrada AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cEmpresaSaida   AS CHARACTER   NO-UNDO.

FIND FIRST trad_org_ext
     WHERE trad_org_ext.cod_matriz_trad_org_ext = 'ems2'
     AND   trad_org_ext.cod_tip_unid_organ = '998'
     AND   trad_org_ext.cod_unid_organ_ext = cEmpresaEntrada.
IF AVAIL trad_org_ext THEN DO:
   ASSIGN cEmpresaSaida = trad_org_ext.cod_unid_organ .
   
END.


