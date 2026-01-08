DEFINE INPUT  PARAMETER cEstabEntrada AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cSistema      AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cEstabSaida   AS CHARACTER   NO-UNDO.

FIND FIRST trad_org_ext
     WHERE trad_org_ext.cod_matriz_trad_org_ext = 'ems2'
     AND   trad_org_ext.cod_tip_unid_organ = '999'
     AND   ( (cSistema  = 'ems5' AND trad_org_ext.cod_unid_organ_ext = cEstabEntrada)
             OR (cSistema = 'ems2' AND trad_org_ext.cod_unid_organ  = cEstabEntrada)
            ).
IF AVAIL trad_org_ext THEN DO:
   IF cSistema = 'ems5' THEN
      ASSIGN cEstabSaida = trad_org_ext.cod_unid_organ .
   ELSE
      ASSIGN cEstabSaida = trad_org_ext.cod_unid_organ_ext .
END.


