DEFINE OUTPUT PARAMETER cEstab AS CHARACTER   NO-UNDO.
{utp/ut-glob.i}


 FIND FIRST
    trad_org_ext
    WHERE cod_unid_organ_ext =  i-ep-codigo-usuario NO-LOCK NO-ERROR.
 IF AVAIL trad_org_ext  THEN

    ASSIGN cEstab =  string(int(trad_org_ext.cod_unid_organ) + 1).
