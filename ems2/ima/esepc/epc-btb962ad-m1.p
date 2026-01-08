DEF NEW GLOBAL SHARED VAR c-cbEmpresa AS CHAR.
DEF VAR i-alias AS INT.

FOR EACH bco_empres WHERE
         bco_empres.cod_empresa = c-cbEmpresa NO-LOCK.
    DO i-alias = 1 TO NUM-ENTRIES(bco_empres.cod_alias_bco).
       DELETE ALIAS VALUE(ENTRY(i-alias,bco_empres.cod_alias_bco)).
    END.
END.

ASSIGN c-cbEmpresa = "".
APPLY 'CHOOSE' TO SELF.
