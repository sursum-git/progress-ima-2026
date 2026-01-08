DEF NEW GLOBAL SHARED VAR c-cbEmpresa AS CHAR.

IF c-cbEmpresa = '' THEN
   ASSIGN c-cbEmpresa = SELF:SCREEN-VALUE.

