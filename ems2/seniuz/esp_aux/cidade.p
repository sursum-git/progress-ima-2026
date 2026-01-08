/* include para remover acentua‡Æo de strings */
{include/i-freeac.i}

DEF VAR c-cidade LIKE cidade.cidade.
DEF VAR c-pais   LIKE cidade.pais.

FOR EACH cidade:
    ASSIGN c-cidade = UPPER(fn-free-accent(cidade.cidade))
           c-pais   = UPPER(fn-free-accent(cidade.pais)).
    ASSIGN cidade.cidade = c-cidade
           cidade.pais   = c-pais.
END.


