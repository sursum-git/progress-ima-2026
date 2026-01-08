DEF TEMP-TABLE tt-arquivos
    FIELD c-arquivo AS CHAR FORMAT "x(30)".

DEF VAR c-arq-dados AS CHAR.
DEF VAR c-comando AS CHAR.

ASSIGN c-arq-dados = SESSION:TEMP-DIRECTORY + 'arquivos.txt'.
         c-comando = 'DIR /b c:\temp\*.txt' + ' >' +
                     c-arq-dados.
  OS-COMMAND SILENT VALUE(c-comando).

  INPUT FROM VALUE(c-arq-dados).
  REPEAT.
     CREATE tt-arquivos.
     IMPORT tt-arquivos.
  END.
  INPUT CLOSE.

  FOR EACH tt-arquivos.
      DISP tt-arquivos.c-arquivo.
  END.
