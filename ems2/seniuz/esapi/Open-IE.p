DEF INPUT PARAMETER p-site AS CHAR FORMAT "x(100)".

DEF VAR c-arq-java AS CHAR.
DEF VAR c-comando AS CHAR.                                

ASSIGN c-arq-java = SESSION:TEMP-DIRECTORY + "abrir.js".

OUTPUT TO VALUE(c-arq-java).

PUT 'var oIE = new ActiveXObject("InternetExplorer.Application");' SKIP
    'oIE.Navigate2("' + p-site + '");' FORMAT "x(150)" SKIP     
    'oIE.Visible = true;' SKIP.
OUTPUT CLOSE.

ASSIGN c-comando = 'wscript.exe ' + c-arq-java.
  
OS-COMMAND SILENT VALUE(c-comando).
