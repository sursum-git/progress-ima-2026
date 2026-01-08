/* Programa: upc-pd4000c2.p
** Objetivo: Trigger de 'Choose' ou "Mouse-selec-click" para o Acondicionamento 
**           Atribuir no valor de tela do Acondiconamento a segunda entrada
**           delimitada por hifen ("-") do menu selecionado
**           O menu com os cortes comerciais foi criado na upc-pd4000 e
**           atribuido como POPUP-MENU (bot∆o direito do mouse) do acondicionamento
** Autor...: Prodb - Toninho  Maráo/2004
*/

DEF NEW GLOBAL SHARED VAR wh-acond AS WIDGET-HANDLE NO-UNDO.

ASSIGN wh-acond:SCREEN-VALUE = TRIM(ENTRY(2,SELF:LABEL,"-")).
