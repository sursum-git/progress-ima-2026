/* Programa: especificos/esupc/upcd-in377.p
** Objetivo: Deletar referencia-ext, quando referencia for deletada.
** Autor...: Toninho-SeniuZ/Gilvando
** Data....: 18/Set/2015
*/

DEFINE PARAMETER BUFFER p-table FOR referencia.

FIND referencia-ext OF p-table NO-ERROR.
IF AVAIL referencia-ext THEN
   DELETE referencia-ext.

