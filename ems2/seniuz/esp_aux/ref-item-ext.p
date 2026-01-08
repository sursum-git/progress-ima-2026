/* Programa: ref-item-ext.p
** Objetivo: Manuten‡Æo no programado/processo/pronto
*/

FIND ref-item-ext WHERE ref-item-ext.it-codigo = "501871"
                    AND ref-item-ext.cod-refer = "0131240".
UPDATE ref-item-ext WITH SIDE-LABELS 1 COLUMN.
