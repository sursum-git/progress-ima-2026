/* Programa: upc-dsb-buttons.p
** Objetivo: Desabilitar os Botäes de Nagega‡Æo de qualquer programa desenvolvido
**           utilizando as Templates do DDK
** Autor...: DBNet - Toninho  Agosto/2005
** Observ..: Para executar esta upc, crie uma upc/epc para o programa
**           principal (ex. pd1001), e execute esta upc (upc-dsb-buttons)
**           passando como parƒmetros apenas o p-ind-object, e o p-wgh-frame
*/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.

/* Global Variable Definitions **********************************************/

/* Variable Definitions *****************************************************/
DEFINE VARIABLE h-objeto AS HANDLE.
DEFINE VARIABLE h-panel  AS HANDLE.
DEFINE VARIABLE h-button AS HANDLE.

/* Variavies criadas na viewer dinamicamente*********************************/

/* Main Block ***************************************************************/

IF p-ind-object = "CONTAINER" OR
   p-ind-object = "BROWSER" THEN DO. 
   ASSIGN h-objeto = p-wgh-frame:FIRST-CHILD.
   ASSIGN h-objeto = h-objeto:FIRST-CHILD.
   DO WHILE VALID-HANDLE(h-objeto).
      IF h-objeto:NAME = "Panel-Frame" THEN DO.
         ASSIGN h-panel = h-objeto:FIRST-CHILD. 
         DO WHILE VALID-HANDLE(h-panel).
            IF h-panel:TYPE = "field-group" THEN DO.
               ASSIGN h-button = h-panel:FIRST-CHILD.
               DO WHILE VALID-HANDLE(h-button).
                  ASSIGN h-button:SENSITIVE = NO.
                  IF h-button:NAME = "bt-exi" OR
                     h-button:NAME = "bt-joi" THEN
                     ASSIGN h-button:SENSITIVE = YES.
                  ELSE
                      ON 'choose':U OF h-button PERSISTENT RUN esapi/dsb-bt.p (INPUT h-button).

                  ASSIGN h-button = h-button:NEXT-SIBLING.
               END.
            END.
            ASSIGN h-panel = h-panel:NEXT-SIBLING.
         END.
      END.
      ASSIGN h-objeto = h-objeto:NEXT-SIBLING.
   END.
END. 
