/* Programa: upc-cd0715.p
** Autor...: Prodb - Toninho  Dezembro/2004
*/

/* Parameter Definitions ****************************************************/
DEFINE INPUT PARAMETER p-ind-event  AS CHARACTER.
DEFINE INPUT PARAMETER p-ind-object AS CHARACTER.
DEFINE INPUT PARAMETER p-wgh-object AS HANDLE.
DEFINE INPUT PARAMETER p-wgh-frame  AS WIDGET-HANDLE.
DEFINE INPUT PARAMETER p-cod-table  AS CHARACTER.
DEFINE INPUT PARAMETER p-row-table  AS ROWID.

/* Global Variable Definitions for CEPONLINE *******************************/
DEFINE NEW GLOBAL SHARED VAR h-cep          AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-endereco     AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-bairro       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-cidade       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-estado       AS HANDLE.
DEFINE NEW GLOBAL SHARED VAR h-pais         AS HANDLE.

/* Variable Definitions *****************************************************/

/* Main Block ***************************************************************/

RUN SeniuZ/CepOnline/upc/upc-cd0705a.p (INPUT p-ind-event,
                                        INPUT p-ind-object,
                                        INPUT p-wgh-object,
                                        INPUT p-wgh-frame,
                                        INPUT p-cod-table,
                                        INPUT p-row-table).
