 
 /*------------------------------------------------------------------------
    File        : HeaderParam
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tadeu.parreiras
    Created     : Sat Aug 09 22:17:18 BRT 2025
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING consumoAPi.IHeaderParam.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS ConsumoAPI.HeaderParam IMPLEMENTS IHeaderParam: 

    DEFINE PUBLIC PROPERTY chave AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC PROPERTY ordem AS INTEGER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC PROPERTY valor AS CHARACTER NO-UNDO 
    GET.
    SET. 

END CLASS.