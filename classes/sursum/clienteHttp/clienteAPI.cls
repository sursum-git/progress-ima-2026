
/*------------------------------------------------------------------------
    File        : clienteApi
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : user
    Created     : Thu Mar 30 17:19:25 BRT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Core.String.
USING OpenEdge.Net.HTTP.ClientBuilder.
USING OpenEdge.Net.HTTP.IHttpClient.
USING OpenEdge.Net.HTTP.IHttpRequest.
USING OpenEdge.Net.HTTP.RequestBuilder.
USING OpenEdge.Net.HTTP.IHttpResponse.
USING Progress.Json.ObjectModel.JsonObject.
USING OpenEdge.Net.HTTP.Credentials FROM PROPATH.
USING OpenEdge.Net.URI FROM PROPATH.

CLASS sursum.ClienteApi:  
    
    DEFINE PRIVATE temp-table ttParams
    field chave     as CHAR 
    field valor     as CHAR 
    index ind       as PRIMARY chave.
    
    
    define PRIVATE temp-table ttPathId
    field cId as char   .
    
     
    //define private variable metodo as char.
    
    CONSTRUCTOR PUBLIC ClienteAPI(pUrl AS CHAR, pMetodo AS CHAR):



    END CONSTRUCTOR.
     
    
  
    DEFINE PRIVATE PROPERTY metodo AS CHARACTER NO-UNDO 
    GET.
     

    DEFINE PRIVATE PROPERTY cUrl AS CHARACTER NO-UNDO 
    GET.
    

    

    METHOD PUBLIC VOID inserirParamUrl (INPUT cChave AS CHAR, INPUT cValor AS CHAR ):



    END METHOD.

    METHOD PUBLIC VOID inserirParamPathUrl (INPUT cChave AS CHAR, INPUT cValor AS CHAR ):



    END METHOD.


    METHOD PUBLIC VOID inserirParamCabec (INPUT cChave AS CHAR, INPUT cValor AS CHAR):



    END METHOD.





    /*
    DEFINE PRIVATE PROPERTY inserirParamUrl AS CHARACTER NO-UNDO 
    GET.
    SET.

    DEFINE PRIVATE PROPERTY inserirParamCabec AS CHARACTER NO-UNDO 
    GET.
    SET.*/

      
    
END CLASS.
