/*************************************************************************
Programa: boAppServer
Objetivo: Controlar quais appservers est∆o disponiveis para utilizaá∆o.
Autor: Tadeu Silva Parreiras
Data: 07/2022
*************************************************************************/

{esp/util.i}    
{esbo/boAppServer.i}

PROCEDURE incluirEmpresa:
    DEFINE INPUT  PARAMETER pEmpresa AS CHARACTER   NO-UNDO.
    CREATE ttEmpresa.
    ASSIGN ttEmpresa.codempresa = pEmpresa .
END PROCEDURE.


PROCEDURE limparTTEmpresa.

    EMPTY TEMP-TABLE ttEmpresa.

END PROCEDURE.

PROCEDURE getEmpresasParam:

    DEFINE VARIABLE cVlParam AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iCont    AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cEmpresa AS CHARACTER   NO-UNDO.

    RUN getVlParametro(INPUT 'empresas_exp_ctbl',
                       OUTPUT cVlParam).
    IF cVlParam <> '' THEN DO:
       REPEAT iCont = 1 TO NUM-ENTRIES(cVlParam):
           ASSIGN cEmpresa = ENTRY(iCont,cVlParam,",").
           RUN incluirEmpresa(cEmpresa).
       END.
    END.   
END PROCEDURE.

PROCEDURE setAppservers:    
    FOR EACH cadastro_appserver
        WHERE  cadastro_appserver.cod_broker <> ''
        AND    cadastro_appserver.cod_aplicat = '*' NO-LOCK.
        FIND ttEmpresa
            WHERE ttEmpresa.codEmpresa = cadastro_appserver.cod_empresa
            NO-ERROR.
        IF NOT AVAIL ttEmpresa THEN NEXT.
        CREATE SERVER ttEmpresa.appserver.
        /*MESSAGE cadastro_appserver.cod_broker
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.*/
        IF NOT ttEmpresa.appserver:CONNECT("-URL " + cadastro_appserver.cod_broker) THEN DO :
           DELETE ttEmpresa.
        END.
    END.    
END PROCEDURE.

PROCEDURE getTTEmpresa:
    DEFINE OUTPUT PARAMETER TABLE FOR ttEmpresa.
END PROCEDURE.    

PROCEDURE getHandleAppServerEmpresa:
    DEFINE INPUT  PARAMETER pEmpresa AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER hAppServer AS HANDLE      NO-UNDO.
    RUN limparTTEmpresa.
    RUN incluirEmpresa(pEmpresa).
    RUN setAppServers.
    FIND FIRST ttEmpresa NO-ERROR.
    ASSIGN hAppServer = IF AVAIL ttEmpresa THEN 
                        ttEmpresa.appserver ELSE
                            ?.
    

END PROCEDURE.

PROCEDURE desconectarAppServer:

  FOR EACH ttEmpresa:
      ttEmpresa.appserver:DISCONNECT().
  END.



END PROCEDURE.










