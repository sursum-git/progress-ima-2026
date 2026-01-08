
                      
                      
DEF VAR iSit AS INT.
DEF VAR dData AS DATE.

DEFINE VARIABLE iSitIni     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSitFim     AS INTEGER     NO-UNDO.
DEFINE VARIABLE dtRecIni    AS DATE        NO-UNDO.
DEFINE VARIABLE dtRecFim    AS DATE        NO-UNDO.


PROCEDURE zerarFiltros:
    ASSIGN iSitIni = 0
           isitFim = 9
           dtRecIni = 01.01.2001
           dtRecFim = 01.01.9999. 
END PROCEDURE.



PROCEDURE setFiltros:
    DEFINE INPUT  PARAMETER pFiltro AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pValor  AS CHARACTER   NO-UNDO.
    
    CASE 'pFiltro':
        WHEN 'situacao' THEN
        ASSIGN iSitIni = INT(pvalor)
               iSitFim = INT(pValor).
  
        WHEN 'dtRecebimento' THEN
        ASSIGN dtRecIni = DATE(pvalor)
               dtRecFim = DATE(pValor).
    END CASE.

END PROCEDURE.

PROCEDURE getDados:

    FOR EACH pp-container NO-LOCK
        WHERE pp-container.situacaco >= iSitIni
        AND   pp-container.situacao <= iSitFim
        AND   pp-container.dt-recebimento >= dtRecIni
        AND   pp-container.dt-recebimento <= dtRecFim .

        ASSIGN i-ct = i-ct + 1.

   DISP pp-container.nr-container
        STRING(i-ct) +  "/" + STRING(i-tot-ct).

   FOR EACH ob-etiqueta WHERE
            ob-etiqueta.nr-container = pp-container.nr-container NO-LOCK.

       FIND tt-itens WHERE 
            tt-itens.nr-container = ob-etiqueta.nr-container AND
            tt-itens.it-codigo = ob-etiqueta.it-codigo NO-ERROR.
       IF NOT AVAIL tt-itens THEN DO.
          CREATE tt-itens.
          ASSIGN tt-itens.nr-container = ob-etiqueta.nr-container
                 tt-itens.it-codigo = ob-etiqueta.it-codigo.
       END.

       IF ob-etiqueta.situacao = 5 THEN DO.
          FIND ped-item-rom WHERE
               ped-item-rom.cod-estabel = ob-etiqueta.cod-estabel AND
               ped-item-rom.num-etiqueta = ob-etiqueta.num-etiqueta 
               NO-LOCK NO-ERROR.

          IF AVAIL ped-item-rom THEN DO.
             FIND ped-item WHERE
                  ped-item.nome-abrev = ped-item-rom.nome-abrev AND
                  ped-item.nr-pedcli = ped-item-rom.nr-pedcli AND
                  ped-item.nr-sequencia = ped-item-rom.nr-sequencia
                  NO-LOCK NO-ERROR.

             ASSIGN tt-itens.qt-vend = tt-itens.qt-vend + ob-etiqueta.quantidade
                    tt-itens.vl-vend = tt-itens.vl-vend + (ob-etiqueta.quantidade * ped-item.vl-preori).

          END.
       END.

       IF ob-etiqueta.situacao <= 3 THEN
          ASSIGN tt-itens.qt-est = tt-itens.qt-est + ob-etiqueta.quantidade.
   END.



    END.


END PROCEDURE.


/*PROCEDURE setPrecoMedioContainer:
   DEFINE INPUT  PARAMETER pSit      AS INTEGER     NO-UNDO.
   DEFINE INPUT  PARAMETER pData    AS DATE   NO-UNDO.
   ASSIGN iSit  = pSit
          dData = pData.
END PROCEDURE.*/

/*PROCEDURE processaPrecoMedioContainer:

    RUN setPrecoMedioContainer(1,01.01.2021).



    MESSAGE iSit dData
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

END PROCEDURE.*/



