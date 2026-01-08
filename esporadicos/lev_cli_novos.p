DEFINE VARIABLE iQt         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iQtPed      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iQtFat      AS INTEGER     NO-UNDO.
DEFINE VARIABLE cPed        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFat        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPortal     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dFat        AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iRep        AS INTEGER     NO-UNDO.
DEFINE VARIABLE nomeRepres  AS CHARACTER   NO-UNDO.
OUTPUT TO c:\temp\clinovos2022.txt.
PUT 'codigo|nome Abrev.|data|nome Emitente|estado|cidade|C¢digo Repres.|Nome Abrev. Repres|Tem Pedido|Tem Fat.|Vl.Fat|Cad.Pelo Portal' SKIP.
FOR EACH emitente
    WHERE emitente.data-implant >= 01.01.2022.
    ASSIGN iRep = emitente.cod-rep.

    ASSIGN iQt = iQt + 1 
           cPed = 'nao'
           cfat = 'nao'
           cPortal = 'nao'.

    FIND FIRST peds_web
           WHERE peds_web.cnpj_novo_cliente = emitente.cgc
           NO-LOCK NO-ERROR.
       IF AVAIL peds_web THEN
          ASSIGN cPortal = 'sim'.


    IF CAN-FIND(FIRST ped-venda WHERE ped-venda.cod-emitente =  emitente.cod-emitente) THEN DO:
       ASSIGN iQtPed = iQtPed + 1 .
       ASSIGN cPed = 'Sim'.
    END.
    IF CAN-FIND(FIRST nota-fiscal WHERE nota-fiscal.cod-emitente =  emitente.cod-emitente) THEN DO:
       ASSIGN iQtFat = iQtFat + 1 
                 cFat = 'sim'
                 dFat = 0.
       FOR EACH nota-fiscal
           WHERE nota-fiscal.cod-emitente = emitente.cod-emitente
           AND dt-cancela = ?
           NO-LOCK.
           ASSIGN  dFat = dFat +  nota-fiscal.vl-tot-nota.

           
       END.

    END.                     
    IF cFat = 'nao' AND AVAIL peds_web AND emitente.cod-rep = 1 THEN DO:
       ASSIGN iRep = peds_web.repres_id.
    END.

    FIND repres
        WHERE repres.cod-rep = iRep
        NO-LOCK NO-ERROR.
    

    EXPORT DELIMITER "|" 
        emitente.cod-emitente 
        emitente.nome-abrev 
        emitente.data-implant
        emitente.nome-emit
        emitente.estado
        emitente.cidade
        iRep
        IF AVAIL repres THEN repres.nome-abrev ELSE ''
        cped
        cfat
        dfat
        cportal        .
END.

DISP iQt iQtPed iQtFat.
