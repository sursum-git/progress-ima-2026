DEFINE VARIABLE hBoTranspCli AS HANDLE      NO-UNDO.
DEFINE VARIABLE iTransp      AS INTEGER     NO-UNDO.
DEFINE VARIABLE id           AS INTEGER     NO-UNDO.
DEFINE VARIABLE cListaCps    AS CHARACTER   NO-UNDO.

RUN esbo/boTranspCli.p PERSIST SET hBoTranspCli .
{esp/util.i}
DEFINE TEMP-TABLE tt
    FIELD nrPedido AS INT
    FIELD nomeTransp            AS CHAR FORMAT 'x(15)'
    FIELD nomeTranspNovo        AS CHAR FORMAT 'x(15)'
    FIELD idRegra               AS INT
    .
RUN iniciar IN hBoTranspCli.
FOR EACH ped-venda //NO-LOCK
    WHERE ped-venda.cidade-cif <> ''
    AND ped-venda.cod-sit-ped = 1 
    AND ped-venda.cod-estabel = '505'.
    //DISP ped-venda.nr-pedido nome-transp .

    RUN setProp IN hboTranspCli('codEstab',0,ped-venda.cod-estabel).
    RUN setProp IN hboTranspCli('codCliente',0, ped-venda.cod-emitente).
    RUN exec    IN hboTranspCli.
    RUN getTransportadora IN hboTranspCli(OUTPUT iTransp, OUTPUT id).
    FIND transporte 
        WHERE transporte.cod-transp = iTransp NO-LOCK NO-ERROR.
    IF ped-venda.nome-transp =  transporte.nome-abrev THEN NEXT.
    //ASSIGN ped-venda.nome-transp = transporte.nome-abrev.
    CREATE tt.
    ASSIGN tt.nrPedido          = ped-venda.nr-pedido
           tt.nomeTransp        = ped-venda.nome-transp
           tt.nomeTranspNovo    = transporte.nome-abrev
           tt.idRegra           = id .

    
END.

OUTPUT TO c:\temp\peds505transp.csv.
RUN getCpsTT(TEMP-TABLE tt:HANDLE,";",OUTPUT cListaCps).
PUT UNFORM cListaCps SKIP.
FOR EACH tt:
    EXPORT DELIMITER ";" tt.
END.

OUTPUT CLOSE.
