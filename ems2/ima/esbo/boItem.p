/*
programa: esbo/boitem.p
objetivo: possibilitar a pesquisa de itens pelas propriedades dos mesmos.
1- pendencias
a) fazer procedimento para retornar temp-table com os itens
b) acrescentar filtros de gramatura e peso liquido nas duas op‡äes de busca(it-codigo e desc-item)
*/

    
DEFINE VARIABLE cItem               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDescrItem          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cListaRef           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lSoOutlet           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cListaOpcoesSaldo   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cListaTpPreco       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dSaldoMinKg         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dSaldoMinMt         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dGramaturaIni       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dGramaturaFim       AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dPesoLiqIni         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dPesoLiqFim         AS DECIMAL     NO-UNDO.

DEFINE TEMP-TABLE ttItens no-undo
   FIELD itCodigo LIKE ITEM.it-codigo 
   INDEX ind AS PRIMARY UNIQUE itCodigo.

    
PROCEDURE  setFiltro:

    DEFINE INPUT  PARAMETER cChave  AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER cvalor  AS CHARACTER   NO-UNDO.

    CASE cChave:

        WHEN 'cod_item' THEN DO:
            ASSIGN cItem = cValor.
        END.
        WHEN 'descr_item' THEN DO:
            ASSIGN cDescrItem = cValor.
        END.
        WHEN 'cod_refer' THEN DO:
           ASSIGN cListaRef = cValor .
        END.
        WHEN 'gramatura_ini' THEN DO:
            ASSIGN dGramaturaIni = DEC(cValor).
        END.
        WHEN 'gramatura_fim' THEN DO:
            ASSIGN dGramaturaFim = DEC(cValor).
        END.                                   
        WHEN 'peso_liquido_ini' THEN DO:
            ASSIGN dPesoLiqIni = DEC(cValor).
        END.
        WHEN 'peso_liquido_fim' THEN DO:
            ASSIGN dPesoLiqFim = DEC(cValor).
        END.
    END CASE.

END PROCEDURE.




PROCEDURE filtrarCodItem:

    FOR EACH ITEM NO-LOCK
        WHERE ITEM.it-codigo MATCHES '*' + cItem + '*'.
        RUN sincrTtItem(ITEM.it-codigo).
    END.


END PROCEDURE.


PROCEDURE filtrarDescitem:
    DEFINE VARIABLE hQuery  AS HANDLE      NO-UNDO.
    DEFINE VARIABLE hBuffer AS HANDLE      NO-UNDO.
    DEFINE VARIABLE cFiltro AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iCont   AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cTermo  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cCond   AS CHARACTER   NO-UNDO.

    
    REPEAT iCont = 1 TO NUM-ENTRIES(cDescrItem,' '):
        ASSIGN cTermo = ENTRY(iCont,cDescrItem,' ')
               cCond  = " Matches '*" + cTermo +  "*'  " .
        RUN incrValor(cFiltro, cCond, " AND ").
    END.    
    ASSIGN cFiltro = " WHERE ".
    CREATE QUERY hQuery.
    CREATE BUFFER hBuffer FOR TABLE 'item'.
    hQuery:SET-BUFFERS(hBuffer). 
    //h:CACHE=500.
    hQuery:QUERY-PREPARE('for each item ' + cFiltro).
    hQuery:QUERY-OPEN.
    REPEAT:                                                                                                                                               
      hQuery:GET-NEXT().                                                                                                                                  
      IF hQuery:QUERY-OFF-END THEN LEAVE.    
      RUN sincrTtItem(hBuffer:BUFFER-FIELD('it-codigo'):BUFFER-VALUE()).
    END.                                                                
    IF VALID-HANDLE(hQuery) THEN
       DELETE OBJECT hQuery.

END PROCEDURE.


PROCEDURE sincrTtItem:
    DEFINE INPUT  PARAMETER pItem LIKE ITEM.it-codigo  NO-UNDO.

    FIND ttItens
        WHERE ttItens.itCodigo = pItem NO-ERROR.
    IF NOT AVAIL ttItens THEN DO:
       CREATE ttItens.
       ASSIGN ttItens.itCodigo = pItem.
    END.

END PROCEDURE.

