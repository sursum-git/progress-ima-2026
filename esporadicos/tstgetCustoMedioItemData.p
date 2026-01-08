DEFINE VARIABLE vlMedio     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dataValor   AS DATE        NO-UNDO.
RUN esapi/getCustoMedioItemData.p(7.23.2023,
                                  '560183',
                                  OUTPUT vlMedio,
                                  OUTPUT dataValor ).

MESSAGE 'vl.medio:' vlMedio SKIP 
        'data:' dataValor SKIP
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
