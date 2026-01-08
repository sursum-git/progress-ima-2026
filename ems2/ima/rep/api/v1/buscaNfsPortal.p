{esp/util.i}
{esapi/analisarJsonObject2.i}
{esapi/getNfsPortal.i}
CREATE ttJson.
ASSIGN ttjson.tag = 'repres_ini'
       ttJson.valor = '11127'.
CREATE ttJson.
ASSIGN ttjson.tag = 'repres_fim'
       ttJson.valor = '11127'.
       
       
       
RUN esapi/getNfsPortal.p(INPUT TABLE ttJson,
                         OUTPUT TABLE ttNF). 
                         
{esp/exportarTabelaCsv3.i ttNF " " " " "ttnf"}                         
