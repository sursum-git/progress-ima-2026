/* Exporta dados de Forncedores, para acerto do cadastro 
*/

def var h-prog as handle no-undo.
run utp/ut-utils.p persistent set h-prog.

OUTPUT TO "c:/lixo/lixo.csv" CONVERT SOURCE "ibm850".
PUT "Codigo;Nome-Abrev;Razao-Social;Endereco;Bairro;Cidade;Pais;UF;Cep;CNPJ;Inscr-Estad;Telefone-1;Telefone-2;Telefax-1;Telefax-2;E-mail" SKIP.

FOR EACH emitente WHERE /*emitente.identific   <> 1 
                    AND */ emitente.cod-gr-cli =  3 
                  NO-LOCK.

    put emitente.cod-emitente ";"
        emitente.nome-abrev ";"
        emitente.nome-emit ";"
        emitente.endereco ";"
        emitente.bairro ";"
        emitente.cidade ";"
        emitente.pais ";"
        emitente.estado ";"
        emitente.cep ";"
        emitente.cgc ";"
        emitente.ins-estadual ";"
        "f" + emitente.telefone[1] FORMAT "x(16)" ";"
        "f" + emitente.telefone[2] FORMAT "x(16)" ";"
        "f" + emitente.telefax  FORMAT "x(16)" ";" 
        "f" + emitente.telef-fac  FORMAT "x(16)" ";" 
        emitente.e-mail ";"
        SKIP.
END.

OUTPUT CLOSE.

run Execute in h-prog(input "excel.exe", input "c:\lixo\lixo.csv").
delete procedure h-prog.

                                 
