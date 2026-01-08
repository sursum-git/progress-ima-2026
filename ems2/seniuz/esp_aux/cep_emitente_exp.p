/* Programa: cep_emitente_exp.p
**           Exporta dados para Verifica‡Æo validade do Cep do emitente
*/

DEF VAR h-acomp         AS HANDLE NO-UNDO.
DEF VAR c-comando       AS CHAR.

DEF VAR c-ender-sn      LIKE emitente.endereco.
DEF VAR c-ender-sn-cob  LIKE emitente.endereco.

OUTPUT TO c:\temp\emitentes.csv CONVERT SOURCE "ibm850".
PUT "Cod;CNPJ;Nome;Cep;Endereco;Bairro;Cidade;UF;Cep-Cob;Endereco-Cob;Bairro-Cob;Cidade-Cob;UF-Cob;"
    "Telefone-1;Telefone-2;Telefax;E-mail" SKIP.

run utp/ut-acomp.p persistent set h-acomp.
{utp/ut-liter.i Exportando_Emitentes *}
run pi-inicializar in h-acomp (input RETURN-VALUE).

FOR EACH emitente WHERE 
         emitente.identific <> 2 NO-LOCK:

    run pi-acompanhar in h-acomp (input "Emitente: " + STRING(emitente.cod-emitente)). 

    IF INDEX(emitente.endereco,",") <> 0 THEN
       ASSIGN c-ender-sn = SUBSTR(emitente.endereco,1,INDEX(emitente.endereco,",") - 1).
    ELSE
       ASSIGN c-ender-sn = emitente.endereco.
    IF INDEX(emitente.endereco-cob,",") <> 0 THEN
       ASSIGN c-ender-sn-cob = SUBSTR(emitente.endereco-cob,1,INDEX(emitente.endereco-cob,",") - 1).
    ELSE
       ASSIGN c-ender-sn-cob = emitente.endereco-cob.

    PUT emitente.cod-emitente ";"
        emitente.cgc ";"
        emitente.nome-emit ";"
        emitente.cep ";"
        c-ender-sn ";"
        emitente.bairro ";"
        emitente.cidade ";"
        emitente.estado ";"
        emitente.cep-cob ";"
        c-ender-sn-cob ";"
        emitente.bairro-cob ";"
        emitente.cidade-cob ";"
        emitente.estado-cob ";"
        emitente.telefone[1] ";"
        emitente.telefone[2] ";"
        emitente.telefax ";"
        emitente.e-mail
        SKIP.
END.
OUTPUT CLOSE.
