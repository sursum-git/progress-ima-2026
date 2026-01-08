DEF INPUT PARAMETER p-estado AS CHAR.
DEF VAR c-arq-java AS CHAR.
DEF VAR c-comando AS CHAR.                                

ASSIGN c-arq-java = SESSION:TEMP-DIRECTORY + "abrir.js".

CASE p-estado.
   WHEN 'AC' THEN
       RUN pi-open-ie (INPUT "http://www.sefaz.ac.gov.br:8080/portalsefaz/servlet/hpfsinco").
   WHEN 'AL' THEN
       RUN pi-open-ie (INPUT "http://www.sefaz.al.gov.br/asp/sintegra/sintegra.asp?estado=AL").
   WHEN 'AP' THEN
       RUN pi-open-ie (INPUT "http://www.sintegra.ap.gov.br/").     
   WHEN 'AM' THEN
       RUN pi-open-ie (INPUT "http://www.sefaz.am.gov.br/sintegra/sintegra0.asp").     
   WHEN 'BA' THEN
       RUN pi-open-ie (INPUT "http://www.sefaz.ba.gov.br/Sintegra/sintegra.asp?estado=BA").     
   WHEN 'CE' THEN
       RUN pi-open-ie (INPUT "http://www.sefaz.ce.gov.br/Sintegra/Sintegra.Asp?estado=CE").
   WHEN 'DF' THEN
       RUN pi-open-ie (INPUT "http://www.fazenda.df.gov.br/area.cfm?id_area=110").
   WHEN 'ES' THEN
       RUN pi-open-ie (INPUT "http://www.sintegra.es.gov.br/").
   WHEN 'GO' THEN
       RUN pi-open-ie (INPUT "http://www.sefaz.go.gov.br/sintegra/sintegra.asp").     
   WHEN 'MA' THEN
       RUN pi-open-ie (INPUT "http://www.sefaz.ma.gov.br/sintegra/sintegra.asp").
   WHEN 'MT' THEN
       RUN pi-open-ie (INPUT "http://www.sefaz.mt.gov.br/sid/consulta/infocadastral/consultar/publica").
   WHEN 'MS' THEN
       RUN pi-open-ie (INPUT "http://www.sintegra.ms.gov.br/").
   WHEN 'MG' THEN
       RUN pi-open-ie (INPUT "http://www.sintegra.fazenda.mg.gov.br/consulta_empresa_pesquisa.asp").
   WHEN 'PA' THEN
       RUN pi-open-ie (INPUT "https://app.sefa.pa.gov.br/Sintegra/").     
   WHEN 'PB' THEN
       RUN pi-open-ie (INPUT "http://sintegra.receita.pb.gov.br/sintegra/sintegra.asp?estado=pb").     
   WHEN 'PR' THEN
       RUN pi-open-ie (INPUT "http://www.sintegra.fazenda.pr.gov.br/sintegra/").     
   WHEN 'PE' THEN
       RUN pi-open-ie (INPUT "http://www.sintegra.sefaz.pe.gov.br").     
   WHEN 'PI' THEN
       RUN pi-open-ie (INPUT "http://web.sintegra.sefaz.pi.gov.br").     
   WHEN 'RJ' THEN
       RUN pi-open-ie (INPUT "http://www.fazenda.rj.gov.br/projetoCPS/consulta.jsp").
   WHEN 'RN' THEN
       RUN pi-open-ie (INPUT "http://ww3.set.rn.gov.br/sintegra").     
   WHEN 'RS' THEN
       RUN pi-open-ie (INPUT "http://www.sefaz.rs.gov.br/SEF_root/inf/sintegra_entrada.asp").     
   WHEN 'RO' THEN
       RUN pi-open-ie (INPUT "http://www.sefin.ro.gov.br/sint_consul.asp").
   WHEN 'RR' THEN
       RUN pi-open-ie (INPUT "http://sintegra.sefaz.rr.gov.br/").     
   WHEN 'SC' THEN
       RUN pi-open-ie (INPUT "http://sistemas3.sef.sc.gov.br/sintegra/consulta_empresa_pesquisa.aspx").     
   WHEN 'SP' THEN
       RUN pi-open-ie (INPUT "http://pfeserv1.fazenda.sp.gov.br/sintegrapfe/consultaSintegraServlet").
   WHEN 'SE' THEN
       RUN pi-open-ie (INPUT "http://www.sefaz.se.gov.br/sintegra").     
   WHEN 'TO' THEN
       RUN pi-open-ie (INPUT "http://sintegra.sefaz.to.gov.br").     
   OTHERWISE
       RUN pi-open-ie (INPUT "http://sintegra.gov.br").     
END CASE.

PROCEDURE pi-open-ie.
    DEF INPUT PARAMETER p-site AS CHAR FORMAT "x(100)".

    DEF VAR c-arq-java AS CHAR.
    DEF VAR c-comando AS CHAR.                                

    ASSIGN c-arq-java = SESSION:TEMP-DIRECTORY + "abrir.js".

    OUTPUT TO VALUE(c-arq-java).
       PUT 'var oIE = new ActiveXObject("InternetExplorer.Application");' SKIP
           'oIE.Navigate2("' + p-site + '");' FORMAT "x(150)" SKIP     
           'oIE.Visible = true;' SKIP.
    OUTPUT CLOSE.

    ASSIGN c-comando = 'wscript.exe ' + c-arq-java.
  
    OS-COMMAND SILENT VALUE(c-comando).
END PROCEDURE.
