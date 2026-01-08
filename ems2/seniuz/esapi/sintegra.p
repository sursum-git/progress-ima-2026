DEF INPUT PARAMETER p-estado AS CHAR.

DEFINE VARIABLE c-URL AS CHARACTER NO-UNDO.

CASE p-estado.
    WHEN 'AC' THEN
         ASSIGN c-URL = "http://www.sefaz.ac.gov.br:8080/portalsefaz/servlet/hpfsinco".
    WHEN 'AL' THEN
         ASSIGN c-URL = "http://sintegra.sefaz.al.gov.br/consulta_empresa_pesquisa.asp".
    WHEN 'AP' THEN
         ASSIGN c-URL = "http://www.sintegra.ap.gov.br/".     
    WHEN 'AM' THEN
         ASSIGN c-URL = "http://www.sefaz.am.gov.br/sintegra/sintegra0.asp".     
    WHEN 'BA' THEN
         ASSIGN c-URL = "http://www.sefaz.ba.gov.br/Sintegra/sintegra.asp?estado=BA".     
    WHEN 'CE' THEN
         ASSIGN c-URL = "http://www.sefaz.ce.gov.br/Sintegra/Sintegra.Asp?estado=CE".
    WHEN 'DF' THEN
         ASSIGN c-URL = "http://www.fazenda.df.gov.br/area.cfm?id_area=110".
    WHEN 'ES' THEN
         ASSIGN c-URL = "http://www.sintegra.es.gov.br/".
    WHEN 'GO' THEN
         ASSIGN c-URL = "http://www.sefaz.go.gov.br/sintegra/sintegra.asp".     
    WHEN 'MA' THEN
         ASSIGN c-URL = "http://www.sefaz.ma.gov.br/sintegra/sintegra.asp".
    WHEN 'MT' THEN
         ASSIGN c-URL = "http://www.sefaz.mt.gov.br/sid/consulta/infocadastral/consultar/publica".
    WHEN 'MS' THEN
         ASSIGN c-URL = "http://www.sintegra.ms.gov.br/".
    WHEN 'MG' THEN
         ASSIGN c-URL = "http://www.sintegra.fazenda.mg.gov.br/consulta_empresa_pesquisa.asp".
    WHEN 'PA' THEN
         ASSIGN c-URL = "https://app.sefa.pa.gov.br/Sintegra/".     
    WHEN 'PB' THEN
         ASSIGN c-URL = "http://sintegra.receita.pb.gov.br/sintegra/sintegra.asp?estado=pb".     
    WHEN 'PR' THEN
         ASSIGN c-URL = "http://www.sintegra.fazenda.pr.gov.br/sintegra/".     
    WHEN 'PE' THEN
         ASSIGN c-URL = "http://www.sintegra.sefaz.pe.gov.br".     
    WHEN 'PI' THEN
         ASSIGN c-URL = "http://web.sintegra.sefaz.pi.gov.br".     
    WHEN 'RJ' THEN
         ASSIGN c-URL = "http://www.fazenda.rj.gov.br/projetoCPS/consulta.jsp".
    WHEN 'RN' THEN
         ASSIGN c-URL = "http://ww3.set.rn.gov.br/sintegra".     
    WHEN 'RS' THEN
         ASSIGN c-URL = "http://www.sefaz.rs.gov.br/asp/sef_root/inf/sintegra_entrada.asp".     
    WHEN 'RO' THEN
         ASSIGN c-URL = "http://www.sefin.ro.gov.br/sint_consul.asp".
    WHEN 'RR' THEN
         ASSIGN c-URL = "http://sintegra.sefaz.rr.gov.br/".     
    WHEN 'SC' THEN
         ASSIGN c-URL = "http://sistemas3.sef.sc.gov.br/sintegra/consulta_empresa_pesquisa.aspx".     
    WHEN 'SP' THEN
         ASSIGN c-URL = "http://pfeserv1.fazenda.sp.gov.br/sintegrapfe/consultaSintegraServlet".
    WHEN 'SE' THEN
         ASSIGN c-URL = "http://www.sefaz.se.gov.br/sintegra".     
    WHEN 'TO' THEN
         ASSIGN c-URL = "http://sintegra.sefaz.to.gov.br".     
    OTHERWISE
         ASSIGN c-URL = "http://sintegra.gov.br".     
END CASE.

RUN esapi/open-firefox.p (INPUT c-URL).
/*RUN esapi/Open-IE.p (INPUT c-URL). */
