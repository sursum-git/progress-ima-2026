
/*
**
*/

//
// RowObject - Used to generate Strongly Typed DataSet xsd file
//


namespace StrongTypesNS
{
    using System;
    using System.Data;


    public class RowObjectDS
    {

        static void Main (string[] args)
        {
            DataSet ds = new DataSet();
            DataRelation drel;
            DataColumn[] parentCols = null, childCols = null;
            DataColumn[] keyCols = null;

            ds.DataSetName = "RowObject" + "DataSet";
            ds.Namespace = "RowObject" + "NS";

            
	    DataTable RowObject = ds.Tables.Add("RowObject");
	    RowObject.Columns.Add("cod-emitente", typeof(int));
	    RowObject.Columns.Add("nome-abrev", typeof(string));
	    RowObject.Columns.Add("cgc", typeof(string));
	    RowObject.Columns.Add("identific", typeof(int));
	    RowObject.Columns.Add("natureza", typeof(int));
	    RowObject.Columns.Add("nome-emit", typeof(string));
	    RowObject.Columns.Add("endereco", typeof(string));
	    RowObject.Columns.Add("bairro", typeof(string));
	    RowObject.Columns.Add("cidade", typeof(string));
	    RowObject.Columns.Add("estado", typeof(string));
	    RowObject.Columns.Add("cep", typeof(string));
	    RowObject.Columns.Add("caixa-postal", typeof(string));
	    RowObject.Columns.Add("pais", typeof(string));
	    RowObject.Columns.Add("ins-estadual", typeof(string));
	    RowObject.Columns.Add("cod-cond-pag", typeof(int));
	    RowObject.Columns.Add("taxa-financ", typeof(decimal));
	    RowObject.Columns.Add("data-taxa", typeof(System.DateTime));
	    RowObject.Columns.Add("cod-transp", typeof(int));
	    RowObject.Columns.Add("cod-gr-forn", typeof(int));
	    RowObject.Columns.Add("linha-produt", typeof(string));
	    RowObject.Columns.Add("atividade", typeof(string));
	    RowObject.Columns.Add("contato1", typeof(string));
	    RowObject.Columns.Add("contato2", typeof(string));
	    RowObject.Columns.Add("telefone1", typeof(string));
	    RowObject.Columns.Add("telefone2", typeof(string));
	    RowObject.Columns.Add("ramal1", typeof(string));
	    RowObject.Columns.Add("ramal2", typeof(string));
	    RowObject.Columns.Add("telefax", typeof(string));
	    RowObject.Columns.Add("ramal-fax", typeof(string));
	    RowObject.Columns.Add("telex", typeof(string));
	    RowObject.Columns.Add("data-implant", typeof(System.DateTime));
	    RowObject.Columns.Add("compr-period", typeof(decimal));
	    RowObject.Columns.Add("end-cobranca", typeof(int));
	    RowObject.Columns.Add("cod-rep", typeof(int));
	    RowObject.Columns.Add("categoria", typeof(string));
	    RowObject.Columns.Add("bonificacao", typeof(decimal));
	    RowObject.Columns.Add("istr", typeof(int));
	    RowObject.Columns.Add("cod-gr-cli", typeof(int));
	    RowObject.Columns.Add("lim-credito", typeof(decimal));
	    RowObject.Columns.Add("dt-lim-cred", typeof(System.DateTime));
	    RowObject.Columns.Add("perc-fat-ped", typeof(int));
	    RowObject.Columns.Add("portador", typeof(int));
	    RowObject.Columns.Add("modalidade", typeof(int));
	    RowObject.Columns.Add("ind-fat-par", typeof(bool));
	    RowObject.Columns.Add("ind-cre-cli", typeof(int));
	    RowObject.Columns.Add("ind-apr-cred", typeof(bool));
	    RowObject.Columns.Add("nat-operacao", typeof(string));
	    RowObject.Columns.Add("observacoes", typeof(string));
	    RowObject.Columns.Add("per-minfat", typeof(decimal));
	    RowObject.Columns.Add("emissao-ped", typeof(int));
	    RowObject.Columns.Add("nome-matriz", typeof(string));
	    RowObject.Columns.Add("telef-modem", typeof(string));
	    RowObject.Columns.Add("ramal-modem", typeof(string));
	    RowObject.Columns.Add("telef-fac", typeof(string));
	    RowObject.Columns.Add("ramal-fac", typeof(string));
	    RowObject.Columns.Add("agencia", typeof(string));
	    RowObject.Columns.Add("nr-titulo", typeof(int));
	    RowObject.Columns.Add("nr-dias", typeof(int));
	    RowObject.Columns.Add("per-max-canc", typeof(decimal));
	    RowObject.Columns.Add("dt-ult-venda", typeof(System.DateTime));
	    RowObject.Columns.Add("emite-bloq", typeof(bool));
	    RowObject.Columns.Add("emite-etiq", typeof(bool));
	    RowObject.Columns.Add("tr-ar-valor", typeof(int));
	    RowObject.Columns.Add("gera-ad", typeof(bool));
	    RowObject.Columns.Add("port-prefer", typeof(int));
	    RowObject.Columns.Add("mod-prefer", typeof(int));
	    RowObject.Columns.Add("bx-acatada", typeof(int));
	    RowObject.Columns.Add("conta-corren", typeof(string));
	    RowObject.Columns.Add("nr-copias-ped", typeof(int));
	    RowObject.Columns.Add("cod-suframa", typeof(string));
	    RowObject.Columns.Add("cod-cacex", typeof(string));
	    RowObject.Columns.Add("gera-difer", typeof(int));
	    RowObject.Columns.Add("nr-tabpre", typeof(string));
	    RowObject.Columns.Add("ind-aval", typeof(int));
	    RowObject.Columns.Add("user-libcre", typeof(string));
	    RowObject.Columns.Add("ins-banc1", typeof(int));
	    RowObject.Columns.Add("ins-banc2", typeof(int));
	    RowObject.Columns.Add("ven-feriado", typeof(int));
	    RowObject.Columns.Add("ven-domingo", typeof(int));
	    RowObject.Columns.Add("ven-sabado", typeof(int));
	    RowObject.Columns.Add("cgc-cob", typeof(string));
	    RowObject.Columns.Add("cep-cob", typeof(string));
	    RowObject.Columns.Add("estado-cob", typeof(string));
	    RowObject.Columns.Add("cidade-cob", typeof(string));
	    RowObject.Columns.Add("bairro-cob", typeof(string));
	    RowObject.Columns.Add("endereco-cob", typeof(string));
	    RowObject.Columns.Add("cx-post-cob", typeof(string));
	    RowObject.Columns.Add("ins-est-cob", typeof(string));
	    RowObject.Columns.Add("nome-mic-reg", typeof(string));
	    RowObject.Columns.Add("tip-cob-desp", typeof(int));
	    RowObject.Columns.Add("nome-tr-red", typeof(string));
	    RowObject.Columns.Add("nat-ope-ext", typeof(string));
	    RowObject.Columns.Add("cod-banco", typeof(int));
	    RowObject.Columns.Add("prox-ad", typeof(int));
	    RowObject.Columns.Add("lim-adicional", typeof(decimal));
	    RowObject.Columns.Add("dt-fim-cred", typeof(System.DateTime));
	    RowObject.Columns.Add("obs-entrega", typeof(string));
	    RowObject.Columns.Add("cod-tip-ent", typeof(int));
	    RowObject.Columns.Add("ins-municipal", typeof(string));
	    RowObject.Columns.Add("nr-peratr", typeof(int));
	    RowObject.Columns.Add("nr-mesina", typeof(int));
	    RowObject.Columns.Add("insc-subs-trib", typeof(string));
	    RowObject.Columns.Add("cod-mensagem", typeof(int));
	    RowObject.Columns.Add("nr-dias-taxa", typeof(int));
	    RowObject.Columns.Add("tp-desp-padrao", typeof(int));
	    RowObject.Columns.Add("tp-rec-padrao", typeof(int));
	    RowObject.Columns.Add("inf-complementar", typeof(string));
	    RowObject.Columns.Add("zip-code", typeof(string));
	    RowObject.Columns.Add("tp-inspecao", typeof(int));
	    RowObject.Columns.Add("forn-exp", typeof(bool));
	    RowObject.Columns.Add("tp-qt-prg", typeof(int));
	    RowObject.Columns.Add("ind-atraso", typeof(int));
	    RowObject.Columns.Add("ind-div-atraso", typeof(int));
	    RowObject.Columns.Add("ind-dif-atrs-1", typeof(int));
	    RowObject.Columns.Add("ind-dif-atrs-2", typeof(int));
	    RowObject.Columns.Add("esp-pd-venda", typeof(int));
	    RowObject.Columns.Add("ind-lib-estoque", typeof(bool));
	    RowObject.Columns.Add("ind-moeda-tit", typeof(int));
	    RowObject.Columns.Add("ind-rendiment", typeof(bool));
	    RowObject.Columns.Add("tp-pagto", typeof(int));
	    RowObject.Columns.Add("vl-min-ad", typeof(decimal));
	    RowObject.Columns.Add("zip-cob-code", typeof(string));
	    RowObject.Columns.Add("hora-ini", typeof(int));
	    RowObject.Columns.Add("hora-fim", typeof(int));
	    RowObject.Columns.Add("pais-cob", typeof(string));
	    RowObject.Columns.Add("resumo-mp", typeof(int));
	    RowObject.Columns.Add("ind-cred-abat", typeof(bool));
	    RowObject.Columns.Add("contrib-icms", typeof(bool));
	    RowObject.Columns.Add("e-mail", typeof(string));
	    RowObject.Columns.Add("home-page", typeof(string));
	    RowObject.Columns.Add("ind-licenciador", typeof(bool));
	    RowObject.Columns.Add("endereco2", typeof(string));
	    RowObject.Columns.Add("ind-aval-embarque", typeof(int));
	    RowObject.Columns.Add("ind-abrange-aval", typeof(int));
	    RowObject.Columns.Add("cod-tax", typeof(int));
	    RowObject.Columns.Add("cn-codigo", typeof(string));
	    RowObject.Columns.Add("cod-entrega", typeof(string));
	    RowObject.Columns.Add("cod-isencao", typeof(int));
	    RowObject.Columns.Add("dias-comp", typeof(int));
	    RowObject.Columns.Add("estoque", typeof(int));
	    RowObject.Columns.Add("flag-pag", typeof(int));
	    RowObject.Columns.Add("item-cli", typeof(bool));
	    RowObject.Columns.Add("moeda-libcre", typeof(int));
	    RowObject.Columns.Add("nr-dias-atraso", typeof(int));
	    RowObject.Columns.Add("valor-minimo", typeof(decimal));
	    RowObject.Columns.Add("cod-parceiro-edi", typeof(int));
	    RowObject.Columns.Add("agente-retencao", typeof(bool));
	    RowObject.Columns.Add("percepcao", typeof(bool));
	    RowObject.Columns.Add("char-1", typeof(string));
	    RowObject.Columns.Add("char-2", typeof(string));
	    RowObject.Columns.Add("dec-1", typeof(decimal));
	    RowObject.Columns.Add("dec-2", typeof(decimal));
	    RowObject.Columns.Add("int-1", typeof(int));
	    RowObject.Columns.Add("int-2", typeof(int));
	    RowObject.Columns.Add("log-1", typeof(bool));
	    RowObject.Columns.Add("log-2", typeof(bool));
	    RowObject.Columns.Add("data-1", typeof(System.DateTime));
	    RowObject.Columns.Add("data-2", typeof(System.DateTime));
	    RowObject.Columns.Add("cod-canal-venda", typeof(int));
	    RowObject.Columns.Add("ind-sit-emitente", typeof(int));
	    RowObject.Columns.Add("ind-emit-retencao", typeof(int));
	    RowObject.Columns.Add("calcula-multa", typeof(bool));
	    RowObject.Columns.Add("prog-emit", typeof(bool));
	    RowObject.Columns.Add("nr-tab-progr", typeof(int));
	    RowObject.Columns.Add("recebe-inf-sci", typeof(bool));
	    RowObject.Columns.Add("cod-classif-fornec", typeof(int));
	    RowObject.Columns.Add("cod-classif-cliente", typeof(int));
	    RowObject.Columns.Add("nr-cheque-devol", typeof(int));
	    RowObject.Columns.Add("periodo-devol", typeof(int));
	    RowObject.Columns.Add("vl-max-devol", typeof(decimal));
	    RowObject.Columns.Add("check-sum", typeof(string));
	    RowObject.Columns.Add("val-quota-media", typeof(decimal));
	    RowObject.Columns.Add("cod-repres-imp", typeof(int));
	    RowObject.Columns.Add("vencto-dia-nao-util", typeof(bool));
	    RowObject.Columns.Add("rend-tribut", typeof(decimal));
	    RowObject.Columns.Add("utiliza-verba", typeof(bool));
	    RowObject.Columns.Add("percent-verba", typeof(decimal));
	    RowObject.Columns.Add("endereco_text", typeof(string));
	    RowObject.Columns.Add("endereco-cob-text", typeof(string));
	    RowObject.Columns.Add("short-name", typeof(string));
	    RowObject.Columns.Add("log-controla-val-max-inss", typeof(bool));
	    RowObject.Columns.Add("cod-inscr-inss", typeof(string));
	    RowObject.Columns.Add("cod-pulmao", typeof(string));
	    RowObject.Columns.Add("idi-tributac-cofins", typeof(int));
	    RowObject.Columns.Add("idi-tributac-pis", typeof(int));
	    RowObject.Columns.Add("log-calcula-pis-cofins-unid", typeof(bool));
	    RowObject.Columns.Add("log-optan-suspens-ipi", typeof(bool));
	    RowObject.Columns.Add("log-optan-cr-presmdo-subst", typeof(bool));
	    RowObject.Columns.Add("retem-pagto", typeof(bool));
	    RowObject.Columns.Add("portador-ap", typeof(int));
	    RowObject.Columns.Add("modalidade-ap", typeof(int));
	    RowObject.Columns.Add("log-contribt-subst-interm", typeof(bool));
	    RowObject.Columns.Add("dat-valid-suframa", typeof(System.DateTime));
	    RowObject.Columns.Add("nom-fantasia", typeof(string));
	    RowObject.Columns.Add("log-possui-nf-eletro", typeof(bool));
	    RowObject.Columns.Add("log-nf-eletro", typeof(bool));
	    RowObject.Columns.Add("dt-atualiza", typeof(System.DateTime));
	    RowObject.Columns.Add("hra-atualiz", typeof(string));
	    RowObject.Columns.Add("cod-email-nfe", typeof(string));
	    RowObject.Columns.Add("log-integr-totvs-colab-vendas", typeof(bool));
	    RowObject.Columns.Add("cdn-atraso-max", typeof(int));
	    RowObject.Columns.Add("dat-nasc", typeof(System.DateTime));
	    RowObject.Columns.Add("log-beneficiario", typeof(bool));
	    RowObject.Columns.Add("r-rowid", typeof(byte[]));

	    keyCols = new DataColumn[1];
	    keyCols[0] = RowObject.Columns["cod-emitente"];
	    RowObject.Constraints.Add(new System.Data.UniqueConstraint("codigo", keyCols, true));
	    keyCols = new DataColumn[1];
	    keyCols[0] = RowObject.Columns["nome-abrev"];
	    RowObject.Constraints.Add(new UniqueConstraint("nome", keyCols, false));


            ds.WriteXmlSchema("RowObject.xsd");

        }


    }
}
