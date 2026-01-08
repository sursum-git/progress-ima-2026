
/*
**
*/

//
// RowObject3 - Used to generate Strongly Typed DataSet xsd file
//


namespace StrongTypesNS
{
    using System;
    using System.Data;


    public class RowObject3DS
    {

        static void Main (string[] args)
        {
            DataSet ds = new DataSet();
            DataRelation drel;
            DataColumn[] parentCols = null, childCols = null;
            DataColumn[] keyCols = null;

            ds.DataSetName = "RowObject3" + "DataSet";
            ds.Namespace = "RowObject3" + "NS";

            
	    DataTable RowObject3 = ds.Tables.Add("RowObject3");
	    RowObject3.Columns.Add("cod-emitente", typeof(int));
	    RowObject3.Columns.Add("nome-abrev", typeof(string));
	    RowObject3.Columns.Add("cgc", typeof(string));
	    RowObject3.Columns.Add("identific", typeof(int));
	    RowObject3.Columns.Add("natureza", typeof(int));
	    RowObject3.Columns.Add("nome-emit", typeof(string));
	    RowObject3.Columns.Add("endereco", typeof(string));
	    RowObject3.Columns.Add("bairro", typeof(string));
	    RowObject3.Columns.Add("cidade", typeof(string));
	    RowObject3.Columns.Add("estado", typeof(string));
	    RowObject3.Columns.Add("cep", typeof(string));
	    RowObject3.Columns.Add("caixa-postal", typeof(string));
	    RowObject3.Columns.Add("pais", typeof(string));
	    RowObject3.Columns.Add("ins-estadual", typeof(string));
	    RowObject3.Columns.Add("cod-cond-pag", typeof(int));
	    RowObject3.Columns.Add("taxa-financ", typeof(decimal));
	    RowObject3.Columns.Add("data-taxa", typeof(System.DateTime));
	    RowObject3.Columns.Add("cod-transp", typeof(int));
	    RowObject3.Columns.Add("cod-gr-forn", typeof(int));
	    RowObject3.Columns.Add("linha-produt", typeof(string));
	    RowObject3.Columns.Add("atividade", typeof(string));
	    RowObject3.Columns.Add("contato1", typeof(string));
	    RowObject3.Columns.Add("contato2", typeof(string));
	    RowObject3.Columns.Add("telefone1", typeof(string));
	    RowObject3.Columns.Add("telefone2", typeof(string));
	    RowObject3.Columns.Add("ramal1", typeof(string));
	    RowObject3.Columns.Add("ramal2", typeof(string));
	    RowObject3.Columns.Add("telefax", typeof(string));
	    RowObject3.Columns.Add("ramal-fax", typeof(string));
	    RowObject3.Columns.Add("telex", typeof(string));
	    RowObject3.Columns.Add("data-implant", typeof(System.DateTime));
	    RowObject3.Columns.Add("compr-period", typeof(decimal));
	    RowObject3.Columns.Add("end-cobranca", typeof(int));
	    RowObject3.Columns.Add("cod-rep", typeof(int));
	    RowObject3.Columns.Add("categoria", typeof(string));
	    RowObject3.Columns.Add("bonificacao", typeof(decimal));
	    RowObject3.Columns.Add("istr", typeof(int));
	    RowObject3.Columns.Add("cod-gr-cli", typeof(int));
	    RowObject3.Columns.Add("lim-credito", typeof(decimal));
	    RowObject3.Columns.Add("dt-lim-cred", typeof(System.DateTime));
	    RowObject3.Columns.Add("perc-fat-ped", typeof(int));
	    RowObject3.Columns.Add("portador", typeof(int));
	    RowObject3.Columns.Add("modalidade", typeof(int));
	    RowObject3.Columns.Add("ind-fat-par", typeof(bool));
	    RowObject3.Columns.Add("ind-cre-cli", typeof(int));
	    RowObject3.Columns.Add("ind-apr-cred", typeof(bool));
	    RowObject3.Columns.Add("nat-operacao", typeof(string));
	    RowObject3.Columns.Add("observacoes", typeof(string));
	    RowObject3.Columns.Add("per-minfat", typeof(decimal));
	    RowObject3.Columns.Add("emissao-ped", typeof(int));
	    RowObject3.Columns.Add("nome-matriz", typeof(string));
	    RowObject3.Columns.Add("telef-modem", typeof(string));
	    RowObject3.Columns.Add("ramal-modem", typeof(string));
	    RowObject3.Columns.Add("telef-fac", typeof(string));
	    RowObject3.Columns.Add("ramal-fac", typeof(string));
	    RowObject3.Columns.Add("agencia", typeof(string));
	    RowObject3.Columns.Add("nr-titulo", typeof(int));
	    RowObject3.Columns.Add("nr-dias", typeof(int));
	    RowObject3.Columns.Add("per-max-canc", typeof(decimal));
	    RowObject3.Columns.Add("dt-ult-venda", typeof(System.DateTime));
	    RowObject3.Columns.Add("emite-bloq", typeof(bool));
	    RowObject3.Columns.Add("emite-etiq", typeof(bool));
	    RowObject3.Columns.Add("tr-ar-valor", typeof(int));
	    RowObject3.Columns.Add("gera-ad", typeof(bool));
	    RowObject3.Columns.Add("port-prefer", typeof(int));
	    RowObject3.Columns.Add("mod-prefer", typeof(int));
	    RowObject3.Columns.Add("bx-acatada", typeof(int));
	    RowObject3.Columns.Add("conta-corren", typeof(string));
	    RowObject3.Columns.Add("nr-copias-ped", typeof(int));
	    RowObject3.Columns.Add("cod-suframa", typeof(string));
	    RowObject3.Columns.Add("cod-cacex", typeof(string));
	    RowObject3.Columns.Add("gera-difer", typeof(int));
	    RowObject3.Columns.Add("nr-tabpre", typeof(string));
	    RowObject3.Columns.Add("ind-aval", typeof(int));
	    RowObject3.Columns.Add("user-libcre", typeof(string));
	    RowObject3.Columns.Add("ins-banc1", typeof(int));
	    RowObject3.Columns.Add("ins-banc2", typeof(int));
	    RowObject3.Columns.Add("ven-feriado", typeof(int));
	    RowObject3.Columns.Add("ven-domingo", typeof(int));
	    RowObject3.Columns.Add("ven-sabado", typeof(int));
	    RowObject3.Columns.Add("cgc-cob", typeof(string));
	    RowObject3.Columns.Add("cep-cob", typeof(string));
	    RowObject3.Columns.Add("estado-cob", typeof(string));
	    RowObject3.Columns.Add("cidade-cob", typeof(string));
	    RowObject3.Columns.Add("bairro-cob", typeof(string));
	    RowObject3.Columns.Add("endereco-cob", typeof(string));
	    RowObject3.Columns.Add("cx-post-cob", typeof(string));
	    RowObject3.Columns.Add("ins-est-cob", typeof(string));
	    RowObject3.Columns.Add("nome-mic-reg", typeof(string));
	    RowObject3.Columns.Add("tip-cob-desp", typeof(int));
	    RowObject3.Columns.Add("nome-tr-red", typeof(string));
	    RowObject3.Columns.Add("nat-ope-ext", typeof(string));
	    RowObject3.Columns.Add("cod-banco", typeof(int));
	    RowObject3.Columns.Add("prox-ad", typeof(int));
	    RowObject3.Columns.Add("lim-adicional", typeof(decimal));
	    RowObject3.Columns.Add("dt-fim-cred", typeof(System.DateTime));
	    RowObject3.Columns.Add("obs-entrega", typeof(string));
	    RowObject3.Columns.Add("cod-tip-ent", typeof(int));
	    RowObject3.Columns.Add("ins-municipal", typeof(string));
	    RowObject3.Columns.Add("nr-peratr", typeof(int));
	    RowObject3.Columns.Add("nr-mesina", typeof(int));
	    RowObject3.Columns.Add("insc-subs-trib", typeof(string));
	    RowObject3.Columns.Add("cod-mensagem", typeof(int));
	    RowObject3.Columns.Add("nr-dias-taxa", typeof(int));
	    RowObject3.Columns.Add("tp-desp-padrao", typeof(int));
	    RowObject3.Columns.Add("tp-rec-padrao", typeof(int));
	    RowObject3.Columns.Add("inf-complementar", typeof(string));
	    RowObject3.Columns.Add("zip-code", typeof(string));
	    RowObject3.Columns.Add("tp-inspecao", typeof(int));
	    RowObject3.Columns.Add("forn-exp", typeof(bool));
	    RowObject3.Columns.Add("tp-qt-prg", typeof(int));
	    RowObject3.Columns.Add("ind-atraso", typeof(int));
	    RowObject3.Columns.Add("ind-div-atraso", typeof(int));
	    RowObject3.Columns.Add("ind-dif-atrs-1", typeof(int));
	    RowObject3.Columns.Add("ind-dif-atrs-2", typeof(int));
	    RowObject3.Columns.Add("esp-pd-venda", typeof(int));
	    RowObject3.Columns.Add("ind-lib-estoque", typeof(bool));
	    RowObject3.Columns.Add("ind-moeda-tit", typeof(int));
	    RowObject3.Columns.Add("ind-rendiment", typeof(bool));
	    RowObject3.Columns.Add("tp-pagto", typeof(int));
	    RowObject3.Columns.Add("vl-min-ad", typeof(decimal));
	    RowObject3.Columns.Add("zip-cob-code", typeof(string));
	    RowObject3.Columns.Add("hora-ini", typeof(int));
	    RowObject3.Columns.Add("hora-fim", typeof(int));
	    RowObject3.Columns.Add("pais-cob", typeof(string));
	    RowObject3.Columns.Add("resumo-mp", typeof(int));
	    RowObject3.Columns.Add("ind-cred-abat", typeof(bool));
	    RowObject3.Columns.Add("contrib-icms", typeof(bool));
	    RowObject3.Columns.Add("e-mail", typeof(string));
	    RowObject3.Columns.Add("home-page", typeof(string));
	    RowObject3.Columns.Add("ind-licenciador", typeof(bool));
	    RowObject3.Columns.Add("endereco2", typeof(string));
	    RowObject3.Columns.Add("ind-aval-embarque", typeof(int));
	    RowObject3.Columns.Add("ind-abrange-aval", typeof(int));
	    RowObject3.Columns.Add("cod-tax", typeof(int));
	    RowObject3.Columns.Add("cn-codigo", typeof(string));
	    RowObject3.Columns.Add("cod-entrega", typeof(string));
	    RowObject3.Columns.Add("cod-isencao", typeof(int));
	    RowObject3.Columns.Add("dias-comp", typeof(int));
	    RowObject3.Columns.Add("estoque", typeof(int));
	    RowObject3.Columns.Add("flag-pag", typeof(int));
	    RowObject3.Columns.Add("item-cli", typeof(bool));
	    RowObject3.Columns.Add("moeda-libcre", typeof(int));
	    RowObject3.Columns.Add("nr-dias-atraso", typeof(int));
	    RowObject3.Columns.Add("valor-minimo", typeof(decimal));
	    RowObject3.Columns.Add("cod-parceiro-edi", typeof(int));
	    RowObject3.Columns.Add("agente-retencao", typeof(bool));
	    RowObject3.Columns.Add("percepcao", typeof(bool));
	    RowObject3.Columns.Add("char-1", typeof(string));
	    RowObject3.Columns.Add("char-2", typeof(string));
	    RowObject3.Columns.Add("dec-1", typeof(decimal));
	    RowObject3.Columns.Add("dec-2", typeof(decimal));
	    RowObject3.Columns.Add("int-1", typeof(int));
	    RowObject3.Columns.Add("int-2", typeof(int));
	    RowObject3.Columns.Add("log-1", typeof(bool));
	    RowObject3.Columns.Add("log-2", typeof(bool));
	    RowObject3.Columns.Add("data-1", typeof(System.DateTime));
	    RowObject3.Columns.Add("data-2", typeof(System.DateTime));
	    RowObject3.Columns.Add("cod-canal-venda", typeof(int));
	    RowObject3.Columns.Add("ind-sit-emitente", typeof(int));
	    RowObject3.Columns.Add("ind-emit-retencao", typeof(int));
	    RowObject3.Columns.Add("calcula-multa", typeof(bool));
	    RowObject3.Columns.Add("prog-emit", typeof(bool));
	    RowObject3.Columns.Add("nr-tab-progr", typeof(int));
	    RowObject3.Columns.Add("recebe-inf-sci", typeof(bool));
	    RowObject3.Columns.Add("cod-classif-fornec", typeof(int));
	    RowObject3.Columns.Add("cod-classif-cliente", typeof(int));
	    RowObject3.Columns.Add("nr-cheque-devol", typeof(int));
	    RowObject3.Columns.Add("periodo-devol", typeof(int));
	    RowObject3.Columns.Add("vl-max-devol", typeof(decimal));
	    RowObject3.Columns.Add("check-sum", typeof(string));
	    RowObject3.Columns.Add("val-quota-media", typeof(decimal));
	    RowObject3.Columns.Add("cod-repres-imp", typeof(int));
	    RowObject3.Columns.Add("vencto-dia-nao-util", typeof(bool));
	    RowObject3.Columns.Add("rend-tribut", typeof(decimal));
	    RowObject3.Columns.Add("utiliza-verba", typeof(bool));
	    RowObject3.Columns.Add("percent-verba", typeof(decimal));
	    RowObject3.Columns.Add("endereco_text", typeof(string));
	    RowObject3.Columns.Add("endereco-cob-text", typeof(string));
	    RowObject3.Columns.Add("short-name", typeof(string));
	    RowObject3.Columns.Add("log-controla-val-max-inss", typeof(bool));
	    RowObject3.Columns.Add("cod-inscr-inss", typeof(string));
	    RowObject3.Columns.Add("cod-pulmao", typeof(string));
	    RowObject3.Columns.Add("idi-tributac-cofins", typeof(int));
	    RowObject3.Columns.Add("idi-tributac-pis", typeof(int));
	    RowObject3.Columns.Add("log-calcula-pis-cofins-unid", typeof(bool));
	    RowObject3.Columns.Add("log-optan-suspens-ipi", typeof(bool));
	    RowObject3.Columns.Add("log-optan-cr-presmdo-subst", typeof(bool));
	    RowObject3.Columns.Add("retem-pagto", typeof(bool));
	    RowObject3.Columns.Add("portador-ap", typeof(int));
	    RowObject3.Columns.Add("modalidade-ap", typeof(int));
	    RowObject3.Columns.Add("log-contribt-subst-interm", typeof(bool));
	    RowObject3.Columns.Add("dat-valid-suframa", typeof(System.DateTime));
	    RowObject3.Columns.Add("nom-fantasia", typeof(string));
	    RowObject3.Columns.Add("log-possui-nf-eletro", typeof(bool));
	    RowObject3.Columns.Add("log-nf-eletro", typeof(bool));
	    RowObject3.Columns.Add("dt-atualiza", typeof(System.DateTime));
	    RowObject3.Columns.Add("hra-atualiz", typeof(string));
	    RowObject3.Columns.Add("cod-email-nfe", typeof(string));
	    RowObject3.Columns.Add("log-integr-totvs-colab-vendas", typeof(bool));
	    RowObject3.Columns.Add("cdn-atraso-max", typeof(int));
	    RowObject3.Columns.Add("dat-nasc", typeof(System.DateTime));
	    RowObject3.Columns.Add("log-beneficiario", typeof(bool));
	    RowObject3.Columns.Add("r-rowid", typeof(byte[]));
	    RowObject3.Columns.Add("rownum", typeof(int));

	    keyCols = new DataColumn[1];
	    keyCols[0] = RowObject3.Columns["cod-emitente"];
	    RowObject3.Constraints.Add(new System.Data.UniqueConstraint("codigo", keyCols, true));
	    keyCols = new DataColumn[1];
	    keyCols[0] = RowObject3.Columns["nome-abrev"];
	    RowObject3.Constraints.Add(new UniqueConstraint("nome", keyCols, false));


            ds.WriteXmlSchema("RowObject3.xsd");

        }


    }
}
