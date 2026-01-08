
/*
**
*/

//
// RowObjectAux - Used to generate Strongly Typed DataSet xsd file
//


namespace StrongTypesNS
{
    using System;
    using System.Data;


    public class RowObjectAuxDS
    {

        static void Main (string[] args)
        {
            DataSet ds = new DataSet();
            DataRelation drel;
            DataColumn[] parentCols = null, childCols = null;
            DataColumn[] keyCols = null;

            ds.DataSetName = "RowObjectAux" + "DataSet";
            ds.Namespace = "RowObjectAux" + "NS";

            
	    DataTable RowObjectAux = ds.Tables.Add("RowObjectAux");
	    RowObjectAux.Columns.Add("cod-emitente", typeof(int));
	    RowObjectAux.Columns.Add("nome-abrev", typeof(string));
	    RowObjectAux.Columns.Add("cgc", typeof(string));
	    RowObjectAux.Columns.Add("identific", typeof(int));
	    RowObjectAux.Columns.Add("natureza", typeof(int));
	    RowObjectAux.Columns.Add("nome-emit", typeof(string));
	    RowObjectAux.Columns.Add("endereco", typeof(string));
	    RowObjectAux.Columns.Add("bairro", typeof(string));
	    RowObjectAux.Columns.Add("cidade", typeof(string));
	    RowObjectAux.Columns.Add("estado", typeof(string));
	    RowObjectAux.Columns.Add("cep", typeof(string));
	    RowObjectAux.Columns.Add("caixa-postal", typeof(string));
	    RowObjectAux.Columns.Add("pais", typeof(string));
	    RowObjectAux.Columns.Add("ins-estadual", typeof(string));
	    RowObjectAux.Columns.Add("cod-cond-pag", typeof(int));
	    RowObjectAux.Columns.Add("taxa-financ", typeof(decimal));
	    RowObjectAux.Columns.Add("data-taxa", typeof(System.DateTime));
	    RowObjectAux.Columns.Add("cod-transp", typeof(int));
	    RowObjectAux.Columns.Add("cod-gr-forn", typeof(int));
	    RowObjectAux.Columns.Add("linha-produt", typeof(string));
	    RowObjectAux.Columns.Add("atividade", typeof(string));
	    RowObjectAux.Columns.Add("contato1", typeof(string));
	    RowObjectAux.Columns.Add("contato2", typeof(string));
	    RowObjectAux.Columns.Add("telefone1", typeof(string));
	    RowObjectAux.Columns.Add("telefone2", typeof(string));
	    RowObjectAux.Columns.Add("ramal1", typeof(string));
	    RowObjectAux.Columns.Add("ramal2", typeof(string));
	    RowObjectAux.Columns.Add("telefax", typeof(string));
	    RowObjectAux.Columns.Add("ramal-fax", typeof(string));
	    RowObjectAux.Columns.Add("telex", typeof(string));
	    RowObjectAux.Columns.Add("data-implant", typeof(System.DateTime));
	    RowObjectAux.Columns.Add("compr-period", typeof(decimal));
	    RowObjectAux.Columns.Add("end-cobranca", typeof(int));
	    RowObjectAux.Columns.Add("cod-rep", typeof(int));
	    RowObjectAux.Columns.Add("categoria", typeof(string));
	    RowObjectAux.Columns.Add("bonificacao", typeof(decimal));
	    RowObjectAux.Columns.Add("istr", typeof(int));
	    RowObjectAux.Columns.Add("cod-gr-cli", typeof(int));
	    RowObjectAux.Columns.Add("lim-credito", typeof(decimal));
	    RowObjectAux.Columns.Add("dt-lim-cred", typeof(System.DateTime));
	    RowObjectAux.Columns.Add("perc-fat-ped", typeof(int));
	    RowObjectAux.Columns.Add("portador", typeof(int));
	    RowObjectAux.Columns.Add("modalidade", typeof(int));
	    RowObjectAux.Columns.Add("ind-fat-par", typeof(bool));
	    RowObjectAux.Columns.Add("ind-cre-cli", typeof(int));
	    RowObjectAux.Columns.Add("ind-apr-cred", typeof(bool));
	    RowObjectAux.Columns.Add("nat-operacao", typeof(string));
	    RowObjectAux.Columns.Add("observacoes", typeof(string));
	    RowObjectAux.Columns.Add("per-minfat", typeof(decimal));
	    RowObjectAux.Columns.Add("emissao-ped", typeof(int));
	    RowObjectAux.Columns.Add("nome-matriz", typeof(string));
	    RowObjectAux.Columns.Add("telef-modem", typeof(string));
	    RowObjectAux.Columns.Add("ramal-modem", typeof(string));
	    RowObjectAux.Columns.Add("telef-fac", typeof(string));
	    RowObjectAux.Columns.Add("ramal-fac", typeof(string));
	    RowObjectAux.Columns.Add("agencia", typeof(string));
	    RowObjectAux.Columns.Add("nr-titulo", typeof(int));
	    RowObjectAux.Columns.Add("nr-dias", typeof(int));
	    RowObjectAux.Columns.Add("per-max-canc", typeof(decimal));
	    RowObjectAux.Columns.Add("dt-ult-venda", typeof(System.DateTime));
	    RowObjectAux.Columns.Add("emite-bloq", typeof(bool));
	    RowObjectAux.Columns.Add("emite-etiq", typeof(bool));
	    RowObjectAux.Columns.Add("tr-ar-valor", typeof(int));
	    RowObjectAux.Columns.Add("gera-ad", typeof(bool));
	    RowObjectAux.Columns.Add("port-prefer", typeof(int));
	    RowObjectAux.Columns.Add("mod-prefer", typeof(int));
	    RowObjectAux.Columns.Add("bx-acatada", typeof(int));
	    RowObjectAux.Columns.Add("conta-corren", typeof(string));
	    RowObjectAux.Columns.Add("nr-copias-ped", typeof(int));
	    RowObjectAux.Columns.Add("cod-suframa", typeof(string));
	    RowObjectAux.Columns.Add("cod-cacex", typeof(string));
	    RowObjectAux.Columns.Add("gera-difer", typeof(int));
	    RowObjectAux.Columns.Add("nr-tabpre", typeof(string));
	    RowObjectAux.Columns.Add("ind-aval", typeof(int));
	    RowObjectAux.Columns.Add("user-libcre", typeof(string));
	    RowObjectAux.Columns.Add("ins-banc1", typeof(int));
	    RowObjectAux.Columns.Add("ins-banc2", typeof(int));
	    RowObjectAux.Columns.Add("ven-feriado", typeof(int));
	    RowObjectAux.Columns.Add("ven-domingo", typeof(int));
	    RowObjectAux.Columns.Add("ven-sabado", typeof(int));
	    RowObjectAux.Columns.Add("cgc-cob", typeof(string));
	    RowObjectAux.Columns.Add("cep-cob", typeof(string));
	    RowObjectAux.Columns.Add("estado-cob", typeof(string));
	    RowObjectAux.Columns.Add("cidade-cob", typeof(string));
	    RowObjectAux.Columns.Add("bairro-cob", typeof(string));
	    RowObjectAux.Columns.Add("endereco-cob", typeof(string));
	    RowObjectAux.Columns.Add("cx-post-cob", typeof(string));
	    RowObjectAux.Columns.Add("ins-est-cob", typeof(string));
	    RowObjectAux.Columns.Add("nome-mic-reg", typeof(string));
	    RowObjectAux.Columns.Add("tip-cob-desp", typeof(int));
	    RowObjectAux.Columns.Add("nome-tr-red", typeof(string));
	    RowObjectAux.Columns.Add("nat-ope-ext", typeof(string));
	    RowObjectAux.Columns.Add("cod-banco", typeof(int));
	    RowObjectAux.Columns.Add("prox-ad", typeof(int));
	    RowObjectAux.Columns.Add("lim-adicional", typeof(decimal));
	    RowObjectAux.Columns.Add("dt-fim-cred", typeof(System.DateTime));
	    RowObjectAux.Columns.Add("obs-entrega", typeof(string));
	    RowObjectAux.Columns.Add("cod-tip-ent", typeof(int));
	    RowObjectAux.Columns.Add("ins-municipal", typeof(string));
	    RowObjectAux.Columns.Add("nr-peratr", typeof(int));
	    RowObjectAux.Columns.Add("nr-mesina", typeof(int));
	    RowObjectAux.Columns.Add("insc-subs-trib", typeof(string));
	    RowObjectAux.Columns.Add("cod-mensagem", typeof(int));
	    RowObjectAux.Columns.Add("nr-dias-taxa", typeof(int));
	    RowObjectAux.Columns.Add("tp-desp-padrao", typeof(int));
	    RowObjectAux.Columns.Add("tp-rec-padrao", typeof(int));
	    RowObjectAux.Columns.Add("inf-complementar", typeof(string));
	    RowObjectAux.Columns.Add("zip-code", typeof(string));
	    RowObjectAux.Columns.Add("tp-inspecao", typeof(int));
	    RowObjectAux.Columns.Add("forn-exp", typeof(bool));
	    RowObjectAux.Columns.Add("tp-qt-prg", typeof(int));
	    RowObjectAux.Columns.Add("ind-atraso", typeof(int));
	    RowObjectAux.Columns.Add("ind-div-atraso", typeof(int));
	    RowObjectAux.Columns.Add("ind-dif-atrs-1", typeof(int));
	    RowObjectAux.Columns.Add("ind-dif-atrs-2", typeof(int));
	    RowObjectAux.Columns.Add("esp-pd-venda", typeof(int));
	    RowObjectAux.Columns.Add("ind-lib-estoque", typeof(bool));
	    RowObjectAux.Columns.Add("ind-moeda-tit", typeof(int));
	    RowObjectAux.Columns.Add("ind-rendiment", typeof(bool));
	    RowObjectAux.Columns.Add("tp-pagto", typeof(int));
	    RowObjectAux.Columns.Add("vl-min-ad", typeof(decimal));
	    RowObjectAux.Columns.Add("zip-cob-code", typeof(string));
	    RowObjectAux.Columns.Add("hora-ini", typeof(int));
	    RowObjectAux.Columns.Add("hora-fim", typeof(int));
	    RowObjectAux.Columns.Add("pais-cob", typeof(string));
	    RowObjectAux.Columns.Add("resumo-mp", typeof(int));
	    RowObjectAux.Columns.Add("ind-cred-abat", typeof(bool));
	    RowObjectAux.Columns.Add("contrib-icms", typeof(bool));
	    RowObjectAux.Columns.Add("e-mail", typeof(string));
	    RowObjectAux.Columns.Add("home-page", typeof(string));
	    RowObjectAux.Columns.Add("ind-licenciador", typeof(bool));
	    RowObjectAux.Columns.Add("endereco2", typeof(string));
	    RowObjectAux.Columns.Add("ind-aval-embarque", typeof(int));
	    RowObjectAux.Columns.Add("ind-abrange-aval", typeof(int));
	    RowObjectAux.Columns.Add("cod-tax", typeof(int));
	    RowObjectAux.Columns.Add("cn-codigo", typeof(string));
	    RowObjectAux.Columns.Add("cod-entrega", typeof(string));
	    RowObjectAux.Columns.Add("cod-isencao", typeof(int));
	    RowObjectAux.Columns.Add("dias-comp", typeof(int));
	    RowObjectAux.Columns.Add("estoque", typeof(int));
	    RowObjectAux.Columns.Add("flag-pag", typeof(int));
	    RowObjectAux.Columns.Add("item-cli", typeof(bool));
	    RowObjectAux.Columns.Add("moeda-libcre", typeof(int));
	    RowObjectAux.Columns.Add("nr-dias-atraso", typeof(int));
	    RowObjectAux.Columns.Add("valor-minimo", typeof(decimal));
	    RowObjectAux.Columns.Add("cod-parceiro-edi", typeof(int));
	    RowObjectAux.Columns.Add("agente-retencao", typeof(bool));
	    RowObjectAux.Columns.Add("percepcao", typeof(bool));
	    RowObjectAux.Columns.Add("char-1", typeof(string));
	    RowObjectAux.Columns.Add("char-2", typeof(string));
	    RowObjectAux.Columns.Add("dec-1", typeof(decimal));
	    RowObjectAux.Columns.Add("dec-2", typeof(decimal));
	    RowObjectAux.Columns.Add("int-1", typeof(int));
	    RowObjectAux.Columns.Add("int-2", typeof(int));
	    RowObjectAux.Columns.Add("log-1", typeof(bool));
	    RowObjectAux.Columns.Add("log-2", typeof(bool));
	    RowObjectAux.Columns.Add("data-1", typeof(System.DateTime));
	    RowObjectAux.Columns.Add("data-2", typeof(System.DateTime));
	    RowObjectAux.Columns.Add("cod-canal-venda", typeof(int));
	    RowObjectAux.Columns.Add("ind-sit-emitente", typeof(int));
	    RowObjectAux.Columns.Add("ind-emit-retencao", typeof(int));
	    RowObjectAux.Columns.Add("calcula-multa", typeof(bool));
	    RowObjectAux.Columns.Add("prog-emit", typeof(bool));
	    RowObjectAux.Columns.Add("nr-tab-progr", typeof(int));
	    RowObjectAux.Columns.Add("recebe-inf-sci", typeof(bool));
	    RowObjectAux.Columns.Add("cod-classif-fornec", typeof(int));
	    RowObjectAux.Columns.Add("cod-classif-cliente", typeof(int));
	    RowObjectAux.Columns.Add("nr-cheque-devol", typeof(int));
	    RowObjectAux.Columns.Add("periodo-devol", typeof(int));
	    RowObjectAux.Columns.Add("vl-max-devol", typeof(decimal));
	    RowObjectAux.Columns.Add("check-sum", typeof(string));
	    RowObjectAux.Columns.Add("val-quota-media", typeof(decimal));
	    RowObjectAux.Columns.Add("cod-repres-imp", typeof(int));
	    RowObjectAux.Columns.Add("vencto-dia-nao-util", typeof(bool));
	    RowObjectAux.Columns.Add("rend-tribut", typeof(decimal));
	    RowObjectAux.Columns.Add("utiliza-verba", typeof(bool));
	    RowObjectAux.Columns.Add("percent-verba", typeof(decimal));
	    RowObjectAux.Columns.Add("endereco_text", typeof(string));
	    RowObjectAux.Columns.Add("endereco-cob-text", typeof(string));
	    RowObjectAux.Columns.Add("short-name", typeof(string));
	    RowObjectAux.Columns.Add("log-controla-val-max-inss", typeof(bool));
	    RowObjectAux.Columns.Add("cod-inscr-inss", typeof(string));
	    RowObjectAux.Columns.Add("cod-pulmao", typeof(string));
	    RowObjectAux.Columns.Add("idi-tributac-cofins", typeof(int));
	    RowObjectAux.Columns.Add("idi-tributac-pis", typeof(int));
	    RowObjectAux.Columns.Add("log-calcula-pis-cofins-unid", typeof(bool));
	    RowObjectAux.Columns.Add("log-optan-suspens-ipi", typeof(bool));
	    RowObjectAux.Columns.Add("log-optan-cr-presmdo-subst", typeof(bool));
	    RowObjectAux.Columns.Add("retem-pagto", typeof(bool));
	    RowObjectAux.Columns.Add("portador-ap", typeof(int));
	    RowObjectAux.Columns.Add("modalidade-ap", typeof(int));
	    RowObjectAux.Columns.Add("log-contribt-subst-interm", typeof(bool));
	    RowObjectAux.Columns.Add("dat-valid-suframa", typeof(System.DateTime));
	    RowObjectAux.Columns.Add("nom-fantasia", typeof(string));
	    RowObjectAux.Columns.Add("log-possui-nf-eletro", typeof(bool));
	    RowObjectAux.Columns.Add("log-nf-eletro", typeof(bool));
	    RowObjectAux.Columns.Add("dt-atualiza", typeof(System.DateTime));
	    RowObjectAux.Columns.Add("hra-atualiz", typeof(string));
	    RowObjectAux.Columns.Add("cod-email-nfe", typeof(string));
	    RowObjectAux.Columns.Add("log-integr-totvs-colab-vendas", typeof(bool));
	    RowObjectAux.Columns.Add("cdn-atraso-max", typeof(int));
	    RowObjectAux.Columns.Add("dat-nasc", typeof(System.DateTime));
	    RowObjectAux.Columns.Add("log-beneficiario", typeof(bool));
	    RowObjectAux.Columns.Add("r-Rowid", typeof(byte[]));

	    keyCols = new DataColumn[1];
	    keyCols[0] = RowObjectAux.Columns["cod-emitente"];
	    RowObjectAux.Constraints.Add(new System.Data.UniqueConstraint("codigo", keyCols, true));
	    keyCols = new DataColumn[1];
	    keyCols[0] = RowObjectAux.Columns["nome-abrev"];
	    RowObjectAux.Constraints.Add(new UniqueConstraint("nome", keyCols, false));


            ds.WriteXmlSchema("RowObjectAux.xsd");

        }


    }
}
