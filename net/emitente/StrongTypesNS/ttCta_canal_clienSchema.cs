
/*
**
*/

//
// ttCta_canal_clien - Used to generate Strongly Typed DataSet xsd file
//


namespace StrongTypesNS
{
    using System;
    using System.Data;


    public class ttCta_canal_clienDS
    {

        static void Main (string[] args)
        {
            DataSet ds = new DataSet();
            DataRelation drel;
            DataColumn[] parentCols = null, childCols = null;
            DataColumn[] keyCols = null;

            ds.DataSetName = "ttCta_canal_clien" + "DataSet";
            ds.Namespace = "ttCta_canal_clien" + "NS";

            
	    DataTable ttCta_canal_clien = ds.Tables.Add("ttCta_canal_clien");
	    ttCta_canal_clien.Columns.Add("cod_pessoa_erp", typeof(string));


            ds.WriteXmlSchema("ttCta_canal_clien.xsd");

        }


    }
}
