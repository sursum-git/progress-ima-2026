
/*
**
*/

//
// ttAccount - Used to generate Strongly Typed DataSet xsd file
//


namespace StrongTypesNS
{
    using System;
    using System.Data;


    public class ttAccountDS
    {

        static void Main (string[] args)
        {
            DataSet ds = new DataSet();
            DataRelation drel;
            DataColumn[] parentCols = null, childCols = null;
            DataColumn[] keyCols = null;

            ds.DataSetName = "ttAccount" + "DataSet";
            ds.Namespace = "ttAccount" + "NS";

            
	    DataTable ttAccount = ds.Tables.Add("ttAccount");
	    ttAccount.Columns.Add("cod_pessoa_erp", typeof(string));
	    ttAccount.Columns.Add("num_id_pessoa", typeof(int));


            ds.WriteXmlSchema("ttAccount.xsd");

        }


    }
}
