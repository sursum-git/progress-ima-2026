
/*
**
*/

//
// ttCtaPagarRec - Used to generate Strongly Typed DataSet xsd file
//


namespace StrongTypesNS
{
    using System;
    using System.Data;


    public class ttCtaPagarRecDS
    {

        static void Main (string[] args)
        {
            DataSet ds = new DataSet();
            DataRelation drel;
            DataColumn[] parentCols = null, childCols = null;
            DataColumn[] keyCols = null;

            ds.DataSetName = "ttCtaPagarRec" + "DataSet";
            ds.Namespace = "ttCtaPagarRec" + "NS";

            
	    DataTable ttCtaPagarRec = ds.Tables.Add("ttCtaPagarRec");
	    ttCtaPagarRec.Columns.Add("da-maior", typeof(System.DateTime));
	    ttCtaPagarRec.Columns.Add("de-vl-maior", typeof(decimal));
	    ttCtaPagarRec.Columns.Add("da-ultimo", typeof(System.DateTime));
	    ttCtaPagarRec.Columns.Add("de-vl-ultimo", typeof(decimal));
	    ttCtaPagarRec.Columns.Add("i-atraso-medio", typeof(int));
	    ttCtaPagarRec.Columns.Add("r-rowid", typeof(byte[]));


            ds.WriteXmlSchema("ttCtaPagarRec.xsd");

        }


    }
}
