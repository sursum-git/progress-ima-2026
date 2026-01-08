
/*
**
*/

//
// RowRaw - Used to generate Strongly Typed DataSet xsd file
//


namespace StrongTypesNS
{
    using System;
    using System.Data;


    public class RowRawDS
    {

        static void Main (string[] args)
        {
            DataSet ds = new DataSet();
            DataRelation drel;
            DataColumn[] parentCols = null, childCols = null;
            DataColumn[] keyCols = null;

            ds.DataSetName = "RowRaw" + "DataSet";
            ds.Namespace = "RowRaw" + "NS";

            
	    DataTable RowRaw = ds.Tables.Add("RowRaw");
	    RowRaw.Columns.Add("RawRecord", typeof(byte[]));


            ds.WriteXmlSchema("RowRaw.xsd");

        }


    }
}
