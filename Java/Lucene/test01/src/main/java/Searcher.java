import org.apache.lucene.document.Document;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.Term;
import org.apache.lucene.search.*;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.store.Directory;
import java.io.IOException;
import java.nio.file.Paths;

public class Searcher {

    public static void main(String[] args) throws IllegalArgumentException,
            IOException {

        String indexDir = "target/index";
        // query string
        String q = "patent";

        search(indexDir, q);
    }

    public static void search(String indexDir, String q)
            throws IOException {

        Directory dir = FSDirectory.open(Paths.get(indexDir)); //3
        IndexReader reader = DirectoryReader.open(dir);
        IndexSearcher searcher = new IndexSearcher(reader);

        Query query = new TermQuery(new Term("contents", q));

        long start = System.currentTimeMillis();
        TopDocs hits = searcher.search(query, 10); //5
        long end = System.currentTimeMillis();

        System.err.println("Found " + hits.totalHits +       //6
                " document(s) (in " + (end - start) +        // 6
                " milliseconds) that matched query '" +      // 6
                q + "':");                                   // 6

        for(ScoreDoc scoreDoc : hits.scoreDocs) {
            Document doc = searcher.doc(scoreDoc.doc);               //7
            System.out.println(doc.get("fullpath"));  //8
        }

        reader.close();                                //9
    }
}

/*
#3 Open index
#4 Parse query
#5 Search index
#6 Write search stats
#7 Retrieve matching document
#8 Display filename
#9 Close IndexSearcher
*/
