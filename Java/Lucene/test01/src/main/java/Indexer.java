import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.StoredField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.document.Document;
import org.apache.lucene.index.IndexWriterConfig;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.store.Directory;
import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.io.FileReader;
import java.nio.file.Paths;

public class Indexer {

    public static void main(String[] args) throws Exception {

        String indexDir = "target/index";
        String dataDir  = "data";
        long start = System.currentTimeMillis();

        /*
            The Indexer class constructor creates an Indexer instance with a String path
            with the location of the index to create.
         */
        Indexer indexer = new Indexer(indexDir);

        int numIndexed;
        try {
            numIndexed = indexer.index(dataDir, new TextFilesFilter());
        } finally {
            indexer.close();
        }
        long end = System.currentTimeMillis();

        System.out.println("Indexing " + numIndexed + " files took "
                + (end - start) + " milliseconds");
    }

    private IndexWriter writer;

    // Lucene API: Creates a Lucene IndexWriter
    public Indexer(String indexDir) throws IOException {
        IndexWriterConfig config  = new IndexWriterConfig(new StandardAnalyzer());
        Directory dir = FSDirectory.open(Paths.get(indexDir));
        writer = new IndexWriter(dir, config);           //3
    }

    public void close() throws IOException {
        writer.close();                             //4
    }

    public int index(String dataDir, FileFilter filter)
            throws Exception {

        File[] files = new File(this.getClass().getClassLoader().getResource(dataDir).getFile()).listFiles();

        for (File f : files) {
            if (!f.isDirectory() &&
                    !f.isHidden() &&
                    f.exists() &&
                    f.canRead() &&
                    (filter == null || filter.accept(f))) {
                indexFile(f);
            }
        }

        //return writer.numDocs();                     //5
        return writer.numRamDocs();
    }

    // Lucene API: Create simple filter with Lucene FileFilter
    private static class TextFilesFilter implements FileFilter {
        public boolean accept(File path) {
            return path.getName().toLowerCase()
                    .endsWith(".txt");
        }
    }

    // Lucene API: Add documents to the index
    protected Document getDocument(File f) throws Exception {
        Document doc = new Document();
        // Index file content
        doc.add(new TextField("contents", new FileReader(f)));
        // Index file name
        doc.add(new StoredField("filename", f.getName()));
        // Index file full path
        doc.add(new StoredField("fullpath", f.getCanonicalPath()));

        return doc;
    }

    // Lucene API: commit the document to the index
    private void indexFile(File f) throws Exception {
        System.out.println("Indexing " + f.getCanonicalPath());
        Document doc = getDocument(f);
        writer.addDocument(doc);
    }
}