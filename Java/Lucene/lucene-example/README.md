# Example of indexing and searching with Apache Lucene

[![Build Status](https://travis-ci.org/cassiomolin/lucene-example.svg?branch=master)](https://travis-ci.org/cassiomolin/lucene-example)
[![MIT Licensed](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/cassiomolin/lucene-example/master/LICENSE.txt)

[Apache Lucene] is a high-performance text search engine library written entirely in Java. 

This example application demonstrates how to perform some operations with Apache Lucene: This application parses some JSON files with Jackson, indexes their content with Lucene and performs some searches.

## Building and running this application

1. Open a command line window or terminal.
1. Navigate to the root directory of the project, where the `pom.xml` resides.
1. Compile the project: `mvn clean compile`.
1. Package the application: `mvn package`.
1. Change into the `target` directory: `cd target`
1. You should see a file with the following or a similar name: `index-and-search-with-lucene-1.0.jar`.
1. Execute the JAR: `java -jar index-and-search-with-lucene-1.0.jar`.
1. The application should be executed and the result should be displayed in the console.

## Indexing and searching with Apache Lucene

A Lucene index ([`Directory`][Directory]) is a collection of entries ([`Document`][Document]) that contains properties ([`Field`][Field]).

A writer ([`IndexWriter`][IndexWriter]) allows you to add entries to the index while a searcher ([`IndexSearcher`][IndexSearcher]) allows you to execute queries ([`Query`][Query]) against the index and get the results ([`TopDocs`][TopDocs]).

For a better on understanding how Apache Lucene works, here's an example on how to perform some simple indexing and searching operations:

### Create an index

```java
// Create an index in memory
Directory index = new RAMDirectory();
```

### Create an index writer

```java
// Create an index writer
StandardAnalyzer analyzer = new StandardAnalyzer();
IndexWriterConfig config = new IndexWriterConfig(analyzer);
IndexWriter indexWriter = new IndexWriter(index, config);
```

### Add entries to the index

```java
// Add a document to the index
Document document = new Document();
document.add(new TextField("name", "John Doe", Field.Store.YES));
document.add(new TextField("address", "80 Summer Hill", Field.Store.YES));
indexWriter.addDocument(document);

// Add a document to the index
document = new Document();
document.add(new TextField("name", "Jane Doe", Field.Store.YES));
document.add(new TextField("address", "9 Main Circle", Field.Store.YES));
indexWriter.addDocument(document);

// Add a document to the index
document = new Document();
document.add(new TextField("name", "John Smith", Field.Store.YES));
document.add(new TextField("address", "9 Dexter Avenue", Field.Store.YES));
indexWriter.addDocument(document);
    
indexWriter.close();
```

### Create an index searcher

```java
// Create an index searcher
IndexReader reader = DirectoryReader.open(index);
IndexSearcher searcher = new IndexSearcher(reader);
```

### Create a query

```java
// Create a query to look for people with "doe" in the name
Query query = new TermQuery(new Term("name", "doe"));
```

### Execute the query and display the results

```java
// Execute que query and show the results
TopDocs topDocs = searcher.search(query, 10);

// Display addresses
for (ScoreDoc scoreDoc : topDocs.scoreDocs) {
    document = searcher.doc(scoreDoc.doc);
    System.out.println(document.get("address"));
}
```

  [Apache Lucene]: http://lucene.apache.org/core/
  [Directory]: https://lucene.apache.org/core/6_5_1/core/org/apache/lucene/store/Directory.html
  [Document]: https://lucene.apache.org/core/6_5_1/core/org/apache/lucene/document/Document.html
  [Field]: https://lucene.apache.org/core/6_5_1/core/org/apache/lucene/document/Field.html
  [IndexWriter]: https://lucene.apache.org/core/6_5_1/core/org/apache/lucene/index/IndexWriter.html
  [IndexSearcher]: https://lucene.apache.org/core/6_5_1/core/org/apache/lucene/search/IndexSearcher.html
  [Query]: https://lucene.apache.org/core/6_5_1/core/org/apache/lucene/search/Query.html
  [TopDocs]: https://lucene.apache.org/core/6_5_1/core/org/apache/lucene/search/TopDocs.html
