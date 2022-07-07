package main;

import org.acme.entities.author.Author;
import org.acme.entities.author.AuthorRepository;
import org.acme.entities.book.Book;
import org.acme.entities.book.BookRepository;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import java.util.List;
import java.util.Optional;

public class JpaExample {
    public static void main(String[] args) {
        /*
        // Create our entity manager
        EntityManagerFactory entityManagerFactory = Persistence.createEntityManagerFactory("Books");
        EntityManager entityManager = entityManagerFactory.createEntityManager();

        // Create our repositories
        BookRepository bookRepository = new BookRepository(entityManager);
        AuthorRepository authorRepository = new AuthorRepository(entityManager);

        // Create an author and add 3 books to his list of books
        Author author = new Author("Author 1");
        author.addBook(new Book("Book 1"));
        author.addBook(new Book("Book 2"));
        author.addBook(new Book("Book 3"));
        Optional<Author> savedAuthor = authorRepository.save(author);
        System.out.println("Saved author: " + savedAuthor.get());

        // Find all authors
        List<Author> authors = authorRepository.findAll();
        System.out.println("Authors:");
        authors.forEach(System.out::println);
        // Find author by name
        Optional<Author> authorByName = authorRepository.findByName("Author 1");
        System.out.println("Searching for an author by name: ");
        authorByName.ifPresent(System.out::println);
        // Search for a book by ID
        Optional<Book> foundBook = bookRepository.findById(2);
        foundBook.ifPresent(System.out::println);
        // Search for a book with an invalid ID
        Optional<Book> notFoundBook = bookRepository.findById(99);
        notFoundBook.ifPresent(System.out::println);
        // List all books
        List<Book> books = bookRepository.findAll();
        System.out.println("Books in database:");
        books.forEach(System.out::println);
        // Find a book by name
        Optional<Book> queryBook1 = bookRepository.findByName("Book 2");
        System.out.println("Query for book 2:");
        queryBook1.ifPresent(System.out::println);
        // Find a book by name using a named query
        Optional<Book> queryBook2 = bookRepository.findByNameNamedQuery("Book 3");
        System.out.println("Query for book 3:");
        queryBook2.ifPresent(System.out::println);
        // Add a book to author 1
        Optional<Author> author1 = authorRepository.findById(1);
        author1.ifPresent(a -> {
            a.addBook(new Book("Book 4"));
            System.out.println("Saved author: " + authorRepository.save(a));
        });
        // Close the entity manager and associated factory
        entityManager.close();
        entityManagerFactory.close();

         */
    }
}