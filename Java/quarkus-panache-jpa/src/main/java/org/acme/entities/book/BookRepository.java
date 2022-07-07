package org.acme.entities.book;

import io.quarkus.hibernate.orm.panache.PanacheRepository;

import javax.enterprise.context.ApplicationScoped;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import java.util.Optional;

@ApplicationScoped
public class BookRepository  implements PanacheRepository<Book> {

    @PersistenceContext
    private EntityManager entityManager;

    public BookRepository(EntityManager entityManager) {
        this.entityManager = entityManager;
    }
    public Optional<Book> findById(Integer id) {
        Book book = entityManager.find(Book.class, id);
        return book != null ? Optional.of(book) : Optional.empty();
    }

    public Optional<Book> findByName(String name) {
        Book book = entityManager.createQuery("SELECT b FROM Book b WHERE b.name = :name", Book.class)
                .setParameter("name", name)
                .getSingleResult();
        return book != null ? Optional.of(book) : Optional.empty();
    }
    public Optional<Book> findByNameNamedQuery(String name) {
        Book book = entityManager.createNamedQuery("Book.findByName", Book.class)
                .setParameter("name", name)
                .getSingleResult();
        return book != null ? Optional.of(book) : Optional.empty();
    }
    public Optional<Book> save(Book book) {
        try {
            entityManager.persist(book);
            return Optional.of(book);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return Optional.empty();
    }
}