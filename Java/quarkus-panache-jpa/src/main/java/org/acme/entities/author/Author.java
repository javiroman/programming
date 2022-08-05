package org.acme.entities.author;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import io.quarkiverse.hibernate.types.json.JsonBinaryType;
import io.quarkiverse.hibernate.types.json.JsonType;
import io.quarkiverse.hibernate.types.json.JsonTypes;
import io.quarkus.hibernate.orm.panache.PanacheEntityBase;
import org.acme.entities.book.Book;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.TypeDef;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@Entity
@Table(name="author")
@NamedQueries({
        @NamedQuery(name = "Author.findByName",
                query = "SELECT a FROM Author a WHERE a.name = :name")
})
@TypeDef(name = JsonTypes.JSON, typeClass = JsonType.class)
@JsonIgnoreProperties({"hibernateLazyInitializer"})
public class Author extends PanacheEntityBase {
    @Id
    @GeneratedValue
    private Integer id;

    private String name;

    @Type(type = JsonTypes.JSON)
    @Column(name = "PARAM", columnDefinition = JsonTypes.JSON)
    private MyParam param = new MyParam();

    @OneToMany(cascade = CascadeType.ALL)
    @JoinColumn(name = "author_id")
    private List<Book> books = new ArrayList<>();

    public Author() {
    }

    public Author(String name) {
        this.name = name;
    }

    public Author(Integer id, String name) {
        this.id = id;
        this.name = name;
    }
    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<Book> getBooks() {
        return books;
    }

    public void addBook(Book book) {
        books.add(book);
    }

    public MyParam getParam() {
        return this.param;
    }

    public void setParam(MyParam param) {
        this.param = param;
    }

    @Override
    public String toString() {
        return "Author{" +
                "id=" + id +
                ", name='" + name + '\'' +
                ", books=" + books +
                '}';
    }
}