package org.acme.resources.author;

import io.quarkus.hibernate.orm.panache.PanacheEntityBase;
import org.acme.entities.author.Author;
import org.acme.entities.author.AuthorRepository;
import org.acme.entities.author.AuthorResponse;
import org.acme.entities.book.Book;
import org.acme.entities.book.BookRepository;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponse;
import org.eclipse.microprofile.openapi.annotations.responses.APIResponses;
import org.eclipse.microprofile.openapi.annotations.tags.Tag;

import javax.enterprise.context.RequestScoped;
import javax.inject.Inject;
import javax.transaction.Transactional;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@RequestScoped
@Path("/v1/author")
@Tag(name = "Author Resource", description = "Book Author information")
public class AuthorResource {

    @Inject
    AuthorRepository authorRepository;

    @POST
    @Transactional
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @APIResponses(value = {
            @APIResponse(responseCode = "202", description = "Accepted (environment creation in process)"),
            @APIResponse(responseCode = "400", description = "Bad Request: Some properties fails"),
            @APIResponse(responseCode = "404", description = "Not Found: User not exists or password is not valid"),
            @APIResponse(responseCode = "500", description = "Internal Server Error")
    }
    )
    public AuthorResponse createNewAuthor(Author payload) {

        System.out.println(payload);
        String name = payload.getName();
        System.out.println(payload.getParam());

        payload.persist();

        return new AuthorResponse(name);
    }
}