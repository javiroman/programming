package org.acme.entities.author;

public class AuthorResponse {
    private String name;

    public AuthorResponse(String name) {
        setName(name);
    }

    public void setName(String r) {
        this.name = r;
    }

    public String getName() {
        return name;
    }

}
