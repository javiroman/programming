package multipledepenedencies;

public class Mobile {

    private String number;

    public Mobile() {
        System.out.println("Mobile constructor");
        this.number = "988438434";
    }

    public String toString() {
        return "[Mobile: " + number + "]";
    }
}

