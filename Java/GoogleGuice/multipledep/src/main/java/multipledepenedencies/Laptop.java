package multipledepenedencies;

public class Laptop {

    private String model;
    private String price;

    public Laptop(){
        System.out.println("Latop constructor");
        this.model = "HP 323233232";
        this.price = "$545034";
    }

    public String toString(){
        return "[Laptop: " + model + "," + price + "]";
    }
}
