/*
  One implementation of Add interface
 */
package add.service;

public class SimpleAdd implements Add{

    public SimpleAdd() {
        System.out.println("constructor SimpleAdd");
    }

    public int add(int a, int b) {
        return a + b;
    }
    public int add2(int a, int b) {
        return (a + b)*2;
    }
}
