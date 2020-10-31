import junit.framework.TestCase;

public class CalculatorTest extends TestCase {
    public void testAdd() {
        Calculator calculator = new Calculator();
        double result = calculator.add(10, 50);
        assertEquals(60, result, 0);
    }
}
