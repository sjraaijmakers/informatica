public class TestComplexeGetallen {
    public static void main(String [] args){
        ComplexGetal a = new ComplexGetal (5.0 , 6.0);
        ComplexGetal b = new ComplexGetal ( -3.0 , 4.0);
        System.out.println("a            = " + a);
        System.out.println("b            = " + b);
        System.out.println("b + a        = " + b.telop(a));
        System.out.println("a * b        = " + a. vermenigvuldig (b));
        System.out.println("(a / b) * b  = " + a.deel(b). vermenigvuldig (b));
    }
}
