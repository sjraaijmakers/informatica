public class TestBreuken {
    public static void main(String [] args){
        Breuk a = new Breuk(1,10);
        Breuk b = new Breuk (2,5);
        System.out.println("Breuk a             = " + a);
        System.out.println("Breuk b             = " + b);
        System.out.println("a + b               = " + a.telop(b));
        System.out.println("a - b               = " + a.trekaf(b));
        System.out.println("a - b + b           = " + a.trekaf(b). telop(b));
        System.out.println("a * b               = " + a.vermenigvuldig(b));
        System.out.println("a / b               = " + a.deel(b));
        System.out.println("(a / b) * b         = " + a.deel(b).vermenigvuldig(b));
        System.out.println("(a / b) * (b / a)   = " + a.deel(b).vermenigvuldig(b.deel(a)));
        System.out.println("omgekeerde(a)       = " + a.omgekeerde());
        System.out.println("a * omgekeerde(a)   = " + a.vermenigvuldig(a.omgekeerde()));
    }
}
