/*
 * Naam:            Steven Raaijmakers
 * Studentnummer:   10804242
 * Studie:          Bachelor Informatica
 * Omschrijving: 	Programma test output van complexe breuken
 */

public class TestComplexeGetallen {
    public static void main(String [] args){
        ComplexGetal a = new ComplexGetal (5, 3, 1, 2);
        ComplexGetal b = new ComplexGetal (1, 6, -1, 3);
        System.out.println("a                   = " + a);
        System.out.println("b                   = " + b);
        System.out.println("a + b               = " + a.telop(b));
        System.out.println("a - b + b           = " + a.trekaf(b).telop(b));
        System.out.println("(a - b) + (b - a)   = " + a.trekaf(b).telop((b.trekaf(a))));
        System.out.println("a * b               = " + a.vermenigvuldig(b));
        System.out.println("a / b               = " + a.deel(b));
        System.out.println("(a / b) * b         = " + (a.deel(b)).vermenigvuldig(b));
        System.out.println("(a / b) * (b / a)   = " + (a.deel(b)).vermenigvuldig(b.deel(a)));
        System.out.println("omgekeerde(a)       = " + a.omgekeerde());
        System.out.println("a * omgekeerde(a)   = " + a.vermenigvuldig(a.omgekeerde()));
    }
}
