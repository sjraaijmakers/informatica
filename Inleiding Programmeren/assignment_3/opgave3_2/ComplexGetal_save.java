/*
 * Naam:            Steven Raaijmakers
 * Studentnummer:   10804242
 * Studie:          Bachelor Informatica
 * Omschrijving: 	Programma kan wiskundige functies met complexe getallen uitvoeren
*/

public class ComplexGetal implements ComplexGetalInterface {
    double a, b;

    ComplexGetal(double a, double b){
        this.a = a;
        this.b = b;
    }

    ComplexGetal(int a){
    }

    ComplexGetal(){
    }

    ComplexGetal(ComplexGetal Original){
    }

    public String toString(){
        /* Een "+ -"-situatie omzetten naar een enkele "-" */
        if(b < 0){
            return "" + a + " - " + b*-1 + "i";
        }
        else {
            return "" + a + " + " + b + "i";
        }
    }

    public ComplexGetal telop(ComplexGetal cg){
        double c = cg.a;
        double d = cg.b;

        /* Optellen d.m.v: (a + c) + (b + d)i */
        double nieuwereeel = a + c;
        double nieuweimaginair = b + d;

        ComplexGetal complex_getal = new ComplexGetal(nieuwereeel, nieuweimaginair);
        return complex_getal;
    }

    public ComplexGetal trekaf(ComplexGetal cg){
        double c = cg.a;
        double d = cg.b;

        /* Aftrekken d.m.v: (a - c) + (b - d)i */

        double nieuw_a = a - c;
        double nieuw_b = b - d;

        /* Wanneer aftrekken met negatief getal; dit optellen. */
        if(c < 0){
            nieuw_a = a + c *-1;
        }
        else if (d < 0){
            nieuw_b = b + d *-1;
        }

        ComplexGetal complex_getal = new ComplexGetal(nieuw_a, nieuw_b);
        return complex_getal;
    }

    public ComplexGetal vermenigvuldig(ComplexGetal cg){
        double c = cg.a;
        double d = cg.b;

        /* Complexgetal vermenigvuldigen d.m.v. formule: (a*c-b*d) + (a*d+b*c)i */
        double nieuw_a = a * c - b * d;
        double nieuw_b = a * d + b * c;

        ComplexGetal complex_getal = new ComplexGetal(nieuw_a, nieuw_b);

        return complex_getal;
    }

    public ComplexGetal deel(ComplexGetal cg){
        double c = cg.a;
        double d = cg.b;

        /* Complexgetal delen d.m.v: (a * c + b * d) / (c² + d²) */
        double nieuw_a_1 = a * c + b * d;
        double nieuw_a_2 = (c * c) + (d * d);
        double nieuw_a = nieuw_a_1 / nieuw_a_2;

        /* Complexgetal delen d.m.v: (b * c - a * d) / (c² + d²) */
        double nieuw_b_1 = (b * c) - (a * d);
        double nieuw_b_2 = (c * c) + (d * d);
        double nieuw_b = nieuw_b_1 / nieuw_b_2;

        ComplexGetal complex_getal = new ComplexGetal(nieuw_a, nieuw_b);

        return complex_getal;
    }

    public ComplexGetal omgekeerd(){
        /* Omgekeerde d.m.v: (a / (a² + b²)) + (-b / (a² + b²)) */

        double nieuw_a = a / (a*a) + (b*b);
        double nieuw_b = -b / (a*a) + (b*b);

        ComplexGetal complex_getal = new ComplexGetal(nieuw_a, nieuw_b);

        return complex_getal;
    }
}
