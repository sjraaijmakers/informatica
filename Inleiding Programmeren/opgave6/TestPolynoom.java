/*
 * Naam:            Steven Raaijmakers
 * Studentnummer:   10804242
 * Studie:          Bachelor Informatica
 * Omschrijving: 	Test output van verschillende berekeningen met polynomen
 */

 import java.util.*;

class TestPolynoom {
    public static void main(String [] args) {
        Polynoom pol1, pol2, som, verschil, product, dif1, dif2, intg1, intg2, intdf1, dfint1, intdf2, dfint2;

        pol1 = new Polynoom("polynoom1");
        System.out.println("polynoom 1          :     " + pol1);
        pol2 = new Polynoom("polynoom2");
        System.out.println("polynoom 2          :     " + pol2);

        som = pol1.telop(pol2);
        System.out.println("som 1 & 2           :     " + som);
        verschil = pol1.trekaf(pol2);
        System.out.println("verschil 1 & 2      :     " + verschil);

        product = pol1.vermenigvuldig(pol2);
        System.out.println("product 1 &2        :     " + product);

        dif1 = pol1.differentieer();
        System.out.println("differentieer pol 1 :     " + dif1);
        dif2 = pol2.differentieer();
        System.out.println("differentieer pol 2 :     " + dif2);

        intg1 = pol1.integreer();
        System.out.println("integreer pol 1     :     " + intg1);
        intg2 = pol2.integreer();
        System.out.println("integreer pol 2     :     " + intg2);

        intdf1 = pol1.differentieer().integreer();
        System.out.println("D -> I pol 1        :     " + intdf1 + gelijkheid(intdf1, pol1));
        dfint1 = pol1.integreer().differentieer();
        System.out.println("I -> D pol 1        :     " + dfint1 + gelijkheid(dfint1, pol1));
        intdf2 = pol2.differentieer().integreer();
        System.out.println("D -> I pol 2        :     " + intdf2 + gelijkheid(intdf2, pol2));
        dfint2 = pol2.integreer().differentieer();
        System.out.println("I -> D pol 2        :     " + dfint2 + gelijkheid(dfint2, pol2));
    }

    /* Methode controleert gelijkheid Polynoom a met Polynoom b */
    public static String gelijkheid(Polynoom a, Polynoom b){
        if (a.equals(b)){
            return " (is gelijk aan origineel)";
        }
        else {
            return " (is niet gelijk aan origineel)";
        }
    }
}