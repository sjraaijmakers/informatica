/*
 * Naam:            Steven Raaijmakers
 * Studentnummer:   10804242
 * Studie:          Bachelor Informatica
 * Omschrijving: 	Klasse beschrijft wiskundige functies van complexe breuken 
 */

public class ComplexGetal implements ComplexGetalInterface {
    Breuk re, im;

    public ComplexGetal(int t1, int n1, int t2, int n2){
        re = new Breuk(t1, n1);
        im = new Breuk(t2, n2);
    }

    ComplexGetal(Breuk a, Breuk b){
        re = a;
        im = b;
    }

    ComplexGetal(){
    }

    ComplexGetal(ComplexGetal Original){
    }

    public String toString(){
        /* Een +- situatie omzetten naar één enkele - */
        if(im.teller < 0){
            return re + " - " + im.teller*-1 + "/" + im.noemer + "i";
        }
        else if(im.noemer < 0){
            return re + " - " + im.teller + "/" + im.noemer*-1 + "i";
        }
        else {
            return re + " + " + im + "i";
        }
    }

    /* Complexe breuk optellen */
    public ComplexGetal telop(ComplexGetal cg){
        Breuk reeel = re.telop(cg.re);
        Breuk imaginair = im.telop(cg.im);
        return new ComplexGetal(reeel, imaginair);
    }

    /* Complexe breuk aftrekken */
    public ComplexGetal trekaf(ComplexGetal cg){
        Breuk reeel = re.trekaf(cg.re);
        Breuk imaginair = im.trekaf(cg.im);
        return new ComplexGetal(reeel, imaginair);
    }

    /* Complexe breuk vermenivuldigen */
    public ComplexGetal vermenigvuldig(ComplexGetal cg){
        /* Waardes breuken opslaan in (herkenbare) variabelen */
        int p1 = re.teller;
        int q1 = re.noemer;
        int r1 = im.teller;
        int s1 = im.noemer;

        int p2 = cg.re.teller;
        int q2 = cg.re.noemer;
        int r2 = cg.im.teller;
        int s2 = cg.im.noemer;

        /* Complexe breuk vermenigvuldigen: (a - b) + (c + d) */

        /* a - b = (p1 * p2 / q1 * q2) - (r1 * r2 / s1 * s2) */
        Breuk a = new Breuk(p1 * p2, q1 * q2);
        Breuk b = new Breuk(r1 * r2, s1 * s2);
        /* c + d = (p1 * r1 / q1 * s2) + (r1 * p2 / s1 * q2) */
        Breuk c = new Breuk(p1 * r2, q1 * s2);
        Breuk d = new Breuk(r1 * p2, s1 * q2);

        return new ComplexGetal(a.trekaf(b), c.telop(d));
    }

    /* Complexe breuk delen */
    public ComplexGetal deel(ComplexGetal cg){
        return this.vermenigvuldig(cg.omgekeerde());
    }

    /* Complexe breuk omkeren */
    public ComplexGetal omgekeerde(){

        /* Waardes breuk opslaan in (herkenbare) variabelen */
        int p = re.teller;
        int q = re.noemer;
        int r = im.teller;
        int s = im.noemer;

        /* Omgekeerde van breuk: b1 - b2 */

        /* b1: b1_t / b1_n: (p * q * s²) / (p² * s² + r² * q²) */
        int b1_t = p * q * (s * s);
        int b1_n = (p * p) * (s * s) + (r * r) * (q * q);

        /* b2: b2_t / b2_n:  r * q² * s) / (p² * s² + r² * q²) */
        int b2_t = r * (q * q) * s;
        int b2_n = (p * p) * (s * s) + (r * r) * (q * q);

        Breuk a = new Breuk(b1_t, b2_n);
        Breuk b = new Breuk(-b2_t, b2_n);

        return new ComplexGetal(a,b);
    }
}
