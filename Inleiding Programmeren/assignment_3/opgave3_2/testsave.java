/*
 * Naam:            Steven Raaijmakers
 * Studentnummer:   10804242
 * Studie:          Bachelor Informatica
 * Omschrijving: 	Programma kan wiskundige functies met complexe getallen uitvoeren
*/

public class ComplexGetal implements ComplexGetalInterface {
    Breuk re;
    Breuk im;

    public ComplexGetal(int t1, int n1, int t2, int n2){
        re = new Breuk(t1, n1);
        im = new Breuk(t2, n2);
    }

    ComplexGetal(Breuk a, Breuk b){
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
        Breuk reeel = re.telop(cg.re);
        Breuk imaginair = im.telop(cg.im);

        return new ComplexGetal(reeel, imaginair)
    }

}
