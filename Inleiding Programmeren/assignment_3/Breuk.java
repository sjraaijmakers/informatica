/*
 * Naam:            Steven Raaijmakers
 * Studentnummer:   10804242
 * Studie:          Bachelor Informatica
 * Omschrijving: 	Klasse beschrijft de wiskundige functies van een breuk
 */

class Breuk implements BreukInterface {
    int teller, noemer;

    /* Breuk */
    Breuk(int t, int n){
        /* Wanneer de noemer 0 is: */
        if(n == 0){
            System.out.println("Fout, kan niet delen door 0.");
            System.exit(0);
        }
        /* Breuk vereenvoudigen d.m.v. grootste gemeenschappelijke deler */
        else {
            int ggd = ggd(t,n);
            teller = t / ggd;
            noemer = n / ggd;
        }
    }

    Breuk(int t){
        /* Noemer gelijk stellen aan 1 wanneer er slechts een waarde is doorgegeven */
        teller = t;
        noemer = 1;
    }

    Breuk(){
    }

    Breuk(Breuk original){
    }

    /* Converteert deze breuk naar string formaat: "teller/noemer" */
    public String toString(){
        if(noemer != 1){
            return teller + "/" + noemer;
        }
        else if(noemer == teller){
            return "1";
        }
        /* Een n/1 breuk printen als n */
        else {
            return "" + teller;
        }
    }

    /* Grootste gemeenschappelijke deler */
    static int ggd(int x, int y){
        int a = x;
        int b = y;
        /* Als a negatief is; dit positief maken */
        if(a < 0){
            a = a * -1;
        }
        /* Algoritme van euclides (bron: http://nl.wikipedia.org/wiki/Algoritme_van_Euclides) */
        int rest;
        while (b != 0){
            rest = a % b;
            a = b;
            b = rest;
        }
        return a;
    }

    /* Telop: kruislinks vermenigvuldigen */
    public Breuk telop(Breuk b2){
        int nieuweteller = this.teller * b2.noemer + b2.teller * this.noemer;
        int nieuwenoemer = this.noemer * b2.noemer;
        return new Breuk(nieuweteller, nieuwenoemer);
    }

    /* Trekaf: kruislinks vermenigvuldigen  */
    public Breuk trekaf(Breuk b2) {
        int nieuweteller = this.teller * b2.noemer - b2.teller * noemer;
        int nieuwenoemer = this.noemer * b2.noemer;
        return new Breuk(nieuweteller, nieuwenoemer);
    }

    /* Vermenigvuldig */
    public Breuk vermenigvuldig(Breuk b2){
        int nieuweteller = this.teller * b2.teller;
        int nieuwenoemer = this.noemer * b2.noemer;
        return new Breuk(nieuweteller, nieuwenoemer);
    }

    /* Delen: vermenigvuldigen met omgekeerde van b2 */
    public Breuk deel(Breuk b2){
        return this.vermenigvuldig(b2.omgekeerde());
    }

    /* Omgekeerd */
    public Breuk omgekeerde(){
        return new Breuk(this.noemer, this.teller);
    }
}
