/*
 * Naam:            Steven Raaijmakers
 * Studentnummer:   10804242
 * Studie:          Bachelor Informatica
 * Omschrijving: 	Klasse Paar representeert enkel paar uit een veelterm (polynoom): ax^b
 */

class Paar implements Comparable<Paar> {
    public double coef;
    public int macht;

    /* Standaard constructor */
    Paar(double a, int b) {
        coef = a;
        macht = b;
    }

    /*  Constructor met een paar (gebruikt voor vermenigvuldig) */
    Paar(Paar a) {
        coef = a.coef;
        macht = a.macht;
    }

    /* Compare functie implementeren */
    public int compareTo(Paar that) {
        if(this.macht < that.macht){
            return -1;
        }
        else if(this.macht == that.macht){
            return 0;
        }
        /* this.macht > that.macht */
        else {
            return 1;
        }
    }

    /* Equals methode, of Paar a volledig overeenkomt met Paar b (machten én coëffecienten) */
    public boolean equals(Paar that) {
        if(this.macht == that.macht && this.coef == that.coef){
            return true;
        }
        else {
            return false;
        }
    }

    /* Paar vermenigvuldigen (gebruikt voor negatief paar in Polynoom.java) */
    public Paar vermenigvuldig (int i){
        double newCoef = this.coef * i;
        Paar tmp = new Paar(newCoef, this.macht);
        return tmp;
    }

    /* ToString methode Paar */
    public String toString () {
        String coefPrint = "" + coef;
        String machtPrint = "x^" + macht;
        /* x^0 printen als x */
        if(macht == 0){
            machtPrint = "";
        }
        /* 0x^b leeg printen */
        if(coef == 0){
            coefPrint = "";
            machtPrint = "";
        }
        /* 1x^b printen als x^b */
        else if(coef == 1 && macht != 0){
            coefPrint = "";
            machtPrint = "x^" + macht;
        }
        return coefPrint + machtPrint;
    }
}