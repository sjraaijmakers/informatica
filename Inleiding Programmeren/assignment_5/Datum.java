public class Datum {

    public int dag, maand, jaar;

    Datum (String geboortedatum){
        if(valideerDatum(geboortedatum) == false || logischGetal(geboortedatum) == false){
            this.dag = 0;
            this.maand = 0;
            this.jaar = 0;
            System.out.println ("Onjuiste geboortedatum, gast niet ingecheckt.");
        }
        else {
            opdelen(geboortedatum);
        }
    }

    /* Ingevoerde datum controleren */
    public boolean valideerDatum(String geboortedatum){
        String patroon = "\\d{2}-\\d{2}-\\d{4}";
        if (geboortedatum.matches(patroon)){
            return true;
        }
        else {
            return false;
        }
    }

    /* Datum string opdelen in apparte waardes */
    public void opdelen(String geboortedatum){
        String [] datum = geboortedatum.split("-");
        for (int i = 0; i < 3; i++){
            /* Dag een waarde geven */
            if(i == 0){
                this.dag = Integer.parseInt(datum[0]);
            }
            /* Maand een waarde geven */
            else if (i == 1){
                this.maand = Integer.parseInt(datum[1]);
            }
            /* Jaar een waarde geven */
            else if(i == 2){
                this.jaar = Integer.parseInt(datum[2]);
            }
        }
    }

    /* Datum controleren op echtheid */
    public boolean logischGetal(String geboortedatum){
        String [] testDatum = geboortedatum.split("-");
        int testDag = Integer.parseInt(testDatum[0]);
        int testMaand = Integer.parseInt(testDatum[1]);
        int testJaar = Integer.parseInt(testDatum[2]);
        
        if(testDag <=31 && testMaand <=12 && (testJaar <= 2014 && testJaar >= 1900)){
            return true;
        }
        else {
            return false;
        }
    }

    /* Print methode van Datum */
    public String toString(){
        if(dag != 0 && maand != 0 && jaar!= 0){
            return dag + "." + maand + "." + jaar;
        }
        else {
            return "";
        }
    }
}
