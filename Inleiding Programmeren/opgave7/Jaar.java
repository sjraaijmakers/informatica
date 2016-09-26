/*
 * Naam:            Steven Raaijmakers
 * Studentnummer:   10804242
 * Studie:          Bachelor Informatica
 * Omschrijving: 	Klasse omschrijft een jaar
 */


public class Jaar {

    public int nummer;
    public int aantalDagen;

    /* Default constructor */
    Jaar(int nummer) {
        this.nummer = nummer;
        if(this.schrikkelJaar()){
            aantalDagen = 366;
        }
        else {
            aantalDagen = 365;
        }
    }

    /* Schrikkeljaar: jaartal is deelbaar door 4, niet door 100 máár wel door 400 */
    public boolean schrikkelJaar(){
        if((double)this.nummer % 4 != 0){
            return false;
        }
        else if((double)this.nummer % 400 == 0){
            return true;
        }
        else if((double)this.nummer % 100 == 0){
            return false;
        }
        else {
            return true;
        }
    }

    public boolean equals(Jaar that){
        if(this.nummer == that.nummer){
            return true;
        }
        else {
            return false;
        }
    }

    /* Print methode Jaar */
    public String toString(){
        return this.nummer + "";
    }
}
