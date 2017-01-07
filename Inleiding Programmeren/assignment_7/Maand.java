/*
 * Naam:            Steven Raaijmakers
 * Studentnummer:   10804242
 * Studie:          Bachelor Informatica
 * Omschrijving: 	Klasse omschrijft voor een maand, bepaalde data
 */

class Maand {

    public String naam;
    public int nummer;

    /* Array maand namen */
    static String[] namen = {
        null,
        "januari", "februari",
        "maart", "april",
        "mei", "juni",
        "juli", "augustus",
        "september", "oktober",
         "november", "december"
    };

    /* Array aantal dagen voor niet-schrikkeljaar */
    static int[] dagen = {
        0,
        31, 28,
        31, 30,
        31, 30,
        31, 31,
        30, 31,
        30, 31
    };

    /* Array aantal dagen voor schrikkeljaar */
    static int[] schrikkelDagen = {
        0,
        31, 29,
        31, 30,
        31, 30,
        31, 31,
        30, 31,
        30, 31
    };

    /* Standaard constructor */
    Maand(int nummer) {
        this.nummer = nummer;
        this.naam = namen[this.nummer];
    }

    /* Aantal dagen (te combineren met schrikkelJaar()) */
    public int aantalDagen(boolean b){
        int tmp;
        if(b){
            tmp = schrikkelDagen[this.nummer];
        }
        else {
            tmp = dagen[this.nummer];
        }
        return tmp;
    }

    /* Print methode */
    public String toString(){
        return naam;
    }

}
