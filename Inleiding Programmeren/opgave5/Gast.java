public class Gast {

    private String voornaam;
    private String achternaam;
    private Datum geboortedatum;

    Gast (String voornaam, String achternaam, Datum geboortedatum){
        this.voornaam = voornaam;
        this.achternaam = achternaam;
        this.geboortedatum = geboortedatum;
    }

    /* Controleer of gebruiker eerder/later is geboren dan "vandaag" */
    public String achttien(){
        if(this.geboortedatum.jaar > Hotel.vandaag.jaar-18){
            return "*";
        }
        else if (this.geboortedatum.jaar == Hotel.vandaag.jaar){
            if( this.geboortedatum.maand == Hotel.vandaag.maand &&
                this.geboortedatum.dag > Hotel.vandaag.dag){
                return "*";
            }
            else if (this.geboortedatum.maand > Hotel.vandaag.maand){
                return "*";
            }
            else {
                return "";
            }
        }
        else {
            return "";
        }
    }

    public String toString(){
        return achternaam + ", " + voornaam + " (" + geboortedatum + ")" + achttien();
    }
}
