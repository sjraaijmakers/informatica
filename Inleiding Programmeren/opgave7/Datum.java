/*
 * Naam:            Steven Raaijmakers
 * Studentnummer:   10804242
 * Studie:          Bachelor Informatica
 * Omschrijving: 	Omschrijving van Data klasse en functies
 * Bronnen:         Weeknummer: http://en.wikipedia.org/wiki/ISO_week_date
 *                  dagInWeek: http://stackoverflow.com/questions/9847213/which-day-of-week-given-a-date-python werkt
 */

public class Datum {
    public Jaar jaar;
    public Maand maand;
    public Dag dag;

    /* Default constructor */
    Datum(Dag dag, Maand maand, Jaar jaar){
        this.dag = dag;
        this.jaar = jaar;
        this.maand = maand;
    }

    /* Constructor met Datum parameter */
    Datum(Datum datum){
        this.jaar = datum.jaar;
        this.maand = datum.maand;
        this.dag = datum.dag;
    }

    /* Dag in de week, van Datum */
    public int dagInWeek(){
        int[] omzetten = {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};
        int naFebruari = 1;
        if (this.maand.nummer > 2){
            naFebruari = 0;
        }
        int aux = this.jaar.nummer - 1700 - naFebruari;
        int dagInWeek = 5;
        dagInWeek += (aux + naFebruari) * 365;
        dagInWeek += aux / 4 - aux / 100 + (aux + 100) / 400;
        dagInWeek += omzetten[this.maand.nummer - 1] + (dag.nummer - 1);
        dagInWeek = (dagInWeek + 13) % 7;
        return dagInWeek;
    }

    /* Dagnaam */
    public String dagNaam(int dagInWeek){
        String[] week = {
            "Maandag", "Dinsdag",
            "Woensdag", "Donderdag",
            "Vrijdag", "Zaterdag",
            "Zondag"};
        return week[dagInWeek];
    }

    /* Hoeveelste dag in het jaar, van Datum */
    public int dagInJaar(){
        int dagInJaar = 0;
        for(int i = 1; i < this.maand.nummer; i++){
            if(jaar.schrikkelJaar()){
                dagInJaar = dagInJaar + this.maand.schrikkelDagen[i];
            }
            else {
                dagInJaar = dagInJaar + this.maand.dagen[i];
            }
        }
        dagInJaar = dagInJaar + this.dag.nummer;
        return dagInJaar;
    }

    /* Weeknummer van Datum */
    public int weekNummer(){
        int weekNummer = (dagInJaar() - dagInWeek() + 10) / 7;
        if(weekNummer >= 53){
            if(this.dagInWeek() >= 4){
                weekNummer = 1;
            }
            else {
                weekNummer = 52;
            }
        }
        else if(weekNummer == 0){
            if(this.dagInWeek() >= 4){
                weekNummer = 1;
            }
            else {
                weekNummer = 52;
            }
        }
        return weekNummer;
    }

    /* Print methode Datum */
    public String toString(){
        return "" + dagNaam(dagInWeek()) + " " + dag + " " + maand.naam + ", " + jaar;
    }
}
