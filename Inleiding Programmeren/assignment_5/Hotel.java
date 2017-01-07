public class Hotel {

    public Kamer[] kamers;
    public static Datum vandaag = new Datum ("01-10-2014");

    Hotel(int aantalKamers){
        kamers = new Kamer[aantalKamers];
        for(int i = 0; i < aantalKamers; i++){
            kamers[i] = new Kamer();
        }
    }

    /* Hotel overzicht */
    public void overzicht() {
        System.out.println("Status van dit hotel");
        for(int i = 0; i < kamers.length; i++){
            System.out.println("Kamer " + (i + 1) + ": " + kamers[i]);
        }
        System.out.println("Aantal gasten: " + aantalGasten());
    }

    /* Aantal ingecheckte gasten */
    public int aantalGasten(){
        int aantalGasten = 0;
        for(int i = 0; i < kamers.length; i++){
            if(kamers[i].vrij == false){
                aantalGasten++;
            }
        }
        return aantalGasten;
    }

    /* Eerst volgende lege kamer */
    public int vrijeKamer() {
        int kamer = 0;
        for(int i = 0; i < kamers.length; i++){
            if(kamers[i].vrij == true){
                kamer = i + 1;
                break;
            }
        }
        return kamer;
    }

    /* Gast inchecken in hotel */
    public void inchecken(Gast g, int k) {
        kamers[k].inchecken(g);
    }

    /* Gast uitchecken uit hotel */
    public void uitchecken(int k) {
        kamers[k].uitchecken();
    }
}
