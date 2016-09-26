public class Kamer {

    public boolean vrij;
    private Gast g;

    Kamer(Gast gast){
        vrij = false;
        this.g = gast;
    }

    Kamer(){
        vrij = true;
    }

    /* Gast inchecken in kamer */
    public void inchecken(Gast g){
        this.g = g;
        vrij = false;
    }

    /* Gast uitchecken bij kamer */
    public void uitchecken(){
        this.g = null;
        vrij = true;
    }

    /* toString methode kamer */
    public String toString(){
        String status = "";
        if(vrij == false){
            status = g.toString();
        }
        else if(vrij == true){
            status = "Vrij";
        }
        return status;
    }
}
