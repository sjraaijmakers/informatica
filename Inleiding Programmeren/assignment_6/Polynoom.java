/*
 * Naam:            Steven Raaijmakers
 * Studentnummer:   10804242
 * Studie:          Bachelor Informatica
 * Omschrijving: 	Klasse beschrijft een Polynoom, en kan hiermee wiskunde fucnties uitvoeren
 */

import java.util.*;
import java.io.*;

class Polynoom implements PolynoomInterface {
    public ArrayList<Paar> termen;

    /* Polynoom vanuit file weergeven (en vereenvoudigen) */
    Polynoom(String filenaam) {
        this.termen = new ArrayList<Paar>();
        leesPolynoom (filenaam, termen);
        Collections.sort(vereenvoudig(termen));
        Collections.reverse(vereenvoudig(termen));
    }

    /* Polynoom vanuit ArrayList<Paar> (en vereenvoudigen)*/
    Polynoom(ArrayList<Paar> termen) {
        this.termen = termen;
        Collections.sort(vereenvoudig(termen));
        Collections.reverse(vereenvoudig(termen));
    }

    /* Polynoom file uitlezen */
    static void leesPolynoom (String filenaam , ArrayList<Paar> termen) {
        try {
            Scanner input = new Scanner(new File(filenaam));
            while (input.hasNext()) {
                int first = input.nextInt();
                int second = input.nextInt();
                Paar paar = new Paar(first, second);
                termen.add(paar);
            }
            input.close();
        }
        catch(Exception e){
            System.out.println("Er is iets misgegaan bij het inlezen van de Polynoom file.\n");
            System.exit(0);
        }
    }

    /* Twee polynomen bij elkaar optellen */
    public Polynoom telop(Polynoom that){
        /* Beiden polynomen in één gezamelijke lijst stoppen */
        ArrayList<Paar> tmp = new ArrayList<Paar>();
        for(int i = 0; i < this.termen.size(); i++){
            tmp.add(this.termen.get(i));
        }
        for(int i = 0; i < that.termen.size(); i++){
            tmp.add(that.termen.get(i));
        }
        Polynoom n = new Polynoom(tmp);
        return n;
    }

    /* Twee polynoom van elkaar aftrekken */
    public Polynoom trekaf(Polynoom that){
        /* Beiden polynomen in één gezamelijke lijst stoppen */
        ArrayList<Paar> tmp = new ArrayList<Paar>();
        for(int i = 0; i < this.termen.size(); i++){
            tmp.add(this.termen.get(i));
        }
        /* Coefficienten tweede polynoom negatief maken */
        for(int i = 0; i < that.termen.size(); i++){
            Paar n = new Paar(that.termen.get(i).vermenigvuldig(-1));
            tmp.add(n);
        }
        Polynoom n = new Polynoom(tmp);
        return n;
    }

    /* Twee polynomen vermenigvuldigen (aX^b * cX^d = acX^b+d) */
    public Polynoom vermenigvuldig (Polynoom that){
        ArrayList<Paar> tmp = new ArrayList<Paar>();
        for(int i = 0; i < this.termen.size(); i++){
            for(int j = 0; j < that.termen.size(); j++){
                double newCoef = this.termen.get(i).coef * that.termen.get(j).coef;
                int newMacht = this.termen.get(i).macht + that.termen.get(j).macht;
                Paar n = new Paar(newCoef, newMacht);
                tmp.add(n);
            }
        }
        Polynoom n = new Polynoom(tmp);
        return n;
    }

    /* Polynoom differentieren (f(x)= aX^b, f'(x)= abX^b-1) */
    public Polynoom differentieer(){
        ArrayList<Paar> tmp = new ArrayList<Paar>();
        for(int i = 0; i < this.termen.size(); i++){
            double newCoef = this.termen.get(i).coef * this.termen.get(i).macht;
            int newMacht = this.termen.get(i).macht - 1;
            if(newCoef != 0){
                Paar n = new Paar(newCoef, newMacht);
                tmp.add(n);
            }
        }
        Polynoom n = new Polynoom(tmp);
        return n;
    }

    /* Polynomen integreren (f(x)= aX^b, F(x)= a/b+1X^b+1) */
    public Polynoom integreer(){
        ArrayList<Paar> tmp = new ArrayList<Paar>();
        for(int i = 0; i < this.termen.size(); i++){
            double newCoef = this.termen.get(i).coef / (this.termen.get(i).macht + 1);
            int newMacht = this.termen.get(i).macht + 1;
            Paar n = new Paar(newCoef, newMacht);
            tmp.add(n);
        }
        /* Integratie cosntante van 1.0 */
        Paar constante = new Paar(1.0, 0);
        tmp.add(constante);
        Polynoom n = new Polynoom(tmp);
        return n;

    }

    /* Equals methode: Paar x van polynoom a vergelijken met Paar x van polynoom b, etc. */
    public boolean equals(Polynoom that){
        boolean b = true;
        if(this.termen.size() == that.termen.size()){
            for(int i = 0; i < this.termen.size(); i++){
                if(!this.termen.get(i).equals(that.termen.get(i))){
                    b = false;
                    break;
                }
            }
        }
        else {
            b = false;
        }
        return b;
    }

    /* Vereenvoudig arraylist van paren (gelijke machten; coef bij elkaar optellen)*/
    public ArrayList<Paar> vereenvoudig(ArrayList<Paar> paren){
        for(int i = 0; i < paren.size() ; i++){
            for(int j = i + 1; j < paren.size(); j++){
                if(paren.get(i).macht == paren.get(j).macht){
                    double newCoef = paren.get(i).coef + paren.get(j).coef;
                    Paar p = new Paar(newCoef, paren.get(i).macht);
                    /* Paar I aanpassen, Paar J verwijderen */
                    paren.set(i, p);
                    paren.remove(j);
                }
            }
        }
        return paren;
    }

    /* To string methode Polynoom */
    public String toString(){
        String polynoom = "";
        for(int i = 0; i < termen.size(); i++){
            /* Negatieve paar printen: - ax^b (mits het niet het eerste paar is) */
            if(termen.get(i).coef < 0 && i != 0){
                polynoom = polynoom + " - " + termen.get(i).vermenigvuldig(-1);
            }
            else {
                /* 0x^b niet printen */
                if(termen.get(i).coef == 0){
                    polynoom = polynoom;
                }
                /* Paar printen: + ax^b (mits het niet het eerste paar is) */
                else if(i != 0){
                    polynoom = polynoom + " + " + termen.get(i);
                }
                /* Het eerste paar printen zonder teken */
                else {
                    polynoom = polynoom + "" + termen.get(i);
                }
            }
        }
        return polynoom;
    }

}
