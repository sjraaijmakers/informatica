interface PolynoomInterface {
    Polynoom vermenigvuldig(Polynoom that);
    Polynoom trekaf(Polynoom that);
    Polynoom telop(Polynoom that);
    Polynoom differentieer();
    Polynoom integreer();
    boolean equals(Object o);
    String toString();
}
