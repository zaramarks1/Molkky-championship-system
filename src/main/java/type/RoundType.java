package type;

public enum RoundType {
    FINNISH("Finlandaise"),
    KNOCKOUT("Éliminatoire"),
    POOL("Poule"),
    SIMPLEGAME("Partie simple"),
    SWISSPOOL("Poule suisse");

    private final String displayValue;

    private RoundType(String displayValue) {
        this.displayValue = displayValue;
    }

    public String getDisplayValue() {
        return displayValue;
    }
}
