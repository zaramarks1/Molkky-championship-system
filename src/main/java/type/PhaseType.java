package type;

public enum PhaseType {
    FINNISH("Finlandaise"),
    KNOCKOUT("Éliminatoire"),
    POOL("Poule"),
    SIMPLEGAME("Partie simple"),
    SWISSPOOL("Poule suisse");

    private final String displayValue;

    private PhaseType(String displayValue) {
        this.displayValue = displayValue;
    }

    public String getDisplayValue() {
        return displayValue;
    }
}
