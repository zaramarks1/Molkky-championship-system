package type;

public enum TournamentStatus {
    AVAILABLE("Disponible"),
    CLOSED("Fermé"),
    INPROGRESS("En cours"),
    ENDED("Fini");

    private final String displayValue;

    private TournamentStatus(String displayValue) {
        this.displayValue = displayValue;
    }

    public String getDisplayValue() {
        return displayValue;
    }
}