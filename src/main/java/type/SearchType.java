package type;

public enum SearchType {
    TOURNAMENT("Tournoi"),
    TEAM("Équipe"),
    USER("Utilisateur");

    private final String displayValue;

    private SearchType(String displayValue) {
        this.displayValue = displayValue;
    }

    public String getDisplayValue() {
        return displayValue;
    }
}
