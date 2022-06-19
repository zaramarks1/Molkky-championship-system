package type;

public enum UserRole {
    ADM("Organisateur"),
    STAFF("Staff"),
    PLAYER("Joueur");

    private final String displayValue;

    private UserRole(String displayValue) {
        this.displayValue = displayValue;
    }

    public String getDisplayValue() {
        return displayValue;
    }
}