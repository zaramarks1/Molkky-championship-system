package type;

public enum UserRole {
    ADM("Administrateur"),
    STAFF("Organisateur"),
    PLAYER("Joueur");

    private final String displayValue;

    private UserRole(String displayValue) {
        this.displayValue = displayValue;
    }

    public String getDisplayValue() {
        return displayValue;
    }
}