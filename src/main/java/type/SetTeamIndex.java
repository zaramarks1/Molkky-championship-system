package type;

public enum SetTeamIndex {
    TEAM1("team1"),
    TEAM2("team2"),
    ORGA("orga");
    private final String displayValue;

    private SetTeamIndex(String displayValue) {
        this.displayValue = displayValue;
    }

    public String getDisplayValue() {
        return displayValue;
    }

}
