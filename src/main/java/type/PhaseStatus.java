package type;

public enum PhaseStatus {
        NOTSTARTED("Pas commencé"),
        INPROGRESS("En cours"),
        ENDED("Fini");

        private final String displayValue;

        private PhaseStatus(String displayValue) {
                this.displayValue = displayValue;
        }

        public String getDisplayValue() {
                return displayValue;
        }

}
