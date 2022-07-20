package infrastructure;

public class SaltServiceResponse {
    private int totalSteps;
    private int completedSteps;
    private String error;
    private String message;

    public SaltServiceResponse(int totalSteps, int completedSteps) {
        setTotalSteps(totalSteps);
        setCompletedSteps(completedSteps);
    }

    public void setTotalSteps(int totalSteps) {
        this.totalSteps = totalSteps;
    }

    public int getTotalSteps() {
        return this.totalSteps;
    }

    public void setCompletedSteps(int completedSteps) {
        this.completedSteps = completedSteps;
    }

    public int getCompletedSteps() {
        return this.completedSteps;
    }

    public void setError(String error) {
        this.error = error;
    }

    public String getError() {
        return this.error;
    }
    public void setMessage(String message) {
        this.message = message;
    }

    public String getMessage() {
        return this.message;
    }
}


