package qova.course;

import java.time.LocalDateTime;

import javax.validation.constraints.NotNull;

public class SurveyForm {

    @NotNull
    private int Q1;

    @NotNull
    private int Q2;

    @NotNull
    private int Q3;

    @NotNull
    private int Q4;

    @NotNull
    private int Q5;

    @NotNull
    private int optQ1;

    @NotNull
    private int optQ2;

    @NotNull
    private int optQ3;

    private String textResponse;

    LocalDateTime submissionDateTime;

    
    public SurveyForm(int Q1, int Q2, int Q3, int Q4, int Q5, int optQ1, int optQ2, int optQ3, String textResponse, LocalDateTime submissionDateTime){
        this.Q1 = Q1;
        this.Q1 = Q2;
        this.Q1 = Q3;
        this.Q1 = Q4;
        this.Q1 = Q5;
        this.optQ1 = optQ1;
        this.optQ2 = optQ2;
        this.optQ3 = optQ3;
        this.textResponse = textResponse;
        this.submissionDateTime = submissionDateTime;
    }

    public int getQ1(){
        return this.Q1;
    }

    public int getQ2(){
        return this.Q2;
    }

    public int getQ3(){
        return this.Q3;
    }

    public int getQ4(){
        return this.Q4;
    }

    public int getQ5(){
        return this.Q5;
    }

    public int getOptQ1(){
        return this.optQ1;
    }

    public int getOptQ2(){
        return this.optQ2;
    }

    public int getOptQ3(){
        return this.optQ3;
    }

    public String getTextResponse(){
        return this.textResponse;
    }

    public LocalDateTime getSubmissionDateTime(){
        return this.submissionDateTime;
    }


}
