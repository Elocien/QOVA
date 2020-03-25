package qova.survey;

import java.time.LocalDateTime;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

@Entity
public class Survey {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private long id;

    private int Q1;

    private int Q2;
    private int Q3;
    private int Q4;
    private int Q5;
    private int optQ1;
    private int optQ2;
    private int optQ3;
    private String textResponse;
    LocalDateTime submissionDateTime;

    //Needed for JPA puposes
    @SuppressWarnings("unused")
	private Survey() {
    }
    
    public Survey(int Q1, int Q2, int Q3, int Q4, int Q5, int optQ1, int optQ2, int optQ3, String textResponse, LocalDateTime submissionDateTime){
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

    public void setQ1(int Q1){
        this.Q1 = Q1;
    }

    public int getQ2(){
        return this.Q2;
    }

    public void setQ2(int Q2){
        this.Q2 = Q2;
    }

    public int getQ3(){
        return this.Q3;
    }

    public void setQ3(int Q3){
        this.Q3 = Q3;
    }

    public int getQ4(){
        return this.Q4;
    }

    public void setQ4(int Q4){
        this.Q4 = Q4;
    }

    public int getQ5(){
        return this.Q5;
    }

    public void setQ5(int Q5){
        this.Q5 = Q5;
    }

    public int getOptQ1(){
        return this.optQ1;
    }

    public void setOptQ1(int optQ1){
        this.optQ1 = optQ1;
    }

    public int getOptQ2(){
        return this.optQ2;
    }

    public void setOptQ2(int optQ2){
        this.optQ2 = optQ2;
    }

    public int getOptQ3(){
        return this.optQ3;
    }

    public void setOptQ3(int optQ3){
        this.optQ3 = optQ3;
    }

    public String getTextResponse(){
        return this.textResponse;
    }

    public void setTextResponse(String text){
        this.textResponse = text;
    }

    public LocalDateTime getSubmissionDateTime(){
        return this.submissionDateTime;
    }

    public void setSubmissionDateTime(LocalDateTime dateTime){
        this.submissionDateTime = dateTime;
    }




}


