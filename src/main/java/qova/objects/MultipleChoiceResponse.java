package qova.objects;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;

import qova.enums.ResponseType;

@Entity
public class MultipleChoiceResponse {


    //-----------------------------------------------------------------------
    @Id @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;

    //container for the question set
    @Column(length = 1024) 
    private String question;

    @ManyToOne
    private SurveyResponse surveyResponse;

    private Integer surveyPosition;

    //Array of the different options presented to the user
    @ElementCollection private List<String> multipleChoiceOptions;

    //Array of the user response
    @ElementCollection private List<Integer> multipleChoiceAnswers;

    private static final ResponseType responseType = ResponseType.MULTIPLE_CHOICE;

    //Needed for JPA puposes
    @SuppressWarnings("unused")
	protected MultipleChoiceResponse(){
    }

    public MultipleChoiceResponse(SurveyResponse response, String question, Integer surveyPosition,List<String> multipleChoiceOptions){
        this.surveyResponse = response;
        this.question = question;
        this.surveyPosition = surveyPosition;
        this.multipleChoiceOptions = multipleChoiceOptions;
        this.multipleChoiceAnswers = new ArrayList<>(this.multipleChoiceOptions.size());
        for(String s: multipleChoiceOptions){
            multipleChoiceAnswers.add(0);
        }
    }
    
    
    public String getQuestion() {
        return this.question;
    }

    public List<String> getMultipleChoiceOptions(){
        return this.multipleChoiceOptions;
    }

    public List<Integer> getMultipleChoiceAnswers(){
        return this.multipleChoiceAnswers;
    }

    public void incrementTotals(List<Integer> totals){
        for(Integer i : totals){
            multipleChoiceAnswers.set(i, multipleChoiceAnswers.get(i) + 1);
        }
    }

    public Integer getNumberOfOptions(){
        return this.multipleChoiceOptions.size();
    }

    public ResponseType getType(){
        return responseType;
    }

    public Integer getSurveyPosition(){
        return this.surveyPosition;
    }

    public SurveyResponse getSurveyResponse(){
        return this.surveyResponse;
    }

}

