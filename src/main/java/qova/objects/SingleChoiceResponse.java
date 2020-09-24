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
public class SingleChoiceResponse {


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
    @ElementCollection private List<String> singleChoiceOptions;

    //Array of the user response
    @ElementCollection private List<Integer> singleChoiceAnswers;

    private static final ResponseType responseType = ResponseType.SINGLE_CHOICE;

    //Needed for JPA puposes
    @SuppressWarnings("unused")
	protected SingleChoiceResponse(){
    }

    public SingleChoiceResponse(SurveyResponse response, String question, Integer surveyPosition,List<String> singleChoiceOptions){
        this.surveyResponse = response;
        this.question = question;
        this.singleChoiceOptions = singleChoiceOptions;
        this.singleChoiceAnswers = new ArrayList<>(this.singleChoiceOptions.size());
        this.surveyPosition = surveyPosition;
        
        //Populate the array
        for(String s : singleChoiceOptions){
            singleChoiceAnswers.add(0);
        }
    }
    
    
    public String getQuestion() {
        return this.question;
    }

    public List<String> getSingleChoiceOptions(){
        return this.singleChoiceOptions;
    }

    public List<Integer> getSingleChoiceAnswers(){
        return this.singleChoiceAnswers;
    }

    public void incrementTotal(Integer pos){
        Integer totalAtPosition = singleChoiceAnswers.get(pos) + 1;
        singleChoiceAnswers.set(pos, totalAtPosition);
    }

    public Integer getNumberOfOptions(){
        return this.singleChoiceOptions.size();
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


