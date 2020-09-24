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
public class TextResponse {

    //-----------------------------------------------------------------------
    @Id @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;

    //container for the question set
    @Column(length = 1024) 
    private String question;

    @ManyToOne
    private SurveyResponse surveyResponse;

    //Position in the survey
    private Integer surveyPosition;
   
    //Container for response
    @ElementCollection
    private List<String> responses = new ArrayList<>();

    private static final ResponseType responseType = ResponseType.TEXT_RESPONSE;

    //Needed for JPA puposes
    @SuppressWarnings("unused")
	protected TextResponse(){
    }

    public TextResponse(SurveyResponse response, String question, Integer surveyPosition){
        this.surveyResponse = response;
        this.question = question;
        this.surveyPosition = surveyPosition;
    }
    
    
    

    public String getQuestion() {
        return this.question;
    }

    public List<String> getResponses() {
        return this.responses;
    }

    public void addTextSubmission(String resp){
        this.responses.add(resp);
    }

    public ResponseType getType(){
        return responseType;
    }

    public Integer getSurveyPosition(){
        return this.surveyPosition;
    }
    
}

