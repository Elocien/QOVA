package qova.objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;


import qova.enums.ResponseType;


@Entity
public class BinaryResponse {

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
    private Integer yesTotal = 0;
    private Integer noTotal = 0;

    private ResponseType responseType = ResponseType.BINARY_ANSWER;

    //Needed for JPA puposes
    @SuppressWarnings("unused")
	protected BinaryResponse(){
    }

    /**
     * Constructor
     * 
     * @param question The question set by the user in the quesitoneditor
     */
    public BinaryResponse(SurveyResponse response, String question, Integer surveyPosition){
        this.question = question;
        this.surveyResponse = response;
        this.surveyPosition = surveyPosition;
    }



    public Long getId(){
        return this.id;
    }
    
    public String getQuestion() {
        return this.question;
    }

    public String getYesTotalString() {
        return String.valueOf(this.yesTotal);
    }

    public String getNoTotalString() {
        return String.valueOf(this.noTotal);
    }

    public Integer getYesTotal() {
        return this.yesTotal;
    }

    public Integer getNoTotal() {
        return this.noTotal;
    }

    public Integer getTotal() {
        return this.noTotal;
    }

    public ResponseType getType(){
        return this.responseType;
    }

    public void incrementYes(){
        this.yesTotal = yesTotal + 1;
    }

    public void incrementNo(){
        this.noTotal = noTotal + 1;
    }

    public Integer getSurveyPosition(){
        return this.surveyPosition;
    }

}