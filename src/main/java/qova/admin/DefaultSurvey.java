package qova.admin;

import java.util.UUID;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Lob;

@Entity
public class DefaultSurvey {

    //Attributes
    @Id 
    private UUID id;
    @Lob
    private String defaultSurveyJson;


    //Needed for JPA purposes
    @SuppressWarnings("unused")
	protected DefaultSurvey() {
    }


    //Constructor
    public DefaultSurvey(UUID id, String surveyJson){
        this.id = id;
        this.defaultSurveyJson = surveyJson;
    }


    //Getters and Setters
    public UUID getId(){
        return this.id;
    }

    public void setDefaultSurvey(String surveyJson){
        this.defaultSurveyJson = surveyJson;
    }

    public String getDefaultSurvey(){
        return this.defaultSurveyJson;
    }
}