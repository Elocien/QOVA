package qova.admin;

import java.util.UUID;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Lob;

@Entity
public class DefaultSurvey {

    //Attributes
    @Id 
    private Long id;
    @Lob
    private String defaultSurveyJson;


    //Needed for JPA purposes
    @SuppressWarnings("unused")
	protected DefaultSurvey() {
    }


    //Constructor
    public DefaultSurvey(Long id, String surveyJson){
        this.id = id;
        this.defaultSurveyJson = surveyJson;
    }


    //Getters and Setters
    public Long getId(){
        return this.id;
    }

    public void setDefaultSurveyJson(String surveyJson){
        this.defaultSurveyJson = surveyJson;
    }

    public String getDefaultSurveyJson(){
        return this.defaultSurveyJson;
    }
}