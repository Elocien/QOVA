package qova.admin;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;

@Entity
public class DefaultSurvey {

    //Attributes
    @Id @GeneratedValue(strategy = GenerationType.AUTO) 
    private Long id;
    @Lob
    private String defaultSurveyJson;


    //Needed for JPA purposes
    @SuppressWarnings("unused")
	protected DefaultSurvey() {
    }


    //Constructor
    public DefaultSurvey(String surveyJson){
        this.defaultSurveyJson = surveyJson;
    }


    //Getters and Setters
    public Long getId(){
        return this.id;
    }

    public void setDefaultSurvey(String surveyJson){
        this.defaultSurveyJson = surveyJson;
    }

    public String getDefaultSurvey(){
        return this.defaultSurveyJson;
    }
}