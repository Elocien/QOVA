package qova.survey;

import java.util.Objects;



public class SurveyManagement {
    
    private final SurveyRepository surveys;

    private String[] questions;
    

    public SurveyManagement(SurveyRepository surveys){
        this.surveys = Objects.requireNonNull(surveys);
    }

    public Survey createSurvey(){

        

        return surveys.save(new Survey(questions));
    }
   
}