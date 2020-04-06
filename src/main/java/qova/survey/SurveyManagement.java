package qova.survey;

import org.springframework.stereotype.Controller;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.Objects;


@Service
@Transactional
public class SurveyManagement {
    
    private final SurveyRepository surveys;
    

    //test attributes
    private String questions;
    

    public SurveyManagement(SurveyRepository surveys){
        this.surveys = Objects.requireNonNull(surveys);
    }

    // public Survey createSurvey(){

    //     Survey testSurvey = new Survey(questions);

    //     return surveys.save(new Survey(questions));
    // }
   
}