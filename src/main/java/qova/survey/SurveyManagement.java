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
    private String questions = "lul";
    private Long id = 2L;
    

    public SurveyManagement(SurveyRepository surveys){
        this.surveys = Objects.requireNonNull(surveys);
    }

    public void createSurvey(){
        Survey n = new Survey();
        n.setQuestions(questions);
        surveys.save(n);
    }

    public void deleteSurvey(){
        surveys.deleteById(id);
        id++;
    }
   
}