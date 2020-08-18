package qova.admin;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import qova.course.SurveyForm;


@Service
@Transactional
public class AdminManagement {
    
    private final DefaultSurveyRepository repo;

    @Autowired
    public AdminManagement(DefaultSurveyRepository repo) {
        
        repo.save(new DefaultSurvey(1L, "[]"));
        
        //Check if defaultSurvey exists.
        try{
            repo.findSpecialInstance();
        }
        catch(IllegalStateException e){
            repo.save(new DefaultSurvey(1L, "[]"));
        }

        this.repo = Objects.requireNonNull(repo);
    }









    //Get Default survey from Repo
    public String getDefaultSurvey() throws Exception{
        try{
            return repo.findSpecialInstance().getDefaultSurvey();
        }
        catch(IllegalStateException e){
            DefaultSurvey defaultSurvey = new DefaultSurvey(1L, "[]");
            repo.save(defaultSurvey);
            return "[]";
        }
    }


    private DefaultSurvey getDefaultSurveyObject(){
        try{
            return repo.findSpecialInstance();
        }
        catch(IllegalStateException e){
            DefaultSurvey defaultSurvey = new DefaultSurvey(1L, "[]");
            return defaultSurvey;
        }
    }






    //Submission of new default survey
    public void updateDefaultSurvey(SurveyForm form) throws Exception {
        getDefaultSurveyObject().setDefaultSurvey(form.getQuestionnairejson());
    }


}



