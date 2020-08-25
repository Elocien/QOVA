package qova.admin;

import java.util.Objects;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import qova.forms.SurveyForm;


@Service
@Transactional
public class AdminManagement {
    
    private final DefaultSurveyRepository repo;

    @Autowired
    public AdminManagement(DefaultSurveyRepository repo) {
        
        repo.save(new DefaultSurvey(1337L, "[]"));

        //Check if defaultSurvey exists.
        try{
            repo.findSpecialInstance();
        }
        catch(IllegalStateException e){
            e.printStackTrace();
            repo.save(new DefaultSurvey(1337L, "[]"));
        }

        this.repo = Objects.requireNonNull(repo);
    }









    //Get Default survey from Repo
    public String getDefaultSurvey() throws Exception{
        try{
            return repo.findSpecialInstance().getDefaultSurvey();
        }
        catch(IllegalStateException e){
            DefaultSurvey defaultSurvey = new DefaultSurvey(1337L, "[]");
            repo.save(defaultSurvey);
            return "[]";
        }
    }


    //Get the DefaultSurvey Object from the repo
    private DefaultSurvey getDefaultSurveyObject(){
        try{
            return repo.findSpecialInstance();
        }
        catch(IllegalStateException e){
            DefaultSurvey defaultSurvey = new DefaultSurvey(1337L, "[]");
            repo.save(defaultSurvey);
            return defaultSurvey;
        }
    }






    //Submission of new default survey
    public void updateDefaultSurvey(SurveyForm form) throws Exception {
        getDefaultSurveyObject().setDefaultSurvey(form.getQuestionnairejson());
    }


}



