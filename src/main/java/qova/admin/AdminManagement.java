package qova.admin;

import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import qova.forms.SurveyForm;


@Service
@Transactional
public class AdminManagement {
    
    private final DefaultSurveyRepository repo;

    /**
     * The Constructor checks if the {@linkplain DefaultSurvey} is present and persisted; if it is not, a new one is created;
     * 
     * @param repo The {@linkplain DefaultSurveyRepository}. Contains the single instance of the defaultSurvey
     */
    @Autowired
    public AdminManagement(DefaultSurveyRepository repo) {
            
        Optional<DefaultSurvey> defaultSurvey = repo.checkForDefaultSurvey();
        if(defaultSurvey.isEmpty()){
            repo.save(new DefaultSurvey(1337L, "[]"));
        }
        
        
        this.repo = Objects.requireNonNull(repo);
    }









    //Get Default survey from Repo
    public String getDefaultSurvey() {
        try{
            return repo.findSpecialInstance().getDefaultSurveyJson();
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
        getDefaultSurveyObject().setDefaultSurveyJson(form.getQuestionnairejson());
    }


}



