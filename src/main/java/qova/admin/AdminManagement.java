package qova.admin;

import java.util.ArrayList;
import java.util.Objects;
import java.util.Optional;

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
        this.repo = Objects.requireNonNull(repo);


        //Bunch of shenanigans to check wether a default survey exists (There has to be some better way to do this...)

        ArrayList<DefaultSurvey> surveyList = new ArrayList<DefaultSurvey>();
        Iterable<DefaultSurvey> iterable = repo.findAll();
        
        for(DefaultSurvey survey : iterable)
            surveyList.add(survey);


        if(surveyList.isEmpty()){
            repo.save(new DefaultSurvey("[]"));
        }
        //delete any excessive surveys
        if (surveyList.size() > 1){
            for(int i = 1; i < surveyList.size(); i++){
                repo.delete(surveyList.get(i));
            }
        } 
    }









    //Get Default survey from Repo
    public String getDefaultSurvey() throws Exception{
        ArrayList<DefaultSurvey> surveyList = new ArrayList<DefaultSurvey>();
        Iterable<DefaultSurvey> iterable = repo.findAll();

        for(DefaultSurvey survey : iterable)
            surveyList.add(survey);


        //default case, return the default survey
        if(  !(surveyList.isEmpty())   &&   !(surveyList.size() > 1)  ){
            return surveyList.get(0).getDefaultSurvey();
        }

        //Create default survey, if one does not exist anymore (not sure how this would happen, but better to catch all cases)
        else if(surveyList.isEmpty()){
            DefaultSurvey surv = new DefaultSurvey("[]");
            repo.save(surv);
            return surv.getDefaultSurvey();
        }

        //delete any excessive surveys
        else if (surveyList.size() > 1){
            for(int i = 1; i < surveyList.size(); i++){
                repo.delete(surveyList.get(i));
            }
            return surveyList.get(0).getDefaultSurvey();
            
        } 

        //
        else{
            throw new Exception("Default survey implementation has been broken, please refer to developers");
        }  
    }


    private DefaultSurvey getDefaultSurveyObject(){

        //There should only be one Default Survey, with id = 1; Using findAll() returns Iterable, so this is the lazy approach
        Long i = 1L;

        //retrive survey from repo
        Optional<DefaultSurvey> defSurvey = repo.findById(i);

        if(!defSurvey.isPresent()){
            DefaultSurvey surv = new DefaultSurvey("[]");
            repo.save(surv);
            return surv;
        }
        else{
            return defSurvey.get();
        }
        
    }






    //Submission of new default survey
    public void updateDefaultSurvey(SurveyForm form) throws Exception {
        getDefaultSurveyObject().setDefaultSurvey(form.getQuestionnairejson());
    }


}



