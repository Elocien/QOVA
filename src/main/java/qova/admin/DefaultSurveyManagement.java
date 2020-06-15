package qova.admin;

import java.util.ArrayList;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


@Service
@Transactional
public class DefaultSurveyManagement {
    
    private final DefaultSurveyRepository repo;

    @Autowired
    public DefaultSurveyManagement(DefaultSurveyRepository repo) {
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
    public DefaultSurvey getDefaultSurvey(){
        DefaultSurvey defaultSurvey;
        for(DefaultSurvey survey : repo.findAll())
            defaultSurvey = survey;

        return defaultSurvey;
    }

}



