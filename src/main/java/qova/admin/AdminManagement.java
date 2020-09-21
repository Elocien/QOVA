package qova.admin;

import java.util.Objects;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import qova.enums.CourseType;
import qova.forms.SurveyForm;


@Service
@Transactional
public class AdminManagement {
    
    private final DefaultSurveyRepository repo;

    private String defaultSurveyString = "[{\"type\":\"YesNo\",\"question\":\"Scheint die Sonne\"},{\"type\":\"MultipleChoice\",\"question\":\"Wie geht es dir?\",\"answers\":[\"Okay\",\"Gut\",\"Schlecht\"]},{\"type\":\"SingleChoice\",\"question\":\"Ja Neine Vielleicht\",\"answers\":[\"Ja\",\"Nein\",\"Vielleicht\"]},{\"type\":\"FreeText\",\"question\":\"STONKS\"}]";

    /**
     * The Constructor checks if the {@linkplain DefaultSurvey} is present and persisted; if it is not, a new one is created;
     * 
     * @param repo The {@linkplain DefaultSurveyRepository}. Contains the single instance of the defaultSurvey.
     */
    @Autowired
    public AdminManagement(DefaultSurveyRepository repo) {
        this.repo = Objects.requireNonNull(repo);
    }



    public String concatenateDefaultSurveyToSurveyString(String surveyJson, CourseType type){
        String defaultSurvey = getDefaultSurvey(type);

        return (defaultSurvey.substring(0, defaultSurvey.length()-1) + "," + surveyJson.substring(1));
    }







    //Get Default survey from Repo
    public String getDefaultSurvey(CourseType type) {
        return repo.findDefaultSurveyForType(type).getDefaultSurveyJson();
    }


    //Get the DefaultSurvey Object from the repo
    private DefaultSurvey getDefaultSurveyObject(CourseType type){
        return repo.findDefaultSurveyForType(type);
    }



    //Submission of new default survey
    public void updateDefaultSurvey(SurveyForm form, CourseType type) {
        getDefaultSurveyObject(type).setDefaultSurveyJson(form.getQuestionnairejson());
    }

}



