package qova.admin;

import java.util.EnumMap;
import java.util.List;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import qova.enums.CourseType;
import qova.forms.SurveyForm;

import qova.objects.Course;

@Service
@Transactional
public class AdminManagement {

    private final DefaultSurveyRepository defaultSurveyRepository;

    /**
     * The Constructor checks if the {@linkplain DefaultSurvey} is present and
     * persisted; if it is not, a new one is created;
     * 
     * @param repo The {@linkplain DefaultSurveyRepository}. Contains the single
     *             instance of the defaultSurvey.
     */
    @Autowired
    public AdminManagement(DefaultSurveyRepository repo) {
        this.defaultSurveyRepository = Objects.requireNonNull(repo);
    }

    public String concatenateDefaultSurveyToSurveyString(String surveyJson, CourseType type) {
        String defaultSurvey = getDefaultSurvey(type);

        return (defaultSurvey.substring(0, defaultSurvey.length() - 1) + "," + surveyJson.substring(1));
    }

    // Get Default survey from Repo
    public String getDefaultSurvey(CourseType type) {
        return defaultSurveyRepository.findDefaultSurveyForType(type).getDefaultSurveyJson();
    }

    // Get the DefaultSurvey Object from the repo
    public DefaultSurvey getDefaultSurveyObject(CourseType type) {
        return defaultSurveyRepository.findDefaultSurveyForType(type);
    }

    // Submission of new default survey
    public void updateDefaultSurvey(SurveyForm form, CourseType type) {
        getDefaultSurveyObject(type).setDefaultSurveyJson(form.getQuestionnaireJson());
    }

    // Get Map containing the default survey for each CourseType
    public EnumMap getDefaultSurveyMap(){

        EnumMap<CourseType, DefaultSurvey> defaultSurveyMap = new EnumMap<>(CourseType.class);
        for(CourseType courseType : CourseType.values()){
            defaultSurveyMap.put(courseType, getDefaultSurveyObject(courseType));
        }

        return defaultSurveyMap;
    }

    //
    public Boolean setCourseOwner(Course course, String userId){
        if (userId.length() == 110){
            if(userId.contains("https://idp.tu-dresden.de/idp/shibboleth!https://qova.med.tu-dresden.de/shibboleth!")){
                course.setOwnerId(userId);
                return true;
            }
        }
        return false;
    }

}
