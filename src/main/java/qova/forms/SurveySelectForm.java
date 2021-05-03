package qova.forms;

import org.springframework.ui.Model;
import qova.objects.CourseInstance;

import java.util.UUID;

/**
 * The form used when a user retrieves the results for a particular {@linkplain qova.objects.Course}, made use of in
 * the methods: {@linkplain qova.logic.ResponseController#selectSurvey(Model, SurveySelectForm, String, UUID, String)},
 * {@linkplain qova.logic.ResponseController#selectSurveySubmission(Model, SurveySelectForm, String, String, UUID)}.
 */
public class SurveySelectForm {

    /**
     * The {@linkplain CourseInstance#getGroupAmount()}
     */
    private String group;
    /**
     * The {@linkplain CourseInstance#getInstanceAmount()}
     */
    private String instance;

    /**
     * Constructor
     * @param group Group Number
     * @param instance Instance Number
     */
    public SurveySelectForm(String group, String instance){
        this.group = group;
        this.instance = instance;
    }

    /**
     * Getter
     * @return Group Number
     */
    public String getGroup(){
        return this.group;
    }

    /**
     * Getter
     * @return Instance Number
     */
    public String getInstance(){
        return this.instance;
    }
}