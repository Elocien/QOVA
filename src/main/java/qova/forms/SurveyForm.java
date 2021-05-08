package qova.forms;


import org.springframework.ui.Model;

import java.util.UUID;

/**
 * The form used in {@linkplain qova.logic.CourseController#questioneditorSubmit(Model, SurveyForm, String, UUID)} and
 * {@linkplain qova.logic.CourseController#questioneditorpreview(Model, SurveyForm, String, UUID)} to retrieve the survey
 * which the user has created in the questioneditor. The survey is sent as a JSON string to the backend.
 */
public class SurveyForm {

    /**
     * The string containing the JSON sent by the questioneditor
     */
    private String questionnairejson;

    /**
     * Constructor
     * @param questionnairejson containing the JSON string
     */
    public SurveyForm(String questionnairejson){
        this.questionnairejson = questionnairejson;
    }

    /**
     * Getter
     * @return the questionnaire in JSON string form
     */
    public String getQuestionnaireJson() {
        return this.questionnairejson;
    }
}
