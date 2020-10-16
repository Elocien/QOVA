package qova.forms;



public class SurveyForm {
    
    private String questionnaireJson;

    public SurveyForm(String questionnairejson){
        this.questionnaireJson = questionnairejson;
    }

    public String getQuestionnaireJson() {
        return questionnaireJson;
    }
}
