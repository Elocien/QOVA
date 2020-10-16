package qova.forms;



public class SurveyForm {
    
    private String questionnairejson;

    public SurveyForm(String questionnairejson){
        this.questionnairejson = questionnairejson;
    }

    public String getQuestionnaireJson() {
        return this.questionnairejson;
    }
}
