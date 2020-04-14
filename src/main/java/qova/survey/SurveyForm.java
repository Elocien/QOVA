package qova.survey;


public class SurveyForm {

    private String questions[];

    public SurveyForm(String[] questions){
        this.questions = questions;
    }

    public String[] getQuestions(){
        return this.questions;
    }

}
