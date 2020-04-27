package qova.course;


import lombok.Data;


public class Form {
    private String questionnairejson;

    public Form(String questionnairejson){
        this.questionnairejson = questionnairejson;
    }

    public String getQuestionnairejson() {
        return questionnairejson;
    }
}
