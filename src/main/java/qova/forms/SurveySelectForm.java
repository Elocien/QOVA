package qova.forms;

public class SurveySelectForm {
    
    private String group;
    private String instance;

    public SurveySelectForm(String group, String instance){
        this.group = group;
        this.instance = instance;
    }

    public String getGroup(){
        return this.group;
    }
    public String getInstance(){
        return this.instance;
    }
}