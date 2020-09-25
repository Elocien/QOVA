package qova.forms;

public class SurveySelectForm {
    
    private Integer group;
    private Integer instance;

    public SurveySelectForm(Integer group, Integer instance){
        this.group = group;
        this.instance = instance;
    }

    public Integer getGroup(){
        return this.group;
    }
    public Integer getInstance(){
        return this.instance;
    }
}