package qova.forms;

import javax.validation.constraints.NotNull;

public class DuplicateCourseForm {
    //Taken as String from model (in the form: "SoSe xx" | "WiSe xx/yy")
    @NotNull private String semesterString;

    public DuplicateCourseForm(String semesterString){
        this.semesterString = semesterString;
    }

    public String getSemesterString(){
        return this.semesterString;
    }
}