package qova.course;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import qova.survey.Survey;

public class CourseForm {

    @NotEmpty
    private String name;

    @NotNull
    private CourseType type;

    //@NotNull
    //private ___ qrcode

    @NotNull
    private Survey survey;

    @NotNull
    private int classTotal;

    @NotNull
    private int semester;

    @NotNull
    private CourseFaculty faculty;



    public CourseForm(String name, CourseType type, Survey survey, int classTotal, int semester, CourseFaculty faculty){
        this.name = name;
        this.type = type;
        //this.qrcode = qrcode
        this.survey = survey;
        this.classTotal = classTotal;
        this.semester = semester;
        this.faculty=faculty;
    }

    public String getName(){
        return this.name;
    }

    public CourseType getType(){
        return this.type;
    }

    // public ___ getQrcode(){
    //    return this.qrcode
    //}

    public Survey getSurvey(){
        return this.survey;
    }

    public int getAmount(){
        return this.classTotal;
    }

    public int getSemester(){
        return this.semester;
    }

    public CourseFaculty getFaculty(){
        return this.faculty;
    }

}