package qova.course;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import qova.survey.Survey;

public class CourseForm {

    @NotEmpty
    private String name;

    @NotNull
    private CourseType type;

    private String[] questions = new String[20];
    private Survey survey = new Survey(questions);

    @NotNull
    private int classTotal;

    @NotNull
    private int semester;

    @NotNull
    private CourseFaculty faculty;



    public CourseForm(String name, CourseType type, Survey survey, int classTotal, int semester, CourseFaculty faculty){
        this.name = name;
        this.type = type;
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

    public Survey getSurvey(){
        return this.survey;
    }

    public int getClassTotal(){
        return this.classTotal;
    }

    public int getSemester(){
        return this.semester;
    }

    public CourseFaculty getFaculty(){
        return this.faculty;
    }

}