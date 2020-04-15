package qova.course;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;


public class CourseForm {

    @NotEmpty
    private String name;

    @NotNull
    private CourseType type;

    private String[] survey = new String[100];
    
    @NotNull
    private int classTotal;

    @NotNull
    private int semester;

    @NotNull
    private CourseFaculty faculty;



    public CourseForm(String name, CourseType type, String[] survey, int classTotal, int semester, CourseFaculty faculty){
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

    public String[] getSurvey(){
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