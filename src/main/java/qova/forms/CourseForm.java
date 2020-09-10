package qova.forms;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

import qova.enums.CourseFaculty;


public class CourseForm {

    //name
    private String name;

    //Existance of each CourseInstance (CourseType)
    @NotNull private Boolean lectureExists;
    @NotNull private Boolean tutorialExists;
    @NotNull private Boolean seminarExists;
    @NotNull private Boolean practicalExists;

    //Total Number of tutorial/seminar/practical !!GROUPS!!
    private Integer groupAmountLecture;      //Assumed to always be one, but in case of future change of functionality, this is included for the sake of completeness   
    private Integer groupAmountTutorial;
    private Integer groupAmountSeminar;
    private Integer groupAmountPractical;

    //Total Number of times a tutorial/seminar/practical !!TAKES PLACE!!
    private Integer instanceAmountLecture;
    private Integer instanceAmountTutorial;
    private Integer instanceAmountSeminar;
    private Integer instanceAmountPractical;

    //'Intended' Semester of Students partaking in the given Subject
    private Integer semesterOfStudents;

    @NotNull private CourseFaculty faculty;

    //Taken as String from model (in the form: "SoSe xx" | "WiSe xx/yy")
    @NotNull private String semesterString;


    //Constructor
    public CourseForm(String name, Boolean lectureExists, Boolean tutorialExists, Boolean seminarExists, Boolean practicalExists, Integer groupAmountLecture, Integer groupAmountTutorial, Integer groupAmountSeminar, Integer groupAmountPractical, Integer instanceAmountLecture, Integer instanceAmountTutorial, Integer instanceAmountSeminar, Integer instanceAmountPractical, Integer semesterOfStudents, CourseFaculty faculty, String semesterString){
        this.name = name;
        this.lectureExists = lectureExists;
        this.tutorialExists = tutorialExists;
        this.seminarExists = seminarExists;
        this.practicalExists = practicalExists;
        this.groupAmountLecture = groupAmountLecture;
        this.groupAmountTutorial = groupAmountTutorial;
        this.groupAmountSeminar = groupAmountSeminar;
        this.groupAmountPractical = groupAmountPractical;
        this.instanceAmountLecture = instanceAmountLecture;
        this.instanceAmountTutorial = instanceAmountTutorial;
        this.instanceAmountSeminar = instanceAmountSeminar;
        this.instanceAmountPractical = instanceAmountPractical;
        this.semesterOfStudents = semesterOfStudents;
        this.faculty=faculty;
        this.semesterString = semesterString;
    }


    //GETTERS

    public String getName(){
        return this.name;
    }


    public Boolean getLectureExists(){
        return this.lectureExists;
    }
    public Boolean getTutorialExists(){
        return this.tutorialExists;
    }
    public Boolean getSeminarExists(){
        return this.seminarExists;
    }
    public Boolean getPracticalExists(){
        return this.practicalExists;
    }


    public Integer getGroupAmountLecture(){
        return this.groupAmountLecture;
    }
    public Integer getGroupAmountTutorial(){
        return this.groupAmountTutorial;
    }
    public Integer getGroupAmountSeminar(){
        return this.groupAmountSeminar;
    }
    public Integer getGroupAmountPractical(){
        return this.groupAmountPractical;
    }


    public Integer getInstanceAmountLecture(){
        return this.instanceAmountLecture;
    }
    public Integer getInstanceAmountTutorial(){
        return this.instanceAmountTutorial;
    }
    public Integer getInstanceAmountSeminar(){
        return this.instanceAmountSeminar;
    }
    public Integer getInstanceAmountPractical(){
        return this.instanceAmountPractical;
    }

    
    public Integer getSemesterOfStudents(){
        return this.semesterOfStudents;
    }

    public CourseFaculty getFaculty(){
        return this.faculty;
    }

    public String getSemesterString(){
        return this.semesterString;
    }

}