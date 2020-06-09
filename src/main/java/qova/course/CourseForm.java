package qova.course;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;


public class CourseForm {

    @NotEmpty
    private String name;

    @NotNull
    private Boolean lectureExists;

    @NotNull
    private Boolean tutorialExists;

    @NotNull
    private Boolean seminarExists;

    private String lectureSurvey;

    private String tutorialSurvey;

    private String seminarSurvey;
    
    @NotNull
    private Integer classTotalSeminar;

    @NotNull
    private Integer classTotalTutorial;

    @NotNull
    private Integer semesterOfStudents;

    @NotNull
    private CourseFaculty faculty;

    //Taken as String from model
    @NotNull
    private String courseInstanceString;


    public CourseForm(String name, Boolean lectureExists, Boolean tutorialExists, Boolean seminarExists, String lectureSurvey, String tutorialSurvey, String seminarSurvey, Integer classTotalSeminar, Integer classTotalTutorial, Integer semesterOfStudents, CourseFaculty faculty, String courseInstanceString){
        this.name = name;
        this.lectureExists = lectureExists;
        this.tutorialExists = tutorialExists;
        this.seminarExists = seminarExists;
        this.lectureSurvey = lectureSurvey;
        this.tutorialSurvey = tutorialSurvey;
        this.seminarSurvey = seminarSurvey;
        this.classTotalSeminar = classTotalSeminar;
        this.classTotalTutorial = classTotalTutorial;
        this.semesterOfStudents = semesterOfStudents;
        this.faculty=faculty;
        this.courseInstanceString = courseInstanceString;
    }

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

    public String getLectureSurvey(){
        return this.lectureSurvey;
    }
    
    public String getTutorialSurvey(){
        return this.tutorialSurvey;
    }

    public String getSeminarSurvey(){
        return this.seminarSurvey;
    }

    public Integer getClassTotalSeminar(){
        return this.classTotalSeminar;
    }

    public Integer getClassTotalTutorial(){
        return this.classTotalTutorial;
    }

    public Integer getSemesterOfStudents(){
        return this.semesterOfStudents;
    }

    public CourseFaculty getFaculty(){
        return this.faculty;
    }

    public String getCourseInstance(){
        return this.courseInstanceString;
    }

}