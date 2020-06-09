package qova.course;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;


public class CourseForm {

    private String name;

    private Boolean lectureExists;

    private Boolean tutorialExists;

    private Boolean seminarExists;

    private String lectureSurvey;

    private String tutorialSurvey;

    private String seminarSurvey;
    
    private Integer classTotalSeminar;

    private Integer classTotalTutorial;

    private Integer semesterOfStudents;

    private CourseFaculty faculty;

    //Taken as String from model
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