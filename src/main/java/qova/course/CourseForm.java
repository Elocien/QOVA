package qova.course;

import java.time.LocalDate;

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
    private int classTotalSeminar;

    @NotNull
    private int classTotalTutorial;

    @NotNull
    private int semester;

    @NotNull
    private CourseFaculty faculty;

    @NotNull
    private LocalDate semesterDate;



    public CourseForm(String name, Boolean lectureExists, Boolean tutorialExists, Boolean seminarExists, String lectureSurvey, String tutorialSurvey, String seminarSurvey, int classTotalSeminar, int classTotaltutorial, int semester, CourseFaculty faculty, LocalDate semesterDate){
        this.name = name;
        this.lectureExists = lectureExists;
        this.tutorialExists = tutorialExists;
        this.seminarExists = seminarExists;
        this.lectureSurvey = lectureSurvey;
        this.tutorialSurvey = tutorialSurvey;
        this.seminarSurvey = seminarSurvey;
        this.classTotalSeminar = classTotalSeminar;
        this.classTotalTutorial = classTotaltutorial;
        this.semester = semester;
        this.faculty=faculty;
        this.semesterDate = semesterDate;
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

    public int getClassTotalSeminar(){
        return this.classTotalSeminar;
    }

    public int getClassTotalTutorial(){
        return this.classTotalTutorial;
    }

    public int getSemester(){
        return this.semester;
    }

    public CourseFaculty getFaculty(){
        return this.faculty;
    }

    public LocalDate getSemesterDate(){
        return this.semesterDate;
    }

}