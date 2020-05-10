package qova.course;


import java.time.LocalDate;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;


//id-generator imports
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;
import qova.IdGenerator;


@Entity
public class Course {

 


    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "custom_gen")
    @GenericGenerator(
        name = "custom_gen", 
        strategy = "qova.IdGenerator", 
        parameters = {
            @Parameter(name = IdGenerator.INCREMENT_PARAM, value = "113"),
            @Parameter(name = IdGenerator.VALUE_PREFIX_PARAMETER, value = "c"),
            @Parameter(name = IdGenerator.NUMBER_FORMAT_PARAMETER, value = "%015d") })
    private String id;

    private String name;
    
    private Boolean lectureExists;

    private Boolean tutorialExists;

    private Boolean seminarExists;

    private String lectureSurvey;

    private String tutorialSurvey;

    private String seminarSurvey;

    private int classTotalTutorial;

    private int classTotalSeminar;

    //The number representing the semester this course is taken by students
    private int semesterOfStudents;

    private CourseFaculty faculty;

    //The date at which indicates to which semester the course belongs to 
    private LocalDate courseInstance;  


    //Needed for JPA purposes
    @SuppressWarnings("unused")
	protected Course() {
    }
    

    
    /**
     * Instance of a Course (meaning a Subject [DE Lehrveranstaltung])
     * 
     * @param name              Name of the course
     * @param type              Enum which is either: Lecture, Tutorial or Seminar
     * @param survey            An {@linkplain Survey} Object containg the questions
     * @param classTotal        How many different tutorial-/seminar groups exist
     * @param semester          What semester is the Subject taken by students
     * @param faculty           Enum defining which faculty the subject belongs to 
     */
    public Course(String name, Boolean lectureExists, Boolean tutorialExists, Boolean seminarExists, String lectureSurvey, String tutorialSurvey, String seminarSurvey, int classTotalTutorial, int classTotalSeminar, int semesterOfStundets, CourseFaculty faculty, LocalDate courseInastance){
        this.name = name;
        this.lectureExists = lectureExists;
        this.tutorialExists = tutorialExists;
        this.seminarExists = seminarExists;
        this.lectureSurvey = lectureSurvey;
        this.tutorialSurvey = tutorialSurvey;
        this.seminarSurvey = seminarSurvey;
        this.classTotalTutorial = classTotalTutorial;
        this.classTotalSeminar = classTotalSeminar;
        this.semesterOfStudents = semesterOfStundets;
        this.faculty=faculty;
        this.courseInstance = courseInastance;
    }

    public String getId(){
        return this.id;
    }

    public String getName(){
        return this.name;
    }

    public void setName(String name){
        this.name = name;
    }

    public Boolean getLectureExists(){
        return this.lectureExists;
    }

    public void setLectureExists(Boolean exists){
        this.lectureExists = exists;
    }

    public Boolean getTutorialExists(){
        return this.tutorialExists;
    }

    public void setTutorialExists(Boolean exists){
        this.tutorialExists = exists;
    }

    public Boolean getSeminarExists(){
        return this.seminarExists;
    }

    public void setSeminarExists(Boolean exists){
        this.seminarExists = exists;
    }

    public String getLectureSurvey(){
        return this.lectureSurvey;
    }

    public void setLectureSurvey(String survey){
        this.lectureSurvey = survey;
    }

    public String getTutorialSurvey(){
        return this.tutorialSurvey;
    }

    public void setTutorialSurvey(String survey){
        this.tutorialSurvey = survey;
    }

    public String getSeminarSurvey(){
        return this.seminarSurvey;
    }

    public void setSeminarSurvey(String survey){
        this.seminarSurvey = survey;
    }

    public int getClassTotalTutorial(){
        return this.classTotalTutorial;
    }

    public void setClassTotalTutorial(int classTotal){
        this.classTotalTutorial = classTotal;
    }

    public int getClassTotalSeminar(){
        return this.classTotalSeminar;
    }

    public void setClassTotalSeminar(int classTotal){
        this.classTotalSeminar = classTotal;
    }

    public int getSemesterOfStudents(){
        return this.semesterOfStudents;
    }

    public void setSemesterOfStudents(int semester){
        this.semesterOfStudents = semester;
    }

    public CourseFaculty getFaculty(){
        return this.faculty;
    }

    public void setFaculty(CourseFaculty faculty){
        this.faculty = faculty;
    }

    public LocalDate getCourseInstance(){
        return this.courseInstance;
    }

    public void setCourseInstance(LocalDate date){
        this.courseInstance = date;
    }
}

