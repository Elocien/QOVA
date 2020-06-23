package qova.course;

import java.time.LocalDate;
import java.util.ArrayList;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;

//id-generator imports
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;
import qova.IdGenerator;





//Terminology

//course types
    //meaning either the lecture, tutorial, seminar or practical
    //auf deutsch:       Vorlesung, übung , seminar oder praktika


//




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

    //Name of the course
    private String name;


    //Booleans representing the existence of each course type
    private Boolean lectureExists;
    private Boolean tutorialExists;
    private Boolean seminarExists;
    private Boolean practicalExists;


    //JSON String of the survey
    @Lob private String lectureSurvey;
    @Lob private String tutorialSurvey;
    @Lob private String seminarSurvey;
    @Lob private String practicalSurvey;


    //Integer representing how many Groups for a certain course types
    //No variable for lecture, because it is assumed to be 1.
    private Integer tutorialGroupAmount;
    private Integer seminarGroupAmount;
    private Integer practicalGroupAmount;


    //Arrays Containing the titles of instances of each course type
    @Lob ArrayList<String> lectureInstanceTitles;
    @Lob ArrayList<String> tutorialInstanceTitles;
    @Lob ArrayList<String> seminarInstanceTitles;
    @Lob ArrayList<String> practicalInstanceTitles;



    //The number representing the semester this course is taken by students
    private Integer semesterOfStudents;

    //Faculty the course belongs to 
    private CourseFaculty faculty;

    //The date at which indicates to which semester the course belongs to (used to find courses using repository methods)
    private LocalDate courseDate;  

    //The string which is displayed in UI
    private String semesterString;


    //Needed for JPA purposes
    @SuppressWarnings("unused")
	protected Course() {
    }
    
    
    /**
     * Instance of a Course (meaning a Subject [DE Lehrveranstaltung])
     * 
     * @param name                      Name of the course
     * @param lectureExists             Does the course have a lecture
     * @param tutorialExists            Does the course have a tutorial
     * @param seminarExists             Does the course have a seminar
     * @param lectureSurvey             The String containing the JSON of the lectureSurvey 
     * @param tutorialSurvey            The String containing the JSON of the tutorialSurvey 
     * @param seminarSurvey             The String containing the JSON of the seminarSurvey 
     * @param classTotalTutorial        Defines the number of classes of type tutorial (How many different tutorials are offered). This field is used to generate class specific resultsPDF's
     * @param classTotalSeminar         Defines the number of classes of type seminar (How many different seminars are offered). This field is used to generate class specific resultsPDF's
     * @param semesterOfStundets        What is the semester of the students attending the subject
     * @param faculty                   Enum defining which faculty the subject belongs to 
     * @param semesterUI            The string displaying the semester (an instance of a course. E.g. algorithms 1 is offered each year, and this is the instance of Summer semester 2020). This field is primarily used in the UI
     * @param courseInastance           The time period (start date) of when the course takes placed. This field is primarily used for sorting purposes
     */
    public Course(String name, Boolean lectureExists, Boolean tutorialExists, Boolean seminarExists, Boolean practicalExists, String lectureSurvey, String tutorialSurvey, String seminarSurvey, String practicalSurvey, Integer tutorialGroupAmount, Integer seminarGroupAmount,  Integer practicalGroupAmount, ArrayList<String> lectureInstanceTitles, ArrayList<String> tutorialInstanceTitles, ArrayList<String> seminarInstanceTitles, ArrayList<String> practiceInstanceTitles, Integer semesterOfStudents, CourseFaculty faculty, String semesterString, LocalDate courseDate){
        this.name = name;
        this.lectureExists = lectureExists;
        this.tutorialExists = tutorialExists;
        this.seminarExists = seminarExists;
        this.practicalExists = practicalExists;
        this.lectureSurvey = lectureSurvey;
        this.tutorialSurvey = tutorialSurvey;
        this.seminarSurvey = seminarSurvey;
        this.practicalSurvey = practicalSurvey;
        this.tutorialGroupAmount = tutorialGroupAmount;
        this.seminarGroupAmount = seminarGroupAmount;
        this.practicalGroupAmount = practicalGroupAmount;
        this.lectureInstanceTitles = lectureInstanceTitles;
        this.tutorialInstanceTitles = tutorialInstanceTitles;
        this.seminarInstanceTitles = seminarInstanceTitles;
        this.practicalInstanceTitles = practiceInstanceTitles;
        this.semesterOfStudents = semesterOfStudents;
        this.faculty=faculty;
        this.semesterString = semesterString;
        this.courseDate = courseDate;
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





    //CourseTypes

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

    public Boolean getPracticalExists(){
        return this.practicalExists;
    }

    public void setPracticalExists(Boolean exists){
        this.practicalExists = exists;
    }




    //Surveys

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

    public String getPracticalSurvey(){
        return this.practicalSurvey;
    }

    public void setPracticalSurvey(String survey){
        this.practicalSurvey = survey;
    }






    //Group Totals

    public int getTutorialGroupAmount(){
        return this.tutorialGroupAmount;
    }

    public void setTutorialGroupAmount (int amount){
        this.tutorialGroupAmount = amount;
    }

    public int getSeminarGroupAmount(){
        return this.seminarGroupAmount;
    }

    public void setSeminarGroupAmount (int amount){
        this.seminarGroupAmount = amount;
    }

    public int getPracticalGroupAmount(){
        return this.practicalGroupAmount;
    }

    public void setPracticalGroupAmount (int amount){
        this.practicalGroupAmount = amount;
    }







    //Instance Titles

    public ArrayList<String> getLectureInstanceTitles(){
        return this.lectureInstanceTitles;
    }

    public void setLectureInstanceTitels(ArrayList<String> list){
        this.lectureInstanceTitles = list;
    }

    public ArrayList<String> getTutorialInstanceTitles(){
        return this.tutorialInstanceTitles;
    }

    public void setTutorialInstanceTitels(ArrayList<String> list){
        this.tutorialInstanceTitles = list;
    }

    public ArrayList<String> getSeminarInstanceTitles(){
        return this.seminarInstanceTitles;
    }

    public void setSeminarInstanceTitels(ArrayList<String> list){
        this.seminarInstanceTitles = list;
    }

    public ArrayList<String> getPracticalInstanceTitles(){
        return this.practicalInstanceTitles;
    }

    public void setPracticalInstanceTitels(ArrayList<String> list){
        this.practicalInstanceTitles = list;
    }





    //the rest

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

    public LocalDate getCourseDate(){
        return this.courseDate;
    }

    public void setCourseDate(LocalDate date){
        this.courseDate = date;
    }

    public String getSemesterString(){
        return this.semesterString;
    }

    public void setSemesterString(String str){
        this.semesterString = str;
    }
}

