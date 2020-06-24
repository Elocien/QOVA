package qova.course;

import java.time.LocalDate;

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

    @Lob private CourseInstance lecture;
    @Lob private CourseInstance tutorial;
    @Lob private CourseInstance seminar;
    @Lob private CourseInstance practical;

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
     * @param lecture                   CourseInstance with courseType LECTURE
     * @param tutorial                  CourseInstance with courseType SEMINAR
     * @param seminar                   CourseInstance with courseType TUTORIAL
     * @param practical                 CourseInstance with courseType PRACTICAL
     * @param semesterOfStundets        What is the semester of the students attending the subject
     * @param faculty                   Enum defining which faculty the subject belongs to 
     * @param semesterString            The string displaying the semester (an instance of a course. E.g. algorithms 1 is offered each year, and this is the instance of Summer semester 2020). This field is primarily used in the UI
     * @param courseDate                The time period (start date) of when the course takes placed. This field is primarily used for sorting purposes
     */
    public Course(String name, CourseInstance lecture, CourseInstance tutorial, CourseInstance seminar, CourseInstance practical, Integer semesterOfStudents, CourseFaculty faculty, String semesterString, LocalDate courseDate){
        this.name = name;
        this.lecture = lecture;
        this.seminar = seminar;
        this.tutorial = tutorial;
        this.practical = practical;
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


    public Boolean getLectureExists(){
        if(lecture.equals(null)) return false;
        else return true;
    }
    public Boolean getTutorialExists(){
        if(tutorial.equals(null)) return false;
        else return true;
    }
    public Boolean getSeminarExists(){
        if(seminar.equals(null)) return false;
        else return true;
    }
    public Boolean getPracticalExists(){
        if(practical.equals(null)) return false;
        else return true;
    }

    public void setLecture(CourseInstance lecture){
        this.lecture = lecture;
    }
    public CourseInstance getLecture(){
        return this.lecture;
    }

    public void setTutorial(CourseInstance tutorial){
        this.tutorial = tutorial;
    }
    public CourseInstance getTutorial(){
        return this.tutorial;
    }

    public void setSeminar(CourseInstance seminar){
        this.seminar = seminar;
    }
    public CourseInstance getSeminar(){
        return this.seminar;
    }

    public void setPractical(CourseInstance practical){
        this.practical = practical;
    }
    public CourseInstance getPractical(){
        return this.practical;
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

