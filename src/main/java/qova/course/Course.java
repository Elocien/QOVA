package qova.course;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToOne;

import qova.survey.Survey;

@Entity
public class Course {

    private @Id @GeneratedValue(strategy = GenerationType.AUTO) long id;

    private String name;
    private CourseType type;

    //private ___ qrcode;

    @OneToOne
    private Survey survey;

    private int classTotal;
    private int semester;
    private CourseFaculty faculty;


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
    public Course(String name, CourseType type, Survey survey, int classTotal, int semester, CourseFaculty faculty){
        this.name = name;
        this.type = type;
        //this.qrcode = qrcode
        // this.survey = survey;
        this.classTotal = classTotal;
        this.semester = semester;
        this.faculty=faculty;
    }

    public long getId(){
        return this.id;
    }

    public String getName(){
        return this.name;
    }

    public void setName(String name){
        this.name = name;
    }

    public CourseType getType(){
        return this.type;
    }

    public void setType(CourseType type){
        this.type = type;
    }

    // public ___ getQrcode(){
    //    return this.qrcode
    //}

    //public void setQrcode(___ qrcode){
    //    this.qrcode = qrcode;
    //}

    public Survey getSurvey(){
        return this.survey;
    }

    public void setSurvey(Survey survey){
        this.survey = survey;
    }

    public int getAmount(){
        return this.classTotal;
    }

    public void setAmount(int classTotal){
        this.classTotal = classTotal;
    }

    public int getSemester(){
        return this.semester;
    }

    public void setSemester(int semester){
        this.semester = semester;
    }

    public CourseFaculty getFaculty(){
        return this.faculty;
    }

    public void setFaculty(CourseFaculty faculty){
        this.faculty = faculty;
    }
}