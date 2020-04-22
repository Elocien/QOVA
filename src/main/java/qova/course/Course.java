package qova.course;


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
    private CourseType type;

    private String[] survey;

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
    public Course(String name, CourseType type, String[] survey, int classTotal, int semester, CourseFaculty faculty){
        this.name = name;
        this.type = type;
        this.survey = survey;
        this.classTotal = classTotal;
        this.semester = semester;
        this.faculty=faculty;
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

    public CourseType getType(){
        return this.type;
    }

    public void setType(CourseType type){
        this.type = type;
    }

    public String[] getSurvey(){
        return this.survey;
    }

    public void setSurvey(String[] survey){
        this.survey = survey;
    }

    public int getClassTotal(){
        return this.classTotal;
    }

    public void setClassTotal(int classTotal){
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

