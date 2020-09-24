package qova.objects;

import java.time.LocalDateTime;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;

import qova.enums.CourseType;
import qova.enums.ResponseType;




@Entity
public class SurveyResponse {


    //-----------------------------------------------------------------------
    @Id @GeneratedValue(strategy = GenerationType.AUTO) 
    private Long id;


    //Used to take a timestamp of when a response is submitted (timestamp is taken at the time when the response is serialized and saved to the database)
    private LocalDateTime dateTime;


    //Course and CourseType are used for more detailed search purposes (primarily when compiling responses into results pdf)
    @ManyToOne
    private Course course;
    

    //Enum, used for retrieval from repository
    private CourseType courseType;


    //The tutorial group number of the SurveyResponse
    private Integer groupNumber;


    //The instance number of the SurveyResponse
    private Integer instanceNumber;

    //Tracks the number of times stundents submitted 
    private Integer numberOfSubmissions;

    //The ID's of all of the people that have submitted to this survey
    @ElementCollection 
    private Map<String, Date> listOfStundentsThatSubmitted;


    //Needed for JPA puposes
    @SuppressWarnings("unused")
    protected SurveyResponse (){}

    
    //Constructor
    public SurveyResponse(Course course, CourseType type, Integer instanceNumber, Integer groupNumber){
        this.dateTime = LocalDateTime.now();
        this.course = course;
        this.courseType = type;
        this.instanceNumber = instanceNumber;
        this.groupNumber = groupNumber;
        this.numberOfSubmissions = 0;
        this.listOfStundentsThatSubmitted = new HashMap<>();
    }
    

    public Long getId(){
        return this.id;
    }

    public LocalDateTime getDateTime(){
        return this.dateTime;
    }

    public Course getCourse(){
        return this.course;
    }

    public CourseType getCourseType(){
        return this.courseType;
    }

    public Integer getGroupNumber(){
        return this.groupNumber;
    }

    public Integer getInstanceNumber(){
        return this.instanceNumber;
    }

    public Map<String, Date> getListOfStudentsThatSubmitted(){
        return this.listOfStundentsThatSubmitted;
    }

    public Integer getNumberOfSubmissions(){
        return this.numberOfSubmissions;
    }

    public void addStundentIdToSubmissionListAndIncrementCounter(String id){
        this.listOfStundentsThatSubmitted.put(id, new Date());
        this.numberOfSubmissions++;
    }

}