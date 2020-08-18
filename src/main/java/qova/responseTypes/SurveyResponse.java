package qova.responseTypes;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.OrderColumn;

import qova.course.Course;
import qova.course.CourseType;


//Each question needs following fields: 
//responseType
//position (can be found via array position)
//Question      textresponse        Binary response         Mutliple/single choice (1 question, multiple options)
//Answer        answer              true/false              true/false (multiple times)

//ArrayList, position = index

@Entity
public class SurveyResponse {

    //Always used

    //-----------------------------------------------------------------------
    @Id @GeneratedValue(strategy = GenerationType.AUTO) private Long id;


    //Used to take a timestamp of when a response is submitted (timestamp is taken at the time when the response is serialized and saved to the database)
    private LocalDateTime dateTime;


    //Course and CourseType are used for more detailed search purposes (primarily when compiling responses into results pdf)
    @ManyToOne
    private Course course;
    

    //Enum, used for retrieval from repository
    private CourseType courseType;


    //Used to indicate to which Tutorial or Seminar the response corresponds, as a tutorial and seminar can have multiple instances for a single course 
    //is 1 if CourseType is Lecture
    private Integer groupNumber;


    //comment
    private Integer instanceNumber;


    //comment
    private Integer numberOfSubmissions;


    //Array of all subtype Objects (BinaryResponse, TextResponse, SingleChoiceResponse and MultipleChoiceResponse)
    @Lob List<AbstractResponse> responses = new ArrayList<>();


    //Needed for JPA puposes
    @SuppressWarnings("unused")
    public SurveyResponse (){}


    public SurveyResponse(Course course, CourseType type, Integer instanceNumber, Integer groupNumber, List<AbstractResponse> responses){
        this.dateTime = LocalDateTime.now();
        this.course = course;
        this.courseType = type;
        this.instanceNumber = instanceNumber;
        this.groupNumber = groupNumber;
        this.responses = responses;
        this.numberOfSubmissions = 0;
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

    public Integer getNumberOfSubmissions(){
        return this.numberOfSubmissions;
    }

    public List<AbstractResponse> getUserResponses(){
        return this.responses;
    }

}