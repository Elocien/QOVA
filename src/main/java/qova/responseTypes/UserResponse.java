package qova.responseTypes;

import java.time.LocalDateTime;
import java.util.ArrayList;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;

import qova.course.Course;
import qova.course.CourseType;


//Each question needs following fields: 
//responseType
//position (can be found via array position)
//Question      textresponse        Binary response         Mutliple/single choice (1 question, multiple options)
//Answer        answer              true/false              true/false (multiple times)

//ArrayList, position = index

@Entity
public class UserResponse {

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
    private Integer classNo;


    //Array of all subtype Objects (BinaryResponse, TextResponse, SingleChoiceResponse and MultipleChoiceResponse)
    @Lob ArrayList<Object> responses;




    //Needed for JPA puposes
    @SuppressWarnings("unused")
    public UserResponse (){}


    public UserResponse(Course course, CourseType type, Integer classNo, ArrayList<Object> responses){
        this.dateTime = LocalDateTime.now();
        this.course = course;
        this.courseType = type;
        this.classNo = classNo;
        this.responses = responses;
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

    public Integer getClassNo(){
        return this.classNo;
    }

    public ArrayList<Object> getUserResponse(){
        return this.responses;
    }

}