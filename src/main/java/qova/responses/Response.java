package qova.responses;

import java.time.LocalDateTime;
import java.util.ArrayList;

import javax.persistence.CollectionTable;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;

import qova.course.Course;
import qova.course.CourseType;


@Entity
public class Response {

    //Always used
    //-----------------------------------------------------------------------
    private @Id @GeneratedValue(strategy = GenerationType.AUTO) Long id;

    //Used to take a timestamp of when a response is submitted (timestamp is taken at the time when the response is serialized and saved to the database)
    private LocalDateTime dateTime;

    //Course and CourseType are used for more detailed search purposes (primarily when compiling responses into results pdf)
    @ManyToOne
    private Course course;
    private CourseType courseType;

    //Used to indicate to which Tutorial or Seminar the response corresponds, as a tutorial and seminar can have multiple instances for a single course 
    //is 1 if CourseType is Lecture
    private Integer classNo;

    //Used to indicate to which position on the survey, the response belongs to.
    //E.g. If a survey has 10 questions, then position is used to mark which quesiton (out of the 10) the response belongs to.
    private Integer position;

    //See ResponseType file 
    private ResponseType responseType;
    //------------------------------------------------------------------------

    //Container in case of text response
    private String textResponse;


    //Container in case of binary response
    private Boolean binaryAnswer;    //---true--- if yes/ja      and       ---false--- if no/nein 



    //Answers for Multiple Choice (MC) and Drop Down (DD) 

    //Response possibilities is used to indicate how many responses were possible for a given Multiple_Choice or Drop_Down quesiton. Used when compiling and analysing the set of all responses
    //to a question
    //E.g. A multiple choice question has the options: bad, okay, good, perfect. Then response possibilities is set to 4
    private Integer responsePossibilities;

    //Used to store the response of the user. 
    //E.g.If responsePossibilities = 4, then the arrayList will contain 3 Booleans of type "false" and one Boolean of type "true". The position of the "true" statement in the array, indicates which
    //option a user selected
    
    // @ElementCollection
	// @CollectionTable(name = "MC_OR_DD_Response", joinColumns = @JoinColumn())
    private ArrayList<Boolean> answerMCDD;


    //Needed for JPA puposes
    @SuppressWarnings("unused")
	private Response() {
    }

    
    public Response(LocalDateTime dateTime, Course course, CourseType courseType, Integer position, Integer classNo, ResponseType responseType, String textResponse, Boolean binaryAnswer, Integer responsePossibilites, Integer MCorDDresponse){
        this.dateTime = dateTime;
        this.course = course;
        this.courseType = courseType;
        this.position = position;
        this.classNo = classNo;
        this.responseType = responseType;

        //Text response
        this.textResponse = textResponse;

        //Binary repsonse
        this.binaryAnswer = binaryAnswer;

        //Drop down and Multiple Choice
        this.responsePossibilities = responsePossibilites;

        if(responsePossibilites == 0){
            //TODO: What to do, if question is not MC or DD
        }
        
        
        for(int i = 0; i < responsePossibilites; i++){
            answerMCDD.add(false);
        }
        //MCorDDresponse gives position of response. We subtract 1, because ArrayList counts from 0
        answerMCDD.set(MCorDDresponse, true);
        

        
    }


    //Getters & Setters

    public Long getId(){
        return this.id;
    }

    public LocalDateTime getDateTime(){
        return this.dateTime;
    }

    public void setDateTime(LocalDateTime dateTime){
        this.dateTime = dateTime;
    }

    public Course getCourse(){
        return this.course;
    }

    public void setCourse(Course course){
        this.course = course;
    }

    public CourseType getCourseType(){
        return this.courseType;
    }

    public void setCourseType(CourseType courseType){
        this.courseType = courseType;
    }

    public int getPosition(){
        return this.position;
    }

    public void setPosition(int pos){
        this.position = pos;
    }

    public int getClassNo(){
        return this.classNo;
    }

    public void setClassNo(int number){
        this.classNo = number;
    }

    public ResponseType getResponseType(){
        return this.responseType;
    }

    public void setResponseType(ResponseType type){
        this.responseType = type;
    }









    //Text response fields
    public String gettextResponse(){
        return this.textResponse;
    }

    public void settextResponse(String response){
        this.textResponse = response;
    }






    
    
    //Binary response fields
    public Boolean getBinaryAnswer(){
        return this.binaryAnswer;
    }

    public void setBinaryAnswer(Boolean answer){
        this.binaryAnswer = answer;
    }








    //Drop Down and Multiple Choice fields
    public int getResponsePossibilities(){
        return this.responsePossibilities;
    }

    public void setResponsePossibilities(int possibilieties){
        this.responsePossibilities= possibilieties;
    }

    public ArrayList<Boolean> getAnswerMCDD(){
        return this.answerMCDD;
    }

    public void setAnswerMCDDTrueAtPos(Integer pos){
        //set all to false, to erase single true statement
        for(int i = 0; i < this.answerMCDD.size(); i++){
            this.answerMCDD.set(i, false);
        }

        //set new true statement
        this.answerMCDD.set(pos, true);
    }
}