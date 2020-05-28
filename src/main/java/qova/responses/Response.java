package qova.responses;

import java.time.LocalDateTime;
import java.util.ArrayList;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;

import qova.course.Course;
import qova.course.CourseType;


@Entity
public class Response {

    //Always used
    //-----------------------------------------------------------------------
    private @Id @GeneratedValue(strategy = GenerationType.AUTO) Long id;

    private LocalDateTime dateTime;

    @ManyToOne
    private Course course;
    private CourseType courseType;
    private Integer classNo;

    private Integer position;

    private ResponseType responseType;
    //------------------------------------------------------------------------

    //For text response
    private ArrayList<String> textResponse;


    //For binary response
    private ArrayList<Boolean> binaryAnswer;    //---true--- if yes/ja      and       ---false--- if no/nein 
 

    //ArrayList of ArrayLists. Amount of ArrayList<Response> objects created is based on responsePossibilities
    private ArrayList<ArrayList<Boolean>> answersMCAndDD;


    //Needed for JPA puposes
    @SuppressWarnings("unused")
	private Response() {
    }

    /**
     * The response Object is used to save user responses of any kind. Each response object corresponds
     * to all user submitted responses for a single course and either its lecture, tutorial or seminar. 
     * Example: Maths for computer scientists in semester 2 (INF-B120) has a 3 response objects; one for the lecture, one for the 
     * 
     * Primary use is to generate PDF of student responses 
     * 
     * @param dateTime Captures the date and time of the users submission (Technically captures 
     * the time when the response is saved to the database
     * 
     * @param course The Course to which the Response belongs to (i.e. to which course did the 
     * user respond to). This field is used to find the Response object when compiling responses
     * for the summary pdf
     * 
     * @param courseType Enumeration which is either LECTURE, SURVEY or SEMINAR (used for more detailed retrieval)
     * 
     * @param classNo Used to determine for which Tutorial or Seminar the response was given. If CourseType.LECTURE this is set to 1
     * 
     * @param position The position of the question in the survey is mapped to the position field (used for more detailed retrieval and grouping)
     * 
     * @param responseType //Emum which is one of the following: MULTIPLE_CHOICE, 
     * DROP_DOWN, TEXT_RESPONSE or BINARY_ANSWER
     * 
     * @param textResponse Captures responses which are of type TEXT_RESPONSE
     * 
     * @param binaryAnswer Captures responses which are of type BINARY_ANSWER 
     * TRUE when response yes/ja      and       FALSE when no/nein 
     * 
     * @param answersMCAndDD This variable is used to track a Users response to a Multiple Choice or Drop Down question. The index of the first arrayList
     * signifies which of the possible responses were chosen. E.g. there are 4 response possibilities to the question: "what is your favorite colour" (blue, green , yellow, red)
     * If a user selects green, and this is the first response, then the arrayList at position 1 (counting from 0) recieves an new element "true" at position 0 (counting from 0)
     * 
     * in the case that they were 
     * selected by the user, otherwise they are set to false
     * 
     */


    //Constructor
    public Response(LocalDateTime dateTime, Course course, CourseType courseType, Integer classNo, Integer position, ResponseType responseType, Integer responsePossibilities){
        this.dateTime = dateTime;
        this.course = course;
        this.courseType = courseType;
        this.classNo = classNo;
        this.position = position;
        this.responseType = responseType;

        //Binary repsonse
        this.binaryAnswer = new ArrayList<Boolean>();

        //Text response
        this.textResponse = new ArrayList<String>();

        this.answersMCAndDD = new ArrayList<ArrayList<Boolean>>();

        //Drop-down and Multiple-choice  
        for(int i = 0; i < responsePossibilities; i++){
            this.answersMCAndDD.add(new ArrayList<Boolean>());
        }
    }


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

    public void setPositio(int pos){
        this.position = pos;
    }

    public ResponseType getResponseType(){
        return this.responseType;
    }

    public void setResponseType(ResponseType type){
        this.responseType = type;
    }









    //Text response fields
    public ArrayList<String> gettextResponses(){
        return this.textResponse;
    }

    public void addTextResponse(String response){
        this.textResponse.add(response);
    }






    
    
    //Binary response fields
    public ArrayList<Boolean> getBinaryAnswer(){
        return this.binaryAnswer;
    }

    public void addBinaryAnswer(Boolean answer){
        this.binaryAnswer.add(answer);
    }








    //Drop Down and Multiple Choice fields
    public Integer getResponsePossibilities(){
        return this.answersMCAndDD.size();
    }

    public ArrayList<ArrayList<Boolean>> getAnswersMcAndDD(){
        return this.answersMCAndDD;
    }


    //count from 1
    public void setMC_or_DD_ResponseTrue(Integer pos){
        this.answersMCAndDD.get(pos-1).add(true);
    }   

    

}