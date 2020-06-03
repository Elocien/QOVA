package qova.responses;

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

    //Container for the question belonging to the response (Used during PDF generation)
    @Lob
    private String question;
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
    @Lob
    private ArrayList<Boolean> answerMCDD;

    private ArrayList<String> optionsMCDD;

    

    //Needed for JPA puposes
    @SuppressWarnings("unused")
	private Response() {
    }

    //Text Response Constructor
    public Response(Course course, CourseType courseType, Integer position, Integer classNo, ResponseType responseType, String question, String textResponse){
        this.dateTime = LocalDateTime.now();
        this.course = course;
        this.courseType = courseType;
        this.position = position;
        this.classNo = classNo;
        this.responseType = responseType;
        this.question = question;

        //Text response
        this.textResponse = textResponse;


        //Binary repsonse
        this.binaryAnswer = null;

        //Drop down and Multiple Choice
        this.responsePossibilities = null;

        this.answerMCDD = new ArrayList<Boolean>();        
    }



    //Binary Answer Constructor
    public Response(Course course, CourseType courseType, Integer position, Integer classNo, ResponseType responseType, String question, Boolean binaryAnswer){
        this.dateTime = LocalDateTime.now();
        this.course = course;
        this.courseType = courseType;
        this.position = position;
        this.classNo = classNo;
        this.responseType = responseType;
        this.question = question;

        //Binary repsonse
        this.binaryAnswer = binaryAnswer;


        //Text response
        this.textResponse = null;

        //Drop down and Multiple Choice
        this.responsePossibilities = null;

        this.answerMCDD = new ArrayList<Boolean>(); 
    }

    public Response(Course course, CourseType courseType, Integer position, Integer classNo, ResponseType responseType, String question, Integer responsePossibilites, Integer MCorDDresponse, ArrayList<String> responseOptions) throws ArrayStoreException{
        this.dateTime = LocalDateTime.now();
        this.course = course;
        this.courseType = courseType;
        this.position = position;
        this.classNo = classNo;
        this.responseType = responseType;
        this.question = question;


        //Drop down and Multiple Choice
        this.responsePossibilities = responsePossibilites;

        this.answerMCDD = new ArrayList<Boolean>(); 
        for(int i = 0; i < responsePossibilites; i++){
            this.answerMCDD.add(false);
        }
        //MCorDDresponse gives position of response. 
        this.answerMCDD.set(MCorDDresponse, true); 

        this.optionsMCDD = responseOptions;

        if(responseOptions.size() != answerMCDD.size()){
            throw new ArrayStoreException();
        }

        //Text response
        this.textResponse = null;

        //Binary repsonse
        this.binaryAnswer = null;
    }


    //Getters & Setters

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

    public int getPosition(){
        return this.position;
    }

    public int getClassNo(){
        return this.classNo;
    }

    public ResponseType getResponseType(){
        return this.responseType;
    }

    public String getQuestion(){
        return this.question;
    }









    //Text response fields
    public String gettextResponse(){
        return this.textResponse;
    }





    
    
    //Binary response fields
    public Boolean getBinaryAnswer(){
        return this.binaryAnswer;
    }

    public String getBinaryQuestion(){
        return question;
    }








    //Drop Down and Multiple Choice fields
    public int getResponsePossibilities(){
        return this.responsePossibilities;
    }

    public ArrayList<Boolean> getListMCDD(){
        return this.answerMCDD;
    }

    public ArrayList<String> getOptionsMCDD(){
        return this.optionsMCDD;
    }

}