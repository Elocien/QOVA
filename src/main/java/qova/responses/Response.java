package qova.responses;

import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;

import qova.course.Course;
import qova.course.CourseType;


@Entity
public class Response {

    private @Id @GeneratedValue(strategy = GenerationType.AUTO) Long id;

    private Date dateTime;

    @ManyToOne
    private Course course;
    private CourseType type;


    private ResponseType responseType;


    //For text response
    private String textResponse;


    //For binary response
    private Boolean binaryAnswer;    //---true--- if yes/ja      and       ---false--- if no/nein 



    //For Drop Down and Multiple Choice
    private int responsePossiblilites;  

    private Boolean answer1;
    private Boolean answer2;
    private Boolean answer3;
    private Boolean answer4;
    private Boolean answer5;
    private Boolean answer6;
    private Boolean answer7;
    private Boolean answer8;
    private Boolean answer9;
    private Boolean answer10;


    //Needed for JPA puposes
    @SuppressWarnings("unused")
	private Response() {
    }

    /**
     * The response Object is used to save user responses of any kind. Each response corresponds
     * to the response of a user to a single questionaire question. 
     * 
     * @param dateTime Captures the date and time of the users submission (Technically captures 
     * the time when the response is saved to the database
     * 
     * @param course The Course to which the Response belongs to (i.e. to which course did the 
     * user respond to). This field is used to find the Response object when compiling responses
     * for the summary pdf
     * 
     * @param type Enumeration which is either LECTURE, SURVEY or SEMINAR (also needed to find 
     * the correct responses when compiling)
     * 
     * @param responsePossiblilites Used to set the number of fields to be evaluated. When
     * summarising the users responses, this field is required to calculate the distribution.
     * E.g. Response possibilities = 2. 20 people picked answer1, 10 answer2. Therefore 
     * answer1 = 67% and answer2 = 33%
     * 
     * @param responseType //Emum which is one of the following: MULTIPLE_CHOICE, 
     * DROP_DOWN, TEXT_RESPONSE or BINARY_ANSWER
     * 
     * @param textResponse Captures responses which are of type TEXT_RESPONSE
     * 
     * @param binaryAnswer Captures responses which are of type BINARY_ANSWER 
     * TRUE when response yes/ja      and       FALSE when no/nein 
     * 
     * @param answer1 All of params of type answerN are set to true, in the case that they were 
     * selected by the user, otherwise they are set to false
     * 
     * @param answer2
     * @param answer3
     * @param answer4
     * @param answer5
     * @param answer6
     * @param answer7
     * @param answer8
     * @param answer9
     * @param answer10
     */
    public Response(Date dateTime, Course course, int responsePossiblilites, ResponseType responseType, String textResponse, Boolean binaryAnswer, Boolean answer1, Boolean answer2, Boolean answer3, Boolean answer4, Boolean answer5, Boolean answer6, Boolean answer7, Boolean answer8, Boolean answer9, Boolean answer10){
        this.dateTime = dateTime;
        this.course = course;
        this.responseType = responseType;

        //Text response
        this.textResponse = textResponse;

        //Binary repsonse
        this.binaryAnswer = binaryAnswer;

        //Drop down and Multiple Choice
        this.responsePossiblilites = responsePossiblilites;
        this.answer1 = answer1;
        this.answer1 = answer2;
        this.answer1 = answer3;
        this.answer1 = answer4;
        this.answer1 = answer5;
        this.answer1 = answer6;
        this.answer1 = answer7;
        this.answer1 = answer8;
        this.answer1 = answer9;
        this.answer1 = answer10;
    }

    public Long getId(){
        return this.id;
    }

    public Date getDateTime(){
        return this.dateTime;
    }

    public void setDateTime(Date dateTime){
        this.dateTime = dateTime;
    }

    public Course getCourse(){
        return this.course;
    }

    public void setCourse(Course course){
        this.course = course;
    }

    public ResponseType getResponseType(){
        return this.responseType;
    }

    public void setResponseType(ResponseType type){
        this.responseType = type;
    }





    //Text response
    public String gettextResponse(){
        return this.textResponse;
    }

    public void settextResponse(String response){
        this.textResponse = response;
    }






    
    
    //Binary response
    public Boolean getBinaryAnswer(){
        return this.binaryAnswer;
    }

    public void setBinaryAnswer(Boolean answer){
        this.binaryAnswer = answer;
    }








    //Drop Down and Multiple Choice
    public int getResponsePossibilities(){
        return this.responsePossiblilites;
    }

    public void setResponsePossibilities(int possibilieties){
        this.responsePossiblilites = possibilieties;
    }

    public Boolean getAnswer1(){
        return this.answer1;
    }

    public void setAnswer1(Boolean answer){
        this.answer1 = answer;
    }

    public Boolean getAnswer2(){
        return this.answer2;
    }

    public void setAnswer2(Boolean answer){
        this.answer2 = answer;
    }

    public Boolean getAnswer3(){
        return this.answer3;
    }

    public void setAnswer3(Boolean answer){
        this.answer3 = answer;
    }

    public Boolean getAnswer4(){
        return this.answer4;
    }

    public void setAnswer4(Boolean answer){
        this.answer4 = answer;
    }

    public Boolean getAnswer5(){
        return this.answer5;
    }

    public void setAnswer5(Boolean answer){
        this.answer5 = answer;
    }


    public Boolean getAnswer6(){
        return this.answer6;
    }

    public void setAnswer6(Boolean answer){
        this.answer6 = answer;
    }

    public Boolean getAnswer7(){
        return this.answer7;
    }

    public void setAnswer7(Boolean answer){
        this.answer7 = answer;
    }

    public Boolean getAnswer8(){
        return this.answer8;
    }

    public void setAnswer8(Boolean answer){
        this.answer8 = answer;
    }

    public Boolean getAnswer9(){
        return this.answer9;
    }

    public void setAnswer9(Boolean answer){
        this.answer9 = answer;
    }

    public Boolean getAnswer10(){
        return this.answer10;
    }

    public void setAnswer10(Boolean answer){
        this.answer10 = answer;
    }

}