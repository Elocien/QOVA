package qova.objects;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.CollectionTable;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.OrderColumn;

import qova.objects.Course;
import qova.enums.CourseType;
import qova.enums.ResponseType;


//Each question needs following fields: 
//responseType
//position (can be found via array position)
//Question      textresponse        Binary response         Mutliple/single choice (1 question, multiple options)
//Answer        answer              true/false              true/false (multiple times)

//ArrayList, position = index

@Entity
public class SurveyResponse {


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


    //Maps of all subtype Objects (BinaryResponse, TextResponse, SingleChoiceResponse and MultipleChoiceResponse)
   @ElementCollection @OrderColumn @CollectionTable private Map<Integer, BinaryResponse> binaryResponses;

   @ElementCollection @OrderColumn @CollectionTable private Map<Integer, TextResponse> textResponses;

   @ElementCollection @OrderColumn @CollectionTable private Map<Integer, SingleChoiceResponse> singleChoiceResponses;

   @ElementCollection @OrderColumn @CollectionTable private Map<Integer, MultipleChoiceResponse> multipleChoiceResponses;


   //
   @ElementCollection @OrderColumn @CollectionTable private List<ResponseType> positionOfResponseTypes;


    //Needed for JPA puposes
    @SuppressWarnings("unused")
    public SurveyResponse (){}

    /** Constructor
     * 
     * @param course 
     * @param type
     * @param instanceNumber
     * @param groupNumber
     * @param responses
     */
    public SurveyResponse(Course course, CourseType type, Integer instanceNumber, Integer groupNumber, List<Object> responses){
        this.dateTime = LocalDateTime.now();
        this.course = course;
        this.courseType = type;
        this.instanceNumber = instanceNumber;
        this.groupNumber = groupNumber;
        this.numberOfSubmissions = 0;

        this.positionOfResponseTypes = new ArrayList<>();

        //Initialise Maps
        this.binaryResponses = new HashMap<>();
        this.textResponses = new HashMap<>();
        this.singleChoiceResponses = new HashMap<>();
        this.multipleChoiceResponses = new HashMap<>();

        //Add the responses to the corresponding map, with the int indicating the position on the questionnaire
        for(int i = 0; i < responses.size(); i++){

            Object rsp = responses.get(i);

            if(rsp instanceof BinaryResponse){
                binaryResponses.put(i, (BinaryResponse) rsp);
                positionOfResponseTypes.add(ResponseType.BINARY_ANSWER);
            }
            if(rsp instanceof TextResponse){
                textResponses.put(i, (TextResponse) rsp);
                positionOfResponseTypes.add(ResponseType.TEXT_RESPONSE);
            }
            if(rsp instanceof SingleChoiceResponse){
                singleChoiceResponses.put(i, (SingleChoiceResponse) rsp);
                positionOfResponseTypes.add(ResponseType.SINGLE_CHOICE);
            }
            if(rsp instanceof MultipleChoiceResponse){
                multipleChoiceResponses.put(i, (MultipleChoiceResponse) rsp);
                positionOfResponseTypes.add(ResponseType.MULTIPLE_CHOICE);
            }
        }
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

    public Integer getNumberOfSubmissions(){
        return this.numberOfSubmissions;
    }


    //Used to assemble an ordered list of the responses. 
    public List<Object> getUserResponses(){

        List<Object> responses = new ArrayList<>();

        for(int i = 0; i < positionOfResponseTypes.size(); i++){
            if(positionOfResponseTypes.get(i).equals(ResponseType.BINARY_ANSWER)){
                responses.add(binaryResponses.get(i));
            }
            if(positionOfResponseTypes.get(i).equals(ResponseType.TEXT_RESPONSE)){
                responses.add(textResponses.get(i));
            }
            if(positionOfResponseTypes.get(i).equals(ResponseType.SINGLE_CHOICE)){
                responses.add(singleChoiceResponses.get(i));
            }
            if(positionOfResponseTypes.get(i).equals(ResponseType.MULTIPLE_CHOICE)){
                responses.add(multipleChoiceResponses.get(i));
            }
        }
        return responses;
    }
}