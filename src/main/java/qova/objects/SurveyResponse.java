package qova.objects;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.AttributeOverride;
import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.OrderColumn;

import qova.enums.CourseType;
import qova.enums.ResponseType;




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


    //Maps of all subtype Objects (BinaryResponse, TextResponse, SingleChoiceResponse and MultipleChoiceResponse). Split in order to persist in db
    @Embedded @ElementCollection @OrderColumn 
    @AttributeOverride( name = "question", column = @Column(name = "binaryResponseQuestion"))
    @AttributeOverride( name = "responseType", column = @Column(name = "binaryResponseType"))
    private Map<Integer, BinaryResponse> binaryResponses;

    
    @Embedded @ElementCollection @OrderColumn
    @AttributeOverride( name = "question", column = @Column(name = "textResponseQuestion"))
    @AttributeOverride( name = "responseType", column = @Column(name = "textResponseType"))
    private Map<Integer, TextResponse> textResponses;

    @Embedded @ElementCollection @OrderColumn
    @AttributeOverride( name = "question", column = @Column(name = "singleChoiceResponseQuestion"))
    @AttributeOverride( name = "responseType", column = @Column(name = "singleChoiceResponseType"))
    private Map<Integer, SingleChoiceResponse> singleChoiceResponses;

    @Embedded @ElementCollection @OrderColumn
    @AttributeOverride( name = "question", column = @Column(name = "multipleChoiceResponseQuestion"))
    @AttributeOverride( name = "responseType", column = @Column(name = "multipleChoiceResponseType"))
    private Map<Integer, MultipleChoiceResponse> multipleChoiceResponses;


    //List which keeps track of the position of each ResponseType, for reassembly of the list from the database.
    @ElementCollection @OrderColumn 
    private List<ResponseType> positionOfResponseTypes;


    //Needed for JPA puposes
    @SuppressWarnings("unused")
    public SurveyResponse (){}

    /** 
     * Constructor
     * 
     * @param course {@linkplain Course} The Course, to which the SurveyResponse is bound. 
     * @param type {@linkplain CourseType} 
     * @param instanceNumber Which instance of the course will this SurveyResponse represent, and accumulate results for
     * @param groupNumber Which group (eg. tutorial group 3 | seminar group 5) of the course will this SurveyResponse represent, and accumulate results for
     * @param responses A list used to track which {@linkplain ResponseType} is in which position
     */
    public SurveyResponse(Course course, CourseType type, Integer instanceNumber, Integer groupNumber, List<Object> responses){
        this.dateTime = LocalDateTime.now();
        this.course = course;
        this.courseType = type;
        this.instanceNumber = instanceNumber;
        this.groupNumber = groupNumber;

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


    /** 
     * Method which reassembles the list of Responses of type {@linkplain BinaryResponse}, {@linkplain TextResponse}, 
     * {@linkplain MultipleChoiceResponse} or {@linkplain SingleChoiceResponse}. The position indicates where the response (representing a question)
     * appears on a survey.
     * 
     * @return a list of {@link java.lang.Object} 
     */
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