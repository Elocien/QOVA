package qova.objects;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import javax.persistence.*;

import org.hibernate.annotations.GenericGenerator;
import org.json.JSONObject;
import org.json.JSONArray;

import jdk.jfr.BooleanFlag;
import qova.admin.DefaultSurvey;
import qova.enums.CourseType;

@Entity
public class CourseInstance {

    // Id
    @Id
    @GeneratedValue(generator = "UUID")
    @GenericGenerator(name = "UUID", strategy = "org.hibernate.id.UUIDGenerator")
    @Column(name = "id", updatable = false, nullable = false, columnDefinition = "BINARY(16)")
    private UUID id;

    // Either LECTURE, TUTORIAL, SEMINAR OR PRACTICAL
    @Enumerated
    private CourseType courseType;

    // The Survey as JSON
    @Lob
    private String survey;

    // The number of Groups a courseInstance has (e.g. if 12 tutorials are offered,
    // then GroupAmount = 12)
    // Note: this is currently always 1, if courseType is LECTURE
    private Integer groupAmount;

    // The number of times an instance occurs (e.g. there are 13 weeks in a
    // semester, and two lectures a week. instanceAmount = 13*2 = 26)
    private Integer instanceAmount;

    // Titles of each instance of the given courseType (e.g. there are 12 lectures,
    // each with a unique title)
    @ElementCollection
    @OrderColumn
    private List<String> instanceTitles;

    // Flag used to indicate wether the instance is active (E.g. The instance is of
    // type practical, but isn't set as being evaluated by the course owner;
    // therefore it is set as active = false)
    @BooleanFlag
    private Boolean active;

    // Flag used to indicate that the survey has been edited an saved in some form
    // (This is primarilly used to allow surveys to be set without adding to the
    // default survey)
    @BooleanFlag
    private Boolean surveyEditedFlag;

    @ManyToOne
    private DefaultSurvey defaultSurvey;

    // Needed for JPA purposes
    @SuppressWarnings("unused")
    protected CourseInstance() {
    }

    /**
     * Instance representing either a Lecture, Tutorial, Seminar or Practical.
     * Nested into {@linkplain Course}. This object holds the survey for
     * 
     * @param courseType     Enumeration of the possible types rendered by JS in
     *                       survey template
     * @param groupAmount    Integer representing the number of the group of this
     *                       specific courseInstance
     * @param instanceAmount Integer representing the number of the instance of this
     *                       specific courseInstance
     * @param instanceTitles The titles for each instance of a {@linkplain Course},
     *                       in the form of a String
     * @param defaultSurvey  A reference to {@linkplain qova.admin.DefaultSurvey}, in
     *                       order to obtain the defaultSurveyJson String
     */
    public CourseInstance(CourseType courseType, Integer groupAmount, Integer instanceAmount,
            List<String> instanceTitles, DefaultSurvey defaultSurvey) {

        this.courseType = courseType;
        this.survey = "[]";
        this.groupAmount = groupAmount;
        this.instanceAmount = instanceAmount;
        this.instanceTitles = instanceTitles;
        this.active = true;
        this.surveyEditedFlag = true;
        this.defaultSurvey = defaultSurvey;
    }

    /**
     * Constructor for inactive {@linkplain qova.objects.CourseInstance}'s'. Used
     * when a {@linkplain qova.objects.Course} is created, but the Instance itself
     * is not set. The CourseInstance is created regardless, in case it must be used
     * in future
     * 
     * @param courseType The {@linkplain qova.enums.CourseType}
     * @param defaultSurvey  A reference to {@linkplain qova.admin.DefaultSurvey}, in
     *      *                       order to obtain the defaultSurveyJson String
     */
    public CourseInstance(CourseType courseType, DefaultSurvey defaultSurvey) {
        this.courseType = courseType;
        this.survey = "[]";
        this.groupAmount = 0;
        this.instanceAmount = 0;
        this.instanceTitles = new ArrayList<>();
        this.active = false;
        this.surveyEditedFlag = false;
        this.defaultSurvey = defaultSurvey;
    }

    public UUID getId() {
        return this.id;
    }

    public CourseType getCourseType() {
        return this.courseType;
    }

    public String getSurvey() {
        return this.survey;
    }

    public void setSurvey(String survey) {
        this.survey = survey;
    }

    public int getGroupAmount() {
        return this.groupAmount;
    }

    public void setGroupAmount(int amount) {
        this.groupAmount = amount;
    }

    public int getInstanceAmount() {
        return this.instanceAmount;
    }

    public void setInstanceAmount(int amount) {
        this.instanceAmount = amount;
    }

    public List<String> getInstanceTitles() {
        return this.instanceTitles;
    }

    public void setInstanceTitles(List<String> list) {
        this.instanceTitles = list;
    }

    public Boolean isActive() {
        return this.active;
    }

    public void setActive() {
        this.active = true;
        this.surveyEditedFlag=true;
    }

    public void setInactive() {
        this.active = false;
        this.groupAmount = 0;
        this.instanceAmount = 0;
        this.instanceTitles = new ArrayList<>();
        this.surveyEditedFlag=false;
    }

    /**
     * Method was used to set flag, but the flag has been deleted from
     */
    @Deprecated
    public void setSurveEditedFlag() {
        this.surveyEditedFlag = true;
    }

    public Boolean getSurveyEditedFlag() {
        return this.surveyEditedFlag;
    }

    public DefaultSurvey getDefaultSurvey(){
        return this.defaultSurvey;
    }

    public Boolean titlesMissing() {
        for(String title: instanceTitles){
            if(title.equals("")){
                return true;
            }
        }
        return false;
    }

}