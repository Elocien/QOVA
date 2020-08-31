package qova.objects;

import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;
import javax.persistence.OrderColumn;

import qova.enums.CourseType;

@Entity
public class CourseInstance {
    
    //Id
    private @Id @GeneratedValue(strategy = GenerationType.AUTO) Long id;


    //Either LECTURE, TUTORIAL, SEMINAR OR PRACTICAL
    private CourseType courseType;


    //The Survey as JSON
    @Lob private String survey;


    //The number of Groups a courseInstance has (e.g. if 12 tutorials are offered, then GroupAmount = 12)
    //Note: this is currently always 1, if courseType is LECTURE
    private Integer groupAmount;


    //The number of times an instance occurs (e.g. there are 13 weeks in a semester, and two lectures a week. instanceAmount = 13*2 = 26)
    private Integer instanceAmount;


    //Titles of each instance of the given courseType (e.g. there are 12 lectures, each with a unique title)
    private @ElementCollection @OrderColumn String[] instanceTitles;



    //Needed for JPA purposes
    @SuppressWarnings("unused")
	protected CourseInstance() {
    }

    /**
     * Instance representing either a Lecture, Tutorial, Seminar or Practical. Nested into {@linkplain Course}
     * 
     * @param courseType     Enumeration of the possible types
     *                       rendered by JS in survey template
     * @param groupAmount    Integer representing the number of the group of this
     *                       specific courseInstance
     * @param instanceAmount Integer representing the number of the instance of this
     *                       specific courseInstance
     * @param instanceTitles The titles for each instance of a {@linkplain Course}, in the form of a String 
     */
    public CourseInstance(CourseType courseType, Integer groupAmount, Integer instanceAmount,
            String[] instanceTitles){
       
        this.courseType = courseType;
        this.survey = "[]";
        this.groupAmount = groupAmount;
        this.instanceAmount = instanceAmount;
        this.instanceTitles = instanceTitles;
    }

    public CourseType getCourseType(){
        return this.courseType;
    }

    public void setCourseType(CourseType type){
        this.courseType = type;
    }


    public String getSurvey(){
        return this.survey;
    }

    public void setSurvey(String survey){
        this.survey = survey;
    }

    public int getGroupAmount(){
        return this.groupAmount;
    }

    public void setGroupAmount (int amount){
        this.groupAmount = amount;
    }

    public int getInstanceAmount(){
        return this.instanceAmount;
    }

    public void getInstanceAmount (int amount){
        this.instanceAmount = amount;
    }

    public String[] getInstanceTitles(){
        return this.instanceTitles;
    }

    public void setInstanceTitles(String[] list){
        this.instanceTitles = list;
    }


}