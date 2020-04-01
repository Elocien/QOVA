package qova.Responses;

import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;

import qova.course.Course;

@Entity
public class BinaryAnswer{

    private @Id @GeneratedValue(strategy = GenerationType.AUTO) long id;
    private Date dateTime;

    @ManyToOne
    private Course course;

    private Boolean answer;             //---true--- if yes/ja      and       ---false--- if no/nein

    //Needed for JPA puposes
    @SuppressWarnings("unused")
	private BinaryAnswer() {
    }

    /**
     * The BinaryAnswer class contains responses to yes/no questions
     * 
     * @param position 
     * @param dateTime
     * @param course
     */
    public BinaryAnswer(Date dateTime, Course course, Boolean answer){
        this.dateTime = dateTime;
        this.course = course;
        this.answer = answer;
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

    public Boolean getAnswer(){
        return this.answer;
    }

    public void setAnswer(Boolean answer){
        this.answer = answer;
    }

}