package qova.Responses;

import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;

import qova.course.Course;


@Entity
public class TextResponse{
    
    //--------------------------------------

    private @Id @GeneratedValue(strategy = GenerationType.AUTO) long id;

    private Date dateTime;

    @ManyToOne
    private Course course;

    private String response;


    //--------------------------------------


    //Needed for JPA puposes
    @SuppressWarnings("unused")
	private TextResponse() {
    }


    //--------------------------------------------------------------------------
    public TextResponse(Date dateTime, Course course, String response){
        this.dateTime = dateTime;
        this.course = course;
        this.response = response;
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

    public String getResponse(){
        return this.response;
    }

    public void setResponse(String response){
        this.response = response;
    }


    


}