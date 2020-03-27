package qova.Questions;

import java.util.Date;

import javax.persistence.ManyToOne;

import qova.course.Course;

public class TextResponse{
    private int position;
    private Date dateTime;

    @ManyToOne
    private Course course;

    private String response;


    public TextResponse(int position, Date dateTime, Course course, String response){
        this.position = position;
        this.dateTime = dateTime;
        this.course = course;
        this.response = response;
    }


    

    public int getPosition(){
        return this.position;
    }

    public void setPosition(int position){
        this.position = position;
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