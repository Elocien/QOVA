package qova.Responses;

import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;

import qova.course.Course;


@Entity
public class MultipleChoice{

    private @Id @GeneratedValue(strategy = GenerationType.AUTO) long id;

    private Date dateTime;

    @ManyToOne
    private Course course;

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
	private MultipleChoice() {
    }


    public MultipleChoice(Date dateTime, Course course, int responsePossiblilites, Boolean answer1, Boolean answer2, Boolean answer3, Boolean answer4, Boolean answer5, 
            Boolean answer6, Boolean answer7, Boolean answer8, Boolean answer9, Boolean answer10){
        this.dateTime = dateTime;
        this.course = course;
        
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