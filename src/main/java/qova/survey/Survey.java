package qova.survey;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

@Entity
public class Survey {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private long id;

    private String[] questions;

    // //Needed for JPA puposes
    // @SuppressWarnings("unused")
	// private Survey() {
    // }
    
    // public Survey(String questions){
    //     this.questions = questions;
    // }

    public long getId(){
        return this.id;
    }

    public void setId(Integer id) {
        this.id = id;
      }    
    
    public String[] getQuestions() {
        return this.questions;
    }

    public void setQuestions(String[] questions){
        this.questions = questions;
    }


}


