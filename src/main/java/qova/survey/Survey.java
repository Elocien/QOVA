package qova.survey;

import java.time.LocalDateTime;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

@Entity
public class Survey {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private long id;

    private int question;

    //Needed for JPA puposes
    @SuppressWarnings("unused")
	private Survey() {
    }
    
    public Survey(int question){
        this.question = question;
    }



}


