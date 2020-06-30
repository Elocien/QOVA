package qova.responseTypes;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;


@Entity
public class BinaryResponse{


    //-----------------------------------------------------------------------
    @Id @GeneratedValue(strategy = GenerationType.AUTO) private Long id;

    //container for the question set
    private String question;

    //Container for response
    private Boolean response;

    private final ResponseType responseType = ResponseType.BINARY_ANSWER;


    //Needed for JPA puposes
    @SuppressWarnings("unused")
	private BinaryResponse(){
    }

    public BinaryResponse(String question, Boolean response){
        this.question = question;
        this.response = response;
    }
    
    
    
    public String getQuestion() {
        return this.question;
    }

    public Boolean getResponse() {
        return this.response;
    }

    public ResponseType getType(){
        return this.responseType;
    }

}