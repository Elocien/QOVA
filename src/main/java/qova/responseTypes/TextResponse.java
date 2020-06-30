package qova.responseTypes;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;

@Entity
public class TextResponse{


    //-----------------------------------------------------------------------
    @Id @GeneratedValue(strategy = GenerationType.AUTO) private Long id;

    //container for the question set
    private String question;

    //Container for response
    @Lob private String response;

    private final ResponseType responseType = ResponseType.TEXT_RESPONSE;

    //Needed for JPA puposes
    @SuppressWarnings("unused")
	private TextResponse(){
    }

    public TextResponse(String question, String response){
        this.question = question;
        this.response = response;
    }
    
    
    

    public String getQuestion() {
        return this.question;
    }

    public String getResponse() {
        return this.response;
    }

    public ResponseType getType(){
        return this.responseType;
    }
    
}

