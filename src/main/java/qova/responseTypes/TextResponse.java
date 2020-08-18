package qova.responseTypes;

import java.util.ArrayList;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;

@Entity
public class TextResponse extends AbstractResponse {


    //-----------------------------------------------------------------------
    @Id @GeneratedValue(strategy = GenerationType.AUTO) private Long id;

    //container for the question set
    private String question;

    //Container for response
    @Lob private ArrayList<String> responses;

    private final ResponseType responseType = ResponseType.TEXT_RESPONSE;

    //Needed for JPA puposes
    @SuppressWarnings("unused")
	private TextResponse(){
    }

    public TextResponse(String question){
        this.question = question;
        this.responses = new ArrayList<String>();
    }
    
    
    

    public String getQuestion() {
        return this.question;
    }

    public ArrayList<String> getResponses() {
        return this.responses;
    }

    public void addTextSubmission(String resp){
        this.responses.add(resp);
    }

    public ResponseType getType(){
        return this.responseType;
    }
    
}

