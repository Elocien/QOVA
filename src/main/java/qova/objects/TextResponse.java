package qova.objects;

import java.util.ArrayList;

import javax.persistence.Embeddable;
import javax.persistence.Lob;

import qova.enums.ResponseType;

@Embeddable
public class TextResponse {

    //-----------------------------------------------------------------------
    //container for the question set
    private String question;

    //Container for response
    @Lob private ArrayList<String> responses;

    private static final ResponseType responseType = ResponseType.TEXT_RESPONSE;

    //Needed for JPA puposes
    @SuppressWarnings("unused")
	private TextResponse(){
    }

    public TextResponse(String question){
        this.question = question;
        this.responses = new ArrayList<>();
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
        return responseType;
    }
    
}

