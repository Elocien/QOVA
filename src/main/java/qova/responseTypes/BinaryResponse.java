package qova.responseTypes;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;


@Entity
public class BinaryResponse {

    //-----------------------------------------------------------------------
    @Id @GeneratedValue(strategy = GenerationType.AUTO) private Long id;

    //container for the question set
    @Lob String question;

    //Container for response
    private Integer yesTotal;
    private Integer noTotal;

    private ResponseType responseType;

    //Needed for JPA puposes
    @SuppressWarnings("unused")
	private BinaryResponse(){
    }

    public BinaryResponse(String question){
        this.question = question;
        this.yesTotal = 0;
        this.noTotal = 0;
        this.responseType = ResponseType.BINARY_ANSWER;
    }
    
    
    public String getQuestion() {
        return this.question;
    }

    public Integer getYesTotal() {
        return this.yesTotal;
    }

    public Integer getNoTotal() {
        return this.noTotal;
    }

    public ResponseType getType(){
        return this.responseType;
    }

    public void incrementYes(){
        this.yesTotal = yesTotal + 1;
    }

    public void incrementNo(){
        this.noTotal = noTotal + 1;
    }

}