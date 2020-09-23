package qova.objects;

import javax.persistence.Embeddable;

import javax.persistence.Lob;

import qova.enums.ResponseType;


@Embeddable
public class BinaryResponse {

    //-----------------------------------------------------------------------

    //container for the question set
    @Lob String question;

    //Container for response
    private Integer yesTotal = 0;
    private Integer noTotal = 0;

    private ResponseType responseType = ResponseType.BINARY_ANSWER;

    //Needed for JPA puposes
    @SuppressWarnings("unused")
	protected BinaryResponse(){
    }

    public BinaryResponse(String question){
        this.question = question;
    }
    
    
    public String getQuestion() {
        return this.question;
    }

    public String getYesTotalString() {
        return String.valueOf(this.yesTotal);
    }

    public String getNoTotalString() {
        return String.valueOf(this.noTotal);
    }

    public Integer getYesTotal() {
        return this.yesTotal;
    }

    public Integer getNoTotal() {
        return this.noTotal;
    }

    public Integer getTotal() {
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