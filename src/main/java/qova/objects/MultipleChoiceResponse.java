package qova.objects;

import java.util.ArrayList;

import javax.persistence.Embeddable;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Lob;

import qova.enums.ResponseType;

@Embeddable
public class MultipleChoiceResponse {


    //-----------------------------------------------------------------------
    @GeneratedValue(strategy = GenerationType.AUTO) private Long id;

    //container for the question set
    private String question;

    //Array of the different options presented to the user
    @Lob private ArrayList<String> multipleChoiceOptions;

    //Array of the user response
    @Lob private ArrayList<Integer> multipleChoiceAnswers;

    private static final ResponseType responseType = ResponseType.MULTIPLE_CHOICE;

    //Needed for JPA puposes
    @SuppressWarnings("unused")
	private MultipleChoiceResponse(){
    }

    public MultipleChoiceResponse(String question, ArrayList<String> multipleChoiceOptions){
        this.question = question;
        this.multipleChoiceOptions = multipleChoiceOptions;
        this.multipleChoiceAnswers = new ArrayList<>(this.multipleChoiceOptions.size());
        for(String s: multipleChoiceOptions){
            multipleChoiceAnswers.add(0);
        }
    }
    
    
    public String getQuestion() {
        return this.question;
    }

    public ArrayList<String> getMultipleChoiceOptions(){
        return this.multipleChoiceOptions;
    }

    public ArrayList<Integer> getMultipleChoiceAnswers(){
        return this.multipleChoiceAnswers;
    }

    public void incrementTotals(ArrayList<Integer> totals){
        for(Integer i : totals){
            multipleChoiceAnswers.set(i, multipleChoiceAnswers.get(i) + 1);
        }
    }

    public Integer getNumberOfOptions(){
        return this.multipleChoiceOptions.size();
    }

    public ResponseType getType(){
        return responseType;
    }

}

