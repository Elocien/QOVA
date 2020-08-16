package qova.responseTypes;

import java.util.ArrayList;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;

@Entity
public class MultipleChoiceResponse{


    //-----------------------------------------------------------------------
    @Id @GeneratedValue(strategy = GenerationType.AUTO) private Long id;

    //container for the question set
    private String question;

    //Array of the different options presented to the user
    @Lob private ArrayList<String> multipleChoiceOptions;

    //Array of the user response
    @Lob private ArrayList<Integer> multipleChoiceAnswers;

    private final ResponseType responseType = ResponseType.MULTIPLE_CHOICE;

    //Needed for JPA puposes
    @SuppressWarnings("unused")
	private MultipleChoiceResponse(){
    }

    public MultipleChoiceResponse(String question, ArrayList<String> multipleChoiceOptions){
        this.question = question;
        this.multipleChoiceOptions = multipleChoiceOptions;
        this.multipleChoiceAnswers = new ArrayList<Integer>(this.multipleChoiceOptions.size());
        for(String s : multipleChoiceOptions){
            multipleChoiceAnswers.add(0);
        }
    }
    
    
    public String getQuestion() {
        return this.question;
    }

    public ArrayList<String> getMutltipleChoiceOptions(){
        return this.multipleChoiceOptions;
    }

    public ArrayList<Integer> getMutltipleChoiceAnswers(){
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
        return this.responseType;
    }

}

