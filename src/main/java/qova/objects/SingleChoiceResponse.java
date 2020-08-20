package qova.objects;

import java.util.ArrayList;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;

import qova.enums.ResponseType;

@Entity
public class SingleChoiceResponse {


    //-----------------------------------------------------------------------
    @Id @GeneratedValue(strategy = GenerationType.AUTO) private Long id;

    //container for the question set
    private String question;

    //Array of the different options presented to the user
    @Lob private ArrayList<String> singleChoiceOptions;

    //Array of the user response
    @Lob private ArrayList<Integer> singleChoiceAnswers;

    private final ResponseType responseType = ResponseType.SINGLE_CHOICE;

    //Needed for JPA puposes
    @SuppressWarnings("unused")
	private SingleChoiceResponse(){
    }

    public SingleChoiceResponse(String question, ArrayList<String> singleChoiceOptions){
        this.question = question;
        this.singleChoiceOptions = singleChoiceOptions;
        this.singleChoiceAnswers = new ArrayList<Integer>(this.singleChoiceOptions.size());
        for(String s : singleChoiceOptions){
            singleChoiceAnswers.add(0);
        }
    }
    
    
    public String getQuestion() {
        return this.question;
    }

    public ArrayList<String> getSingleChoiceOptions(){
        return this.singleChoiceOptions;
    }

    public ArrayList<Integer> getSingleChoiceAnswers(){
        return this.singleChoiceAnswers;
    }

    public void incrementTotal(Integer pos){
        Integer totalAtPosition = singleChoiceAnswers.get(pos) + 1;
        singleChoiceAnswers.set(pos, totalAtPosition);
    }

    public Integer getNumberOfOptions(){
        return this.singleChoiceOptions.size();
    }

    public ResponseType getType(){
        return this.responseType;
    }
}


