package qova.responseTypes;

import java.util.ArrayList;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;

@Entity
public class SingleChoiceResponse{


    //-----------------------------------------------------------------------
    @Id @GeneratedValue(strategy = GenerationType.AUTO) private Long id;

    //container for the question set
    private String question;

    //Array of the different options presented to the user
    @Lob private ArrayList<String> singleChoiceOptions;

    //Array of the user response
    @Lob private ArrayList<Integer> singleChoiceAnswers;

    private final ResponseType responseType = ResponseType.MULTIPLE_CHOICE;

    //Needed for JPA puposes
    @SuppressWarnings("unused")
	private SingleChoiceResponse(){
    }

    public SingleChoiceResponse(String question, ArrayList<String> singleChoiceOptions){
        this.question = question;
        this.singleChoiceOptions = singleChoiceOptions;
        this.singleChoiceAnswers = new ArrayList<Integer>(singleChoiceOptions.size());
    }
    
    
    public String getQuestion() {
        return this.question;
    }

    public ArrayList<String> getMutltipleChoiceOptions(){
        return this.singleChoiceOptions;
    }

    public ArrayList<Integer> getMutltipleChoiceAnswers(){
        return this.singleChoiceAnswers;
    }

    public void incrementTotal(Integer pos){
        singleChoiceAnswers.set(pos, singleChoiceAnswers.get(pos) + 1);
    }

    public ResponseType getType(){
        return this.responseType;
    }
}


