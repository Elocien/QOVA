package qova.objects;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import qova.enums.ResponseType;

@Entity
public class SingleChoiceResponse extends AbstractResponse {

    // ----------------------------------------------------------------------

    // Array of the different options presented to the user
    @ElementCollection
    private List<String> singleChoiceOptions;

    // Array of the user response
    @ElementCollection
    private List<Integer> singleChoiceAnswers;

    // Needed for JPA puposes
    @SuppressWarnings("unused")
    protected SingleChoiceResponse() {
    }

    public SingleChoiceResponse(String question, Integer surveyPosition, List<String> singleChoiceOptions) {
        super(question, surveyPosition, ResponseType.SINGLE_CHOICE);
        this.singleChoiceOptions = singleChoiceOptions;
        this.singleChoiceAnswers = new ArrayList<>(this.singleChoiceOptions.size());

        // Populate the array
        for (String s : singleChoiceOptions) {
            singleChoiceAnswers.add(0);
        }
    }

    public List<String> getSingleChoiceOptions() {
        return this.singleChoiceOptions;
    }

    public List<Integer> getSingleChoiceAnswers() {
        return this.singleChoiceAnswers;
    }

    public void incrementTotal(Integer pos) {
        Integer totalAtPosition = singleChoiceAnswers.get(pos) + 1;
        singleChoiceAnswers.set(pos, totalAtPosition);
    }

    public Integer getNumberOfOptions() {
        return this.singleChoiceOptions.size();
    }
}
