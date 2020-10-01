package qova.objects;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.ElementCollection;
import javax.persistence.Entity;

import qova.enums.ResponseType;

@Entity
public class MultipleChoiceResponse extends AbstractResponse {

    // Array of the different options presented to the user
    @ElementCollection
    private List<String> multipleChoiceOptions;

    // Array of the user response
    @ElementCollection
    private List<Integer> multipleChoiceAnswers;

    // Needed for JPA puposes
    @SuppressWarnings("unused")
    protected MultipleChoiceResponse() {
    }

    public MultipleChoiceResponse(String question, Integer surveyPosition, List<String> multipleChoiceOptions) {
        super(question, surveyPosition, ResponseType.MULTIPLE_CHOICE);
        this.multipleChoiceOptions = multipleChoiceOptions;
        this.multipleChoiceAnswers = new ArrayList<>(this.multipleChoiceOptions.size());
        for (String s : multipleChoiceOptions) {
            multipleChoiceAnswers.add(0);
        }
    }

    public List<String> getMultipleChoiceOptions() {
        return this.multipleChoiceOptions;
    }

    public List<Integer> getMultipleChoiceAnswers() {
        return this.multipleChoiceAnswers;
    }

    public void incrementTotals(List<Integer> totals) {
        for (Integer i : totals) {
            multipleChoiceAnswers.set(i, multipleChoiceAnswers.get(i) + 1);
        }
    }

    public Integer getNumberOfOptions() {
        return this.multipleChoiceOptions.size();
    }

}
