package qova.objects;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.ElementCollection;
import javax.persistence.Entity;

import org.json.JSONArray;

import qova.enums.ResponseType;

@Entity
public class MultipleChoiceResponse extends AbstractResponse {

    // Array of the user response
    @ElementCollection
    private List<Integer> multipleChoiceAnswers;

    private Integer numberOfAnswerPossibilities;

    // Needed for JPA puposes
    @SuppressWarnings("unused")
    protected MultipleChoiceResponse() {
    }

    public MultipleChoiceResponse(Integer surveyPosition, Integer numberOfAnswerPossibilities,
            Boolean isDefaultQuestion) {
        super(surveyPosition, ResponseType.MULTIPLE_CHOICE, isDefaultQuestion);
        this.numberOfAnswerPossibilities = numberOfAnswerPossibilities;
        this.multipleChoiceAnswers = new ArrayList<>();
        for (int i = 0; i < numberOfAnswerPossibilities; i++) {
            multipleChoiceAnswers.add(0);
        }
    }

    public List<Integer> getMultipleChoiceAnswers() {
        return this.multipleChoiceAnswers;
    }

    public void incrementTotals(JSONArray jsonArray) {
        for (int i = 0; i < jsonArray.length(); i++) {
            int position = jsonArray.getInt(i);
            multipleChoiceAnswers.set(position, multipleChoiceAnswers.get(position) + 1);
        }
    }

    public Integer getNumberOfAnswerPossibilites() {
        return this.numberOfAnswerPossibilities;
    }

}
