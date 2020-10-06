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

    // Array of the user response
    @ElementCollection
    private List<Integer> singleChoiceAnswers;

    private Integer numberOfAnswerPossibilities;

    // Needed for JPA puposes
    @SuppressWarnings("unused")
    protected SingleChoiceResponse() {
    }

    public SingleChoiceResponse(Integer surveyPosition, Integer numberOfAnswerPossibilities,
            Boolean isDefaultQuestion) {
        super(surveyPosition, ResponseType.SINGLE_CHOICE, isDefaultQuestion);
        this.numberOfAnswerPossibilities = numberOfAnswerPossibilities;
        this.singleChoiceAnswers = new ArrayList<>();

        for (int i = 0; i < numberOfAnswerPossibilities; i++) {
            singleChoiceAnswers.add(0);
        }
    }

    public List<Integer> getMultipleChoiceAnswers() {
        return this.singleChoiceAnswers;
    }

    public void incrementTotals(List<Integer> totals) {
        for (Integer i : totals) {
            singleChoiceAnswers.set(i, singleChoiceAnswers.get(i) + 1);
        }
    }

    public Integer getNumberOfAnswerPossibilites() {
        return this.numberOfAnswerPossibilities;
    }

}
