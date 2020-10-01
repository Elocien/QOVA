package qova.objects;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.ElementCollection;
import javax.persistence.Entity;

import qova.enums.ResponseType;

@Entity
public class TextResponse extends AbstractResponse {

    // -----------------------------------------------------------------------

    // Container for response
    @ElementCollection
    private List<String> responses = new ArrayList<>();

    // Needed for JPA puposes
    @SuppressWarnings("unused")
    protected TextResponse() {
    }

    public TextResponse(String question, Integer surveyPosition) {
        super(question, surveyPosition, ResponseType.TEXT_RESPONSE);
    }

    public List<String> getResponses() {
        return this.responses;
    }

    public void addTextSubmission(String resp) {
        this.responses.add(resp);
    }

}
