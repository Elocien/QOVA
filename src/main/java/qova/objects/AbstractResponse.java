package qova.objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;

import jdk.jfr.BooleanFlag;
import qova.enums.ResponseType;

@Entity
@Inheritance(strategy = InheritanceType.JOINED)
public class AbstractResponse {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;

    // container for the question set
    @Column(length = 1024)
    private String question;

    // Position in the survey
    private Integer surveyPosition;

    @BooleanFlag
    private Boolean isDefaultQuestionFlag;

    /**
     * see {@linkplain qova.enums.ResponseType}
     */
    @Enumerated
    private ResponseType responseType;

    // Needed for JPA puposes
    @SuppressWarnings("unused")
    protected AbstractResponse() {
    }

    /**
     * An abstract version of a Response, used to inherit from. An
     * {@linkplain qova.objects.AbstractResponse} is
     * 
     * @param question       The question corresponding to the response of the
     *                       student.
     * @param surveyPosition The position in the survey, that this question is
     *                       positioned at.
     * @param responseType   The {@linkplain qova.enums.ResponseType} of the
     *                       response object
     */
    public AbstractResponse(String question, Integer surveyPosition, ResponseType responseType,
            Boolean isDefaultQuestion) {
        this.question = question;
        this.surveyPosition = surveyPosition;
        this.responseType = responseType;
        this.isDefaultQuestionFlag = isDefaultQuestion;
    }

    public Long getId() {
        return this.id;
    }

    public ResponseType getType() {
        return this.responseType;
    }

    public String getQuestion() {
        return this.question;
    }

    public Integer getSurveyPosition() {
        return this.surveyPosition;
    }

    public Boolean getIsDefaultQuestion() {
        return this.isDefaultQuestionFlag;
    }

}
