package qova.objects;

import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

import jdk.jfr.BooleanFlag;
import qova.enums.ResponseType;

@Entity
@Inheritance(strategy = InheritanceType.JOINED)
@OnDelete(action = OnDeleteAction.CASCADE)
public class AbstractResponse {

    @Id
    @GeneratedValue(generator = "UUID")
    @GenericGenerator(name = "UUID", strategy = "org.hibernate.id.UUIDGenerator")
    @Column(name = "id", updatable = false, nullable = false, columnDefinition = "BINARY(16)")
    private UUID id;

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
     * @param surveyPosition    The position in the survey, that this question is
     *                          positioned at.
     * @param responseType      The {@linkplain qova.enums.ResponseType} of the
     *                          response object
     * @param isDefaultQuestion Flag used to indicate wether the Response is one of
     *                          the questions set in the default survey
     */
    public AbstractResponse(Integer surveyPosition, ResponseType responseType, Boolean isDefaultQuestion) {
        this.surveyPosition = surveyPosition;
        this.responseType = responseType;
        this.isDefaultQuestionFlag = isDefaultQuestion;
    }

    public UUID getId() {
        return this.id;
    }

    public ResponseType getType() {
        return this.responseType;
    }

    public Integer getSurveyPosition() {
        return this.surveyPosition;
    }

    public Boolean getIsDefaultQuestion() {
        return this.isDefaultQuestionFlag;
    }

    public String getQuestion(SurveyResponse surveyResponse, Integer surveyPosition) {
        return surveyResponse.getCourse().getInstance(surveyResponse.getCourseType())
                .getQuestionTextForQuestionAtPosition(surveyPosition);

    }

}
