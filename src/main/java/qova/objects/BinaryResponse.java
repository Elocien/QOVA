package qova.objects;

import javax.persistence.Entity;

import qova.enums.ResponseType;

@Entity
public class BinaryResponse extends AbstractResponse {

    // -----------------------------------------------------------------------

    // Container for response
    private Integer yesTotal = 0;
    private Integer noTotal = 0;

    // Needed for JPA puposes
    @SuppressWarnings("unused")
    protected BinaryResponse() {
    }

    /**
     * Implementation of an {@linkplain qova.objects.AbstractResponse}. A
     * {@linkplain qova.objects.BinaryResponse} has only two options: yes or no. The
     * variables yesTotal and noTotal are used to tally the users responses.
     * 
     * @param question       The question corresponding to the yes/no answer.
     * @param surveyPosition The position in the survey, that this question is
     *                       positioned at.
     */
    public BinaryResponse(Integer surveyPosition, Boolean isDefaultQuestion) {
        super(surveyPosition, ResponseType.BINARY_ANSWER, isDefaultQuestion);
    }

    /**
     * Return the total amount of respondents that answered yes to the set question,
     * as a String. This primary use for this function is in the pdf and csv
     * generator.
     * 
     * @return yesTotal as String
     */
    public String getYesTotalString() {
        return String.valueOf(this.yesTotal);
    }

    /**
     * Identical to {@code getYesTotalString()}, but for the noTotal
     * 
     * @return noTotal as String
     */
    public String getNoTotalString() {
        return String.valueOf(this.noTotal);
    }

    /**
     * Return the total amount of respondents that answered yes to the set question.
     * 
     * @return Integer
     */
    public Integer getYesTotal() {
        return this.yesTotal;
    }

    /**
     * Return the total amount of respondents that answered no to the set question.
     * 
     * @return Integer
     */
    public Integer getNoTotal() {
        return this.noTotal;
    }

    /**
     * The total amount of responses
     * 
     * @return Integer
     */
    public Integer getTotal() {
        return this.noTotal + this.yesTotal;
    }

    /**
     * Increments the {@code yesTotal} attribute
     */
    public void incrementYes() {
        this.yesTotal = yesTotal + 1;
    }

    /**
     * Increments the {@code noTotal} attribute
     */
    public void incrementNo() {
        this.noTotal = noTotal + 1;
    }

}