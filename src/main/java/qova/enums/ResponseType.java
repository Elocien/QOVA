package qova.enums;

/**
 * <p>Enumeration describing the types of possible questions which can be set in a questionnaire, excluding <b>one-to-five</b> questions, as these are handled as
 * single choice quesitons in the backend, with possible answers being initialised as 1 through to 5. </p>
 *
 * <p>For clarification, binary answer questions are simply "yes or no" questions, but yesNoQuestion was a bit too clunky of a term to use</p>
 */
public enum ResponseType {
    MULTIPLE_CHOICE, SINGLE_CHOICE, TEXT_RESPONSE, BINARY_ANSWER;
}