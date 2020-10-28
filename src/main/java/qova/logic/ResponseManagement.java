package qova.logic;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import qova.enums.CourseType;
import qova.enums.LocalizationOption;
import qova.enums.ResponseType;
import qova.objects.AbstractResponse;
import qova.objects.BinaryResponse;
import qova.objects.Course;
import qova.objects.CourseInstance;
import qova.objects.MultipleChoiceResponse;
import qova.objects.SingleChoiceResponse;
import qova.objects.SurveyResponse;
import qova.objects.TextResponse;
import qova.repositories.BinaryResponseRepository;
import qova.repositories.MultipleChoiceResponseRepository;
import qova.repositories.SingleChoiceResponseRepository;
import qova.repositories.SurveyResponseRepository;
import qova.repositories.TextResponseRepository;

@Service
@Transactional
public class ResponseManagement {

    private final SurveyResponseRepository surveyResponseRepository;

    private final BinaryResponseRepository binaryResponseRepository;

    private final TextResponseRepository textResponseRepository;

    private final SingleChoiceResponseRepository singleChoiceResponseRepository;

    private final MultipleChoiceResponseRepository multipleChoiceResponseRepository;

    @Autowired
    public ResponseManagement(SurveyResponseRepository surveyResponseRepository,
            BinaryResponseRepository binaryResponseRepository, TextResponseRepository textResponseRepository,
            SingleChoiceResponseRepository singleChoiceResponseRepository,
            MultipleChoiceResponseRepository multipleChoiceResponseRepository) {
        this.surveyResponseRepository = Objects.requireNonNull(surveyResponseRepository);
        this.binaryResponseRepository = Objects.requireNonNull(binaryResponseRepository);
        this.textResponseRepository = Objects.requireNonNull(textResponseRepository);
        this.singleChoiceResponseRepository = Objects.requireNonNull(singleChoiceResponseRepository);
        this.multipleChoiceResponseRepository = Objects.requireNonNull(multipleChoiceResponseRepository);
    }

    public CourseType parseCourseType(String stringType) {

        CourseType type;

        switch (stringType) {
            case "LECTURE":
                type = CourseType.LECTURE;
                break;
            case "TUTORIAL":
                type = CourseType.TUTORIAL;
                break;
            case "SEMINAR":
                type = CourseType.SEMINAR;
                break;
            case "PRACTICAL":
                type = CourseType.PRACTICAL;
                break;
            default:
                type = null;
                break;
        }

        return type;
    }

    public ResponseType parseResponseType(Object rsp) {
        if (rsp instanceof qova.objects.BinaryResponse) {
            return ResponseType.BINARY_ANSWER;
        } else if (rsp instanceof qova.objects.TextResponse) {
            return ResponseType.TEXT_RESPONSE;
        } else if (rsp instanceof qova.objects.SingleChoiceResponse) {
            return ResponseType.SINGLE_CHOICE;
        } else if (rsp instanceof qova.objects.MultipleChoiceResponse) {
            return ResponseType.MULTIPLE_CHOICE;
        } else {
            return null;
        }
    }

    // PDF Generation (ENGLISH)
    public byte[] generatePDFEnglish(Course course, CourseType type, Integer groupNumber, Integer instanceNumber)
            throws Exception {

        // retrieve the SurveyResponse object from repository
        Optional<SurveyResponse> rsp = surveyResponseRepository
                .findByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(course, type, groupNumber, instanceNumber);

        // Generate PDF
        PDFGenerator pdfGen = new PDFGenerator();
        return pdfGen.createPdf(rsp.get(), LocalizationOption.EN);
    }

    /**
     * Generates a CSV file of out of the student responses compiled in the
     * {@linkplain qova.objects.SurveyResponse}
     *
     * @param listOfSurveyResponses A {@link java.util.List} of
     *                              {@linkplain qova.objects.SurveyResponse}
     * @return a ayte[] containing the CSV
     * @throws Exception WriterException from the csvGen
     */
    public byte[] generateCSVEnglish(List<SurveyResponse> listOfSurveyResponses) throws Exception {

        if (listOfSurveyResponses.isEmpty()) {
            return new byte[0];
        }

        // Generate PDF
        CSVGenerator csvGen = new CSVGenerator();
        return csvGen.createCSV(listOfSurveyResponses, LocalizationOption.EN);
    }

    /**
     * Verifies that the length of the submitted JSONArray does not exceed 100
     * elements, as the number of questions per survey is limited to 100;
     *
     * @param jsonArray A {@link org.json.JSONArray}
     * @return A boolean flag, which is handled in the controller
     */
    public Boolean verifyJsonArray(JSONArray jsonArray) {

        // check for too large surveys (more than 100 quesitons)
        if (jsonArray.length() > 100) {
            return false;
        }

        for (int i = 0; i < jsonArray.length(); i++) {
            JSONObject question = jsonArray.getJSONObject(i);
            if (question.getString("question").length() > 1024) {
                return false;
            }
        }

        // TODO: Check for other malicious strings

        // example string
        // [{"type":"YesNo","question":""},{"type":"MultipleChoice","question":"","answers":["1","2","3","4","5"]},{"type":"DropDown","question":"","answers":["Answer","Answer","Answer"]}]

        return true;
    }

    /**
     * Verifies the response of the student. Also makes sure that all quesitons were
     * answered
     *
     * @param studentResponseJson A {@link org.json.JSONArray}
     * @param surveyAsString      The survey String that is saved in a
     *                            {@linkplain qova.objects.CourseInstance}
     * @return A boolean flag, which is handled in the controller
     */
    public Boolean verifyStudentResponseJson(JSONArray studentResponseJson, String surveyAsString) {

        try {
            int surveyArrayLength = new JSONArray(surveyAsString).length();

            //Check that the length of the JSON sent matches the length of the survey, as for each survey question, one answer
            //must be sent.
            if (studentResponseJson.length() == surveyArrayLength) {
                return true;
            }
        } catch (Exception e) {
            return false;
        }

        // TODO: Check for other malicious strings

        return false;
    }

    public Integer getTotalResponses(List<SurveyResponse> listOfSurveyResponses){
        Integer totalNumberOfResponses = 0;
        for (SurveyResponse surveyResponse : listOfSurveyResponses){
            totalNumberOfResponses += surveyResponse.getNumberOfSubmissions();
        }
        return totalNumberOfResponses;
    }

    /**
     * Used in the finalisation process of a {@linkplain qova.objects.Course}. When
     * a User finalises a Course, this method is used to serialise the
     * {@linkplain SurveyResponse}s, as well as all
     * {@linkplain qova.objects.AbstractResponse} needed to persist students
     * answers.
     *
     * @param jsonArray json.org.JSONArray containing the JSON string representing
     *                  the created survey
     * @param course    {@linkplain Course}
     * @param type      {@linkplain CourseType} as a String, passed from the model
     */
    public void createSurveyResponse(JSONArray jsonArray, Course course, CourseType type) {

        // Resolve type and find correct instance of course (lecture, tutorial, etc.)
        CourseInstance courseInstance = course.getInstance(type);

        int groupAmount = courseInstance.getGroupAmount();
        int instanceAmount = courseInstance.getInstanceAmount();

        List<List<AbstractResponse>> listOfResponses = generateResponseListFromJsonArray(jsonArray,
                groupAmount * instanceAmount);

        // The list conating all SurveyResponses
        List<SurveyResponse> listOfSurveyResponses = new ArrayList<>();

        // Used to iterate through the listOfResponses
        int listposition = 0;

        // for each instance and group, create a SurveyResponse and persist it
        for (int group = 1; group <= groupAmount; group++) {
            for (int instance = 1; instance <= instanceAmount; instance++) {

                SurveyResponse response = new SurveyResponse(course, type, instance, group,
                        listOfResponses.get(listposition));
                listposition++;

                // save the new response
                listOfSurveyResponses.add(response);
            }
        }

        surveyResponseRepository.saveAll(listOfSurveyResponses);
    }

    /**
     * The method parses the json generated by the questioneditor. The json
     * represents the default survey, concatenated with the survey created by the
     * course owner. This method takes care of serialising the Response Objects, as
     * well as persisting them. It creates a list containing sublists of
     * {@linkplain qova.objects.AbstractResponse}. Each sublist is then passed to a
     * {@linkplain qova.objects.SurveyResponse}
     *
     * @param jsonArray      An {@link org.json.JSONArray} containing the survey
     *                       generated by the questioneditor
     * @param numberOfCopies The field indicates how often the ResponseObject needs
     *                       to be created and persisted. The number is a multiple
     *                       of group and instance amount, because this is the
     *                       amount of SurveyResponses, that must be created.
     * @return A {@link java.util.List} of {@link java.util.List}'s, which contain
     *         {@linkplain qova.objects.AbstractResponse}
     */
    public List<List<AbstractResponse>> generateResponseListFromJsonArray(JSONArray jsonArray, Integer numberOfCopies) {

        List<List<AbstractResponse>> listOfResponses = new ArrayList<>();
        for (int i = 0; i < numberOfCopies; i++) {
            listOfResponses.add(new ArrayList<>());
        }

        // parse json to serialise response objects
        for (int surveyPosition = 0; surveyPosition < jsonArray.length(); surveyPosition++) {

            // The Json Object containing all of the data for the quesiton at the given
            // position
            JSONObject jsonObject = jsonArray.getJSONObject(surveyPosition);

            // Variables used to set Attributes
            Boolean defaultQuestionFlag;
            Integer numberOfAnswerPossibilities;

            switch (jsonObject.getString("type")) {
                case "YesNo":

                    // The values taken by the BinaryResponse
                    try {
                        defaultQuestionFlag = jsonObject.getBoolean("default");
                    } catch (Exception e) {
                        defaultQuestionFlag = false;
                    }

                    for (int sublistPosition = 0; sublistPosition < numberOfCopies; sublistPosition++) {
                        BinaryResponse br = new BinaryResponse(surveyPosition, defaultQuestionFlag);
                        binaryResponseRepository.save(br);
                        listOfResponses.get(sublistPosition).add(br);
                    }
                    break;

                case "FreeText":
                    try {
                        defaultQuestionFlag = jsonObject.getBoolean("default");
                    } catch (Exception e) {
                        defaultQuestionFlag = false;
                    }

                    for (int sublistPosition = 0; sublistPosition < numberOfCopies; sublistPosition++) {
                        TextResponse tr = new TextResponse(surveyPosition, defaultQuestionFlag);
                        textResponseRepository.save(tr);
                        listOfResponses.get(sublistPosition).add(tr);
                    }
                    break;

                case "MultipleChoice":
                    // Array of all possibilities
                    numberOfAnswerPossibilities = jsonObject.getJSONArray("answers").length();
                    try {
                        defaultQuestionFlag = jsonObject.getBoolean("default");
                    } catch (Exception e) {
                        defaultQuestionFlag = false;
                    }

                    for (int sublistPosition = 0; sublistPosition < numberOfCopies; sublistPosition++) {
                        MultipleChoiceResponse mcr = new MultipleChoiceResponse(surveyPosition,
                                numberOfAnswerPossibilities, defaultQuestionFlag);
                        multipleChoiceResponseRepository.save(mcr);
                        listOfResponses.get(sublistPosition).add(mcr);
                    }
                    break;

                case "SingleChoice":
                    // Array of all possibilities
                    numberOfAnswerPossibilities = jsonObject.getJSONArray("answers").length();
                    try {
                        defaultQuestionFlag = jsonObject.getBoolean("default");
                    } catch (Exception e) {
                        defaultQuestionFlag = false;
                    }

                    for (int sublistPosition = 0; sublistPosition < numberOfCopies; sublistPosition++) {
                        SingleChoiceResponse scr = new SingleChoiceResponse(surveyPosition, numberOfAnswerPossibilities,
                                defaultQuestionFlag);
                        singleChoiceResponseRepository.save(scr);
                        listOfResponses.get(sublistPosition).add(scr);
                    }
                    break;

                case "OnetoFive":
                    // Array of all possibilities
                    numberOfAnswerPossibilities = 5;
                    try {
                        defaultQuestionFlag = jsonObject.getBoolean("default");
                    } catch (Exception e) {
                        defaultQuestionFlag = false;
                    }

                    for (int sublistPosition = 0; sublistPosition < numberOfCopies; sublistPosition++) {
                        SingleChoiceResponse scr = new SingleChoiceResponse(surveyPosition, numberOfAnswerPossibilities,
                                defaultQuestionFlag);
                        singleChoiceResponseRepository.save(scr);
                        listOfResponses.get(sublistPosition).add(scr);
                    }
                    break;

                default:
                    break;
            }
        }
        return listOfResponses;
    }

    /**
     * This function takes a list containing the surveyResponses selected by a user, and generates
     * the {@linkplain org.json.JSONArray} containing the compiled results. The JSON is of the following structure:
     * <br>
     * <br>
     * <h2>JSON Structure</h2>
     * <br>
     * [{"type": STRING, "default": bool, "question": STRING, "options": [STRING, STRING, ...], "answers": [STRING, STRING]}
     * <br>
     * <br>
     * It makes use of {@link #generateJsonResultsArray(SurveyResponse)} to generate the Structure of the array, and then populates it
     * with the results. After population, the array is then iterated through, and the answers array is replaced by decimal values, to indicate percentages
     *
     *
     * @param listOfSurveyResponses A list of {@linkplain qova.objects.SurveyResponse}'s, which is compiled based on which results
     *                              the user has selected to see
     * @return A {@linkplain JSONArray} containing the students compiled responses
     */
    public JSONArray generateSurveyResultsJson(List<SurveyResponse> listOfSurveyResponses) {


        // A SurveyResponse object. All surveyResponses have the same survey and the
        // same ListOfResponses, because they belong to the same CourseInstance
        SurveyResponse response = listOfSurveyResponses.get(0);

        // Generate the resultsArray Structure
        JSONArray resultsArray = generateJsonResultsArray(response);

        // Iterate through an populate JSON Objects with data
        for (SurveyResponse surveyResponse : listOfSurveyResponses) {

            // Iterate through all abstract response, for the current SurveyResponse
            for (int surveyPosition = 0; surveyPosition < surveyResponse.getListOfResponses().size(); surveyPosition++) {

                AbstractResponse ar = surveyResponse.getListOfResponses().get(surveyPosition);

                // JSONObject of the current AbstractResponse
                JSONObject jsonObject = resultsArray.getJSONObject(surveyPosition);

                if (ar instanceof BinaryResponse) {

                    // Get the BinaryResponse
                    BinaryResponse br = (BinaryResponse) ar;

                    // Get the JSONArray containing the yes- and noTotals, and increment the values
                    JSONArray binaryAnswersArray = jsonObject.getJSONArray("answers");

                    binaryAnswersArray.put(0, binaryAnswersArray.getInt(0) + ((BinaryResponse) ar).getYesTotal());

                    binaryAnswersArray.put(1, binaryAnswersArray.getInt(1) + ((BinaryResponse) ar).getNoTotal());

                    jsonObject.put("answers", binaryAnswersArray);

                }
                if (ar instanceof TextResponse) {
                    // Get the TextResponse
                    TextResponse tr = (TextResponse) ar;

                    // Get the JSONArray containing user text answers
                    for (String userTextAnswer : tr.getResponses()) {
                        jsonObject.accumulate("answers", userTextAnswer);
                    }
                }
                if (ar instanceof SingleChoiceResponse) {
                    // Get the SingleChoiceResponse
                    SingleChoiceResponse scr = (SingleChoiceResponse) ar;

                    // Get the JSONArray containing the accumulation of user answers
                    JSONArray singleChoiceAnswersArray = jsonObject.getJSONArray("answers");

                    // The List of user
                    List<Integer> singleChoiceAnswers = scr.getSingleChoiceAnswers();

                    for (int i = 0; i < scr.getNumberOfAnswerPossibilites(); i++) {
                        try {
                            singleChoiceAnswersArray.put(i,
                                    (singleChoiceAnswersArray.getInt(i) + singleChoiceAnswers.get(i)));
                        } catch (JSONException e) {
                            e.printStackTrace();
                        }
                    }
                    jsonObject.put("answers", singleChoiceAnswersArray);
                }
                if (ar instanceof MultipleChoiceResponse) {
                    // Get the MultipleChoiceResponse
                    MultipleChoiceResponse mcr = (MultipleChoiceResponse) ar;

                    // Get the JSONArray containing the accumulation of user answers
                    JSONArray multipleChoiceAnswersArray = jsonObject.getJSONArray("answers");

                    // The List of user
                    List<Integer> multipleChoiceAnswersChoiceAnswers = mcr.getMultipleChoiceAnswers();

                    for (int i = 0; i < mcr.getNumberOfAnswerPossibilites(); i++) {
                        try {
                            multipleChoiceAnswersArray.put(i,
                                    multipleChoiceAnswersArray.getInt(i) + multipleChoiceAnswersChoiceAnswers.get(i));
                        } catch (JSONException e) {
                            e.printStackTrace();
                        }
                    }
                    jsonObject.put("answers", multipleChoiceAnswersArray);
                }
            }
        }



        return resultsArray;
    }

    private JSONArray generateJsonResultsArray(SurveyResponse response) {

        // Initialise Array
        JSONArray resultsArray = new JSONArray();

        int surveyPosition = 0;

        // Generate JSON Objects for populating
        for (AbstractResponse abstractResponse : response.getListOfResponses()) {
            JSONObject jsonResponseObject = new JSONObject();
            jsonResponseObject.append("default", abstractResponse.getIsDefaultQuestion());
            jsonResponseObject.append("question", response.getQuestionTextForQuestionAtPosition(surveyPosition));

            if (abstractResponse instanceof BinaryResponse) {
                jsonResponseObject.append("type", "binary");
                JSONArray binaryOptionsArray = new JSONArray(new ArrayList<>(Arrays.asList("yesTotal", "noTotal")));
                jsonResponseObject.put("options", binaryOptionsArray);
                jsonResponseObject.put("answers", new JSONArray(Arrays.asList(0, 0)));
            }

            if (abstractResponse instanceof TextResponse) {
                jsonResponseObject.put("type", "text");
                jsonResponseObject.put("options", new JSONArray());
                jsonResponseObject.put("answers", new JSONArray());
            }

            if (abstractResponse instanceof SingleChoiceResponse) {
                // The single choice response
                SingleChoiceResponse scr = (SingleChoiceResponse) abstractResponse;

                jsonResponseObject.put("type", "singleChoice");
                JSONArray singleChoiceOptionsArray = new JSONArray(
                        response.getOptionsForResponseAtPosition(surveyPosition));
                jsonResponseObject.put("options", singleChoiceOptionsArray);
                JSONArray answersArray = new JSONArray();
                for (int i = 0; i < scr.getNumberOfAnswerPossibilites(); i++) {
                    answersArray.put(i, 0);
                }

                jsonResponseObject.put("answers", answersArray);
            }

            if (abstractResponse instanceof MultipleChoiceResponse) {
                // The multiple choice response
                MultipleChoiceResponse mcr = (MultipleChoiceResponse) abstractResponse;

                jsonResponseObject.put("type", "multipleChoice");
                JSONArray multipleChoiceOptionsArray = new JSONArray(
                        response.getOptionsForResponseAtPosition(surveyPosition));
                jsonResponseObject.put("options", multipleChoiceOptionsArray);
                JSONArray answersArray = new JSONArray();
                for (int i = 0; i < mcr.getNumberOfAnswerPossibilites(); i++) {
                    answersArray.put(i, 0);
                }

                jsonResponseObject.put("answers", answersArray);
            }

            resultsArray.put(surveyPosition, jsonResponseObject);
            surveyPosition++;
        }

        return resultsArray;
    }

    public List<SurveyResponse> findSurveyResponses(Course course, CourseType type, String groupNumber,
            String instanceNumber) {

        // Initalise List of SurveyResponses to be passed to the CSV generator
        List<SurveyResponse> listOfSurveyResponses = new ArrayList<>();

        if (groupNumber.equals("all") && instanceNumber.equals("all")) {

            findSurveyResponseByCourseAndCourseType(course, type).forEach(listOfSurveyResponses::add);

        } else if (groupNumber.equals("all")) {
            findSurveyResponseByCourseAndCourseTypeAndInstanceNumber(course, type, Integer.parseInt(instanceNumber))
                    .forEach(listOfSurveyResponses::add);
        } else if (instanceNumber.equals("all")) {
            findSurveyResponseByCourseAndCourseTypeAndGroupNumber(course, type, Integer.parseInt(groupNumber))
                    .forEach(listOfSurveyResponses::add);
        } else {
            Optional<SurveyResponse> s = findSurveyResponseByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(course,
                    type, Integer.parseInt(groupNumber), Integer.parseInt(instanceNumber));
            listOfSurveyResponses.add(s.get());
        }

        return listOfSurveyResponses;
    }

    public void submitStudentResponse(SurveyResponse surveyResponse, JSONArray studentResponseJson) {

        for (int surveyPosition = 0; surveyPosition < surveyResponse.getListOfResponses().size(); surveyPosition++) {

            // The current abstractResponse
            AbstractResponse abstractResponse = surveyResponse.getListOfResponses().get(surveyPosition);

            // The current JSONArray
            JSONArray currentPositionArray = studentResponseJson.getJSONArray(surveyPosition);

            if (abstractResponse instanceof qova.objects.BinaryResponse) {
                if (currentPositionArray.getInt(0) == 0) {
                    ((BinaryResponse) abstractResponse).incrementYes();
                } else {
                    ((BinaryResponse) abstractResponse).incrementNo();
                }
            }

            if (abstractResponse instanceof qova.objects.TextResponse) {
                ((TextResponse) abstractResponse).addTextSubmission(currentPositionArray.getString(0));
            }

            if (abstractResponse instanceof qova.objects.SingleChoiceResponse) {
                ((SingleChoiceResponse) abstractResponse).incrementTotal(currentPositionArray.getInt(0));
            }

            if (abstractResponse instanceof qova.objects.MultipleChoiceResponse) {
                ((MultipleChoiceResponse) abstractResponse).incrementTotals(currentPositionArray);
            }
        }
    }

    /**
     * @param id the response id
     * @return an {@linkplain Optional} of an {@linkplain SurveyResponse} with the
     *         given id
     */
    public Optional<SurveyResponse> findSurveyResponseById(UUID id) {
        return surveyResponseRepository.findById(id);
    }

    /**
     * @param course         {@linkplain Course} object
     * @param type           {@linkplain CourseType}
     * @param groupNumber    The number of the corresponding tutorial or seminar (=1
     *                       in case CourseType is Lecture)
     * @param instanceNumber The number corresponding to the instance of the Course.
     * @return an Iterable containing all Responses that fit criteria
     */
    public Optional<SurveyResponse> findSurveyResponseByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(
            Course course, CourseType type, Integer groupNumber, Integer instanceNumber) {
        return surveyResponseRepository.findByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(course, type,
                groupNumber, instanceNumber);
    }

    /**
     * @param course      {@linkplain Course} object
     * @param type        {@linkplain CourseType}
     * @param groupNumber The number of the corresponding tutorial or seminar (=1 in
     *                    case CourseType is Lecture)
     * @return an Iterable containing all Responses that fit criteria
     */
    public Iterable<SurveyResponse> findSurveyResponseByCourseAndCourseTypeAndGroupNumber(Course course,
            CourseType type, Integer groupNumber) {
        return surveyResponseRepository.findByCourseAndCourseTypeAndGroupNumber(course, type, groupNumber);
    }

    /**
     * @param course         {@linkplain Course} object
     * @param type           {@linkplain CourseType}
     * @param instanceNumber The number corresponding to the instance of the Course.
     * @return an Iterable containing all Responses that fit criteria
     */
    public Iterable<SurveyResponse> findSurveyResponseByCourseAndCourseTypeAndInstanceNumber(Course course,
            CourseType type, Integer instanceNumber) {
        return surveyResponseRepository.findByCourseAndCourseTypeAndInstanceNumber(course, type, instanceNumber);
    }

    /**
     * @param course {@linkplain Course} object
     * @param type   {@linkplain CourseType}
     * @return an Iterable containing all Responses that fit criteria
     */
    public Iterable<SurveyResponse> findSurveyResponseByCourseAndCourseType(Course course, CourseType type) {
        return surveyResponseRepository.findByCourseAndCourseType(course, type);
    }

    // TEST METHODS
    //
    //
    //
    //
    //
    //
    //
    //
    //
    //
    //
    //
    //
    //
    //
    // Test Methods, remove in build
    public void createTestResponses(Course course) {

        String surveyJson = "[{\"type\":\"OnetoFive\",\"question\":\"Hat die Übung Wissen vermittelt, welches du dir nicht im Selbststudium hättest erarbeiten können?\"},{\"type\":\"OnetoFive\",\"question\":\"Hat der/die Leiter/in den aktiven Austausch mit den Studierenden gesucht?\"},{\"type\":\"OnetoFive\",\"question\":\"Waren die Anforderung dem Wissensstand der Studierenden angemessen?\"},{\"type\":\"OnetoFive\",\"question\":\"Konnte die Übung gezielt Schwerpunkte setzen und Struktur vermitteln?\"},{\"type\":\"OnetoFive\",\"question\":\"Konnte der/die Leiter/in dein Interesse an dem Thema wecken?\"},{\"type\":\"OnetoFive\",\"question\":\"Hat der/die Leiter/in die Möglichkeiten einer Übung gegenüber der Vorlesung ausgeschöpft?\"},{\"type\":\"OnetoFive\",\"question\":\"Online Lehre v.s. Präsenzveranstaltung\",\"answers\":[\"Die Übung war digital und soll digital bleiben.\",\"Die Übung war digital und wäre als Präsenzveranstaltung besser.\",\"Die Übung war eine Präsenzveranstaltung und soll eine bleiben.\",\"Die Übung war eine Präsenzveranstaltung und sollte digital werden.\"]},{\"type\":\"FreeText\",\"question\":\"An dieser Stelle würden wir uns über konstruktive Kritik, aber auch über Anregungen und Lob freuen!\"}]";

        createSurveyResponse(new JSONArray(surveyJson), course, CourseType.TUTORIAL);

        Iterable<SurveyResponse> responses = findSurveyResponseByCourseAndCourseType(course, CourseType.TUTORIAL);

        List<Integer> Answers1 = new ArrayList<>();
        Answers1.add(0);
        Answers1.add(1);
        Answers1.add(3);
        JSONArray mcAnswers1 = new JSONArray(Answers1);

        for (SurveyResponse resp : responses) {
            for (int i = 0; i < 50; i++) {
                String id = UUID.randomUUID().toString();
                resp.addStundentIdToSubmissionListAndIncrementCounter(id);
                List<AbstractResponse> listOfResponses = resp.getListOfResponses();
                for (AbstractResponse ar : listOfResponses) {
                    if (ar instanceof BinaryResponse) {
                        if (i % 3 == 0) {
                            ((BinaryResponse) ar).incrementNo();
                        } else {
                            ((BinaryResponse) ar).incrementYes();
                        }
                    }
                    if (ar instanceof TextResponse) {
                        ((TextResponse) ar).addTextSubmission("This is a test Response");
                    }
                    if (ar instanceof SingleChoiceResponse) {
                        ((SingleChoiceResponse) ar).incrementTotal(i % 5);
                    }
                    if (ar instanceof MultipleChoiceResponse) {
                        ((MultipleChoiceResponse) ar).incrementTotals(mcAnswers1);
                    }
                }
            }
        }

    }

}