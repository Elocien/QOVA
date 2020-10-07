package qova.logic;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import org.json.JSONArray;
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

        if (stringType.equals("LECTURE")) {
            type = CourseType.LECTURE;
        } else if (stringType.equals("TUTORIAL")) {
            type = CourseType.TUTORIAL;
        } else if (stringType.equals("SEMINAR")) {
            type = CourseType.SEMINAR;
        } else if (stringType.equals("PRACTICAL")) {
            type = CourseType.PRACTICAL;
        } else
            type = null;

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
     * This method retrieves all of the surveyResponses and compiles their results
     * into a csv file
     * 
     * @param course
     * @param type
     * @param groupNumber
     * @param instanceNumber
     * @return
     * @throws Exception
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
     * @param json {@link org.json.JSONArray}
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
     * Used in the finalisation process of a {@linkplain qova.objects.Course}. When
     * a User finalises a Course, this method is used to serialise the
     * {@linkplain SurveyResponse}s, as well as all
     * {@linkplain qova.objects.AbstractResponses} needed to persist students
     * answers.
     * 
     * @param jsonArray  json.org.JSONArray containing the JSON string representing
     *                   the created survey
     * @param course     {@linkplain Course}
     * @param stringType {@linkplain CourseType} as a String, passed from the model
     */
    public void createSurveyResponse(JSONArray jsonArray, Course course, CourseType type) {

        // Resolve type and find correct instance of course (lecture, tutorial, etc.)
        CourseInstance courseInstance = course.getInstance(type);

        Integer groupAmount = courseInstance.getGroupAmount();
        Integer instanceAmount = courseInstance.getInstanceAmount();

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
     * {@linkplain qova.objects.AbstracResponses}. Each sublist is then passed to a
     * {@linkplain qova.objects.SurveyResponse}
     * 
     * @param jsonArray      An {@link org.json.JSONArray} containing the survey
     *                       generated by the questioneditor
     * @param numberOfCopies The field indicates how often the ResponseObject needs
     *                       to be created and persisted. The number is a multiple
     *                       of group and instance amount, because this is the
     *                       amount of SurveyResponses, that must be created.
     * @return A {@link java.util.List} of {@link java.util.List}'s, which contain
     *         {@linkplain qova.objects.AbstractResponses}
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

    public JSONArray generateSurveyResultsJson(CourseInstance courseInstance,
            List<SurveyResponse> listOfSurveyResponses) {

        // Die CourseInstance hat eine methode,
        // getOptionsForResponseAtPosition(Integer position), welche dir die optionen
        // (also z.b. "Die Vorlesung war gut", "Die Vorlesung war ok", "Die Vorlesung
        // war schlecht") als List zurück gibt. Das brauchts du für die MultipleChoice
        // und SingleChoiceResponse dinger, um die optionen darzustellen. Dazu hat jede
        // AbstractResponse, die superclass von den BinaryResponse, TextResponse, etc.
        // eine methode getQuestion(), welche dir die frage zu der response liefert.
        // Beide methoden brauchen die position der frage in der survey, welche bei 0
        // startet. Essentially musst du dir alle werte holen, zusammenrechnen, und dann
        // in den JSONArray packen, um ihn dann im frontend zu bearbeiten.

        JSONArray jsonArray = new JSONArray();
        return jsonArray;
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

    /**
     * @param id the response id
     * @return an {@linkplain Optional} of an {@linkplain SurveyResponse} with the
     *         given id
     */
    public Optional<SurveyResponse> findSurveyResponseById(long id) {
        return surveyResponseRepository.findById(id);
    }

    /**
     * 
     * @param course         {@linkplain Course} object
     * @param type           {@linkplain CourseType}
     * @param groupNumber    The number of the corresponding tutorial or seminar (=1
     *                       in case CourseType is Lecture)
     * @param instanceNumber The number corresponding to the instance of the Course.
     * 
     * @return an Iterable containing all Responses that fit criteria
     */
    public Optional<SurveyResponse> findSurveyResponseByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(
            Course course, CourseType type, Integer groupNumber, Integer instanceNumber) {
        return surveyResponseRepository.findByCourseAndCourseTypeAndGroupNumberAndInstanceNumber(course, type,
                groupNumber, instanceNumber);
    }

    /**
     * 
     * @param course      {@linkplain Course} object
     * @param type        {@linkplain CourseType}
     * @param groupNumber The number of the corresponding tutorial or seminar (=1 in
     *                    case CourseType is Lecture)
     * 
     * @return an Iterable containing all Responses that fit criteria
     */
    public Iterable<SurveyResponse> findSurveyResponseByCourseAndCourseTypeAndGroupNumber(Course course,
            CourseType type, Integer groupNumber) {
        return surveyResponseRepository.findByCourseAndCourseTypeAndGroupNumber(course, type, groupNumber);
    }

    /**
     * 
     * @param course         {@linkplain Course} object
     * @param type           {@linkplain CourseType}
     * @param instanceNumber The number corresponding to the instance of the Course.
     * 
     * @return an Iterable containing all Responses that fit criteria
     */
    public Iterable<SurveyResponse> findSurveyResponseByCourseAndCourseTypeAndInstanceNumber(Course course,
            CourseType type, Integer instanceNumber) {
        return surveyResponseRepository.findByCourseAndCourseTypeAndInstanceNumber(course, type, instanceNumber);
    }

    /**
     * 
     * @param course {@linkplain Course} object
     * @param type   {@linkplain CourseType}
     * 
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

        String surveyJson = "[{\"type\":\"SingleChoice\",\"question\":\"Hat die Vorlesung Wissen vermittelt, welches du dir nicht im Selbststudium hättest erarbeiten können?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"],\"default\":\"true\",\"default\":\"true\"},{\"type\":\"SingleChoice\",\"question\":\"Hat der/die Vorlesende den aktiven Austausch mit den Studierenden gesucht?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"],\"default\":\"true\"},{\"type\":\"SingleChoice\",\"question\":\"Waren die Anforderung dem Wissensstand der Studierenden angemessen?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"],\"default\":\"true\"},{\"type\":\"SingleChoice\",\"question\":\"Konnte die Vorlesung gezielt Schwerpunkte setzen und Struktur vermitteln?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"],\"default\":\"true\"},{\"type\":\"SingleChoice\",\"question\":\"Konnte der/die Vorlesende dein Interesse an dem Thema wecken?\",\"answers\":[\"1\",\"2\",\"3\",\"4\",\"5\"],\"default\":\"true\"},{\"type\":\"SingleChoice\",\"question\":\"Online Lehre v.s. Präsenzveranstaltung\",\"answers\":[\"Die Vorlesung war digital und soll digital bleiben.\",\"Die Vorlesung war digital und wäre als Präsenzveranstaltung besser.\",\"Die Vorlesung war eine Präsenzveranstaltung und soll eine bleiben.\",\"Die Vorlesung war eine Präsenzveranstaltung und sollte digital werden.\"],\"default\":\"true\"}},{\"type\":\"FreeText\",\"question\":\"An dieser Stelle würden wir uns über konstruktive Kritik, aber auch über Anregungen und Lob freuen!\",\"default\":\"true\"}]";

        createSurveyResponse(new JSONArray(surveyJson), course, CourseType.LECTURE);

        Iterable<SurveyResponse> responses = findSurveyResponseByCourseAndCourseType(course, CourseType.LECTURE);

        for (SurveyResponse resp : responses) {
            for (int i = 0; i < 50; i++) {
                String id = UUID.randomUUID().toString();
                resp.addStundentIdToSubmissionListAndIncrementCounter(id);
                List<AbstractResponse> listOfResponses = resp.getListOfResponses();
                for (AbstractResponse ar : listOfResponses) {
                    // Continue this later
                }
            }
        }

    }

}