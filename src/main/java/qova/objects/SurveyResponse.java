package qova.objects;

import java.util.*;

import javax.persistence.*;

import org.hibernate.annotations.GenericGenerator;

import org.json.JSONArray;
import org.json.JSONObject;
import qova.enums.CourseType;

@Entity
public class SurveyResponse {

    // -----------------------------------------------------------------------
    @Id
    @GeneratedValue(generator = "UUID")
    @GenericGenerator(name = "UUID", strategy = "org.hibernate.id.UUIDGenerator")
    @Column(name = "id", updatable = false, nullable = false, columnDefinition = "BINARY(16)")
    private UUID id;

    // Course and CourseType are used for more detailed search purposes (primarily
    // when compiling responses into results pdf)
    @ManyToOne
    private Course course;

    // Enum, used for retrieval from repository
    private CourseType courseType;

    // The tutorial group number of the SurveyResponse
    private Integer groupNumber;

    // The instance number of the SurveyResponse
    private Integer instanceNumber;


    // The ID's of all of the people that have submitted to this survey
    @ElementCollection
    private List<String> listOfStudentsThatSubmitted;

    @ElementCollection
    @OneToMany
    @OrderColumn
    private List<AbstractResponse> listOfResponses;

    // Needed for JPA puposes
    @SuppressWarnings("unused")
    protected SurveyResponse() {
    }

    /**
     * A {@linkplain SurveyResponse} represents the data that is collected for unique paring of group and instance, for a
     * given {@linkplain Course}. E.g. If a course has 4 groups and 3 instances, then there are 12 {@linkplain SurveyResponse}'s,
     * one for each unique pairing. A SurveyResponse tracks who submitted to the surveyResponse
     *
     * @param course The {@linkplain Course} the SurveyResponse is associated to
     * @param type The {@linkplain CourseType}
     * @param instanceNumber The instance number of this particular SurveyResults Object. Used for retrieval in the repository,
     *                       as well as submission to the responses from {@link #listOfResponses}
     * @param groupNumber The group number of this particular SurveyResults Object. Used for retrieval in the repository,
     *      *                       as well as submission to the responses from {@link #listOfResponses}
     * @param listOfResponses A list of {@linkplain AbstractResponse}'s, which belong to this SurveyResponse. When a user submits,
     *                        the SurveyResponse is found using the {@linkplain Course} and group and instance numbers. Then the
     *                        {{@link #listOfResponses}} is iterated through and the according values incremented. 
     */
    public SurveyResponse(Course course, CourseType type, Integer instanceNumber, Integer groupNumber,
            List<AbstractResponse> listOfResponses) {
        this.course = course;
        this.courseType = type;
        this.instanceNumber = instanceNumber;
        this.groupNumber = groupNumber;
        this.listOfStudentsThatSubmitted = new ArrayList<>();
        this.listOfResponses = listOfResponses;
    }

    public UUID getId() {
        return this.id;
    }

    public Course getCourse() {
        return this.course;
    }

    public CourseInstance getCourseInstance() {
        return this.course.getInstance(courseType);
    }

    public CourseType getCourseType() {
        return this.courseType;
    }

    public Integer getGroupNumber() {
        return this.groupNumber;
    }

    public Integer getInstanceNumber() {
        return this.instanceNumber;
    }

    public List<String> getListOfStudentsThatSubmitted() {
        return this.listOfStudentsThatSubmitted;
    }

    public Integer getNumberOfSubmissions() {
        return this.listOfStudentsThatSubmitted.size();
    }

    public void addStundentIdToSubmissionListAndIncrementCounter(String id) {
        this.listOfStudentsThatSubmitted.add(id);
    }

    public List<AbstractResponse> getListOfResponses() {
        return this.listOfResponses;
    }

    // Survey related fields

    // We assume a JSONArray can be created without exception, as this is checked
    // when a created survey is submitted
    public String getQuestionTextForQuestionAtPosition(Integer position) {

        // Concatenate the default survey to the customised one
        JSONArray jsonArray = new JSONArray(conactenateSurveytoDefault());

        return jsonArray.getJSONObject(position).getString("question");
    }

    /**
     * Returns a {@link List} of response options (i.e. the options a user is able to pick from, for a given singleChoice question).
     *
     * @param position The position of the response, in the survey (starting from 0, as represented in the surveyArray)
     * @return A List of options
     */
    public List<String> getOptionsForResponseAtPosition(Integer position) {

        JSONArray jsonArray = new JSONArray(conactenateSurveytoDefault());
        JSONObject jsonObject;
        try {
            jsonObject = jsonArray.getJSONObject(position);
        } catch (Exception e) {
            return new ArrayList<>();
        }

        if (jsonObject.getString("type").equals("OnetoFive")) {
            return new ArrayList<>(Arrays.asList("1", "2", "3", "4", "5"));
        } else if (jsonObject.getString("type").equals("SingleChoice")) {
            JSONArray answerOptions = jsonObject.getJSONArray("answers");

            // Array of all possibilieties, passed to the constructor of the
            // MultipleChoiceResponse
            ArrayList<String> singleChoiceOptions = new ArrayList<>(answerOptions.length());

            for (int j = 0; j < answerOptions.length(); j++) {
                singleChoiceOptions.add(answerOptions.getString(j));
            }

            return singleChoiceOptions;
        }

        else {
            return new ArrayList<>();
        }
    }

    private String conactenateSurveytoDefault(){
        // Get the default Survey
        String defaultSurvey = getCourseInstance().getDefaultSurvey().getDefaultSurveyJson();

        // Concatenate the default survey to the customised one

        return defaultSurvey.substring(0, defaultSurvey.length() - 1) + ","
                + getCourseInstance().getSurvey().substring(1);
    }

}