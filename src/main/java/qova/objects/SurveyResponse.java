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

    // Tracks the number of times stundents submitted
    private Integer numberOfSubmissions;

    // The ID's of all of the people that have submitted to this survey
    @ElementCollection
    @MapKeyColumn
    private Map<String, Date> listOfStudentsThatSubmitted;

    @ElementCollection
    @OneToMany
    private List<AbstractResponse> listOfResponses;

    // Needed for JPA puposes
    @SuppressWarnings("unused")
    protected SurveyResponse() {
    }

    // Constructor
    public SurveyResponse(Course course, CourseType type, Integer instanceNumber, Integer groupNumber,
            List<AbstractResponse> listOfResponses) {
        this.course = course;
        this.courseType = type;
        this.instanceNumber = instanceNumber;
        this.groupNumber = groupNumber;
        this.numberOfSubmissions = 0;
        this.listOfStudentsThatSubmitted = new HashMap<>();
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

    public Map<String, Date> getListOfStudentsAndDatesWithSubmissions() {
        return this.listOfStudentsThatSubmitted;
    }

    public List<String> getListOfStudentsThatSubmitted() {
        List<String> listOfStudentIds = new ArrayList<>();
        for (Map.Entry<String, Date> entry : listOfStudentsThatSubmitted.entrySet()) {
            listOfStudentIds.add(entry.getKey());
        }

        return listOfStudentIds;
    }

    public Integer getNumberOfSubmissions() {
        return this.numberOfSubmissions;
    }

    public void addStundentIdToSubmissionListAndIncrementCounter(String id) {
        this.listOfStudentsThatSubmitted.put(id, new Date());
        this.numberOfSubmissions++;
    }

    public List<AbstractResponse> getListOfResponses() {
        return this.listOfResponses;
    }

    //Survey related fields


    // We assume a JSONArray can be created without exception, as this is checked
    // when a created survey is submitted
    public String getQuestionTextForQuestionAtPosition(Integer position) {
        JSONArray jsonArray = new JSONArray(getCourseInstance().getSurvey());

        return jsonArray.getJSONObject(position).getString("question");
    }

    public List<String> getOptionsForResponseAtPosition(Integer position) {

        JSONArray jsonArray = new JSONArray(getCourseInstance().getSurvey());
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

}