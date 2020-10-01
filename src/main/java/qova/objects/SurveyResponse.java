package qova.objects;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.ElementCollection;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;

import qova.enums.CourseType;

@Entity
public class SurveyResponse {

    // -----------------------------------------------------------------------
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;

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

    public Long getId() {
        return this.id;
    }

    public Course getCourse() {
        return this.course;
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

}