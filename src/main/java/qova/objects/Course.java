package qova.objects;

import java.time.LocalDate;
import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToOne;

//id-generator imports
import org.hibernate.annotations.GenericGenerator;

import jdk.jfr.BooleanFlag;
import qova.enums.CourseFaculty;
import qova.enums.CourseType;

//Terminology

//course types
//meaning either the lecture, tutorial, seminar or practical
//auf deutsch:       Vorlesung, übung , seminar oder praktika

/**
 * This object is an Instance of a Subject, for a given semester.
 */
@Entity
public class Course {

    @Id
    @GeneratedValue(generator = "UUID")
    @GenericGenerator(name = "UUID", strategy = "org.hibernate.id.UUIDGenerator")
    @Column(name = "id", updatable = false, nullable = false, columnDefinition = "BINARY(16)")
    private UUID id;

    // The user Id of the owner.
    private String ownerId;

    // Name of the course
    private String name;

    @ManyToOne
    private CourseInstance lecture;
    @ManyToOne
    private CourseInstance tutorial;
    @ManyToOne
    private CourseInstance seminar;
    @ManyToOne
    private CourseInstance practical;

    // The number representing the semester this course is taken by students
    private Integer semesterOfStudents;

    // Faculty the course belongs to
    @Enumerated
    private CourseFaculty faculty;

    // The date at which indicates to which semester the course belongs to (used to
    // find courses using repository methods)
    private LocalDate courseDate;

    // The string which is displayed in UI
    private String semesterString;

    // Flag used to indicate that the course is finalised and cannot be edited
    // anymore
    @BooleanFlag
    private Boolean finalisedFlag;

    // Needed for JPA purposes
    @SuppressWarnings("unused")
    protected Course() {
    }

    /**
     * Instance of a Course (meaning a Subject [DE Lehrveranstaltung]).
     * 
     * @param name               Name of the course
     * @param ownerId            The ajpPersistentId of the {@linkplain qova.users.User} who created the Course
     * @param lecture            CourseInstance with courseType LECTURE
     * @param tutorial           CourseInstance with courseType SEMINAR
     * @param seminar            CourseInstance with courseType TUTORIAL
     * @param practical          CourseInstance with courseType PRACTICAL
     * @param semesterOfStudents What is the semester of the students attending the
     *                           subject
     * @param faculty            Enum defining which faculty the subject belongs to
     * @param semesterString     The string displaying the semester (an instance of
     *                           a course. E.g. algorithms 1 is offered each year,
     *                           and this is the instance of Summer semester 2020).
     *                           This field is primarily used in the UI
     * @param courseDate         The time period (start date) of when the course
     *                           takes place. This field is primarily used for
     *                           sorting purposes
     */
    public Course(String name, String ownerId, CourseInstance lecture, CourseInstance tutorial, CourseInstance seminar,
            CourseInstance practical, Integer semesterOfStudents, CourseFaculty faculty, String semesterString,
            LocalDate courseDate) {
        this.name = name;
        this.ownerId = ownerId;
        this.lecture = lecture;
        this.seminar = seminar;
        this.tutorial = tutorial;
        this.practical = practical;
        this.semesterOfStudents = semesterOfStudents;
        this.faculty = faculty;
        this.semesterString = semesterString;
        this.courseDate = courseDate;
        this.finalisedFlag = false;
    }

    public UUID getId() {
        return this.id;
    }

    public String getName() {
        return this.name;
    }

    public String getOwnerId(){
        return this.ownerId;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Boolean getLectureExists() {
        return (Boolean.TRUE.equals(getLecture().isActive()));
    }

    public Boolean getTutorialExists() {
        return (Boolean.TRUE.equals(getTutorial().isActive()));
    }

    public Boolean getSeminarExists() {
        return (Boolean.TRUE.equals(getSeminar().isActive()));
    }

    public Boolean getPracticalExists() {
        return (Boolean.TRUE.equals(getPractical().isActive()));
    }

    public CourseInstance getLecture() {
        return this.lecture;
    }

    public CourseInstance getTutorial() {
        return this.tutorial;
    }

    public CourseInstance getSeminar() {
        return this.seminar;
    }

    public CourseInstance getPractical() {
        return this.practical;
    }

    /**
     * Returns the {@linkplain CourseInstance} of the given {@linkplain CourseType} for this course.
     *
     * @param type The {@linkplain CourseType}.
     * @return The {@linkplain CourseInstance} of the given {@linkplain CourseType}.
     */
    public CourseInstance getInstance(CourseType type) {
        switch (type) {
            case LECTURE:
                return this.lecture;
            case TUTORIAL:
                return this.tutorial;
            case SEMINAR:
                return this.seminar;
            case PRACTICAL:
                return this.practical;
            default:
                return null;
        }
    }

    /**
     * Returns a {@link Boolean} which indicates whether the user has decided to use the given Instance.
     * For example, a user may want to receive feedback for the Lecture and the Tutorial, but does not run a Practical or
     * Seminar for that given course. He will then create the course, and this method will return true for the
     * {@linkplain CourseType}'s Lecture and Tutorial, but false for Seminar and Practical
     *
     * @param type The {@linkplain CourseType}.
     * @return A {@link Boolean} depending on the isActive flag of the {@linkplain CourseInstance}
     */
    public Boolean getInstanceExists(CourseType type) {
        switch (type) {
            case LECTURE:
                return (Boolean.TRUE.equals(getLecture().isActive()));
            case TUTORIAL:
                return (Boolean.TRUE.equals(getTutorial().isActive()));
            case SEMINAR:
                return (Boolean.TRUE.equals(getSeminar().isActive()));
            case PRACTICAL:
                return (Boolean.TRUE.equals(getPractical().isActive()));
            default:
                return false;
        }
    }

    public int getSemesterOfStudents() {
        return this.semesterOfStudents;
    }

    public void setSemesterOfStudents(int semester) {
        this.semesterOfStudents = semester;
    }

    public CourseFaculty getFaculty() {
        return this.faculty;
    }

    public void setFaculty(CourseFaculty faculty) {
        this.faculty = faculty;
    }

    public LocalDate getCourseDate() {
        return this.courseDate;
    }

    public void setCourseDate(LocalDate date) {
        this.courseDate = date;
    }

    public String getSemesterString() {
        return this.semesterString;
    }

    public void setSemesterString(String str) {
        this.semesterString = str;
    }

    public Boolean getFinalisedFlag() {
        return this.finalisedFlag;
    }

    public void setCourseAsFinalised() {
        this.finalisedFlag = true;
    }

}
