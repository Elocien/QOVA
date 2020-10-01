package qova.objects;

import java.time.LocalDate;
import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.Temporal;

//id-generator imports
import org.hibernate.annotations.GenericGenerator;

import jdk.jfr.BooleanFlag;
import qova.enums.CourseFaculty;
import qova.enums.CourseType;

//Terminology

//course types
//meaning either the lecture, tutorial, seminar or practical
//auf deutsch:       Vorlesung, übung , seminar oder praktika

//

@Entity
public class Course {

    @Id
    @GeneratedValue(generator = "UUID")
    @GenericGenerator(name = "UUID", strategy = "org.hibernate.id.UUIDGenerator")
    @Column(name = "id", updatable = false, nullable = false, columnDefinition = "BINARY(16)")
    private UUID id;

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
     * Instance of a Course (meaning a Subject [DE Lehrveranstaltung])
     * 
     * @param name               Name of the course
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
    public Course(String name, CourseInstance lecture, CourseInstance tutorial, CourseInstance seminar,
            CourseInstance practical, Integer semesterOfStudents, CourseFaculty faculty, String semesterString,
            LocalDate courseDate) {
        this.name = name;
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

    public void setLecture(CourseInstance lecture) {
        this.lecture = lecture;
    }

    public CourseInstance getLecture() {
        return this.lecture;
    }

    public void setTutorial(CourseInstance tutorial) {
        this.tutorial = tutorial;
    }

    public CourseInstance getTutorial() {
        return this.tutorial;
    }

    public void setSeminar(CourseInstance seminar) {
        this.seminar = seminar;
    }

    public CourseInstance getSeminar() {
        return this.seminar;
    }

    public void setPractical(CourseInstance practical) {
        this.practical = practical;
    }

    public CourseInstance getPractical() {
        return this.practical;
    }

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
