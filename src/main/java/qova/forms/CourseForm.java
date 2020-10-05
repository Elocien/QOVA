package qova.forms;

import javax.validation.constraints.NotNull;

import qova.enums.CourseFaculty;
import qova.enums.CourseType;

public class CourseForm {

    // name
    private String name;

    // Existance of each CourseInstance (CourseType)
    @NotNull
    private Boolean lectureExists;
    @NotNull
    private Boolean tutorialExists;
    @NotNull
    private Boolean seminarExists;
    @NotNull
    private Boolean practicalExists;

    @NotNull
    private Integer groupAmountLecture;
    @NotNull
    private Integer groupAmountTutorial;
    @NotNull
    private Integer groupAmountSeminar;
    @NotNull
    private Integer groupAmountPractical;

    @NotNull
    private Integer instanceAmountLecture;
    @NotNull
    private Integer instanceAmountTutorial;
    @NotNull
    private Integer instanceAmountSeminar;
    @NotNull
    private Integer instanceAmountPractical;

    // 'Intended' Semester of Students partaking in the given Subject
    @NotNull
    private Integer semesterOfStudents;

    @NotNull
    private CourseFaculty faculty;

    // Taken as String from model (in the form: "SoSe xx" | "WiSe xx/yy")
    @NotNull
    private String semesterString;

    // Constructor
    public CourseForm(String name, Boolean lectureExists, Boolean tutorialExists, Boolean seminarExists,
            Boolean practicalExists, Integer groupAmountLecture, Integer groupAmountTutorial,
            Integer groupAmountSeminar, Integer groupAmountPractical, Integer instanceAmountLecture,
            Integer instanceAmountTutorial, Integer instanceAmountSeminar, Integer instanceAmountPractical,
            Integer semesterOfStudents, CourseFaculty faculty, String semesterString) {
        this.name = name;
        this.lectureExists = lectureExists;
        this.tutorialExists = tutorialExists;
        this.seminarExists = seminarExists;
        this.practicalExists = practicalExists;
        this.groupAmountLecture = groupAmountLecture;
        this.groupAmountTutorial = groupAmountTutorial;
        this.groupAmountSeminar = groupAmountSeminar;
        this.groupAmountPractical = groupAmountPractical;
        this.instanceAmountLecture = instanceAmountLecture;
        this.instanceAmountTutorial = instanceAmountTutorial;
        this.instanceAmountSeminar = instanceAmountSeminar;
        this.instanceAmountPractical = instanceAmountPractical;
        this.semesterOfStudents = semesterOfStudents;
        this.faculty = faculty;
        this.semesterString = semesterString;
    }

    // GETTERS

    public String getName() {
        return this.name;
    }

    public Boolean getInstanceExists(CourseType type) {
        switch (type) {
            case LECTURE:
                return this.lectureExists;
            case TUTORIAL:
                return this.tutorialExists;
            case SEMINAR:
                return this.seminarExists;
            case PRACTICAL:
                return this.practicalExists;
            default:
                return false;
        }
    }

    public Integer getGroupAmount(CourseType type) {
        switch (type) {
            case LECTURE:
                return this.groupAmountLecture;
            case TUTORIAL:
                return this.groupAmountTutorial;
            case SEMINAR:
                return this.groupAmountSeminar;
            case PRACTICAL:
                return this.groupAmountPractical;
            default:
                return 0;
        }
    }

    public Integer getInstanceAmount(CourseType type) {
        switch (type) {
            case LECTURE:
                return this.instanceAmountLecture;
            case TUTORIAL:
                return this.instanceAmountTutorial;
            case SEMINAR:
                return this.instanceAmountSeminar;
            case PRACTICAL:
                return this.instanceAmountPractical;
            default:
                return 0;
        }
    }

    public Boolean getLectureExists() {
        return this.lectureExists;
    }

    public Boolean getTutorialExists() {
        return this.tutorialExists;
    }

    public Boolean getSeminarExists() {
        return this.seminarExists;
    }

    public Boolean getPracticalExists() {
        return this.practicalExists;
    }

    public Integer getGroupAmountLecture() {
        return this.groupAmountLecture;
    }

    public Integer getGroupAmountTutorial() {
        return this.groupAmountTutorial;
    }

    public Integer getGroupAmountSeminar() {
        return this.groupAmountSeminar;
    }

    public Integer getGroupAmountPractical() {
        return this.groupAmountPractical;
    }

    public Integer getInstanceAmountLecture() {
        return this.instanceAmountLecture;
    }

    public Integer getInstanceAmountTutorial() {
        return this.instanceAmountTutorial;
    }

    public Integer getInstanceAmountSeminar() {
        return this.instanceAmountSeminar;
    }

    public Integer getInstanceAmountPractical() {
        return this.instanceAmountPractical;
    }

    public Integer getSemesterOfStudents() {
        return this.semesterOfStudents;
    }

    public CourseFaculty getFaculty() {
        return this.faculty;
    }

    public String getSemesterString() {
        return this.semesterString;
    }

}