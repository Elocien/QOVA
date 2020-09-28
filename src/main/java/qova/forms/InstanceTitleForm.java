package qova.forms;

import java.util.List;

public class InstanceTitleForm {

    // String Arrays containing titles

    private List<String> lectureInstanceTitles;
    private List<String> tutorialInstanceTitles;
    private List<String> seminarInstanceTitles;
    private List<String> practicalInstanceTitles;

    public InstanceTitleForm(List<String> lectureInstanceTitles, List<String> tutorialInstanceTitles,
            List<String> seminarInstanceTitles, List<String> practicalInstanceTitles) {
        this.lectureInstanceTitles = lectureInstanceTitles;
        this.tutorialInstanceTitles = tutorialInstanceTitles;
        this.seminarInstanceTitles = seminarInstanceTitles;
        this.practicalInstanceTitles = practicalInstanceTitles;
    }

    public List<String> getLectureInstanceTitles() {
        return this.lectureInstanceTitles;
    }

    public List<String> getTutorialInstanceTitles() {
        return this.tutorialInstanceTitles;
    }

    public List<String> getSeminarInstanceTitles() {
        return this.seminarInstanceTitles;
    }

    public List<String> getPracticalInstanceTitles() {
        return this.practicalInstanceTitles;
    }
}