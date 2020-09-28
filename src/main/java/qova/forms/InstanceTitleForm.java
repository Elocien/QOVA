package qova.forms;

import java.util.List;

public class InstanceTitleForm {

    // String Arrays containing titles

    private List<String> lectureInstanceTitles;
    private List<String> tutorialInstanceTitles;
    private List<String> seminarInstanceTitles;
    private List<String> practicalInstanceTitles;

    public InstanceTitleForm(List<String> lectureTitles, List<String> tutorialTitles, List<String> seminarTitles,
            List<String> practicalTitles) {
        this.lectureInstanceTitles = lectureTitles;
        this.tutorialInstanceTitles = tutorialTitles;
        this.seminarInstanceTitles = seminarTitles;
        this.practicalInstanceTitles = practicalTitles;
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