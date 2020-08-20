package qova.forms;

public class InstanceTitleForm {
    
    //String Arrays containing titles

    private String[] lectureInstanceTitles;
    private String[] tutorialInstanceTitles;
    private String[] seminarInstanceTitles;
    private String[] practicalInstanceTitles;


    public InstanceTitleForm(String[] lectureTitles, String[] tutorialTitles, String[] seminarTitles, String[] practicalTitles){
        this.lectureInstanceTitles = lectureTitles;
        this.tutorialInstanceTitles = tutorialTitles;
        this.seminarInstanceTitles = seminarTitles;
        this.practicalInstanceTitles = practicalTitles;
    }

    public String[] getLectureInstanceTitles(){
        return this.lectureInstanceTitles;
    }

    public String[] getTutorialInstanceTitles(){
        return this.tutorialInstanceTitles;
    }

    public String[] getSeminarInstanceTitles(){
        return this.seminarInstanceTitles;
    }

    public String[] getPracticalInstanceTitles(){
        return this.practicalInstanceTitles;
    }
}