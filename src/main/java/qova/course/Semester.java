package qova.course;

import java.time.LocalDate;

public class Semester{
    
    private SemesterType type;

    private LocalDate date;

    private String semester;


    public Semester(SemesterType type, LocalDate date){
        this.type = type;
        this.date = date;
        this.semester =  semesterToString();
    }

    public String semesterToString(){
        return this.type.toString() + " " + String.valueOf(this.date.getYear());
    }

    public SemesterType getSemesterType(){
        return this.type;
    }

    public void setSemesterType(SemesterType type){
        this.type = type;
        this.semester = semesterToString();
    }

    public LocalDate getDate(){
        return this.date;
    }

    public void setDate(LocalDate date){
        this.date = date;
        this.semester = semesterToString();
    }

    public String getSemester(){
        return this.semester;
    }

}