package qova.forms;

import qova.objects.Course;

import javax.validation.constraints.NotNull;
import java.util.UUID;

/**
 * Used to copy a {@linkplain Course} object and assign it a new semester. Primary use case is {@linkplain qova.users.User}s with the user role "STAFF"
 * being able to duplicate a previous course for the current year. For example: the course Trigonometry 1 was offered in the winter semester of 2020, and
 * is being offered again in the winter semester of 2021. Instead of having to manually set all fields again in course creation, the user can simply copy the course.
 * This is relevant in order to contain all results for a given semester.
 * <br><br>
 * Used in {@linkplain qova.logic.CourseController#duplicateCourseWithNewSemester(DuplicateCourseForm, UUID)}
 */
public class DuplicateCourseForm {
    //Taken as String from model (in the form: "SoSe xx" | "WiSe xx/yy")
    @NotNull private String semesterString;


    public DuplicateCourseForm(String semesterString){
        this.semesterString = semesterString;
    }

    public String getSemesterString(){
        return this.semesterString;
    }
}