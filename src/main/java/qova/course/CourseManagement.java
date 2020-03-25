package qova.course;

import java.util.Objects;

public class CourseManagement {
    
    private final CourseRepository courses;

    public CourseManagement(CourseRepository courses){
        this.courses = Objects.requireNonNull(courses);
    }

    public void addCourse(Course course){
        courses.save(course);
    }

    
}