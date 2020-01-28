package qova.course;

import java.util.Objects;

public class courseManagement {
    
    private final courseRepository courses;

    public courseManagement(courseRepository courses){
        this.courses = Objects.requireNonNull(courses);
    }

    public void addCourse(course course){
        courses.save(course);
    }

    
}