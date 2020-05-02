package qova.course;

import qova.course.Course;

import org.springframework.stereotype.Repository;

import java.time.LocalDate;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface CourseRepository extends CrudRepository <Course, String> {
    
    public Iterable<Course> findByCreationDateTimeAfter(LocalDate date);

    public Iterable<Course> findByCreationDateTimeBefore(LocalDate date);

    public Iterable<Course> findByCreationDateTimeBetween(LocalDate date, LocalDate date2);

}


