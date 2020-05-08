package qova.course;

import qova.course.Course;

import org.springframework.stereotype.Repository;

import java.time.LocalDate;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface CourseRepository extends CrudRepository <Course, String> {
    
    public Iterable<Course> findBycourseInstanceAfter(LocalDate date);

    public Iterable<Course> findBycourseInstanceBefore(LocalDate date);

    public Iterable<Course> findBycourseInstanceBetween(LocalDate date, LocalDate date2);

}


