package qova.course;

import qova.course.Course;

import org.springframework.stereotype.Repository;

import java.time.LocalDate;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface CourseRepository extends CrudRepository <Course, String> {
    
    public Iterable<Course> findBySemesterDateAfter(LocalDate date);

    public Iterable<Course> findBySemesterDateBefore(LocalDate date);

    public Iterable<Course> findBySemesterDateBetween(LocalDate date, LocalDate date2);

}


