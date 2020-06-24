package qova.course;

import org.springframework.stereotype.Repository;

import java.time.LocalDate;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface CourseRepository extends CrudRepository <Course, String> {
    
    public Iterable<Course> findBycourseDateAfter(LocalDate date);

    public Iterable<Course> findBycourseDateBefore(LocalDate date);

    public Iterable<Course> findBycourseDateBetween(LocalDate date, LocalDate date2);

}


