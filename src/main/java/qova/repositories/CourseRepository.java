package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.Course;

import java.time.LocalDate;
import java.util.UUID;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface CourseRepository extends CrudRepository <Course, UUID> {
    
    public Iterable<Course> findBycourseDateAfter(LocalDate date);

    public Iterable<Course> findBycourseDateBefore(LocalDate date);

    public Iterable<Course> findBycourseDateBetween(LocalDate date, LocalDate date2);

}


