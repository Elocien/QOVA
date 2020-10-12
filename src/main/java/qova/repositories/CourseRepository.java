package qova.repositories;

import org.springframework.stereotype.Repository;

import qova.objects.Course;

import java.time.LocalDate;
import java.util.UUID;

import org.springframework.data.repository.CrudRepository;

@Repository
public interface CourseRepository extends CrudRepository <Course, UUID> {
    
    Iterable<Course> findByCourseDateAfter(LocalDate date);

    Iterable<Course> findByCourseDateBefore(LocalDate date);

    Iterable<Course> findByCourseDateBetween(LocalDate date, LocalDate date2);

}


