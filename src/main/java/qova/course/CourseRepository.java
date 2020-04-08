package qova.course;

import qova.course.Course;

import org.springframework.stereotype.Repository;
import org.springframework.data.repository.CrudRepository;

@Repository
public interface CourseRepository extends CrudRepository <Course, Long> {
   
}


